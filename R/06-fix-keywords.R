#' Fix keywords
#'
#' After choosing a name for keywords that have more than one
#' in a spreadsheet, join those results back in. This step also
#' fixes cases where the same keyword is given more than one rank
#' (this is just an error in the data).
#'
#' @param consolidated_unnested_df output of `nyt_clean_keywords()`
#' @param multi_names_output_folder folder to find corrected keyword values csv
#' @return an unnested df with consolidated, non-duplicate keyword values and names
#' @export
#'
#' @examples
#' \dontrun{
#' unnested_df_values_fixed <- nyt_fix_keywords(consolidated_unnested_df)
#' }
nyt_fix_keywords <- function(consolidated_unnested_df,
                             multi_names_output_folder = "single_named_values") {

  if (!rlang::is_empty(list.files(multi_names_output_folder))) {

    fn <- here::here(multi_names_output_folder, "single_named_keywords.csv")
    revised_keyword_names <- readr::read_csv(fn) %>%
      dplyr::distinct()

    unnested_df_single_names <- consolidated_unnested_df %>%
      dplyr::left_join(revised_keyword_names, by = "value") %>%
      dplyr::mutate(
        name = dplyr::if_else(is.na(new_name), name, new_name)
      ) %>%
      dplyr::select(-new_name)

  } else {

    unnested_df_single_names <- consolidated_unnested_df

  }

  unnested_df_values_fixed <- unnested_df_single_names %>%
    # remove articles with a keyword like 'india' repeated many times with different rank
    dplyr::group_by_at(dplyr::vars(tidyselect::one_of(names(.)[names(.) != "rank"]))) %>%
    dplyr::filter(rank == min(rank)) %>%
    dplyr::ungroup() %>%
    #redo rankings after removing some repeated keywords
    dplyr::select(-rank) %>%
    dplyr::group_by(url) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    mutate(
      name = case_when(
        # keywords like "[country], relations with" should not be glocations
        name == "glocations" & str_detect(value, "relations") ~ "subject",
        # no use geocoding a keyword like "[some huge geo] areas"
        name == "glocations" & str_detect(value, "areas") ~ "subject",
        TRUE ~ name
      )
    )

  return(unnested_df_values_fixed)

}
