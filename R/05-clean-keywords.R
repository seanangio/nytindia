#' Clean keywords
#'
#' Once keywords (values) have been unnested, they need a lot of
#' cleaning using the same shiny app used for news desks. Once
#' you have joined in the replacement keywords, you also have to
#' ensure every keyword has one name (or category). Possible names
#' are "subjects", "persons", "glocations", "organizations", or
#' "creative_works". These cases are somewhat rare but simplify
#' network analysis. This file must be named "single_named_keywords.csv"
#' and found in the "single_named_values" folder under the top level
#' project folder.
#'
#' @export
#' @param values_output_path folder to find keyword values post-cleaning
#' @examples
#' \dontrun{
#' nyt_clean_keywords(unnested_df)
#' }
#' @importFrom magrittr %>%
nyt_bind_values_lookups <- function(values_output_path = "renamed_values") {

  files <- dir(path = values_output_path,
               pattern = "*.rds", full.names = TRUE)

  files %>%
    purrr::map(readr::read_rds) %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble() %>%
    dplyr::distinct()
}

#' @param unnested_df output of `nyt_unnest_df()`
#' @param values_output_path folder to find keyword values post-cleaning
#' @param multi_names_input_path folder to find keyword values with more than 1 name (category)
#' @param multi_names_output_path folder to save corrected keyword values csv
#' @return `nyt_clean_keywords()` returns an unnested df with
#' replaced keyword values and writes out a file of all keywords
#' with more than one name
#' @rdname nyt_bind_values_lookups
#' @export
nyt_clean_keywords <- function(unnested_df,
                               values_output_path = "renamed_values",
                               multi_names_input_path = "multi_named_values",
                               multi_names_output_path = "single_named_values") {

  if (!rlang::is_empty(list.files(values_output_path))) {

    values_table <- nyt_bind_values_lookups()

    consolidated_unnested_df <- unnested_df %>%
      dplyr::left_join(values_table,
                by = c("value" = "replaced_value")
      ) %>%
      dplyr::mutate(
        value = dplyr::if_else(is.na(keeping), value, keeping)
      ) %>%
      dplyr::select(-keeping)

  } else {

    consolidated_unnested_df <- unnested_df

  }
  # write out file of keywords that have more than 1 name
  keywords_with_different_names <- consolidated_unnested_df %>%
    dplyr::select(name, value) %>%
    dplyr::distinct(name, value) %>%
    dplyr::count(value, sort = TRUE) %>%
    dplyr::filter(n > 1) %>%
    dplyr::select(value) %>%
    dplyr::mutate(new_name = NA_character_)

  if (!dir.exists(multi_names_input_path)) {
    dir.create(multi_names_input_path)
  }

  fn <- here::here(multi_names_input_path, "multi_named_keywords.csv")
  readr::write_rds(keywords_with_different_names, fn)

  if (!dir.exists(multi_names_output_path)) {
    dir.create(multi_names_output_path)
    # place corrected sheet here
  }

  return(consolidated_unnested_df)

}
