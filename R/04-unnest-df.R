#' Unnest keyword values from nested df
#'
#' Up until now, each row is an article with keywords stored
#' as a nested dataframe. Unnesting keywords makes it easier
#' to wrangle those values. This function also writes out
#' a file of keyword values to be used as input to the
#' lookup table shiny app and creates a folder for replacements.
#' After running this function, run `nyt_run_example("lookup_table_app")`.
#' Upload the file in the "values_input_folder". Place the
#' output in the folder "values_output_folder".
#'
#' @param nested_df output from `nyt_clean_news_desks()`
#' @param max_keyword_length somewhat arbitrarily set to 200 for long strings that aren't really keywords
#' @param values_input_folder folder name to write keyword values pre-cleaning
#' @param values_output_folder folder to find keyword values post-cleaning
#' @return unnested data frame where each row is a keyword in an article instead of each row being an article
#' @export
#'
#' @examples
#' \dontrun{
#' unnested_df <- nyt_unnest_df(nested_df)
#' }
#' @importFrom magrittr %>%
nyt_unnest_df <- function(nested_df,
                          max_keyword_length = 200,
                          values_input_folder = "values_lookup_input",
                          values_output_folder = "renamed_values") {

  unnested_df <- nested_df %>%
    tidyr::unnest(keywords) %>%
    dplyr::select(-major) %>% # unclear what it is but all values are N
    dplyr::mutate(
      value = stringr::str_trim(stringr::str_to_lower(value)),
      value = stringr::str_replace(value, " ,", ","),
      # some values are very long (ie not keywords)
      value = dplyr::if_else(stringr::str_length(value) >= max_keyword_length,
                             NA_character_, value)
    )

  # like with news desks, too many inconsistencies to fix keywords
  # with a `case_when()` so use the same shiny app approach
  keyword_values <- unnested_df %>%
    dplyr::select(value)

  if (!dir.exists(values_input_folder)) {
    dir.create(values_input_folder)
  }

  fn <- here::here(values_input_folder, "keyword_values.rds")
  readr::write_rds(keyword_values, fn)

  if (!dir.exists(values_output_folder)) {
    dir.create(values_output_folder)
    # place lookup table output here
  }

  return(unnested_df)
}
