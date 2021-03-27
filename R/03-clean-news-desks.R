#' Clean 'news_desk' values
#'
#' Values for 'news_desk' are often slightly different spellings of the same item.
#' There are too many instances to fix with a `case_when()` statement
#' so use the shiny app to create a lookup table.
#' Then join in the replacement values. If you don't have a file of
#' replacements in the "renamed_news_desks" folder, it will skip that step.
#'
#' You may not be able to write out the entire lookup table in one file
#' so you first need to bind together multiple files, if they exist.
#'
#' @param news_desk_output_path folder to find news desks post-cleaning
#' @return `nyt_clean_news_desks()` returns a nested df where news desk values have been consolidated based on the lookup table created from the shiny app
#' @export
#'
#' @examples
#' \dontrun{
#' nyt_clean_news_desks(combined_df)
#' }
#' @importFrom magrittr %>%
nyt_bind_news_desk_lookups <- function(news_desk_output_path = "renamed_news_desks") {

  files <- dir(path = news_desk_output_path,
               pattern = "*.rds", full.names = TRUE)

  files %>%
    purrr::map(readr::read_rds) %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble() %>%
    dplyr::distinct()
}

#' @export
#' @rdname nyt_bind_news_desk_lookups
#' @param combined_df output of `nyt_clean_api_tbl()`
#' @param news_desk_output_path folder to find news desks post-cleaning
nyt_clean_news_desks <- function(combined_df,
                                 news_desk_output_path = "renamed_news_desks") {

  if (!rlang::is_empty(list.files(news_desk_output_path))) {

    news_desk_table <- nyt_bind_news_desk_lookups()

    df_with_clean_news_desks <- combined_df %>%
      dplyr::left_join(news_desk_table,
                       by = c("news_desk" = "replaced_value")
      ) %>%
      dplyr::mutate(
        news_desk = dplyr::if_else(is.na(keeping),
                                   news_desk, keeping)
      ) %>%
      dplyr::select(-keeping)

  } else {

    df_with_clean_news_desks <- combined_df

  }

  df_with_clean_news_desks %>%
    dplyr::select(url, pub_date, headline,
                  news_desk, section, material, byline, abstract,
                  lead_paragraph, front_page, printed, keywords)
}
