#' Write final nested dataset for shiny app
#'
#' Write final dataset, along with a sample, to the folder
#' that will house the app. The unnested df gets created in the app
#' so you don't need to create it here. It also writes the built-in
#' `govt` dataset to the same location. Once you have those datasets,
#' copy-paste into the same folder the files global.R, server.R,
#' ui.R, about.md, and the www directory found in the
#' inst/examples/nyt_india_app folder.
#'
#' @param full_nested_df output of `nyt_re_nest_keywords()`
#' @param shiny_app_folder name of shiny app folder
#' @param sample whether to write a smaller sample file from full_nested_df
#' @param sample_pct percentage of rows to use in the sample
#'
#' @return written nested dataframe (and sample)
#' @export
#'
#' @examples
#' \dontrun{
#' nyt_write_final_nested_df(full_nested_df)
#' }
#' @importFrom magrittr %>%
nyt_write_final_nested_df <- function(full_nested_df,
                                     shiny_app_folder = "nyt_shiny_app",
                                     sample = FALSE,
                                     sample_pct = 0.1) {

  if (!dir.exists(shiny_app_folder)) {
    dir.create(shiny_app_folder)
  }

  # write the full nested df to the correct folder
  # the unnested df is created in the app itself

  fn_nested_df <- here::here(shiny_app_folder, "full_nested_df.rds")
  readr::write_rds(full_nested_df, fn_nested_df)

  # write the built-in govt dataset to the same folder
  fn_govt <- here::here(shiny_app_folder, "govt.csv")
  readr::write_csv(govt, fn_govt)

  # get a sample for testing the shiny app
  if (isTRUE(sample)) {
    n_sample_rows <- nrow(full_nested_df) * sample_pct

    nested_sample <- full_nested_df %>%
      dplyr::sample_n(n_sample_rows)

    fn_nested_sample <- here::here(shiny_app_folder, "nested_sample.rds")
    readr::write_rds(nested_sample, fn_nested_sample)

  }
}
