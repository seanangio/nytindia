#' Write final nested dataset for shiny app
#'
#' Write final dataset, along with a sample, to the folder
#' that will house the app. The unnested df gets created in the app
#' so you don't need to create it here. It also writes the built-in
#' `govt` dataset to the same location.
#'
#' @param full_nested_df output of `nyt_re_nest_keywords()`
#' @param shiny_app_path name of shiny app folder
#'
#' @return written nested dataframe (and sample)
#' @export
#'
#' @examples
#' \dontrun{
#' nyt_write_final_dataset(full_nested_df)
#' }
#' @importFrom magrittr %>%
nyt_write_final_nested_df <- function(full_nested_df,
                                     shiny_app_path = "nyt_shiny_app") {

  if (!dir.exists(shiny_app_path)) {
    dir.create(shiny_app_path)
  }

  # write the full nested df to the correct folder
  # the unnested df is created in the app

  fn_nested_df <- here::here(shiny_app_path, "full_nested_df.rds")
  readr::write_rds(full_nested_df, fn_nested_df)

  # get a sample for testing
  nested_sample <- full_nested_df %>%
    dplyr::sample_n(1000)

  fn_nested_sample <- here::here(shiny_app_path, "nested_sample.rds")
  readr::write_rds(nested_sample, fn_nested_sample)

  # write the built-in govt dataset to the same folder
  fn_govt <- here::here(shiny_app_path, "govt.csv")
  readr::write_csv(govt, fn_govt)

  # unnest once more after having added new columns [do this in the app]
  # full_unnested_df <- full_nested_df %>%
  #   tidyr::unnest(keywords)

  #fn_unnested_df <- here::here(shiny_app_path, "full_unnested_df.rds")
  #readr::write_rds(full_unnested_df, fn_unnested_df)
  # unnested_sample <- nested_sample %>%
  #   tidyr::unnest(keywords)

  #fn_unnested_sample <- here::here(shiny_app_path, "unnested_sample.rds")

  #readr::write_rds(nested_sample, fn_nested_sample)


}
