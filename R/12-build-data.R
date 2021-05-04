#' Build data
#'
#' Once you have finished querying data from the API, this
#' function will send the folder of files retrieved from the
#' API through a pipeline of functions to produce the output
#' dataset to the shiny app. You can use this if several
#' manual data cleaning steps (renaming desks or keyword values),
#' as well as having already geocoded locations, are already
#' complete or are being skipped.
#'
#' @param desks_to_replace whether you need to rename desks
#' @param keyword_values_to_replace whether you need to rename keyword values
#' @param keyword_names_to_replace whether you need to rename keyword categories
#' @param glocations_to_code whether you need to geocode locations
#' @param ... any other arguments
#'
#' @return The full nested_df and govt.csv required as input to the shiny app
#' @export
#'
#' @examples
#' \dontrun{
#' nyt_build_data()
#' }
nyt_build_data <- function(desks_to_replace = FALSE,
                           keyword_values_to_replace = FALSE,
                           keyword_names_to_replace = FALSE,
                           glocations_to_code = FALSE,
                           ...
) {

  api_df <- nyt_bind_api_files(...)
  combined_df <- nyt_clean_api_tbl(api_df)

  if (isTRUE(desks_to_replace)) {
    return(unnested_df)
  }

  nested_df <- nyt_clean_news_desks(combined_df)

  unnested_df <- nyt_unnest_df(nested_df)

  if (isTRUE(keyword_values_to_replace)) {
    return(unnested_df)
  }

  consolidated_unnested_df <- nyt_clean_keywords(unnested_df)

  if (isTRUE(keyword_names_to_replace)) {
    return(consolidated_unnested_df)
  }

  unnested_df_values_fixed <- nyt_fix_keywords(consolidated_unnested_df)

  if (isTRUE(glocations_to_code)) {
    nyt_query_mapquest_api(unnested_df_values_fixed, ...)
  }

  full_unnested_df <- nyt_join_coords_countries(unnested_df_values_fixed)

  full_nested_df <- nyt_re_nest_keywords(full_unnested_df)

  nyt_write_final_nested_df(full_nested_df, ...)

  nyt_download_shiny_files()


}
