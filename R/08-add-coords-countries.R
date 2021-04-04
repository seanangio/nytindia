#' Join in coordinates and country status for location keywords
#'
#' Using the Mapquest API, you found lat-lon coordinates for every
#' glocation keyword. Join in those results to the unnested df.
#' Use a list of countries to identify which glocation keywords
#' are countries.
#'
#' @param unnested_df_values_fixed output of `nyt_fix_keywords()`
#' @param mapquest_folder where mapquest API output is found
#'
#' @return unnested df with lat-lon coords for every glocation keyword
#' @export
#'
#' @examples
#' \dontrun{
#' full_unnested_df <- nyt_join_coords_countries(unnested_df_values_fixed)
#' }
#' @importFrom magrittr %>%
nyt_join_coords_countries <- function(unnested_df_values_fixed,
                                      mapquest_folder = "mapquest") {

  # check if geocoding is done
  if (!rlang::is_empty(list.files(mapquest_folder))) {

    # prepare raw mapquest results
    fn <- here::here(mapquest_folder, "raw_mapquest.rds")
    glocations_table <- readr::read_rds(fn) %>%
      jsonlite::rbind_pages() %>%
      dplyr::rename(
        glocation = providedLocation.location,
        lat = latLng.lat,
        lon = latLng.lng
      ) %>%
      dplyr::select(glocation, lat, lon) %>%
      tibble::as_tibble()

  } else {

    glocations_table <- tibble::tibble(glocation = NA_character_,
                                       lat = NA_real_,
                                       lon = NA_real_)
  }


  unnested_df_coords <- unnested_df_values_fixed %>%
    dplyr::left_join(glocations_table,
                     by = c("value" = "glocation"))

  # create a country YES NO column
  # countries found in data-raw
  full_unnested_df <- unnested_df_coords %>%
    dplyr::mutate(country = dplyr::if_else(value %in% countries$country,
                                           TRUE, FALSE))

  return(full_unnested_df)
}
