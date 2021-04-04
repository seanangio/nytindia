#' Query Mapquest API for lat-lon coordinates
#'
#' Many keyword values are "glocations". Here you write a file
#' of lat-lon coordinates for every unique glocation keyword.
#'
#' @param unnested_df_values_fixed unnested df containing all glocation keywords
#' @param mapquest_folder folder where to write mapquest output
#' @param api_key "MAPQUEST_KEY" environmental variable
#' @param nyt_user_agent "NYT_USER_AGENT" environmental variable
#'
#' @return file of glocation keywords and their lat-lon coordinates
#' @export
#'
#' @examples
#' \dontrun{
#' nyt_query_mapquest_api(unnested_df_values_fixed)
#' }
nyt_query_mapquest_api <- function(unnested_df_values_fixed,
                                   mapquest_folder = "mapquest",
                                   api_key = Sys.getenv("MAPQUEST_KEY"),
                                   nyt_user_agent = Sys.getenv("NYT_USER_AGENT")) {

  # get unique glocations for geocoding
  glocations <- unnested_df_values_fixed %>%
    dplyr::filter(name == "glocations") %>%
    dplyr::count(value, sort = TRUE) %>%
    dplyr::pull(value)

  # define query parameters -------------------------------------------------

  mapquest_api_url <- "http://www.mapquestapi.com/geocoding/v1/address"

  query_params <- list(
    key = api_key,
    location = "",
    maxResults = 1 # return only top result
  )

  # query mapquest geocoding api --------------------------------------------

  pages <- vector("list", length = length(glocations))

  for (i in seq_along(glocations)) {

    query_params$location <- glocations[i]

    request <- httr::GET(url = mapquest_api_url,
                         query = query_params,
                         httr::user_agent(nyt_user_agent))

    if (httr::http_error(request)) {

      print(httr::http_status(request)$message)
      break

    } else {

      response <- httr::content(request, as = "text") %>%
        jsonlite::fromJSON(flatten = TRUE)

      pages[[i]] <- response$results %>%
        dplyr::group_by(providedLocation.location) %>%
        tidyr::unnest(cols = c(locations))

      message(glue::glue("Retrieving {query_params$location}"))
      Sys.sleep(5)
    }
  }

  if (!dir.exists(mapquest_folder)) {
    dir.create(mapquest_folder)
  }

  fn <- here::here(mapquest_folder, "raw_mapquest.rds")
  readr::write_rds(pages, fn)

}
