#' Query India-tagged articles from the New York Times Article Search API
#'
#' To query data, call `nyt_get_data()` in the top level folder
#' of a new RStudio project. If no beginning and end dates
#' are provided, it uses 1855 or the most recent date in the folder
#' as the beginning and today as the end date. You need
#' environmental variables called"NYTIMES_KEY" for your API key
#' and "NYT_USER_AGENT" for a user agent.
#'
#' `nyt_get_data()` first calls `nyt_set_query_params()`. No
#' arguments are required unless using a different query.
#'
#' It then calls `nyt_set_time_chunks()`. It inherits the
#' dates from `nyt_get_data()`.
#'
#' `nyt_get_data()` feeds the output of `nyt_set_query_params()`
#' and `nyt_set_time_chunks()` to `nyt_query_api()`, which
#' writes API data to a folder called "api_data" in monthly increments
#' stored as rds files.
#'
#' @param begin_date character vector in YYYY-MM-DD format
#' @param end_date character vector in YYYY-MM-DD format; must be greater than begin_date
#' @param ... any other arguments (e.g. if changing the query params)
#' @export
#'
#' @examples
#' \dontrun{
#' # from 1855 (if api_data folder) or last found file to today
#' nyt_get_data()
#'
#' # from 2010 to today
#' nyt_get_data(begin_date = "2010-01-01")
#'
#' # from 2010 to 2012
#' nyt_get_data(begin_date = "2010-01-01", end_date = "2012-01-01")
#' }
nyt_get_data <- function(begin_date = NULL,
                         end_date = NULL, ...) {

  query_params <- nyt_set_query_params()
  time_chunks <- nyt_set_time_chunks(begin_date, end_date)

  nyt_query_api(query_params, time_chunks)
}

#' @rdname nyt_get_data
#' @param q empty by default
#' @param fq NYT articles with an India glocation tag
#' @param sort_order set to oldest; alternative is newest
#' @param api_key "NYTIMES_KEY" environmental variable for API key
#'
#' @return `nyt_set_query_params()` returns a list of parameters for the query
#' @export
nyt_set_query_params <- function(
  q = "",
  fq = NULL,
  sort_order = "oldest",
  api_key = Sys.getenv("NYTIMES_KEY")
) {

  fq = 'glocations:("India") AND document_type:("article") AND source:("The New York Times" "International New York Times" "International Herald Tribune")'

  list(
    q = q,
    fq = fq,
    begin_date = "",
    end_date = "",
    sort_order = sort_order,
    `api-key` = api_key,
    page = 0 # starting with outer loop, page is 0
  )
}

#' @export
#' @rdname nyt_get_data
#' @return `nyt_set_query_params()` returns a Date vector
#' @param api_data_folder name of folder where to write api data
#' @importFrom magrittr %>%
#' @importFrom rlang .data
nyt_set_time_chunks <- function(begin_date = NULL,
                                end_date = NULL,
                                api_data_folder = "api_data") {

  if (!dir.exists(api_data_folder)) {
    dir.create(api_data_folder)
  }
  # earliest date API is available
  api_origin_date <- as.Date("1851-01-01")

  begin_date <- if (!is.null(begin_date)) {

    as.Date(begin_date)

  } else {

    if (rlang::is_empty(list.files(api_data_folder))) {

      api_origin_date

    } else {

      begin_date_as_chr <- list.files(api_data_folder) %>%
        stringr::str_extract("^\\d+") %>%
        as.integer() %>% max() %>% as.character()

      sub("(\\d{4})(\\d{2})", "\\1-\\2-", begin_date_as_chr) %>%
        # get the month after the latest already gotten
        lubridate::ymd() %>%
        # %m+% months(1) worked interactively but not in package
        lubridate::add_with_rollback(months(1), preserve_hms = FALSE)
    }
  }

  end_date <- if (is.null(end_date)) {
    Sys.Date()
  } else {
    as.Date(end_date)
  }

  # set up to collect monthly
  time_chunks <- seq(begin_date, end_date, by = "1 month")
  time_chunks
}

#' @export
#' @rdname nyt_get_data
#' @param query_params list output from `nyt_set_query_params()`
#' @param time_chunks Date vector output from `nyt_set_time_chunks()`
#' @param nyt_api_url NULL unless querying another api
#' @param nyt_user_agent "NYT_USER_AGENT" environmental variable
#' @param api_data_folder name of folder where to write api data
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @return `nyt_query_api()` writes monthly rds files to api_data_folder
nyt_query_api <- function(query_params,
                          time_chunks,
                          nyt_api_url = NULL,
                          nyt_user_agent = NULL,
                          api_data_folder = "api_data") {

  nyt_api_url <- if (is.null(nyt_api_url)) {
    "http://api.nytimes.com/svc/search/v2/articlesearch.json?"
  }

  nyt_user_agent <- if (is.null(nyt_user_agent)) {
    Sys.getenv("NYT_USER_AGENT")
  }


  for (i in seq_along(time_chunks)) { # outer loop over month-yr combinations

    query_params$begin_date <- time_chunks[i] %>%
      stringr::str_replace_all("-", "")
    # want end_date to be the last day of previous month
    query_params$end_date <- as.Date(time_chunks[i + 1]-1) %>%
      stringr::str_replace_all("-", "")

    # run an initial query to get maxPages for each time chunk
    initial_query <- httr::GET(url = nyt_api_url,
                               query = query_params,
                               httr::user_agent(nyt_user_agent))

    if (httr::http_error(initial_query)) {

      print(httr::http_status(initial_query)$message)
      break

    } else {

      # use number of hits to determine max number of pages for a month
      hits <- httr::content(initial_query, as = "text",
                            encoding = "UTF-8") %>%
        jsonlite::fromJSON() %>%
        .$response %>%
        .$meta %>%
        .$hits

      maxPages <- dplyr::if_else(hits <= 0,
                                 0, ceiling(hits / 10) - 1)

      # initialize an empty list of the needed length
      pages <- vector("list", length = maxPages + 1)

      # within a month, loop over pages from 0; results 1-10 on page 0
      for (i in 0:maxPages) {

        query_params$page <- i

        request <- httr::GET(url = nyt_api_url,
                             query = query_params,
                             httr::user_agent(nyt_user_agent))

        if (httr::http_error(request)) {

          print(httr::http_status(request)$message)
          break

        } else {

          response <- httr::content(request, as = "text",
                                    encoding="UTF-8") %>%
            jsonlite::fromJSON(flatten = TRUE)

          if (rlang::is_empty(response$response$docs)) {
            # if no results, assign an empty df
            pages[[i + 1]] <- data.frame()

          } else {

            pages[[i + 1]] <- response %>% data.frame()

          }

          message(glue::glue("Retrieving time chunk beginning \\
                             {query_params$begin_date}, \\
                             page {i} of {maxPages}"))
          Sys.sleep(5)
        }
      }

      # reduce monthly list of dfs to output a single df
      pages_df <- jsonlite::rbind_pages(pages)
      # the list-column makes it difficult to write as csv; but rds works fine
      file <- glue::glue("{query_params$begin_date}.rds")
      fn <- here::here(api_data_folder, file)
      readr::write_rds(pages_df, fn)
    }
  }
}

