#' Download shiny app files
#'
#' `nyt_build_data()` creates the final nested dataset.
#' But to run the shiny app locally, you'll need the files
#' [here](https://github.com/seanangio/nytindia/tree/master/inst/examples/nyt_india_app)
#'
#'
#' @param shiny_app_folder name of shiny app folder
#'
#' @return `nyt_download_shiny_files()` downloads necessary ui.R/server.R etc to run shiny app locally
#' @export
#'
#' @examples
#' \dontrun{
#' nyt_download_shiny_files()
#' }
#' @importFrom magrittr %>%
nyt_download_shiny_files <- function(shiny_app_folder = "nyt_shiny_app") {

  my_github_repo_tree <- "https://api.github.com/repos/seanangio/nytindia/git/trees/master?recursive=1"
  raw_github_str <- "https://raw.githubusercontent.com/seanangio/nytindia/master/inst/examples/nyt_india_app/"
  my_shiny_folder_repo_path <- "^inst/examples/nyt_india_app/"

  req <- httr::GET(my_github_repo_tree)
  httr::stop_for_status(req)

  full_repo_filelist <- unlist(lapply(httr::content(req)$tree, "[", "path"),
                               use.names = F)

  filenames <- stringr::str_subset(full_repo_filelist,
                                   my_shiny_folder_repo_path) %>%
    stringr::str_subset("^(?!.*rds)") %>% # get rid of data files
    stringr::str_subset("\\.") %>% # get rid of directories
    stringr::str_replace(my_shiny_folder_repo_path, "") # get filenames

  origin_urls <- stringr::str_c(raw_github_str, filenames)

  urls <- stringr::str_subset(full_repo_filelist, my_shiny_folder_repo_path)

  if (!dir.exists(here::here(shiny_app_folder))) {
    dir.create(here::here(shiny_app_folder))
  }

  if (!dir.exists(here::here(shiny_app_folder, "www"))) {
    dir.create(here::here(shiny_app_folder, "www"))
  }

  purrr::walk2(.x = filenames,
               .y = origin_urls,
               .f = purrr::safely(~download.file(url = .y,
                                                 destfile = here::here(shiny_app_folder, .x))))


}
