#' Run NYT India app with sample data or lookup table app
#'
#' Launch a Shiny app that shows a demo of what can be done
#' after querying and preparing data. You can also launch the
#' lookup table app for renaming news desks and keyword values.
#'
#' All credit to this \href{https://github.com/daattali/shinyjs/blob/master/R/runExample.R}{shinyjs example}.
#'
#' @param example either "nyt_india_app" or "lookup_table_app"
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   runExample("nyt_india_app")
#'   runExample("lookup_table_app")
#' }
#' @export
runExample <- function(example) {

  validExamples <-
    paste0(
      'Valid examples are: "',
      paste(list.files(system.file("examples",
                                   package = "nytindia")),
            collapse = '", "'),
      '"')

  if (missing(example) || !nzchar(example)) {
    message(
      'Please run `runExample()` with a valid example app as an argument.\n',
      validExamples)
    return(invisible(NULL))
  }

  appDir <- system.file("examples", example,
                        package = "nytindia")
  if (appDir == "") {
    paste0(sprintf("could not find example app `%s`\n%s",
                   example, validExamples))
  }

  shiny::runApp(appDir, display.mode = "normal")
}
