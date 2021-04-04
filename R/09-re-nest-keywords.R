#' Re-nest keywords
#'
#' Now that you have a final unnested version of the data, you need
#' to create a nested version. In the process you are also ready to
#' create new columns (india_rank, max_kword, and in_of_n_kword),
#' which detail the relevance of India to an article.
#'
#' `extract_rank()` and `find_max_kword()` are small helper functions
#' used when iterating over each article.
#'
#' @rdname nyt_re_nest_keywords
#' @param df keyword df belonging to one article
#'
#' @return `nyt_re_nest_keywords()` returns a nested df
#' @export
#'
#' @examples
#' \dontrun{
#' full_nested_df <- nyt_re_nest_keywords(full_unnested_df)
#' }
#' @importFrom magrittr %>%
extract_rank <- function(df) {
  # get rank of india keyword
  df %>%
    dplyr::filter(name == "glocations",
                  value == "india") %>%
    dplyr::pull(rank)
}

#' @rdname nyt_re_nest_keywords
#' @export
find_max_kword <- function(df) {
  # find the maximum rank (total number of keywords in an article)
  max(df$rank)
}

#' @param full_unnested_df output of `nyt_join_coords_countries()`
#' @param max_allowable_india_rank upper limit of india keyword rank of an article
#' @export
nyt_re_nest_keywords <- function(full_unnested_df,
                                 max_allowable_india_rank = 30) {

  non_keyword_columns <- full_unnested_df %>%
    dplyr::select(-c(name, value, rank, lat, lon, country)) %>%
    names()

  full_nested_df <- full_unnested_df %>%
    tidyr::nest(keywords = c(name, value, rank, lat, lon, country)) %>%
    # pull out india rank as a separate var so you can filter by india rank and other keywords at same time
    # couldn't do this earlier because need to remove duplicate india ranks
    dplyr::mutate(
      india_rank = purrr::map_dbl(keywords, extract_rank),
      max_kword = purrr::map_dbl(keywords, find_max_kword),
      in_of_n_kword = stringr::str_glue('{india_rank} of {max_kword}')
    ) %>%
    # remove about articles that aren't remotely about india
    # 30 removes about 6 articles in this query
    dplyr::filter(india_rank < max_allowable_india_rank)

  return(full_nested_df)
}
