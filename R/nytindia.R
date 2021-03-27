#' \code{nytindia} package
#'
#' Prepare and visualize India-tagged data from the NYT Article Search API
#'
#' See the README on
#' \href{https://github.com/seanangio/nytindia#readme}{GitHub}
#'
#' @docType package
#' @name nytindia
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".",
                                                        "_id", "abstract", "byline", "byline.organization", "byline.original", "byline.person",
                                                        "copyright", "desc", "document_type", "headline.content_kicker", "headline.kicker",
                                                        "headline.main", "headline.name", "headline.print_headline", "headline.seo",
                                                        "headline.sub", "headline_print", "lead_paragraph", "material", "multimedia",
                                                        "news_desk", "print_page", "print_section", "pub_date", "section", "section_name",
                                                        "snippet", "status", "subsection_name", "type_of_material", "uri", "web_url",
                                                        "word_count", "keeping", "headline", "front_page", "printed", "keywords",
                                                        "major", "value", "name", "new_name", "n", "providedLocation.location", "locations",
                                                        "countries", "latLng.lat", "latLng.lng", "glocation", "lat", "lon", "country",
                                                        "india_rank", "govt", "mapquest_path", "url_len"))
