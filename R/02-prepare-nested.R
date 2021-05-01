#' Bind monthly rds files into single nested df and perform initial cleaning
#'
#' `nyt_bind_api_files()` combines folder of rds files into one tbl.
#' `nyt_clean_api_tbl()` performs initial cleaning on the output
#' of `nyt_bind_api_files()`, such as removing unnecessary columns.
#' It also writes a separate rds file of news desk values to use as
#' input in the lookup table shiny app. Your working directory
#' must be the same top level folder of the new project where
#' `nyt_get_data()` was called.
#'
#' This function also provides a fairly accurate fix for a
#' bug in the NYT API where the same article is returned 2x.
#' The URL is different but the date, headline, author etc
#' is the same. It's especially common in 2006. Luckily,
#' the "extra" article has a shorter URL so the function
#' keeps only the longest URL for every headline/pub_date pair.
#'
#' @param api_data_folder name of folder where to write API data
#' @return `nyt_bind_api_files()` returns a tbl of raw API data
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' api_df <- bind_api_files()
#' combined_df <- nyt_clean_api_tbl(api_df)
#' }
nyt_bind_api_files <- function(api_data_folder = "api_data") {

  files <- dir(path = "api_data", pattern = "*.rds", full.names = TRUE)

  files %>%
    purrr::map(readr::read_rds) %>%
    purrr::set_names(stringr::str_extract(files, "[[:digit:]]+")) %>%
    purrr::compact() %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()

}

#' @rdname nyt_bind_api_files
#' @export
#' @param api_df tbl resulting from `nyt_bind_api_files()`
#' @param news_desk_input_folder folder name to write news desks pre-cleaning
#' @param news_desk_output_folder folder to find news desks post-cleaning
#' @return `nyt_clean_api_tbl()` returns a cleaner tbl with nested keywords
nyt_clean_api_tbl <- function(api_df,
                              news_desk_input_folder = "news_desk_lookup_input",
                              news_desk_output_folder = "renamed_news_desks") {

  nested_df <- api_df %>%
    dplyr::rename_all(.funs = list(~sub("^response.docs.", "",.))) %>%
    # delete uninteresting columns
    dplyr::select(-c(status, copyright,
                     dplyr::contains("response.meta."),
                     uri,`_id`, headline.seo, headline.sub,
                     multimedia, headline.content_kicker,
                     headline.name)) %>%
    # snippet is either exact same as abstract or missing
    # `source` always NYT or international variant
    # `headline.kicker` a kind of series tag like 'frugal traveler'
    # document_type  always article or NA
    # `byline.organization` dropped (usually NA or NYT)
    # `byline.original` converted to byline; byline.person not unnesting nicely
    # word_count not needed; lots of missing data
    # print_section not in original query
    # for recent results, it's uninteresting (A, B, etc)
    # interesting for some older results, but simpler to not use as a replacement for section
    # subsection_name is interesting but largely covered by section
    # e.g. if section is World, subsection will be Asia Pacific
    # [it also wasn't present in original query]
    # should be conditional-- if it exists, remove it...?
    dplyr::select(-c(snippet, source, headline.kicker,
                     document_type, byline.organization,
                     byline.person, word_count,
                     print_section)) %>%
    # subsection name isn't always present
    dplyr::select(-tidyselect::any_of("subsection_name")) %>%
    # remove any duplicates
    dplyr::distinct(web_url, .keep_all = TRUE) %>%
    dplyr::rename(
      url = web_url,
      headline = headline.main,
      headline_print = headline.print_headline,
      section = section_name,
      material = type_of_material,
      byline = byline.original
    ) %>%
    dplyr::mutate_if(is.character, stringr::str_trim) %>%
    dplyr::mutate(
      pub_date = as.Date(pub_date),
      front_page = dplyr::if_else(readr::parse_number(print_page) == 1,
                                  TRUE, FALSE),
      # headline_print is different (typically shorter) than headline_main
      # it is missing for blogs; simpler to have one headline so dropping headline_print in favor of headline_main
      # but headline_print is still useful for creating a printed variable; FALSE values are mostly blogs or continuous news
      # 2001 is a rough estimate to help identify blogs
      printed = dplyr::if_else(is.na(headline_print) & pub_date > "2001-01-01",
                               FALSE, TRUE),
      # for strings, str_detect() treats "" as boundary character so change to " "
      # don't want NA because it gets counted as a value for barplots
      lead_paragraph = dplyr::if_else(lead_paragraph == "",
                                      " ", lead_paragraph),
      abstract = dplyr::if_else(abstract == "",
                                " ", abstract),
      news_desk = stringr::str_remove(news_desk, ";$"),
      news_desk = stringr::str_trim(stringr::str_remove(news_desk, "Desk$")),
      section = dplyr::if_else(is.na(section), "NA", section),
      section = dplyr::case_when(
        section %in% c("International Home", "NA") ~ "World",
        section %in% c("Video") ~ "Multimedia/Photos",
        TRUE ~ section
      ),
      material = dplyr::if_else(is.na(material), NA_character_, material),
      material = dplyr::case_when(
        # where more than one material type listed; go with first
        material %in% c("Obituary (Obit)",
                        "Obituary; Biography; Chronology",
                        "Obituary; Biography") ~ "Obituary",
        material %in% c("An Analysis; News Analysis",
                        "An Analysis") ~ "News Analysis",
        material %in% c("Chronology; Series") ~ "Chronology",
        material %in% c("Editorial; Series",
                        "Editors' Note") ~ "Editorial",
        material %in% c("Series; Interview",
                        "Series; Chronology") ~ "Series",
        # very low counts of these
        material %in% c("List", "Sidebar", "briefing",
                        "Web Log", "Newsletter") ~ "News",
        TRUE ~ material
      )
    ) %>%
    # `print_page` not needed once front_page created;
    # lots of suspicious page number data for it to be useful
    # `headline_print` not needed after `printed` created
    dplyr::select(-c(print_page, headline_print)) %>%
    # roughly clean byline values
    dplyr::mutate(
      byline = stringr::str_trim(stringr::str_to_lower(byline)),
      byline = stringr::str_trim(stringr::str_remove(byline, "^by ")),
      byline = stringr::str_trim(stringr::str_remove_all(byline, "[[:punct:]]")),
      byline = stringr::str_replace(byline, "^copyright.*", ""),
      byline = stringr::str_replace(byline, "^\\d*", ""),
      byline = stringr::str_replace(byline, "^by ", ""),
      byline = stringr::str_replace(byline, "new delhi.*$", ""),
      byline = stringr::str_trim(stringr::str_replace(byline, "nyt$", "")),
      byline = stringr::str_trim(stringr::str_replace(byline, "^compiled by", "")),
      byline = dplyr::case_when(
        byline %in% c("special to the new york times",
                      "special correspondence the new york times",
                      "wireless to the new york times",
                      "special cable to the new york times",
                      "cable to the new york times",
                      "telephone to the new york times",
                      "special to the new york timesthe new york times",
                      "special correspondence of the new york times") ~ "the new york times",
        TRUE ~ byline
      ),
      byline = stringr::str_replace(byline, "special to the new york times.*$", ""),
      byline = stringr::str_replace(byline, "wireless to the new york times.*$", ""),
      byline = stringr::str_replace(byline, "special cable to the new york times.*$", ""),
      byline = stringr::str_replace(byline, "cable to the new york times.*$", ""),
      byline = stringr::str_replace(byline, "^bernard weinraub.*", "bernard weinraub"),
      byline = stringr::str_trim(byline),
      byline = dplyr::case_when(
        byline == "a m rosenthal" ~ "am rosenthal",
        byline == "the associated press" ~ "ap",
        byline == "by" ~ "",
        byline == "united press international" ~ "upi",
        TRUE ~ byline
      ),
      byline = stringr::str_trim(stringr::str_replace(byline, " by$", "")),
      byline = stringr::str_to_title(byline),
      byline = stringr::str_replace(byline, " And ", " and "),
      byline = dplyr::case_when(
        byline == "Ap" ~ "AP",
        byline == "Upi" ~ "UPI",
        is.na(byline) ~ "",
        TRUE ~ byline
      ),
      byline = dplyr::if_else(byline == "", "None", byline)
    ) %>%
    # fix NYT bug of duplicate articles esp in 2006
    dplyr::group_by(pub_date, headline) %>%
    dplyr::mutate(url_len = stringr::str_length(url)) %>%
    purrr::when(
      .$news_desk != "Editorial" | .$material != "Letter",
      dplyr::filter(., url_len == max(url_len))
    ) %>%
    #dplyr::filter(url_len == max(url_len)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-url_len) %>%
    dplyr::arrange(desc(pub_date))

  # too many inconsistencies in "news_desk" to fix with case_when
  # create a lookup table with a shiny app instead
  nested_news_desks <- nested_df %>%
    dplyr::select(news_desk)

  if (!dir.exists(news_desk_input_folder)) {
    dir.create(news_desk_input_folder)
  }

  fn <- here::here(news_desk_input_folder, "nested_news_desks.rds")
  readr::write_rds(nested_news_desks, fn)

  if (!dir.exists(news_desk_output_folder)) {
    dir.create(news_desk_output_folder)
    # place lookup table output here
  }

  return(nested_df)

}
