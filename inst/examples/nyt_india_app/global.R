library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(forcats)
library(tibble)
library(scales)

# for UI
library(shinyWidgets)
library(bsplus)
library(shinycssloaders)

# for table tab
library(DT)

# for counts and keyword pairs
library(ggiraph)
library(tidytext)

# for timeline tab
library(lubridate)
library(tsbox)
library(dygraphs)
library(gt)

# for map tab
library(leaflet)
library(leaflet.extras)


# read data sources -------------------------------------------------------

full_nested_df <- read_rds("data/full_nested_df.rds") # full_nested_df.rds nested_sample.rds
full_unnested_df <- full_nested_df %>% unnest(keywords)
govt <- read_csv("data/govt.csv")

# static UI choices -------------------------------------------------------
app_title <- "India in The New York Times"

desks <- full_nested_df %>%
  count(news_desk, sort = TRUE) %>%
  pull(news_desk)

sections <- full_nested_df %>%
  count(section, sort = TRUE) %>%
  pull(section)

materials <- full_nested_df %>%
  count(material, sort = TRUE) %>%
  pull(material)

bylines <- full_nested_df %>%
  count(byline, sort = TRUE) %>%
  pull(byline)

values <- full_unnested_df %>%
  count(value, sort = TRUE) %>%
  pull(value)

governments <- govt %>%
  pull(govt_name)

# need to break long options over multiple lines
# https://stackoverflow.com/questions/51355878/how-to-text-wrap-choices-from-a-pickerinput-if-the-length-of-the-choices-are-lo/51406191#51406191
bylines_broken <- bylines %>%
  str_wrap(width = 40) %>%
  str_replace_all("\\n", "<br>")

values_broken <- values %>%
  str_wrap(width = 50) %>%
  str_replace_all("\\n", "<br>")


# define input functions --------------------------------------------------

input_table <- tribble(
  ~id, ~label, ~widget, ~choices, ~broken_values, ~df_var, ~helper,
  "single_date_range", "Single Continuous Date Range", "switch", NA_character_, NA_character_, NA_character_, NA_character_,
  "dates", "Date Range (yyyy-mm-dd)", "dateRange", NA_character_, NA_character_, NA_character_, "Instead of a single date range, you can also select specific governments.",
  "government", "Date Range by Governments", "picker", governments, NA_character_, NA_character_, "Government data pulled from <a href='https://en.wikipedia.org/wiki/List_of_prime_ministers_of_India' target='_blank'>Wikipedia list</a> of Indian Prime Ministers.",
  "desk", "News Desk", "picker", desks, NA_character_, "news_desk", "All 'News Desk' values before 1981 are categorized as 'None'",
  "section_name", "Section", "picker", sections, NA_character_, "section", "All 'Section' values before 1981 are categorized as 'Archives'",
  "material_type", "Material", "picker", materials, NA_character_, "material", "All 'Material' values before '1963-11-23' are categorized as 'Archives'",
  "by_line", "Byline", "picker", bylines, bylines_broken, "byline", "In searching for a particular author, the search bar is helpful to find spelling variations and co-authored pieces.",
  "filter_keyword", "Filter by Keywords", "switch", NA_character_, NA_character_, NA_character_, NA_character_,
  "keyword_condition_and", "Logical Operator", "switch", NA_character_, NA_character_, NA_character_, NA_character_,
  "keywords", "Keyword(s)", "picker", values, values_broken, "value", "The 'AND' condition requires all selected keywords to be included in every article returned.",
  "ind_rnk", "India Keyword Rank", "slider", NA_character_, NA_character_, "india_rank", "Every article includes 'India' as a location keyword. Most articles have multiple keywords, and the ranking of those keywords in order of importance is also reported.",
  "filter_text", "Filter by Specific Words", "switch", NA_character_, NA_character_, NA_character_, NA_character_,
  "text", "Search in Headline/Abstract/Lead (Case Insensitive)", "text", NA_character_, NA_character_, "word", "The API does not return the full text of articles. For all time periods, it returns a headline; after 2004 it returns an abstract...",
  "page1", "Include Page 1 Articles Only", "switch", NA_character_, NA_character_, "front_page", NA_character_, # default is FALSE...
  "is_printed", "Exclude Non-Printed Results (e.g. blogs)", "switch", NA_character_, NA_character_, "is_printed", NA_character_
)

mySwitchInput <- function(id, label, ...) {
  if (id == "keyword_condition_and") {
    switchInput(id, label,
      value = FALSE, # FALSE is OR (default)
      labelWidth = "200px",
      onLabel = "AND", offLabel = "OR",
      onStatus = "warning", offStatus = "warning"
    )
  } else {
    switchInput(id, label, ..., labelWidth = "300px")
  }
}

myPickerInput <- function(id, label, ...) {
  # not sure why label needed here again...
  label <- input_table$label[input_table$id == id]
  choices <- input_table$choices[input_table$id == id][[1]]
  broken_values <- input_table$broken_values[input_table$id == id][[1]]

  pickerInput(id, label,
    choices = choices,
    selected = choices,
    multiple = TRUE,
    options = list(
      `actions-box` = TRUE,
      `live-search` = TRUE,
      `liveSearchNormalize` = TRUE,
      `selected-text-format` = "count > 3"
    ),
    choicesOpt = list(content = broken_values)
  )
}

myInput <- function(id, ...) {
  label <- input_table$label[input_table$id == id]
  widget <- input_table$widget[input_table$id == id]
  helper <- input_table$helper[input_table$id == id]

  if (widget == "switch") {
    the_input <- mySwitchInput(id, label, ...)
  } else if (widget == "picker") {
    the_input <- myPickerInput(id, label, ...)
  } else if (widget == "dateRange") {
    the_input <- dateRangeInput(id, label,
      start = min(full_nested_df$pub_date),
      end = max(full_nested_df$pub_date),
      min = min(full_nested_df$pub_date),
      max = max(full_nested_df$pub_date),
      startview = "decade"
    )
  } else if (widget == "slider") {
    the_input <- sliderInput(id, label,
      min = 1, max = max(full_nested_df$india_rank),
      value = c(1, max(full_nested_df$india_rank)),
      step = 1
    )
  } else if (widget == "text") {
    the_input <- textInput(id, label)
  }

  if (!is.na(helper)) {
    the_input %>%
      shinyInput_label_embed(
        icon("info") %>%
          bs_embed_tooltip(title = helper)
      )
  } else {
    the_input
  }
}

withMySpinner <- function(ui_element) {
  ui_element %>%
    withSpinner(color = "#337ab7")
}


# filtering functions -----------------------------------------------------

filter_unnested <- function(single_date_range, dates, government,
                            desk, section_name, material_type,
                            by_line, filter_keyword,
                            keyword_condition_and, keywords,
                            ind_rnk, filter_text, text, page1,
                            is_printed) {
  # keyword filtering is complicated; need to get desired urls first
  if (filter_keyword == TRUE) {
    urls <- full_unnested_df %>%
      filter(value %in% keywords) %>%
      {
        if (keyword_condition_and == TRUE) {
          group_by(., url) %>%
            mutate(n = n()) %>%
            filter(n == length(keywords)) %>%
            ungroup()
        } else {
          .
        }
      } %>%
      distinct(url) %>%
      pull(url)
  }

  # if non-inclusive date filtering, process is different
  from <- as.Date(str_sub(government, 1, 10))
  to <- as.Date(str_sub(government, 14, 23))

  temp_tbl <- tibble()
  if (single_date_range != TRUE) {
    # prevent failure if 0 govts selected
    if (is.null(government)) {
      temp_tbl <- full_unnested_df[0, ]
    } else {
      for (i in seq_along(government)) {
        temp_tbl <- rbind(temp_tbl, full_unnested_df %>%
          filter(pub_date >= from[i] &
            pub_date <= to[i]))
      }
    }
  } else {
    temp_tbl <- full_unnested_df %>%
      filter(pub_date >= dates[1] & pub_date <= dates[2])
  }

  temp_tbl %>%
    arrange(desc(pub_date)) %>%
    {
      if (filter_keyword == TRUE) {
        filter(., url %in% urls)
      } else {
        .
      }
    } %>%
    {
      if (page1 == TRUE) {
        filter(., front_page == TRUE)
      } else {
        .
      }
    } %>%
    {
      if (is_printed == TRUE) {
        filter(., printed == TRUE)
      } else {
        .
      }
    } %>%
    {
      if (filter_text == TRUE) {
        mutate(.,
          text_cols = str_c(headline, " ", abstract, " ", lead_paragraph),
          text_cols = str_replace_all(
            str_to_lower(text_cols),
            "[^[:alnum:] ]", ""
          )
        ) %>%
          filter(
            .,
            str_detect(text_cols, text)
          )
      } else {
        .
      }
    } %>%
    filter(
      news_desk %in% desk,
      section %in% section_name,
      material %in% material_type,
      byline %in% by_line,
      india_rank >= ind_rnk[1] & india_rank <= ind_rnk[2]
    )
}

nest_df <- function(unnested_df) {
  # instead of actually nesting/unnesting,
  # much easier to get distinct url and then join
  unnested_df %>%
    distinct(url) %>%
    left_join(full_nested_df, by = "url")
}

count_filters_on <- function(dates, government, desk,
                             section_name, material_type,
                             by_line, filter_keyword,
                             keywords, ind_rnk, filter_text, text,
                             page1, is_printed) {
  n_filters <- 0

  if (dates[1] != min(full_nested_df$pub_date) ||
    dates[2] != max(full_nested_df$pub_date)) {
    n_filters <- n_filters + 1
  } else if (length(government) != length(governments)) {
    n_filters <- n_filters + 1
  }

  if (length(desk) != length(desks)) {
    n_filters <- n_filters + 1
  }

  if (length(section_name) != length(sections)) {
    n_filters <- n_filters + 1
  }

  if (length(material_type) != length(materials)) {
    n_filters <- n_filters + 1
  }

  if (length(by_line) != length(bylines)) {
    n_filters <- n_filters + 1
  }

  if (filter_keyword == TRUE && length(keywords) != length(values)) {
    n_filters <- n_filters + 1
  }

  if (ind_rnk[1] != 1 || ind_rnk[2] != max(full_nested_df$india_rank)) {
    n_filters <- n_filters + 1
  }

  if (filter_text == TRUE && text != "") {
    n_filters <- n_filters + 1
  }

  if (page1 != FALSE) {
    n_filters <- n_filters + 1
  }

  if (is_printed != FALSE) {
    n_filters <- n_filters + 1
  }

  n_filters
}

# table tab ---------------------------------------------------------------

prep_dt <- function(df) {
  # prepare the nested df for the DataTable
  df %>%
    mutate(
      Headline = str_glue('<a href="{url}" target="_blank">{headline}</a>')
    ) %>%
    select(
      Date = pub_date,
      Headline,
      `News Desk` = news_desk,
      Section = section,
      Material = material,
      Byline = byline,
      `India Keyword` = in_of_n_kword,
      Abstract = abstract,
      Lead = lead_paragraph
    )
}

draw_dt <- function(df) {
  # once df is correct format, use DT package to format it
  datatable(df,
    caption = tags$caption(
      em(h5('Click on a cell in the News Desk, Section, Material, or Byline columns to filter for that value; click the "India Keyword" cell for keyword table.'))
    ),
    class = "cell-border stripe order-column compact",
    selection = list(mode = "single", target = "cell"),
    extensions = "Responsive",
    options = list(
      # Cursor icon changes to hand (pointer) on Hover for keyword col
      rowCallback = JS(
        "function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {",
        "$('td:eq(6)', nRow).css('cursor', 'pointer');",
        "}"
      ),
      columnDefs = list(
        list(className = "dt-center", targets = 6), # center kw col
        list(
          # ellipsis for long text cols
          targets = c(5, 7, 8),
          render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 20 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
            "}"
          )
        )
      )
    ),
    rownames = FALSE,
    filter = "top",
    escape = FALSE
  )
}

prep_keyword_df <- function(cell_clicked, nested_df) {
  if (is.null(cell_clicked$col) || cell_clicked$col != 6) {
    return()
  } else { # if (cell_clicked()$col == 6)
    nested_df[cell_clicked$row, ]$keywords[[1]] %>%
      select(
        Name = name,
        Value = value,
        Rank = rank
      )
  }
}

draw_keyword_dt <- function(keyword_df) {
  datatable(
    keyword_df,
    options = list(dom = "tipr"),
    rownames = FALSE,
    selection = "multiple"
  )
}

myDTModal <- function(df, cell) {
  col_name <- names(df)[cell$col + 1]
  modalDialog(
    title = str_glue('Do you want to filter for only articles that include "{cell$value}" as the choice for {col_name}?'),
    str_glue("Doing so will remove all other {col_name} filters."),
    actionButton("myDTActionButton", "Yes"),
    easyClose = TRUE
  )
}

myDownloadHandler <- function(df, ext) {
  if (ext == ".rds") {
    filename <- function() {
      paste0("nyt-india-nested-", Sys.Date(), ".rds")
    }
    content <- function(file) {
      write_rds(df, file)
    }
  } else if (ext == ".csv") {
    filename <- function() {
      paste0("nyt-india-unnested-", Sys.Date(), ".csv")
    }
    content <- function(file) {
      write_csv(df, file)
    }
  }
  downloadHandler(filename, content)
}


# counts tab --------------------------------------------------------------

# manually defined relationship between var in df and name in dropdown
count_choices <- list(
  Categorical = list(
    "News Desk" = "news_desk",
    "Section" = "section",
    "Material" = "material",
    "Byline" = "byline"
  ),
  Keywords = list(
    "All Keywords" = "all",
    "Subject Keywords" = "subject",
    "Location Keywords" = "glocations",
    "Person Keywords" = "persons",
    "Organization Keywords" = "organizations",
    "Creative Work Keywords" = "creative_works"
  ),
  Text = list(
    "Headline/Abstract/Lead" = "word",
    "+/- Sentiment Words" = "sentiment"
  ),
  Miscellaneous = list(
    "India Keyword Rank" = "india_rank",
    "Keywords per Article" = "max_kword",
    "Front Page" = "front_page"
  )
)

count_choices_df <- tibble(count_choices) %>%
  unnest_longer(count_choices,
    values_to = "var_name",
    indices_to = "plot_name"
  )

# words like india should be considered stop words
custom_stop_words <- bind_rows(
  tibble(
    word = c("india", "indian", "india's", "indians", "india’s", "iindian", "indias"),
    lexicon = c("custom")
  ),
  stop_words
)

prep_count_data <- function(nested_df, unnested_df, var) {
  if (var == "word" || var == "sentiment") {
    # need to combine 3 text columns into one to calculate sum counts
    df <- nested_df %>%
      mutate(
        text = str_c(headline, " ", abstract, " ", lead_paragraph),
        text = str_replace_all(str_to_lower(text), "[^[:alnum:] ]", "")
      ) %>%
      select(text) %>%
      unnest_tokens(word, text) %>% # from {tidytext} [already does some normalizing]
      anti_join(custom_stop_words, by = "word") %>%
      {
        if (var == "sentiment") {
          inner_join(., get_sentiments("bing"), by = "word")
        } else {
          .
        }
      } %>%
      {
        if (var == "sentiment") {
          count(., word, sentiment, sort = TRUE)
        } else {
          count(., word, sort = TRUE)
        }
      }
  } else if (var %in% c("india_rank", "max_kword", "news_desk", "section", "material", "byline", "front_page")) {

    # need a simple count for categorical vars
    df <- nested_df %>%
      count(!!rlang::sym(var), sort = TRUE)
  } else {

    # last case is keyword categories: find distinct urls and join back
    df <- unnested_df %>%
      distinct(url) %>%
      left_join(unnested_df, by = "url") %>%
      filter(value != "india") %>%
      {
        if (var != "all") {
          filter(., name == var)
        } else {
          .
        }
      } %>%
      count(value, name, sort = TRUE) %>%
      rename(!!var := value)
  }
}

trim_count_df <- function(count_df, n_obs) {
  var <- names(count_df)[1]

  if (var %in% c("india_rank", "max_kword", "front_page")) {
    df <- count_df
  } else if (var %in% c("news_desk", "section", "material", "byline")) {
    # categorical vars get lumped into an Other category
    top_cats <- count_df %>% pull(!!rlang::sym(var))
    df <- count_df %>%
      mutate(!!var := ifelse(!!rlang::sym(var) %in% top_cats[1:n_obs - 1],
        !!rlang::sym(var), "Other"
      )) %>%
      count(!!rlang::sym(var), wt = n)
  } else {
    # words, sentiment and keywords just get cut off
    df <- count_df %>%
      slice_head(n = n_obs)
  }

  df %>%
    mutate(tip = str_c(.[[1]], ": ",
                       number(n,
                          accuracy = 1,
                          big.mark = ",")))
}

get_subtitle_str <- function(count_df, n_obs) {
  if (nrow(count_df) <= n_obs) {
    NULL
  } else {
    var <- names(count_df)[1]
    left_out <- format(nrow(count_df) - n_obs, big.mark = ",")

    if (var %in% c("india_rank", "max_kword", "front_page")) {
      NULL
    } else if (var %in% c("news_desk", "section", "material", "byline")) {
      if (left_out == 1) {
        str_glue('{left_out} category in "Other"')
      } else {
        str_glue('{left_out} categories in "Other"')
      }
    } else {
      if (left_out == 1) {
        str_glue("{left_out} other not shown")
      } else {
        str_glue("{left_out} others not shown")
      }
    }
  }
}

keyword_pal <- c( # RColorBrewer::brewer.pal(n = 8, name = "Dark2")
  "subject" = "#1B9E77",
  "glocations" = "#D95F02",
  "persons" = "#7570B3",
  "organizations" = "#E7298A",
  "creative_works" = "#66A61E"
)

sentiment_pal <- c( # RColorBrewer::brewer.pal(n = 3, name = "RdBu")
  "positive" = "#67A9CF",
  "negative" = "#EF8A62"
)

build_gg <- function(trimmed_count_df, sub_title) {

  var <- names(trimmed_count_df)[1]
  # fix to account for sentiment
  title_var <- count_choices_df %>%
    {
      if (names(trimmed_count_df)[2] == "sentiment") {
        filter(., var_name == "sentiment")
      } else {
        filter(., var_name == var)
      }
    } %>%
    pull(plot_name)

  if (var %in% c("india_rank", "max_kword", "front_page")) {
    # no re-ordering
    gg <- trimmed_count_df %>%
      ggplot(aes(
        x = !!rlang::sym(var),
        y = n
      ))
  } else if ( names(trimmed_count_df)[2] == "sentiment") {
    gg <- trimmed_count_df %>%
      ggplot(aes(
        x = fct_reorder(!!rlang::sym(var), n),
        y = n,
        fill = sentiment
      ))
  } else {
    gg <- trimmed_count_df %>%
      ggplot(aes(
        x = fct_reorder(!!rlang::sym(var), n),
        y = n
      ))
  }

  gg +
    guides(fill = FALSE) +
    {
      if (var %in% c("all", "subject", "glocations", "persons", "organizations", "creative_works")) {
        list(
          geom_col_interactive(aes(
            tooltip = tip,
            data_id = !!rlang::sym(var),
            fill = name
          )),
          scale_fill_manual(
            values = keyword_pal,
            limits = names(keyword_pal)
          )
        )
      } else {
        list(
          geom_col_interactive(aes(
            tooltip = tip,
            data_id = !!rlang::sym(var)
          )),
          scale_fill_manual(
            values = sentiment_pal,
            limits = names(sentiment_pal)
          )
        )
      }
    } +
    {
      if (var != "word") {
        scale_y_continuous("Number of Articles",
          labels = scales::comma
        )
      } else {
        scale_y_continuous("Number of Appearances",
          labels = scales::comma
        )
      }
    } +
    {
      if (!var %in% c("india_rank", "max_kword", "front_page")) {
        scale_x_discrete(labels = function(x) str_wrap(x, width = 30))
      }
    } +
    {
      if (!var %in% c("india_rank", "max_kword", "front_page")) {
        coord_flip()
      }
    } +
    labs(
      x = NULL,
      title = str_glue("Frequency of {title_var} from Current Selection"),
      subtitle = sub_title
    ) +
    {
      if (var == "india_rank") {
        xlab("Rank of 'India' keyword for an article")
      }
    } +
    {
      if (var == "max_kword") {
        xlab("Total number of keywords for an article")
      }
    } +
    theme_classic(base_size = 16)
}

draw_girafe <- function(gg, var) {

  hover_fill <- if_else(var == "sentiment", "#fee090", "#a6cee3")
  select_fill <- if_else(var == "sentiment", "#fec44f", "#1f78b4")

  girafe(
    ggobj = gg,
    width_svg = 11,
    height_svg = 9,
    options = list(
      opts_hover(
        css = str_glue("fill:{hover_fill};stroke:gray;stroke-width:2;")
      ),
      opts_selection(
        type = "multiple",
        only_shiny = FALSE,
        css = str_glue("fill:{select_fill};stroke:gray;stroke-width:4;")
      )
    )
  )
}

get_current_n <- function(nested_df) {
  total_n <- format(nrow(full_nested_df),
    big.mark = ","
  )

  n <- format(nrow(nested_df),
    big.mark = ","
  )

  percentage_included <- sprintf(
    "%.1f%%",
    (nrow(nested_df) / nrow(full_nested_df)) * 100
  )

  if (n == total_n) {
    str_glue("All {total_n} articles included (100%)")
  } else {
    str_glue("{n} of {total_n} articles included ({percentage_included})")
  }
}

format_count_selection <- function(count_selected) {
  paste(shQuote(count_selected), collapse = ", ")
}

counts_keyword_legend <- tribble(
  ~`Keyword Categories`,
  "Subjects",
  "Locations",
  "Persons",
  "Organizations",
  "Creative Works"
) %>%
  gt() %>%
  data_color(
    columns = vars(`Keyword Categories`),
    colors = scales::col_factor( # RColorBrewer::brewer.pal(n = 8, name = "Dark2")
      palette = c(
        "Subjects" = "#1B9E77",
        "Locations" = "#D95F02",
        "Persons" = "#7570B3",
        "Organizations" = "#E7298A",
        "Creative Works" = "#66A61E"
      ),
      domain = NULL,
      ordered = TRUE
    )
  ) %>%
  tab_options(
    table.align = "left",
    table.font.size = "medium",
    data_row.padding = px(2)
  )

counts_sentiment_legend <- tribble(
  ~`Sentiment`,
  "Positive",
  "Negative"
) %>%
  gt() %>%
  data_color(
    columns = vars(`Sentiment`),
    colors = scales::col_factor( # RColorBrewer::brewer.pal(n = 3, name = "BrBG")
      palette = c(
        "Positive" = "#67A9CF",
        "Negative" = "#EF8A62"
      ),
      domain = NULL,
      ordered = TRUE
    )
  ) %>%
  tab_options(
    table.align = "left",
    table.font.size = "medium",
    data_row.padding = px(2)
  )

# timeline tab ------------------------------------------------------------

get_xts_by_span <- function(df, time_span, count_or_sentiment) {
  # prepare nested df in correct dygraph format
  df %>%
    mutate(time = floor_date(pub_date, time_span)) %>%
    {
      if (count_or_sentiment == "Sentiment Ratio") {
        calculate_sentiment_ratio(.)
      } else {
        count(., time)
      }
    } %>%
    ts_xts()
}

calculate_sentiment_ratio <- function(df) {
  df %>%
    mutate(text = str_c(headline, " ", abstract, " ", lead_paragraph),
           text = str_replace_all(str_to_lower(text),
                                  "[^[:alnum:] ]", "")
    ) %>%
    select(time, text) %>%
    unnest_tokens(word, text) %>% # from {tidytext} [already does some normalizing]
    anti_join(custom_stop_words, by = "word") %>%
    inner_join(get_sentiments("bing"), by = "word") %>%
    count(time, sentiment) %>%
    spread(sentiment, n) %>%
    mutate(n = positive / negative) %>%
    select(-c(negative, positive))
}

dyUnzoom <- function(dygraph) {
  # unzoom plugin: https://rstudio.github.io/dygraphs/gallery-plugins.html
  dyPlugin(
    dygraph = dygraph,
    name = "Unzoom",
    path = system.file("plugins/unzoom.js", package = "dygraphs")
  )
}

# for shading the timeline background
govt_list <- split(govt, seq(nrow(govt)))
add_shades <- function(x, periods, ...) {
  # https://stackoverflow.com/questions/30805017/dyshading-r-dygraph
  for (period in periods) {
    x <- dyShading(x,
      from = period$from,
      to = period$to,
      color = period$color, ...
    )
  }
  x
}

draw_dygraph <- function(xts, nested_df, count_or_sentiment) {

  if (count_or_sentiment == "Sentiment Ratio") {
    main <- "Sentiment Ratio over Time"
    label <- "+/- Sentiment"
    strokeWidth <- 0
  } else {
    main <- "Number of Articles over Time"
    label <- "Article Count"
    strokeWidth <- 1
  }

  dygraph(xts, main = main) %>%
    dyOptions(
      drawPoints = TRUE,
      pointSize = 2,
      axisLineWidth = 2.5,
      colors = "black",
      strokeWidth = strokeWidth
    ) %>%
    dySeries(label = label) %>%
    dyHighlight(
      highlightCircleSize = 5,
      highlightSeriesBackgroundAlpha = 0.2
    ) %>%
    dyRangeSelector(
      dateWindow = c(
        min(nested_df$pub_date),
        max(nested_df$pub_date)
      ),
      retainDateWindow = FALSE
    ) %>%
    add_shades(govt_list) %>%
    dyUnzoom() %>%
    {
      if (count_or_sentiment == "Sentiment Ratio") {
        dyLimit(., limit = 1, color = "white")
      } else {
        .
      }
    }
}

shading_table <- tribble(
  ~`Timeline Shading`,
  "British Raj",
  "Indian National Congress / UPA",
  "Bharatiya Janata Party / NDA",
  "Janata Party",
  "Janata Party (Secular) with INC",
  "Janata Dal (National / United Front)",
  "Samajwadi Janata Party with INC"
) %>%
  gt() %>%
  data_color(
    columns = vars(`Timeline Shading`),
    colors = scales::col_factor(
      palette = c(
        "British Raj" = "#fb9a99",
        "Indian National Congress / UPA" = "#a6cee3",
        "Bharatiya Janata Party / NDA" = "#FF9933",
        "Janata Party" = "#1f78b4",
        "Janata Party (Secular) with INC" = "#CAB2D6",
        "Janata Dal (National / United Front)" = "#33a02c",
        "Samajwadi Janata Party with INC" = "#b2df8a"
      ),
      domain = NULL,
      ordered = TRUE
    )
  ) %>%
  tab_options(
    table.align = "left",
    table.font.size = "medium",
    data_row.padding = px(2)
  )

# the map -----------------------------------------------------------------

clat <- 22.5
clon <- 82.5
my_zoom <- 5

prep_map_data <- function(unnested_df) {
  # from unnested df, get counts of locations and coords
  unnested_df %>%
    filter(name == "glocations", value != "india") %>%
    count(value, country, lat, lon) %>%
    mutate(
      value = str_to_title(value),
      label = str_glue("{value}: {n}")
    ) %>%
    filter(!is.na(lat)) # can remove this when fix the geocoding error Puducherry
}

draw_base_map <- function() {
  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    setView(clon, clat, my_zoom) %>%
    addResetMapButton() %>%
    addMiniMap(toggleDisplay = TRUE, minimized = TRUE)
}

update_map <- function(m, df, base_radius) {
  lpal <- colorFactor(
    palette = c("#1b9e77", "#d95f02"), # Dark2
    domain = df$country
  )

  leafletProxy(m, data = df) %>%
    clearShapes() %>%
    # getting an error from label argument w/ 0 rows: https://stackoverflow.com/questions/62000375/r-leaflet-error-when-plotting-labels-with-a-0-row-dataframe
    {
      if (nrow(df) != 0) {
        addCircles(.,
          lng = ~lon, lat = ~lat,
          radius = ~ sqrt(n) * base_radius * 1000,#5000,
          label = ~label, color = ~ lpal(country)
        )
      } else {
        .
      }
    }
}

get_location_clicked <- function(unnested_df, latitude, longitude) {
  unnested_df %>%
    filter(
      lat == latitude,
      lon == longitude
    ) %>%
    head(1) %>%
    pull(value)
}

myMapModal <- function(location) {
  modalDialog(
    title = str_glue('Do you want to filter for only articles that include "{str_to_title(location)}" as a keyword?'),
    "Doing so will remove all other keyword filters.",
    actionButton("filter_location", "Yes"),
    easyClose = TRUE
  )
}


# keyword pairs tab -------------------------------------------------------

get_top_kword_counts <- function(unnested_df, n_keywords) {
  unnested_df %>%
    filter(value != "india") %>% # india would be in every pair
    group_by(value) %>%
    mutate(n = n()) %>% # keyword value counts
    ungroup() %>%
    arrange(desc(n)) %>%
    distinct(value, n) %>%
    top_n(n_keywords) %>%
    mutate(Var2 = factor(value, levels = value)) %>%
    select(-value, Var2_n = n)
}

percent <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}

get_keyword_pairs <- function(unnested_df, top_kword_counts) {
  unnested_df %>%
    filter(value %in% top_kword_counts$Var2) %>% # only looking at associations among top keywords
    select(url, Var1 = value) %>%
    mutate(n = 1) %>%
    spread(Var1, n, fill = 0) %>%
    select(-url) %>%
    {
      crossprod(as.matrix(.))
    } %>% # var1 and var2 as factor
    as.data.frame.table(responseName = "weight") %>%
    left_join(top_kword_counts, by = c("Var2")) %>%
    mutate(
      weight_over_var2n = weight / Var2_n,
      weight_over_var2n = if_else(weight_over_var2n == 1.0,
        NA_real_,
        weight_over_var2n
      ),
      pct = if_else(is.na(weight_over_var2n),
        NA_character_,
        percent(weight_over_var2n)
      ),
      tip = if_else(is.na(pct),
        NA_character_,
        as.character(str_glue("{pct} of articles with the keyword '{Var2}' also have the keyword '{Var1}'"))
      )
    ) %>%
    arrange(desc(Var2_n), desc(weight)) %>%
    mutate(
      Var1 = factor(Var1, levels = unique(Var2)),
      Var2 = factor(Var2, levels = unique(Var2))
    ) %>%
    rowid_to_column()
}

draw_keyword_heatmap <- function(keyword_pairs) {
  gg <- ggplot(
    keyword_pairs,
    aes(x = Var1, y = Var2)
  ) +
    geom_tile_interactive(aes(
      fill = weight_over_var2n,
      data_id = rowid,
      tooltip = tip
    )) +
    scale_fill_viridis_c() +
    scale_x_discrete(label = function(x) str_trunc(x, width = 20)) +
    labs(x = NULL, y = NULL) +
    theme_classic(base_size = 16) +
    theme(
      legend.title = element_blank(),
      legend.key.size = unit(1.5, "cm")
    ) +
    guides(x = guide_axis(angle = 45))

  girafe(
    ggobj = gg,
    width_svg = 12,
    height_svg = 9,
    options = list(
      opts_selection(
        type = "single",
        css = "stroke:black;stroke-width:8"
      ),
      opts_toolbar(saveaspng = FALSE),
      opts_hover(css = "stroke:white;stroke-width:4")
    )
  )
}

format_heatmap_selection <- function(keyword_pairs, heatmap_cell) {
  heatmap_row <- keyword_pairs %>%
    filter(rowid == heatmap_cell)

  str_glue("Of {heatmap_row$Var2_n} articles with the keyword '{heatmap_row$Var2}', {heatmap_row$weight} also have the keyword '{heatmap_row$Var1}'.")
}
