# this is my external script

## @knitr read_data
library(readr)
library(here)
library(dplyr)
library(ggplot2)
library(stringr)
library(magrittr)
library(DT)
library(tidyr)
library(ggiraph)
library(dygraphs)
library(tsbox)
library(lubridate)
library(forcats)

full_nested_df <- read_rds(here("inst","examples",
                                "nyt_india_app",
                                "full_nested_df.rds")) %>%
    filter(pub_date < "2021-01-01")

govt <- read_csv(here("inst","examples",
                      "nyt_india_app",
                      "govt.csv"))

## @knitr keyword_changes
avg_kw <- full_nested_df %>%
    mutate(year = as.integer(format(pub_date, "%Y"))) %>%
    group_by(year) %>%
    summarize(avg_kw = mean(max_kword))

avg_kw_1860 <- avg_kw[ which(avg_kw$year=="1860"),][["avg_kw"]]
avg_kw_2020 <- round(avg_kw[ which(avg_kw$year=="2020"),][["avg_kw"]], digits = 1)

## @knitr avg_keywords
gg_avg_kw <- full_nested_df %>%
    mutate(year = as.integer(format(pub_date, "%Y"))) %>%
    group_by(year) %>%
    summarize(avg_kw = mean(max_kword)) %>%
    mutate(tip = str_c(year, ": ", round(avg_kw, digits = 1))) %>%
    ggplot(aes(x = year, y = avg_kw)) +
    geom_point_interactive(aes(tooltip = tip), size = 2) +
    scale_x_continuous("") +
    scale_y_continuous("Average keywords per article") +
    ggtitle("The average number of keywords per article has increased \ndramatically over time.") +
    theme_classic()
girafe(code = print(gg_avg_kw))

## @knitr early_gandhi
prep_dt <- function(df) {
    # prepare the nested df for the DataTable
    df %>%
        mutate(
            Headline = str_glue('<a href="{url}" target="_blank">{headline}</a>')
        ) %>%
        select(
            Date = pub_date,
            Headline,
            #`News Desk` = news_desk,
            #Section = section,
            #Material = material,
            Byline = byline,
            `India Keyword` = in_of_n_kword,
            Abstract = abstract#,
            #Lead = lead_paragraph
        )
}
draw_dt <- function(df) {
    # once df is correct format, use DT package to format it
    datatable(df,
              class = "cell-border stripe order-column compact",
              selection = list(mode = "single", target = "cell"),
              extensions = "Responsive",
              options = list(
                  pageLength = 5,
                  # Cursor icon changes to hand (pointer) on Hover for keyword col
                  rowCallback = JS(
                      "function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {",
                      "$('td:eq(2)', nRow).css('cursor', 'pointer');",
                      "}"
                  ),
                  columnDefs = list(
                      list(className = "dt-center", targets = 3), # center kw col
                      list(
                          # ellipsis for long text cols
                          targets = c(4),
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
full_nested_df %>%
  filter(
    pub_date > "1947-01-01" & pub_date < "1947-08-15",
    str_detect(headline, "Gandhi")
  ) %>%
    prep_dt() %>%
    draw_dt()

## @knitr avg_word_count
gg_avg_word_count <- full_nested_df %>%
    mutate(
        wc_headline = str_count(headline, "\\S+"),
        wc_abstract = str_count(abstract, "\\S+"),
        wc_lead = str_count(lead_paragraph, "\\S+"),
        year = as.integer(format(pub_date, "%Y"))
    ) %>%
    group_by(year) %>%
    summarize(
        headline = mean(wc_headline),
        abstract = mean(wc_abstract),
        lead_paragraph = mean(wc_lead)
    ) %>%
    pivot_longer(-year, names_to = "text_col", values_to = "avg_word_count") %>%
    mutate(tip = str_c(year, ": ", round(avg_word_count, digits = 2))) %>%
    ggplot(aes(x = year, y = avg_word_count, color = text_col)) +
    geom_point_interactive(aes(tooltip = tip), size = 2) +
    scale_x_continuous("") +
    scale_y_continuous("Average word count per text column") +
    ggtitle("What to expect in a text column varies widely by year.") +
    theme_classic() +
    theme(
        legend.title = element_blank(),
        legend.position="top"
    )
girafe(code = print(gg_avg_word_count))

non_printed_pct <- full_nested_df %>%
    count(printed) %>%
    mutate(pct = n / sum(n)*100) %>%
    filter(printed == FALSE) %>%
    pull(pct) %>%
    round(digits = 2)

front_page_pct <- full_nested_df %>%
    count(front_page) %>%
    mutate(pct = n / sum(n)*100) %>%
    filter(front_page == TRUE) %>%
    pull(pct) %>%
    round(digits = 2)

## @knitr letters_to_editor
full_nested_df %>%
    filter(str_detect(headline, "Why Outsource?")) %>%
    prep_dt() %>%
    draw_dt
# kristof_letters <- full_nested_df %>%
#     filter(material=="Letter") %>%
#     group_by(pub_date) %>%
#     mutate(n = n()) %>%
#     filter(n>6) %>% View()
#     count(pub_date)

## @knitr india_ink
india_ink <- full_nested_df %>%
  filter(str_detect(url, "india.blogs")) %>%
  summarize(n = n(),
            first_day = min(pub_date),
            last_day = max(pub_date),
            interval = as.numeric(difftime(last_day, first_day, units = "days")),
            posts_per_day = n / interval
            )

## @knitr timeline_printed

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

draw_article_blog_dygraph <- function(ts) {

  dygraph(ts, main = "Printed and digital-only article counts") %>%
    dyOptions(
      drawPoints = TRUE,
      pointSize = 2,
      axisLineWidth = 2.5,
      colors = c("white", "black"),
      strokeWidth = 1
    ) %>%
    dySeries(label = "Article Count") %>%
    dyHighlight(
      highlightCircleSize = 5,
      highlightSeriesBackgroundAlpha = 0.2
    ) %>%
    add_shades(govt_list)
}

full_nested_df %>%
  mutate(time = floor_date(pub_date, "year")) %>%
  group_by(printed) %>%
  count(time) %>%
  ts_xts() %>%
  draw_article_blog_dygraph()

## @knitr timeline_front_page

draw_front_page_dygraph <- function(ts) {

  dygraph(ts, main = "Front page Article Counts") %>%
    dyOptions(
      drawPoints = TRUE,
      pointSize = 2,
      axisLineWidth = 2.5,
      colors = c("black"),
      strokeWidth = 1
    ) %>%
    dySeries(label = "Article Count") %>%
    dyHighlight(
      highlightCircleSize = 5,
      highlightSeriesBackgroundAlpha = 0.2
    ) %>%
    add_shades(govt_list)
}

full_nested_df %>%
  filter(front_page==TRUE) %>%
  filter(!str_detect(headline, "World Business Briefing")) %>%
  mutate(time = floor_date(pub_date, "year")) %>%
  count(time) %>%
  ts_xts() %>%
  draw_front_page_dygraph()

## @knitr location_counts
count_df <- full_nested_df %>%
  unnest(keywords) %>%
  filter(name == "glocations",
         value != "india") %>%
  count(value, sort = TRUE)

trim_count_df <- count_df %>%
  slice_head(n = 15) %>%
  mutate(tip = str_c(.[[1]], ": ",
                     scales::number(n,
                            accuracy = 1,
                            big.mark = ",")))

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
sub_title <- get_subtitle_str(count_df, 15)

location_count_gg <- trim_count_df %>%
  ggplot(aes(
    x = fct_reorder(value, n),
    y = n
  )) +
  guides(fill = FALSE) +
  geom_col_interactive(aes(
    tooltip = tip,
    data_id = value,
    fill = "#D95F02"
  )) +
  # scale_fill_manual(
  #   values = "#D95F02"#keyword_pal,
  #   #limits = names(keyword_pal)
  # ) +
  scale_y_continuous("Number of Articles",
                     labels = scales::comma
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x,
                                                 width = 30)) +
  coord_flip() +
  labs(
    x = NULL,
    title = str_glue("Frequency of Location Keywords among all Articles"),
    subtitle = sub_title
  ) +
  theme_classic(base_size = 24) + #16
  theme(
    # fix weird alignment when deployed
    axis.text.y = element_text(hjust = 0.89)
  )

girafe(
  ggobj = location_count_gg,
  width_svg = 15, #11
  height_svg = 10, #9
  options = list(
    opts_hover(
      css = str_glue("fill:#a6cee3;stroke:gray;stroke-width:2;")
    )
  )
)
