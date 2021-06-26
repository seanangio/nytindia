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
library(scales)
library(tibble)
library(purrr)

full_nested_df <- read_rds(here("inst","examples",
                                "nyt_india_app",
                                "full_nested_df.rds")) %>%
    filter(pub_date < "2021-01-01")

govt <- read_csv(here("inst","examples",
                      "nyt_india_app",
                      "govt.csv"))

my_base_size <- 24
my_point_size <- 3
my_bar_count <- 15
my_width_svg <- 15
my_height_svg <- 10
## @knitr keyword_changes

avg_kw <- full_nested_df %>%
    mutate(year = as.integer(format(pub_date, "%Y"))) %>%
    group_by(year) %>%
    summarize(avg_kw = mean(max_kword))

avg_kw_1860 <- avg_kw[ which(avg_kw$year=="1860"),][["avg_kw"]]
avg_kw_2020 <- round(avg_kw[ which(avg_kw$year=="2020"),][["avg_kw"]], digits = 1)

## @knitr avg_keywords

draw_girafe <- function(gg) {
  girafe(
    ggobj = gg,
    width_svg = 15, #11
    height_svg = 10, #9
    options = list(
      opts_hover(
        #css = str_glue("fill:#a6cee3;stroke:gray;stroke-width:2;")
        css = "fill:#a6cee3;stroke:gray;stroke-width:2;"
      ),
      # turn off for heatmap...
      opts_hover_inv(css = "opacity:0.1;")
    )
  )
}

avg_keywords_gg <- full_nested_df %>%
    mutate(year = as.integer(format(pub_date, "%Y"))) %>%
    group_by(year) %>%
    summarize(avg_kw = mean(max_kword)) %>%
    mutate(tip = str_glue("{year}: {round(avg_kw, digits = 1)}")
           ) %>%
    ggplot(aes(x = year, y = avg_kw)) +
    geom_point_interactive(aes(tooltip = tip,
                               data_id = tip),
                           size = my_point_size+1) +
    scale_x_continuous("") +
    scale_y_continuous("Average keywords per article") +
    ggtitle("The avg. keywords per article has increased dramatically over time.") +
    theme_classic(base_size = my_base_size) +
    theme(axis.text=element_text(size=28),
         axis.title=element_text(size=30)
         )

girafe(
  ggobj = avg_keywords_gg,
  width_svg = my_width_svg,
  height_svg = my_height_svg,
  options = list(
    opts_hover(
      css = str_glue("fill:#a6cee3;stroke:#a6cee3;stroke-width:5;")
    ),
    opts_hover_inv(css = "opacity:0.5;")
  )
)

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
draw_dt <- function(df, caption) {
    # once df is correct format, use DT package to format it
    datatable(df,
              class = "cell-border stripe order-column compact",
              selection = list(mode = "single", target = "cell"),
              caption = caption,
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
    draw_dt(caption = "Selection of 1947 articles with 'Gandhi' in the headline, but not as a keyword")

## @knitr avg_word_count
avg_word_count_gg <- full_nested_df %>%
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
    pivot_longer(-year, names_to = "text_col",
                 values_to = "avg_word_count") %>%
    mutate(tip = str_glue("{text_col} ({year}): {round(avg_word_count, digits = 2)}")) %>%
    ggplot(aes(x = year, y = avg_word_count,
               color = text_col)) +
    geom_point_interactive(aes(tooltip = tip,
                               data_id = tip),
                           size = my_point_size) +
    scale_x_continuous("") +
    scale_y_continuous("Average word count per text column") +
    ggtitle("The contents of a text column varies widely by era.") +
    theme_classic(base_size = my_base_size) +
    theme(
        legend.title = element_blank(),
        legend.position="top",
        axis.text=element_text(size=28),
        axis.title=element_text(size=30),
        legend.text=element_text(size=28)
    ) +
    guides(color= guide_legend(override.aes = list(size = 5)))

girafe(
  ggobj = avg_word_count_gg,
  width_svg = my_width_svg,
  height_svg = my_height_svg,
  options = list(
    opts_hover(
      css = str_glue("stroke-width:5;")
    ),
    opts_hover_inv(css = "opacity:0.5;")
  )
)

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
    draw_dt(caption = "Example of how letters to the editor are represented as 'articles'")
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
            interval = as.numeric(difftime(last_day,
                                           first_day,
                                           units = "days")),
            posts_per_day = n / interval
            )

## @knitr timeline_printed

timeline_gg <- full_nested_df %>%
  mutate(
    year = year(pub_date),
    all = 1,
    print = if_else(printed == TRUE, 1, 0),
    page_1 = if_else(front_page == TRUE &
                    !str_detect(headline, "World Business Briefing"),
                    1, 0)
  ) %>%
  group_by(year) %>%
  summarise(
    page_1 = sum(page_1, na.rm = TRUE),
    print = sum(print, na.rm = TRUE),
    all = sum(all, na.rm = TRUE)
  ) %>%
  pivot_longer(!year, names_to = "type",
               values_to = "count") %>%
  mutate(
    tip = str_glue("{type} ({year}): {count}"),
    type = as_factor(type)
  ) %>%
  ggplot(aes(x = year, y = count,
             group = type, color = type)) +
  geom_point_interactive(aes(tooltip = tip,
                             data_id = tip),
                         size = my_point_size) +
  geom_line() +
  scale_x_continuous(NULL) +
  scale_y_continuous("Number of Articles") +
  scale_color_discrete(NULL,
                       labels = c("Front Page News",
                                  "Printed Articles",
                                  "All Articles (including blogs)")) +
  labs(title = "The India Ink blog inflates the 'article' count for 2012-14.") +
  theme_classic(base_size = 24) +
  theme(
    legend.position="top",
    axis.text=element_text(size=28),
    axis.title=element_text(size=30),
    legend.text=element_text(size=28)
  ) +
  guides(color = guide_legend(override.aes = list(size = 5)))

girafe(
  ggobj = timeline_gg,
  width_svg = my_width_svg,
  height_svg = my_height_svg,
  options = list(
    opts_hover(
      css = str_glue("stroke-width:5;")
    ),
    opts_hover_inv(css = "opacity:0.5;")
  )
)
## @knitr location_counts

count_df <- full_nested_df %>%
  unnest(keywords) %>%
  filter(name == "glocations",
         value != "india") %>%
  count(value, sort = TRUE)

trim_count_df <- count_df %>%
  slice_head(n = my_bar_count) %>%
  mutate(tip = str_c(.[[1]], ": ",
                     number(n,
                            accuracy = 1,
                            big.mark = ",")))

get_subtitle_str <- function(count_df, n_obs) {
  if (nrow(count_df) <= n_obs) {
    NULL
  } else {
    var <- names(count_df)[1]
    left_out <- format(nrow(count_df) - n_obs,
                       big.mark = ",")

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
sub_title <- get_subtitle_str(count_df, my_bar_count)

location_count_gg <- trim_count_df %>%
  ggplot(aes(
    x = fct_reorder(value, n),
    y = n
  )) +
  guides(fill = FALSE) +
  geom_col_interactive(aes(
    tooltip = tip,
    data_id = value
  )) +
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
  theme_classic(base_size = my_base_size) +
  theme(
    axis.text=element_text(size=24),
    axis.title=element_text(size=26)
  )

girafe(
  ggobj = location_count_gg,
  width_svg = my_width_svg,
  height_svg = my_height_svg,
  options = list(
    opts_hover(
      css = str_glue("fill:#a6cee3;stroke:#a6cee3;stroke-width:5;")
    ),
    opts_hover_inv(css = "opacity:0.5;")
  )
)

## @knitr keyword_counts
keyword_counts_gg <- full_nested_df %>%
  unnest(keywords) %>%
  filter(value != "india") %>%
  count(value, sort = TRUE) %>%
  slice_head(n = my_bar_count) %>%
  mutate(tip = str_c(.[[1]], ": ",
                     number(n,
                            accuracy = 1,
                            big.mark = ","))) %>%
  ggplot(aes(
    x = fct_reorder(value, n),
    y = n
  )) +
  geom_col_interactive(aes(
    tooltip = tip,
    data_id = value
  )) +
  scale_x_discrete(NULL, labels = function(x) str_wrap(x,
                                                 width = 30)) +
  scale_y_continuous("Number of Appearances",
                     labels = scales::comma
  ) +
  labs(title = "Among all articles, 'politics and government' \nis a near ubiquitous keyword.") +
  coord_flip() +
  theme_classic(base_size = my_base_size) +
  theme(
    axis.text=element_text(size=24),
    axis.title=element_text(size=26)
  )

girafe(
  ggobj = keyword_counts_gg,
  width_svg = my_width_svg,
  height_svg = my_height_svg,
  options = list(
    opts_hover(
      css = str_glue("fill:#a6cee3;stroke:#a6cee3;stroke-width:5;")
    ),
    opts_hover_inv(css = "opacity:0.5;")
  )
)

## @knitr jk_timeline
jk_timeline_gg <- full_nested_df %>%
  mutate(year = year(pub_date)) %>%
  unnest(keywords) %>%
  filter(value == "kashmir and jammu (india)") %>%
  count(year) %>%
  mutate(tip = str_glue("{year}: {n}")) %>%
  ggplot(aes(x = year, y = n)) +
  geom_point_interactive(aes(
    tooltip = tip,
    data_id = tip
  ), size = my_point_size+1) +
  geom_line() +
  scale_x_continuous(NULL) +
  scale_y_continuous("Number of Appearances") +
  labs(title = "Articles on Kashmir rise and fall sharply over time.") +
  theme_classic(base_size = my_base_size) +
  theme(
    axis.text=element_text(size=24),
    axis.title=element_text(size=26)
  )

girafe(
  ggobj = jk_timeline_gg,
  width_svg = my_width_svg,
  height_svg = my_height_svg,
  options = list(
    opts_hover(
      css = str_glue("fill:#a6cee3;stroke:#a6cee3;stroke-width:5;")
    ),
    opts_hover_inv(css = "opacity:0.5;")
  )
)

## @knitr keyword_timeline

key_keywords <- c(
  "modi, narendra", "pakistan", "china",
  "economic conditions and trends", "hinduism",
  "kashmir and jammu (india)", "terrorism")

keyword_timeline_gg <- full_nested_df %>%
  filter(pub_date >= "2000-01-01") %>%
  unnest(keywords) %>%
  filter(value %in% key_keywords) %>%
  nest(keywords = c(name, value, rank, lat, lon, country)) %>%
  mutate(
    year = year(pub_date),
    kw_pakistan = purrr::map_dbl(keywords,
                                 function(df) if_else("pakistan" %in% df$value, 1, 0)
    ),
    kw_kashmir = purrr::map_dbl(keywords,
                                 function(df) if_else("kashmir and jammu (india)" %in% df$value, 1, 0)
    ),
    kw_modi = purrr::map_dbl(keywords,
                  function(df) if_else("modi, narendra" %in% df$value, 1, 0)
    ),
    kw_china = purrr::map_dbl(keywords,
                             function(df) if_else("china" %in% df$value, 1, 0)
    ),
    kw_hinduism = purrr::map_dbl(keywords,
                             function(df) if_else("hinduism" %in% df$value, 1, 0)
    ),
    kw_economics = purrr::map_dbl(keywords,
                                 function(df) if_else("economic conditions and trends" %in% df$value, 1, 0)
    ),
    kw_terrorism = purrr::map_dbl(keywords,
                                  function(df) if_else("terrorism" %in% df$value, 1, 0)
    )
  ) %>%
  select(year | starts_with("kw_")) %>%
  rename_with(~ gsub("kw_", "", .x), starts_with("kw_")) %>%
  group_by(year) %>%
  summarise_all(mean) %>%
  pivot_longer(!year, names_to = "keyword", values_to = "pct") %>%
  mutate(
    pct_round = round(pct, 2),
    tip = str_glue("{year}: {keyword}, {pct_round}")) %>%
  ggplot(aes(x = year, y = pct,
             group = keyword, color = keyword)) +
  geom_line_interactive(aes(tooltip = keyword,
                            data_id = keyword)) +
  geom_point_interactive(aes(tooltip = tip,
                             data_id = tip),
                         size = my_point_size) +
  scale_color_brewer("Keyword", palette = "Dark2") +
  scale_y_continuous("Percentage of Articles including Keyword") +
  scale_x_continuous("") +
  ggtitle("In relative terms, a keyword like 'Modi, Narendra' has risen and \n'Pakistan' has fallen over the past 20 years.") +
  theme_classic(base_size = 24) +
  theme(
    axis.text=element_text(size=24),
    axis.title=element_text(size=26)
  )

girafe(
  ggobj = keyword_timeline_gg,
  width_svg = my_width_svg,
  height_svg = my_height_svg,
  options = list(
    opts_hover(
      css = str_glue("stroke-width:5;")
    ),
    opts_hover_inv(css = "opacity:0.5;")
  )
)

## @knitr keyword_heatmap_stats
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

top_kwords <- full_nested_df %>%
  unnest(keywords) %>%
  get_top_kword_counts(12)

keyword_pairs <- full_nested_df %>%
  unnest(keywords) %>%
  get_keyword_pairs(top_kwords)

keyword_tip <- keyword_pairs %>%
  filter(Var1 == "politics and government",
         Var2 == "elections") %>%
  pull(tip)

has_politics <- function(df) {
  if_else("politics and government" %in% df$value, 1, 0)
}

politics_pct <- full_nested_df %>%
  mutate(pol_govt = map_dbl(keywords, has_politics)) %>%
  summarise(sum = sum(pol_govt),
            n = n(),
            pct = sum / n * 100) %>%
  pull(pct)

## @knitr keyword_heatmap
heatmap_gg <- ggplot(
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
  labs(x = NULL,
       y = NULL,
       title = "How often does a keyword appear with another?") +
  theme_classic(base_size = my_base_size) +
  theme(
    legend.title = element_blank(),
    legend.key.size = unit(1.5, "cm"),
    axis.text=element_text(size=24)
  ) +
  guides(x = guide_axis(angle = 45))

girafe(
  ggobj = heatmap_gg,
  width_svg = my_width_svg,
  height_svg = my_height_svg,
  options = list(
    opts_hover(
      css = str_glue("stroke:black;stroke-width:5;")
    )
  )
)

