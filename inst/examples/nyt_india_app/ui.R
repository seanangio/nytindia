library(shiny)
library(shinyWidgets)
library(markdown)
library(shinydashboard) # for infoBox
library(bsplus) # for info tooltips
library(waiter)

tagList(
    tags$head(tags$script(type="text/javascript", src = "code.js")),

navbarPage("India in The New York Times",
           id = "tab",
           position = "fixed-top",
           theme = "my.css",
    tabPanel(uiOutput("n_filters"),
             use_waiter(),
             waiter_show_on_load(html = spin_fading_circles()),
        sidebarPanel(
            use_bs_tooltip(),
            myInput("single_date_range", TRUE),
            conditionalPanel(
                condition = "input.single_date_range == true",
                myInput("dates")
            ),
            conditionalPanel(
                condition = "input.single_date_range == false",
                myInput("government")
            ),
            hr(),
            myInput("desk"),
            myInput("section_name"),
            myInput("material_type"),
            myInput("by_line"),
            hr(),
            myInput("filter_keyword"),
            conditionalPanel(
                condition = "input.filter_keyword == true",
                myInput("keyword_condition_and")
            ),
            conditionalPanel(
                condition = "input.filter_keyword == true",
                myInput("keywords")
            ),
            myInput("ind_rnk"),
            hr(),
            myInput("filter_text"),
            conditionalPanel(
                condition = "input.filter_text == true",
                myInput("text")
            ),
            myInput("page1"),
            myInput("is_printed")
        ),
        mainPanel(
            fluidRow(
                infoBoxOutput("article_count_box1"),
                infoBoxOutput("filter_count_box"),
                actionBttn("reset_all", "Reset All",
                           style = "simple",
                           icon = icon("refresh"),
                           size = "lg")
            ),
            includeMarkdown("about.md")
        )
    ),

# table tab ---------------------------------------------------------------


    tabPanel("Table",
         fluidRow(
             column(
                 DTOutput("table") %>% withMySpinner(),
                 width = 12
             ),
             column(
                 downloadButton("download_nested", "Keyword-nested RDS file"),
                 downloadButton("download_unnested", "Keyword-unnested CSV file"),
                 width = 12
             )
         )
    ),

# counts tab --------------------------------------------------------------


    tabPanel("Counts",
        sidebarPanel(
            fluidRow(infoBoxOutput("article_count_box2",
                                   width = 11)),
            hr(),
            pickerInput("count_var", "Variable to plot",
                        choices = count_choices),
            conditionalPanel(
                condition = "input.count_var != 'india_rank' &&
                            input.count_var != 'max_kword' &&
                            input.count_var != 'front_page'",
                numericInput("top_n", "Number of items to plot",
                             value = 15, min = 1, step = 1)
            ),
            hr(),
            h5("Where appropriate, clicking on a value in the chart offers the option to filter the dataset for that value."),
            hr(),
            conditionalPanel(
                condition = "input.counts_selected != null &&
                            input.counts_selected != '' &&
                            input.count_var != 'front_page' &&
                            input.count_var != 'india_rank' &&
                            input.count_var != 'max_kword'",
                h5("Current Selection:"),
                textOutput("count_selection"),
                actionButton("filter_counts", "Filter for Current Selection")
            ),
            conditionalPanel(
                condition = "input.count_var == 'all'",
                gt_output("counts_keyword_legend")
            ),
            conditionalPanel(
                condition = "input.count_var == 'sentiment'",
                gt_output("counts_sentiment_legend"),
                h5("Positive and negative sentiment is assigned according to the Bing lexicon. For more information, please see the chapter on Sentiment analysis in the book ",
                   a(href="https://www.tidytextmining.com/sentiment.html",
                     "Tidy Text Mining.", target="_blank"))
            )
        ),
        mainPanel(
            girafeOutput("counts") %>% withMySpinner()
        )
    ),

# timeline tab ------------------------------------------------------------


    tabPanel("Timeline",
             sidebarPanel(
                 fluidRow(infoBoxOutput("article_count_box3",
                                        width = 11)),
                 #hr(),
                 awesomeRadio("count_or_sentiment", "Plot:",
                              choices = c("Article Count",
                                          "Sentiment Ratio"),
                              selected = "Article Count", inline = TRUE),
                 hr(),
                 awesomeRadio("time_aggregation", "Aggregate Articles by:",
                              choices = c("Year" = "year",
                                          "Month" = "month",
                                          "Day" = "day"),
                              selected = "year", inline = TRUE),
                 hr(),
                 div(h5("Dataset's Date Range:", style="display:inline"),
                     textOutput("data_date_from", inline = TRUE),
                     strong(" <> "),
                     textOutput("data_date_to", inline = TRUE)),
                 div(h5("Line Plot's Date Range:", style="display:inline"),
                     textOutput("from", inline = TRUE),
                     strong(" <> "),
                     textOutput("to", inline = TRUE)),
                 br(),
                 actionButton("filter_date_window", "Apply Line Plot's Date Range to Dataset"),
                 br(),
                 br(),
                 actionButton("reset_dates", "Reset Dataset's Date Range to Original"),
                 hr(),
                 gt_output("shading_table")
             ),
             mainPanel(
                 dygraphOutput("dygraph") %>% withMySpinner(),
                 br(),
                 h5("Adjust the date range with the slider or click-and-drag within the plot. A double click in the plot resets the zoom."),
                 h5("Timeline shading data from",
                    a(href="https://en.wikipedia.org/wiki/List_of_prime_ministers_of_India",
                      "Wikipedia.", target="_blank")),
                 br(),
                 h5(uiOutput("sentiment_note"))

             )
    ),

# map tab -----------------------------------------------------------------


    tabPanel("Map",
             tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
             leafletOutput("map") %>% withMySpinner(),
             absolutePanel(
                 id = "map-info", class = "panel panel-default",
                 fixed = TRUE,
                 top = 55,
                 right = "5%",
                 bottom = "auto",
                 width = 0, height = 0,
                 dropdownButton(
                     icon = icon("info"),
                     right = TRUE,
                     status = "primary",
                     fluidRow(infoBoxOutput("article_count_box5",
                                            width = 11)),
                     sliderInput("base_radius", "Base Radius (km)",
                                 min = 1, max = 10,
                                 step = 1, value = 5),
                     hr(),
                     h6("Location keywords representing a country are marked in orange. All other pins are green."),
                     h6("To accommodate the vast differences in counts, the circles are scaled with a square root function."),
                     h6("Click on a circle to open a dialog to filter for that location keyword."),
                     h6("Like with other keywords, effort was made to unify locations under one name. Therefore, a keyword like 'Bombay' was changed to 'Mumbai (India)'."),
                     h6("Geocoding done with Mapquest API. Some errors expected.")
                 )
             )
    ),


# keyword pairs tab -------------------------------------------------------

    tabPanel("Keyword Pairs",
             sidebarPanel(
                 fluidRow(infoBoxOutput("article_count_box4",
                               width = 11)),
                 hr(),
                 numericInput("n_heatmap_keywords",
                              "Number of keywords to include",
                              value = 12, min = 1, step = 1),
                 hr(),
                 h5("The heatmap plots the most frequent keywords starting from the bottom left corner."),
                 h5("For every keyword on the Y-axis, the grid plots the percentage of articles that also have the keyword shown on the X-axis."),
                 h5("Hover over the grid for an explanatory tooltip."),
                 h5("Gray NA squares represent values of 100%."),
                 h5("Click on a square for the raw totals."),
                 hr(),
                 conditionalPanel(
                     condition = "input.heatmap_selection != ''",
                     textOutput("heatmap_selection")
                 ),
             ),
             mainPanel(
                 girafeOutput("keyword_heatmap") %>% withMySpinner()
             )
    ),


# analysis tab ------------------------------------------------------------

    tabPanel("Analysis",
             fluidRow(
                 column(width = 2),
                 column(width = 8,
                        #includeMarkdown("analysis.md"),
                        ),
                 column(width = 2)
             )
    ),


# technical details tab ---------------------------------------------------

    tabPanel("Technical Details",
             fluidRow(
                 column(width = 2),
                 column(width = 8,
                        #includeMarkdown("technical_details.md"),
                 ),
                 column(width = 2)
             )
    )

)
)
