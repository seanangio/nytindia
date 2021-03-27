library(shiny)
source("global.R")

shinyServer(function(input, output, session) {
    session$onSessionEnded(stopApp)
  
  waiter_hide()
  
# observer to reset inputs -----------------------------------------------

  observeEvent(input$reset_all, {
        updateSwitchInput(session, "single_date_range", 
                          value = TRUE)
        updateDateRangeInput(session, "dates",
                             start = min(full_nested_df$pub_date),
                             end = max(full_nested_df$pub_date))
        updatePickerInput(session, "government", 
                          selected = governments)
        updatePickerInput(session, "desk", 
                          selected = desks)
        updatePickerInput(session, "section_name", 
                          selected = sections)
        updatePickerInput(session, "material_type", 
                          selected = materials)
        updatePickerInput(session, "by_line", 
                          selected = bylines)
        updateSwitchInput(session, "filter_keyword", 
                          value = FALSE)
        updateSwitchInput(session, "keyword_condition_and", 
                          value = FALSE)
        updatePickerInput(session, "keywords", 
                          selected = values)
        updateSliderInput(session, "ind_rnk", 
                          value = c(1, max(full_nested_df$india_rank)))
        updateSwitchInput(session, "filter_text", 
                          value = FALSE)
        updateTextInput(session, "text", 
                        value = "")
        updateSwitchInput(session, "page1", 
                          value = FALSE)
        updateSwitchInput(session, "is_printed", 
                          value = FALSE)
  })

# filter dataframes -------------------------------------------------------

    cleaned_text <- reactive({
        str_to_lower(input$text)
    })
    
    unnested_df <- reactive({
        
        filter_unnested(
            single_date_range = input$single_date_range,
            dates = input$dates,
            government = input$government,
            desk = input$desk,
            section_name = input$section_name, 
            material_type = input$material_type, 
            by_line = input$by_line,
            filter_keyword = input$filter_keyword,
            keyword_condition_and = input$keyword_condition_and,
            keywords = input$keywords,
            ind_rnk = input$ind_rnk,
            filter_text = input$filter_text,
            text = cleaned_text(),
            page1 = input$page1,
            is_printed = input$is_printed)
    }) #%>% bindCache(
    #   input$single_date_range,
    #   input$dates,
    #   input$government,
    #   input$desk,
    #   input$section_name, 
    #   input$material_type, 
    #   input$by_line,
    #   input$filter_keyword,
    #   input$keyword_condition_and,
    #   input$keywords,
    #   input$ind_rnk,
    #   input$filter_text,
    #   cleaned_text(),
    #   input$page1,
    #   input$is_printed
    # ) %>% bindEvent(input$reset_all)
    
    nested_df <- reactive({
        nest_df(unnested_df())
    })
    
    filter_count <- reactive({
      count_filters_on(input$dates, input$government, input$desk, 
                       input$section_name, input$material_type, 
                       input$by_line, input$filter_keyword, input$keywords,
                       input$ind_rnk, input$filter_text, input$text,
                       input$page1, input$is_printed)
    })
    
    output$n_filters <- renderUI({
      str_glue("Filters ({filter_count()})")
    })
    
    output$filter_count_box <- renderInfoBox({
      infoBox(
        "Filters On", paste0(filter_count()), 
        icon = icon("filter"),
        color = "blue"
      )
    })
    
    article_count_box <- reactive({
      infoBox(
        NULL, current_n(), 
        icon = icon("newspaper"),
        color = "blue"
      )
    })
    
    output$article_count_box1 <- renderInfoBox({
      article_count_box() # Filter tab
    })
    
    output$article_count_box2 <- renderInfoBox({
      article_count_box() # Counts tab
    })
    
    output$article_count_box3 <- renderInfoBox({
      article_count_box() # Timeline tab
    })
    
    output$article_count_box4 <- renderInfoBox({
      article_count_box() # Keyword Pairs tab
    })
    
    output$article_count_box5 <- renderInfoBox({
      article_count_box() # Map tab
    })
    

# testing (to be removed) -------------------------------------------------
    
    output$nested_output <- renderPrint({
        nested_df()
    })
    output$unnested_output <- renderPrint({
        unnested_df()
    })

# Table tab ---------------------------------------------------------------

    dt_df <- reactive({
        prep_dt(nested_df())
    })
    
    output$table <- renderDT({
        draw_dt(dt_df())
    })

    # when filtering by keyword, need to reset cell_clicked
    # seems reactiveValues are way to do that instead of eventReactive
    rv <- reactiveValues(cell_clicked = NULL)
    observeEvent(input$table_cell_clicked,
                 rv$cell_clicked <- input$table_cell_clicked
    )
    
    keyword_df <- reactive({
        prep_keyword_df(rv$cell_clicked, nested_df())
    })
    
    output$keyword_table <- renderDT({
        draw_keyword_dt(keyword_df())
    })

    keyword_rows_selected <- eventReactive(input$keyword_table_rows_selected,
                                          input$keyword_table_rows_selected
    )

    keyword_values_selected <- reactive({
        keyword_df() %>%
          slice(keyword_rows_selected()) %>%
          pull(Value)
    })
    
    observeEvent(input$table_cell_clicked, {

        # 5 is India Keyword column
        if (is.null(rv$cell_clicked$row) || ! rv$cell_clicked$col %in% c(2:6)) {
          return()
        } else if (rv$cell_clicked$col %in% 2:5) { # news desk, section, material, byline
         showModal(
           myDTModal(dt_df(), rv$cell_clicked)
         )
        } else { # india keyword col
          showModal(
            # would like this in a function but not sure how...
            modalDialog(
              title = "Article Keywords",
              DTOutput("keyword_table"),
              fluidRow(
                column(2,
                  actionButton("filter_selected_keyword", 
                               "Filter for selected keyword(s)")
                ),
              ),
              easyClose = TRUE
            )
          )
        }
    })
    
    observeEvent(input$myDTActionButton, {
      # handles filtering for non-keyword cols
      var_id <- case_when(
        rv$cell_clicked$col == 2 ~ "desk",
        rv$cell_clicked$col == 3 ~ "section_name",
        rv$cell_clicked$col == 4 ~ "material_type",
        rv$cell_clicked$col == 5 ~ "by_line",
      )

      updatePickerInput(session, var_id, 
                        selected = rv$cell_clicked$value)
      removeModal()
    })
    
    observeEvent(input$filter_selected_keyword, {
      updatePickerInput(session, "filter_keyword", 
                        selected = TRUE)
      updatePickerInput(session, "keyword_condition_and", 
                        selected = FALSE) 
      updatePickerInput(session, "keywords", 
                        selected = keyword_values_selected())
      rv$cell_clicked <- NULL
      removeModal()
    })
    
    output$download_nested <- myDownloadHandler(nested_df(), ".rds")
    output$download_unnested <- myDownloadHandler(unnested_df(), ".csv")

# Timeline tab ------------------------------------------------------------

    xts <- reactive({
        get_xts_by_span(nested_df(), input$time_aggregation, 
                        input$count_or_sentiment)
    })
    
    output$dygraph <- renderDygraph({
        draw_dygraph(xts(), nested_df(), 
                     input$count_or_sentiment)
    })
    
    output$data_date_from <- renderText({
        as.character(min(nested_df()$pub_date))
    })
    output$data_date_to <- renderText({
        as.character(max(nested_df()$pub_date))
    })
    
    output$from <- renderText({
        strftime(req(input$dygraph_date_window[[1]]), "%F")
    })
    output$to <- renderText({
        strftime(req(input$dygraph_date_window[[2]]), "%F")
    })
    
    observeEvent(input$filter_date_window, {
        updateDateRangeInput(session, "dates",
                             start = strftime(req(input$dygraph_date_window[[1]]), "%F"),
                             end = strftime(req(input$dygraph_date_window[[2]]), "%F"))
        if (input$single_date_range != TRUE) {
          updateSwitchInput(session, "single_date_range", TRUE)
        }
    })
    
    observeEvent(input$reset_dates, {
          updateDateRangeInput(session, "dates",
                               start = min(full_nested_df$pub_date),
                               end = max(full_nested_df$pub_date))
          
          if (input$single_date_range != TRUE) {
              updateSwitchInput(session, "single_date_range", TRUE) 
          }
    })
    
    output$shading_table <- render_gt({shading_table})
    
    output$sentiment_note <- renderUI({
      if (input$count_or_sentiment == "Sentiment Ratio") {
        html('Note: Sentiment ratio is the number of positive words divided by the number of negative words according to the Bing lexicon. For more information, please see the chapter on Sentiment analysis in the book <a href="https://www.tidytextmining.com/sentiment.html" target="_blank">Tidy Text Mining.</a>')
      }
    })


# Counts tab --------------------------------------------------------------

    current_n <- reactive({
        #  n of total n string
        get_current_n(nested_df())
    })
    
    count_df <- reactive({
        prep_count_data(nested_df(), unnested_df(), 
                        input$count_var)
    })
    
    trimmed_count_df <- reactive({
        trim_count_df(count_df(), input$top_n)
    })
    
    gg_subtitle <- reactive({
        get_subtitle_str(count_df(), input$top_n)
    })
    
    gg <- reactive({
        build_gg(trimmed_count_df(), gg_subtitle())
    })
    
    output$counts <- renderGirafe({ 
        draw_girafe(gg(), input$count_var)
    })
    
    selected_count <- eventReactive(input$counts_selected,{
        input$counts_selected
    })
    
    output$count_selection <- renderText({
        format_count_selection(input$counts_selected)
    })
    
    output$counts_keyword_legend <- render_gt({
        counts_keyword_legend
    })
    
    output$counts_sentiment_legend <- render_gt({
      counts_sentiment_legend
    })

    observeEvent(input$filter_counts, {
      if (input$count_var == "news_desk") {
        updatePickerInput(session, "desk", 
                          selected = selected_count())
      } else if (input$count_var == "section") {
        updatePickerInput(session, "section_name", 
                          selected = selected_count())
      } else if (input$count_var == "material") {
        updatePickerInput(session, "material_type", 
                          selected = selected_count())
      } else if (input$count_var == "byline") {
        updatePickerInput(session, "by_line", 
                          selected = selected_count())
      } else if (input$count_var %in% c("all", "subject", "glocations", "persons", "organizations", "creative_works")) {
        updateSwitchInput(session, "filter_keyword", 
                          value = TRUE)
        updateSwitchInput(session, "keyword_condition_and", 
                          value = FALSE)
        updatePickerInput(session, "keywords", 
                          selected = selected_count())
      } else if (input$count_var == "word" || input$count_var == "sentiment") {
        updateSwitchInput(session, "filter_text", 
                          value = TRUE)
        updateTextInput(session, "text", 
                        value = selected_count())
      } else if (input$count_var %in% c("india_rank", "max_kword", "front_page")) {
        NULL
      } 
    })

# Map tab -----------------------------------------------------------------

    map_df <- reactive({
        prep_map_data(unnested_df())
    })

    output$map <- renderLeaflet({
        draw_base_map()
    })
    
    observe({
        # https://github.com/rstudio/leaflet/issues/590
        req(input$tab == "Map") 
        update_map("map", map_df(), input$base_radius)
    })
    
    location_clicked <- eventReactive(input$map_shape_click, {
        get_location_clicked(unnested_df(), 
                             input$map_shape_click$lat, 
                             input$map_shape_click$lng)
    })
    
    observeEvent(input$map_shape_click, {
        showModal(
          myMapModal(location_clicked())
        )
    })
    
    # location clicked is "jammu and kashmir (india)"

    observeEvent(input$filter_location, {
        updateSwitchInput(session, "filter_keyword", 
                          value = TRUE)
        updateSwitchInput(session, "keyword_condition_and", 
                          value = FALSE)
        updatePickerInput(session, "keywords", 
                          selected = location_clicked())
        removeModal()
    })
    
    output$current_n_map <- renderText({
      current_n()
    })
  

# keyword pairs tab -------------------------------------------------------

      
    top_kword_counts <- reactive({
        get_top_kword_counts(unnested_df(), 
                             input$n_heatmap_keywords)
    })
    
    keyword_pairs <- reactive({
        get_keyword_pairs(unnested_df(), 
                          top_kword_counts())
    })
    
    output$keyword_heatmap <- renderGirafe({
        draw_keyword_heatmap(keyword_pairs())
    })
    
    heatmap_cell <- eventReactive(input$keyword_heatmap_selected,
                                       input$keyword_heatmap_selected)
    
    output$heatmap_selection <- renderText({
        format_heatmap_selection(keyword_pairs(), 
                                 heatmap_cell())  
    })
  
    
})



