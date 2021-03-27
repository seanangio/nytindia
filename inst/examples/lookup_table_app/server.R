library(shiny)
library(DT)
library(stringdist)
library(dplyr)
library(stringr)
library(tibble)
library(readr)

calculate_sim_scores <- function(df, val) {
    # find most likely strings to be replaced
    expr <- quo(val)
    
    df %>% 
        mutate(sim_score = stringsim(!! expr, value_name)) %>%
        select(1, 3) %>%
        arrange(desc(sim_score))
}

time_stripped <- function() {
    str_replace_all(Sys.time(), " ", "_") %>% 
        str_replace_all("-", "") %>% 
        str_replace_all(":", "_")
}


shinyServer(function(input, output, session) {
    session$onSessionEnded(stopApp)
    
    uploaded_df <- reactive({
        # upload file should be rds; 
        # 1 col df of values like news_desk or keywords
        inFile <- input$file

        if (is.null(inFile))
            return(NULL)

        readRDS(inFile$datapath)
    })
    
    value_counts <- reactive({
        #prepares data on left (values to retain)
        if (is.null(uploaded_df()))
            return(NULL)
        
        uploaded_df() %>% 
            select(value_name = 1) %>% 
            count(value_name, sort = TRUE)
    })
    
    output$display_values <- renderDataTable({
        # displays data on left
        datatable(value_counts(),
                  rownames = FALSE,
                  selection = "single",
                  caption = "Value to Keep")
    })
    
    kept_value <- eventReactive(input$display_values_cell_clicked, {
        # retain selection of value to keep
        input$display_values_cell_clicked$value
    })
    
    replacements <- reactive({
        # prepares data on right (values to be replaced)
        if (!is.null(kept_value())) {
            calculate_sim_scores(value_counts(), kept_value())  
        }
    })
    
    output$display_replacements <- renderDataTable({
        # displays data on right
        datatable(replacements(),
                  rownames = FALSE,
                  selection = "multiple",
                  caption = "Values to Replace")
    })
    
    index <- eventReactive(input$display_replacements_rows_selected, {
        # vector of selected replacement rows
        input$display_replacements_rows_selected
    })
    
    df_to_be_added <- reactive({
        # df of current selection from right plus original
        replacements()[index(),] %>% 
            select(replaced_value = value_name, -sim_score) %>% 
            mutate(keeping = kept_value())
    })
    
    # lookup table needs to accumulate selections
    rv <- reactiveValues()
    rv$lookup_table <- tribble(~replaced_value, ~keeping)
    
    observeEvent(input$write, {
        rv$lookup_table <- rv$lookup_table %>% 
            bind_rows(df_to_be_added())
    })
    
    output$lookup_table <- renderDataTable({
        datatable(rv$lookup_table)
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste0(names(uploaded_df()), "_lookup_", 
                   time_stripped(), ".rds")
        },
        content = function(file) {
            write_rds(rv$lookup_table, file)
        }
    )
    
    

})

