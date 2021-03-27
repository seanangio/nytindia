library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Create Lookup Tables"),
  fluidRow(
      column(
          width = 6,
          fileInput("file", "Upload rds file",
                    accept = c(".rds"))
      )
  ),
  fluidRow(
      column(
          width = 6,
          dataTableOutput("display_values")
      ),
      column(
          width = 6,
          dataTableOutput("display_replacements")
      )
  ),
  fluidRow(
      column(
          width = 12,
          actionButton("write", "Write rows to lookup table"),
          downloadButton("downloadData", "Download"),
          dataTableOutput("lookup_table")
      )
  )
  
))
