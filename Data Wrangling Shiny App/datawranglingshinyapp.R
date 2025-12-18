#zoom recording: https://illinois.zoom.us/rec/share/ZDzeL_jky66QPRqCzPzuwywOxKEzYmREFlKyonmgAcSsf8GNnBh5sn-FytNaRNFr.l8pClSgUuxoyzbav?startTime=1765322260000

library(tidyverse)
library(shiny)


ui <- fluidPage(
  
  titlePanel("Data Wrangling App"),
  #sidebar user interface
  sidebarLayout(
    sidebarPanel(
      # csv url input and loading
      textInput("csv_url", "Enter CSV URL:", 
                value = ""),
      actionButton("load_btn", "Load Data"),
      hr(),
      
      h4("Wrangling Options"),
      uiOutput("na_col_selector"),
      actionButton("remove_na", "Remove NA Rows"),
      
      uiOutput("column_selector"),
      uiOutput("filter_ui"),
      
      downloadButton("download", "Download Cleaned CSV")
    ),
    
    mainPanel(
      h3("Data Overview"),
      verbatimTextOutput("dimensions"),
      h3("Column Data Types"),
      dataTableOutput("col_types"),
      
      h3("Preview"),
      dataTableOutput("table")
    )
  )
)

server <- function(input, output, session){
  
  #reactive data storage
  data_wrangle <- reactiveVal(NULL)
  
  #Load data
  observeEvent(input$load_btn, {
    req(input$csv_url)
    df <- tryCatch(read.csv(input$csv_url), error = function(e) NULL)
    
    if (is.null(df)) {
      showNotification("Failed to load CSV. Check the URL.", type = "error")
    } else {
      data_wrangle(df)
    }
  })
  
  #Dimensions of current data
  output$dimensions <- renderText({
    req(data_wrangle())
    paste("Rows:", nrow(data_wrangle()), " | Columns:", ncol(data_wrangle()))
  })
  
  #Display current table
  output$table <- renderDataTable({
    req(data_wrangle())
    data_wrangle()
  })
  
  #Column selector UI
  output$column_selector <- renderUI({
    req(data_wrangle())
    checkboxGroupInput("selected_cols", "Select Columns:", 
                       choices = names(data_wrangle()),
                       selected = names(data_wrangle()))
  })
  
  #Filter UI based on selected column
  output$filter_ui <- renderUI({
    req(data_wrangle())
    selectInput("filter_col", "Column to Filter:", choices = names(data_wrangle()))
  })
  
  output$na_col_selector <- renderUI({
    req(data_wrangle())
    selectizeInput(
      "na_cols",
      "Columns to apply NA removal:",
      choices = names(data_wrangle()),
      multiple = TRUE
    )
  })
  
  #Remove NA button
  observeEvent(input$remove_na, {
    req(data_wrangle())
    req(input$na_cols)
    df <- data_wrangle()
    df <- df[complete.cases(df[, input$na_cols, drop = FALSE]), ]
    data_wrangle(df)
  })
  
  
  #Apply column selection
  observeEvent(input$selected_cols, {
    req(data_wrangle())
    if (!is.null(input$selected_cols)) {
      data_wrangle(data_wrangle()[, input$selected_cols, drop = FALSE])
    }
  })
  
  #Filtering 
  observeEvent(input$filter_col, {
    req(data_wrangle())
    showModal(modalDialog(
      title = paste("Filter:", input$filter_col),
      
      textInput("filter_value", "Keep rows where column contains:", ""),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("apply_filter", "Apply Filter")
      )
    ))
  })
  
  # Apply filter logic
  observeEvent(input$apply_filter, {
    req(input$filter_value)
    df <- data_wrangle()
    col <- input$filter_col
    val <- input$filter_value
    
    # contains filter
    df_filtered <- df[grepl(val, df[[col]], ignore.case = TRUE), ]
    
    data_wrangle(df_filtered)
    removeModal()
  })
  
  output$download <- downloadHandler(
    filename = function() "cleaned_data.csv",
    content = function(file){
      write.csv(data_wrangle(), file, row.names = FALSE)
    }
  )
  
  output$col_types <- renderDataTable({
    req(data_wrangle())
    data.frame(
      Column = names(data_wrangle()),
      Type = sapply(data_wrangle(), class)
    )
  })
}

shinyApp(ui, server)
