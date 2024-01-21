#' Data_Upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom DT DTOutput renderDT
mod_Data_Upload_ui <- function(id) {
    ns <- NS(id)
    tagList(
        # Read file
        fileInput(ns("input_path_data"), label = "Select file to be uploaded.",
                  accept = c(".txt", ".txt.gz", ".csv", ".rda")), ## input_path_counts,



        # Show Table Button
        actionButton(ns("button_show_table"), label = "Show data table!"),

        # Interactive Table of Input Data
        DT::dataTableOutput(ns("input_data_table"), width = "100%", height = "auto", fill = TRUE)
    )
}

#' Data_Upload Server Functions
#'
#' @noRd
mod_Data_Upload_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        # Read file
        input_data <- reactive({
            req(input$input_path_data)
            dt <- input_prep(input$input_path_data$datapath)
            return(dt)
        })

        # Interactive Table of Input Data: show table if button_show_table is clicked
        output$input_data_table <- DT::renderDataTable(input_data()) %>% bindEvent(input$button_show_table)

        return(input_data)
    })
}

## To be copied in the UI
# mod_Data_Upload_ui("Data_Upload_1")

## To be copied in the server
# mod_Data_Upload_server("Data_Upload_1")
