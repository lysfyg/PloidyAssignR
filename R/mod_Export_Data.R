#' Export_Data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Export_Data_ui <- function(id) {
    ns <- NS(id)
    tagList()
}

#' Export_Data Server Functions
#'
#' @noRd
mod_Export_Data_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
    })
}

## To be copied in the UI
# mod_Export_Data_ui("Export_Data_1")

## To be copied in the server
# mod_Export_Data_server("Export_Data_1")
