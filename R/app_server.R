#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @noRd
#'

app_server <- function(input, output, session) {
    # to address "no visible binding for global variable"
    data_K562_SC_norm <- NULL

    # bslib::bs_themer()


    # Your application server logic
    ##############
    # Upload & View Data: counts, fraction_w, ploidy ....
    ##############

    upload_data <- mod_Data_Upload_server("Data_Upload_1") # produces a reactive


    ##############
    # Inspect Ploidy Results: either from current analysis, after uploading from previous analysis or example data set.
    # If no data is uploaded/created show example data.
    ##############
    # input selection & information
    ##############
    input_data <- reactive({
        if (input$select_input == "upload") {
            validate(
                need("cons_ploidy" %in% colnames(upload_data()), "Please upload a ploidy analysis data set to browse.")
            )
            return(upload_data())
        } else if (input$select_input == "output_tool") {
            validate(
                need(output_data(), "Run Analysis to plot the results.")
            )
            return(output_data())
        } else if (input$select_input == "K562") {
            if (!exists("data_K562_SC_norm")) {
                shinyFeedback::showFeedbackDanger(inputId = "plot_karyogram", "Danger: Internal dataset of K562 compromised.")
            } else {
                shinyFeedback::hideFeedback(inputId = "plot_karyogram")
                return(data_K562_SC_norm)
            }
        }
    })


    observe({
        validate(
            need(input_data(), "Please select a dataset to plot ploidy analysis results.")
        )
        mod_Plot_Parameters_server("plot_karyogram", input_data = input_data(), plot_style = "karyogram")
        mod_Plot_Parameters_server("plot_pattern", input_data = input_data(), plot_style = "pattern")
        mod_Plot_Parameters_server("plot_heatmap", input_data = input_data(), plot_style = "heatmap")
        # mod_Export_Plot_server("Export_Plot_1", input_data = input_data(), plot_style = "karyogram")
        # mod_Export_Plot_server("Export_Plot_2",input_data = input_data(), plot_style = "pattern")
        # mod_Export_Plot_server("Export_Plot_3",input_data = input_data(), plot_style = "heatmap")
    })




    # plot selection
    ##############
    # View Karyogram
    ##############


    # View Distribution Pattern
    ##############

    # View SC Heatmap
    ##############



    output_data <- mod_PloidyAnalysis_server("PloidyAnalysis_1")
}
