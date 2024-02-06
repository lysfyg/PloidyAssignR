#' The application server logic
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @noRd
#'

app_server <- function(input, output, session) {

    ###
    # NAVIGATION
    ###




    # PloidyAnalysis
    ############
    output_data <- mod_PloidyAnalysis_server("PloidyAnalysis_1")

    # Upload & View Data: counts, fraction_w, ploidy ....
    ##############
    upload_ploidy <- mod_Data_Upload_server("Upload_Data") # produces a reactive
    output$view_data <- renderPrint({
        validate(
            need("cons_ploidy" %in% colnames(upload_ploidy()), "Please upload a ploidy analysis data set to browse.")
        )
        upload_ploidy()
    })


#    upload_data <- mod_Data_Upload_server("Upload_Data_Ploidy") # produces a reactive



    ##############
    # Inspect Ploidy Results: either from current analysis, after uploading from previous analysis or example data set.
    # If no data is uploaded/created show example data.
    ##############
    # input selection & information
    ##############
    input_data <- reactive({
        if (input$select_input == "upload") {
            validate(
                need("cons_ploidy" %in% colnames(upload_ploidy()), "Please upload a ploidy analysis data set to browse.")
            )
            return(upload_ploidy())
        } else if (input$select_input == "output_tool") {
            #validate(
             #   need(output_data(), "Run Analysis to plot the results.")
            #)
            #return(output_data())
            if (!exists(output_data())) {
                shinyFeedback::showFeedbackDanger(inputId = "plot_karyogram", "Run analysis and then view results here.")
                return(NULL) #test
            } else {
                shinyFeedback::hideFeedback(inputId = "plot_karyogram")
                return(output_data())
            }
        } else if (input$select_input == "K562") {
            if (!exists("data_K562_ploidy")) {
                shinyFeedback::showFeedbackDanger(inputId = "plot_karyogram", "Danger: Internal dataset of K562 compromised.")
            } else {
                shinyFeedback::hideFeedback(inputId = "plot_karyogram")
                return(data_K562_ploidy)
            }
        }
    })


    observe({
        validate(
            need(input_data(), "Please select a dataset to plot ploidy analysis results.")
        )

        # plot selection
        ##############

        # View Karyogram
        ##############
        mod_Plot_Parameters_server("plot_karyogram", input_data = input_data(), plot_style = "karyogram")
        # View Distribution Pattern
        ##############
        mod_Plot_Parameters_server("plot_pattern", input_data = input_data(), plot_style = "pattern")

        # View SC Heatmap
        ##############
        mod_Plot_Parameters_server("plot_heatmap", input_data = input_data(), plot_style = "heatmap")

        # View SC Heatmap
        ##############
       # mod_Plot_Parameters_server("strand-seq", input_data = upload_strand_seq(), plot_style = "strand_seq")

    })#observe


}
