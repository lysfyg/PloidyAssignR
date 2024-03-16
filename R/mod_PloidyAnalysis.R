#' PloidyAnalysis UI Function
#'
#' PloidyAnalysis user interface module. Several input text fields and sliders to be used for ploidy analysis.
#'
#' @description A shiny Module.

#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import data.table
#' @importFrom shinyFeedback showFeedbackDanger hideFeedback showFeedbackWarning useShinyFeedback
#' @importFrom shiny NS tagList
#' @importFrom DT DTOutput renderDT
mod_PloidyAnalysis_ui <- function(id) {
    ns <- NS(id)

    tagList(
        fluidRow(
            column(
                3,
                # Upload DATA
                ##################
                h4("Consensus Ploidy Analysis"),
                helpText("Upload a Strand-seq data set containing columns for Watson- and Crick-oriented read-count."),
                wellPanel(

                    # Strand-seq
                    fileInput(
                        ns("upload_data_strand_seq"),
                        label = "Upload Strand-seq Count Data:",
                        #accept = c(".txt", ".csv", ".rda", ".txt.gz")
                    ),
                    ## input_path_counts,
                    # Check Strand-seq Data
                    helpText("Preview Strand-seq count data:"),
                    verbatimTextOutput(ns("view_data_strand_seq"), placeholder = TRUE),
                ),
                hr(),
                helpText("Select Parameters for Ploidy Analysis."),
                wellPanel(
                    # PARAMETERS
                    #################
                    # Optional settings
                    sliderInput(
                        inputId = ns("window"),
                        label = "Window Size [bases]",
                        min = 100000,
                        max = 10000000,
                        value = 1000000,
                        step = 100000
                    ),
                    #  input_window = 10000000L,
                    sliderInput(
                        inputId = ns("step"),
                        label = "Step Size [bases]",
                        min = 100000,
                        max = 5000000,
                        value = 500000,
                        step = 100000
                    ),
                    #  input_step = 5000000L,
                    selectInput(
                        ns("chrom"),
                        label = "Select chromsomes for analysis",
                        choices = c("all" = "all"),
                        selected = "all",
                        multiple = TRUE
                    ),
                    sliderInput(
                        inputId = ns("max_ploidy"),
                        label = "Select Maximum Ploidy",
                        min = 2,
                        max = 10,
                        value = 5,
                        step = 1
                    ),
                    uiOutput(ns("overview_info")), # suggested max ploidy
                    # Run Analysis
                    actionButton(ns("button_run_analysis"), "Run Ploidy Assignment!"),
                ) # wellpanel
            ), # column
            column(
                3,
                # Regions of Confidence
                #################
                h4("Single Cell Copy Number Assignment"),
                helpText("Inspect distribution patterns to choose regions of confidence."),
                wellPanel(
                    selectInput(
                        ns("pattern_chrom"),
                        label = "Select chromsome to display",
                        choices = c("Choose" = "")
                    ),
                    plotOutput(ns("plot_pattern"), height = "200px"),
                ),
                hr(),
                helpText("Create a file.txt containing the ROC and upload to PloidyAssignR."),
                wellPanel(
                    # # Interactive Table of ROC
                    # verbatimTextOutput(ns("txt_running"), placeholder = TRUE),


                    # ROC data
                    fileInput(
                        ns("upload_data_ROC"),
                        label = "Upload Regions of Confidence Table:",
                        accept = c(".txt", ".txt.gz", ".csv", ".rda")
                    ),
                    helpText("Preview ROC Data:"),
                    verbatimTextOutput(ns("view_data_ROC"), placeholder = TRUE),
                    actionButton(ns("button_run_SCNorm"), "Run Single Cell Normalization!"),
                )
            ),
            column(
                6,
                h4("Preview and Save Results"),
                helpText("Set options to download the results displayed above."),
                wellPanel(
                    # After Button click
                    DT::dataTableOutput(
                        ns("view_data_ploidy"),
                        width = "100%",
                        height = "auto",
                        fill = TRUE
                    ),
                    #textInput(ns("output_dir"), "Enter Output Path*"), #  output_dir,
                    textInput(ns("analysis_name"), "Enter Sample Name*", value = "YourAnalysis"), #  analysis_name,


                    # Output
                    downloadButton(ns("download_data_ploidy"), "Download Data!")
                )
            ), # column
        ), # fluid row)
    )
} # taglist


#' PloidyAnalysis Server Functions
#'
#' PloidyAnalysis server logic that takes the input and performs ploidy analysis.
#'
#' @noRd
mod_PloidyAnalysis_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        # local binding of variable to function
        cell <- NULL

        #notifications
        notify <- function(msg, id = NULL) {
            showNotification(msg, id = id, duration = NULL, closeButton = TRUE, type = "message")
        }


        # STRANDSEQ DATA
        ###################
        # upload strandseq data
        data_strand_seq <- reactive({
            req(input$upload_data_strand_seq) # if not supplied all downstream reactives stop executing
            dt <- input_prep(input$upload_data_strand_seq$datapath)
            return(dt)
        })

        # inspect strandseq data
        output$view_data_strand_seq <- renderPrint({
            data_strand_seq()
        })
        # PARAMETERS
        ###############
        observe(updateSelectInput(
            session,
            "chrom",
            choices = c("all" = "all", extract_chromosomes(data_strand_seq()))
        ))



        # Parameter User Feedback
        output$overview_info <- renderUI({
            helpText(
                "The sample ", tags$b(unique(data_strand_seq()[, sample])), "contains ",
                tags$b(length(unique(data_strand_seq()[, cell]))), "cells.\n We suggest max_ploidy = ",
                trunc(log(length(data_strand_seq()[, unique(cell)]), base = 2)), "."
            )
        })

        observe({
            if (input$step >= input$window) {
                shinyFeedback::showFeedbackDanger(inputId = "step",
                                                  text = "Danger: Step-size should be smaller than window-size.")
            } else {
                shinyFeedback::hideFeedback(inputId = "window")
                shinyFeedback::hideFeedback(inputId = "step")
            }
        }) %>% bindEvent(input$step)

        observe({
            if (input$step >= input$window) {
                shinyFeedback::showFeedbackDanger(inputId = "window",
                                                  text = "Danger: Window-size should be larger than step-size.")
            } else {
                shinyFeedback::hideFeedback(inputId = "window")
                shinyFeedback::hideFeedback(inputId = "step")
            }
        }) %>% bindEvent(input$window)


        observe({
            if (input$max_ploidy > trunc(log(length(data_strand_seq()[, unique(cell)]), base = 2))) {
                shinyFeedback::showFeedbackWarning(inputId = "max_ploidy", text = "Maximum ploidy too large.")
            } else {
                # shinyFeedback::hideFeedback("info_max_ploidy")
                shinyFeedback::hideFeedback(inputId = "max_ploidy")
            }
        }) %>% bindEvent(input$max_ploidy)

        # ROC DATA
        ###################
        data_ROC <- reactive({
            req(input$upload_data_ROC) # if not supplied all downstream reactives stop executing
            dt <- data.table::fread(input$upload_data_ROC$datapath)
            return(dt)
        })
        # inspect ROC upload
        output$view_data_ROC <- renderPrint({
            data_ROC()
        })



        # ANALYSIS
        ############

        data_fractions <- reactive({
            # req(input$chrom) # if not supplied all downstream reactives stop executing
            if ("all" %in% input$chrom) {

                id_notify<- notify("Running... Preparing analysis... for ALL chromosomes")
                dt <- calWatsonFractions(
                    data_strand_seq(),
                    input_chrom = NULL,
                    # all chrom
                    input_window = input$window,
                    input_step = input$step
                )
            } else {

                id_notify<- notify("Running... Preparing analysis... for SELECTED chromosomes")
                dt <- calWatsonFractions(
                    data_strand_seq(),
                    input_chrom = input$chrom,
                    input_window = input$window,
                    input_step = input$step
                )
            }
            removeNotification(id_notify)
            id_notify<- notify("Finished preparations for analysis!")
            return(dt)


        }) %>% bindEvent(input$button_run_analysis)

        data_ploidy <- reactive({
            validate(
                need(input$button_run_analysis, "Please upload data and press button to run analysis.")
            )
            id_notify<- notify("Running... assigning ploidy states from distribution patterns!")
            # req (input$chrom) # if not supplied all downstream reactives stop executing
            dt <- assignConsensusPloidy(
                data_fractions(),
                input_chrom = input$chrom,
                max_ploidy_force = input$max_ploidy
            )
            removeNotification(id_notify)
            id_notify<- notify("Finished ploidy assignment!")
            return(dt)


        })

        data_sc_norm <- reactive({

            id_notify<- notify("Running... using ROC to detect single cell copy number!")
            validate(
                need(input$upload_data_strand_seq, "Please upload Strand-seq data."),
                need(input$upload_data_ROC, "Please upload Regions of Confidence."),
            )
            dt <- scCoverageNorm(data_ploidy(), data_ROC())
            removeNotification(id_notify)
            id_notify<- notify("Finished single cell copy number detection!")
            return(dt)

        }) %>% bindEvent(input$button_run_SCNorm)



        # PROGRESS OUTPUT
        ##############
        output$txt_running <- renderPrint({
            "Starting Analysis ... \n"
        }) %>% bindEvent(data_fractions())



        # Ploidy Table
        output$view_data_ploidy <- DT::renderDataTable({
            validate(
                need(data_sc_norm(), "Please upload Strand-seq data and run analysis."),
            )
            data_sc_norm()
        })


        output$download_data_ploidy <- downloadHandler(
            filename = function() {
                paste0(Sys.Date(), "_", input$analysis_name, ".csv")
            },
            content = function(file) {
                data.table::fwrite(data_sc_norm(), file,
                    quote = F
                )

            }
        )

        ## PLOT
        output$plot_pattern <- renderPlot({
            validate(
                need(data_ploidy(), "Run Consensus Ploidy Analysis to inspect Distribution Patterns."),
                need(input$pattern_chrom, "Select a chromosome to find possible regions of confidence.")
            )
            plot <- fct_plot_distribution_patterns(data_ploidy(), input_chrom = input$pattern_chrom)
            plot
        })

        observe(
            updateSelectInput(
                session,
                "pattern_chrom",
                choices = c(extract_chromosomes(data_ploidy()))
            )
        )




        return(data_sc_norm)

    })
}

## To be copied in the UI
# mod_PloidyAnalysis_ui("PloidyAnalysis_1")

## To be copied in the server
# mod_PloidyAnalysis_server("PloidyAnalysis_1")
