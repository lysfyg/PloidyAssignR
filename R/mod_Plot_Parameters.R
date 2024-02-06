#' Sidebar_Plot_Parameters UI Function
#'
#' @description A shiny Module that generates the UI to select the plotting parameters
#' and uses the PloidyAssignR plotting fucntions to generate plots.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import Cairo
#'
mod_Plot_Parameters_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            column(
                3,
                # INFOS
                ############
                h4("Info & Options"),
                wellPanel(
                    uiOutput(ns("overview_info")),
                    uiOutput(ns("select_chrom")),
                    uiOutput(ns("slider_point_size")),
                    uiOutput(ns("select_cell")),
                ), # info & options
                # Download
                #############
                h4("Download Plot"),
                wellPanel(
                    helpText("Set options to download the plot displayed on the right."),
                    textInput(ns("download_plot_name"), "Enter File Name", value = "PloidyAssignR"),
                    radioButtons(ns("download_plot_options"), "File Format",
                        choices = c(
                            "PDF" = "pdf",
                            "PNG" = "png"
                        ),
                        selected = "png"
                    ),
                    numericInput(ns("download_plot_width"), "Enter Plot Width [mm]", value = "400"),
                    numericInput(ns("download_plot_height"), "Enter Plot Height [mm]", value = "300"),
                    numericInput(ns("download_plot_dpi"), "Enter Resolution [dpi]", value = "60"),
                    downloadButton(ns("download_plot"), "Download Plot!")
                ), # download
            ), # column left
            column(
                9,
                h4("PloidyAssignR Plot of Ploidy Assignment"),
                plotOutput(ns("plot_render"),
                    click = "click",
                    hover = "hover",
                    height = "800px"
                ),
            ), # column main
        ) # fluidrow
    ) # taglist
}

#' Sidebar_Plot_Parameters Server Functions
#'
#' @noRd
mod_Plot_Parameters_server <- function(id, input_data, plot_style = "karyogram") {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        # local binding of variable to function
        cell <- cons_ploidy <- NULL

        ###############
        # COLUMN LEFT
        ##############

        # Plotting Options
        ##############################
        output$overview_info <- renderUI({
            validate(
                need(input_data, "Select Data to plot.")
            )
            tagList(
                p(
                    "The sample ", tags$b(unique(input_data[, sample])), "contains ",
                    tags$b(length(unique(input_data[, cell]))), "cells and is near",
                    tags$b(median(na.omit(input_data[, cons_ploidy]))), "-ploid."
                )
            )
        })
        if (plot_style != "karyogram") {
            output$select_chrom <- renderUI({
                ns <- session$ns
                selectInput(ns("chrom"),
                    label = "Select chromsome to display:",
                    choices = c(extract_chromosomes(input_data))
                )
            }) # renderUI


            if (plot_style == "pattern") {
                output$select_cell <- renderUI({
                    ns <- session$ns
                    selectInput(ns("cell"),
                                label = "Select cell:",
                                choices = c(
                                    "all cells" = "all",
                                    input_data[, unique(cell)]
                                ),
                                selected = "all"
                    )
                }) # renderUI
                output$slider_point_size <- renderUI({
                    ns <- session$ns
                    sliderInput(ns("point_size"), label = "Choose point size:",
                                min = 0.1, max = 2, value = 1, step = 0.1)
                })
            } # if pattern

        } # if not karyogram

        output$download_plot <- downloadHandler(
            filename = function() {
                paste0(Sys.Date(), "_", input$download_plot_name, ".", input$download_plot_options)
            },
            content = function(file) {
                ggplot2::ggsave(
                    filename = file,
                    plot = plot_data(),
                    device = input$download_plot_options,
                    dpi = input$download_plot_dpi,
                    units = "mm",
                    width = input$download_plot_width,
                    height = input$download_plot_height,
                )
            }
        )

        #################
        # Main Panel with Plots
        #########################
        plot_data <- reactive({
            # req(input_data)
            # validate(
            #   need(input_data, "Please select data for upload.")
            # )
            if (plot_style == "karyogram") {
                updateTextInput(session, "download_plot_name",
                    value = "PloidyAssignR_Karyogram"
                )
                fct_plot_karyogram(input_data)
            } else {
                validate(
                    need(input$chrom, "Select chromosome to display."),
                    need(plot_style, "Please supply plotting style.")
                )
                if (plot_style == "pattern") {
                    updateTextInput(session, "download_plot_name",
                        value = "PloidyAssignR_DistributionPattern"
                    )
                    if (input$cell == "all") {
                        fct_plot_distribution_patterns(input_data,
                                                       input_chrom = input$chrom, point_size = input$point_size)
                    } else {
                        fct_plot_distribution_patterns(input_data,
                                                       input_chrom = input$chrom, point_size = input$point_size,
                                                       input_cell = input$cell)
                    }
                } else if (plot_style == "heatmap") {
                    updateTextInput(session, "download_plot_name",
                        value = "PloidyAssignR_SC_Heatmap"
                    )
                    fct_plot_sc_heatmap(input_data, input_chrom = input$chrom)
                } #heatmap
            } #not karyogram
        })#reactive

        output$plot_render <- renderPlot({
            plot_data()
        }) # renderplot
    })
}
## To be copied in the UI
# mod_Sidebar_Plot_Parameters_ui("Sidebar_Plot_Parameters_1")

## To be copied in the server
# mod_Sidebar_Plot_Parameters_server("Sidebar_Plot_Parameters_1")
