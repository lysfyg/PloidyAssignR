#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @import data.table
#' @importFrom shinyFeedback showFeedbackDanger hideFeedback showFeedbackWarning useShinyFeedback
#' @importFrom bslib bs_theme
#'
#' @noRd
app_ui <- function(request) {
    tagList(
        # Leave this function for adding external resources
        golem_add_external_resources(),
        # Your application UI logic
        tags$head(
            tags$style(
                # position of notifications
                HTML(".shiny-notification {
                         position:fixed;
                         top: calc(90%);
                         left: calc(10%);
                         }
                         "
                )
            )
        ), #css

        navbarPage("PloidyAssignR",
                   id= "navbar",
            theme = bslib::bs_theme(
                verion = 5, bootswatch = "united",
                bg = "#fff",
                fg = "#000", # font color,
                primary = "#e95420",
                secondary = "#668b8b",
                success = "#acc151",
                info = "#6cb4ca",
                warning = "#e5c009",
                danger = "#df382c",
            ),

            ##############
            # Landing Page
            ##############
            tabPanel(
                title = "Start here", icon = icon("circle-play"),
                h1("PloidyAssignR"),
                div(
                    "We developed a framework that uses the unique properties of Strand-seq
                    to automatically assign copy number states in single cells with complex
                    karyotypes including highly aneuploid such as near tetraploid and near
                    hexaploid cells.",
                    br(),
                    "This framework can characterize complex aneuploidy at a
                    single cell level and as a result can also detect subclonal copy number
                    changes."
                ),
                div("We focused the implementation on the following key features:",
                    tags$ul(
                        tags$li("self-contained and reference independent computation"),
                        tags$li("automated copy
    number assignment for both consensus and single cell ploidy"),
                        tags$li("straight-forward, time efficient and user-friendly interactive analysis
    with appealing graphical output options")
                    )
                ),
                div("PloidyAssignR is offered as a R package for users with prior knowledge in bioinformatics and cluster
                computing.",
                    br(),
                    "To make our tool accessible to the entire genomics community
                we created an easy-to-use Rshiny app for Strand-seq copy number
                assignment studies."),
                hr(),
                tabsetPanel(id = "tabset_start", type = "pills",

                            tabPanel("PloidyAnalysis",
                                     div("PloidyAssignR is an R package with an optional R shiny app, which you are currently running.",
                                     br(),
                                         "You can perform ploidy analysis from Strand-seq count data either by the R console directly or through the GUI of this R shiny app.",
                                     br(),
                                         "You will need to:",
                                         tags$ul(
                                             tags$li("Upload Strand-seq count data."),
                                             tags$li("Set the parameters to meet your requirements."),
                                             tags$li("Run analysis... and then use the app's visualization module to inspect the results.")
                                         )
                                     ) #div
                                     ), #tab analysis

                            tabPanel("Visualize Results",
                                     div("Ploidy AssignR includes several options to visualize the results of ploidy analysis.",
                                         tags$ul(
                                             tags$li("Karyogram style plot"),
                                             tags$li("Scatter plot of the calculated relative W-strand state frequencies."),
                                             tags$li("Heatmap of single cell copy number states.")
                                         ), #list
                                         "We have included the ploidy analysis results for 81 cells of the cell line K562.
                                         You can inspect this example data set to understand the visualization options."
                                         )#div
                                     ) #tab visualize results
                ) #tabset
            ), #tabpanel


            ##############
            # Run Analysis
            ##############
            tabPanel(
                "Ploidy Analysis", icon = icon("circle-nodes"),
                h1("Ploidy Analysis"),
                mod_PloidyAnalysis_ui("PloidyAnalysis_1")
            ),

            ##############
            # Inspect Ploidy Results: either from current analysis, after uploading from
            # previous analysis or example data set. If no data is uploaded/created show example data.
            ##############
            tabPanel(
                title = "Browse Data Set", icon = icon("binoculars"),
                h1("Browse Data Set"),
                # input selection & information
                ##############
                fluidRow(
                    column(3,
                           # select data type
                selectInput("select_input",
                    label = "Select Data Set",
                    choices = list(
                        "Uploaded File" = "upload",
                        #"Output File from PloidyAssignR" = "output_tool",
                        "K562 - near triploid complex karyotype" = "K562"
                    ),
                    selected = "K562"
                ), # select_input
                    ),
                column(9,
                       conditionalPanel(
                           condition = "input.select_input == 'upload'",
                           p("Upload data from previous analysis:"),
                       mod_Data_Upload_ui("Upload_Data")
                       )#cond panel
                       ) # column
                ), #fluid row

                # plot selection
                ##############
                tabsetPanel(
                    id = "tabset_plot", type = "pills",
                    # View Karyogram
                    ##############
                    tabPanel("Karyogram",
                        icon = icon("chart-bar"),
                        mod_Plot_Parameters_ui("plot_karyogram")
                    ), # tabpanel karyogram

                    # View Distribution Pattern
                    ##############
                    tabPanel("Distribution Pattern",
                        value = "tab_patterns", icon = icon("shuffle"),
                        mod_Plot_Parameters_ui("plot_pattern")
                    ), # tabpanel pattern

                    # View SC Heatmap
                    ##############
                    tabPanel("Single Cell Heatmap",
                        icon = icon("sitemap"),
                        mod_Plot_Parameters_ui("plot_heatmap")
                    ), # tabpanel heatmap


                ) # tabsetpanel plots
            ) # tabpanel browse
        ) # navbarpage
    ) # taglist
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
    add_resource_path(
        "www",
        app_sys("app/www")
    )

    tags$head(
        #favicon(),
        bundle_resources(
            path = app_sys("app/www"),
            app_title = "PloidyAssignR"
        ),
        # Add here other external resources
        # for example, you can add shinyalert::useShinyalert()

        shinyFeedback::useShinyFeedback(),
        # shinythemes::themeSelector(),
        # theme = bslib::bs_theme(version=5, bootswatch = "united"),
    )
}
