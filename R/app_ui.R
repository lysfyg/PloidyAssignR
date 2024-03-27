#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @import data.table
#' @import markdown
#' @import htmltools
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
                title = "About", icon = icon("circle-play"),

                htmltools::includeMarkdown(app_sys("app/www/About.Rmd"))
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
                        "UploadFile" = "upload",
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
        favicon(
            ico = "favicon",
            rel = "shortcut icon",
            resources_path = "www",
            ext = "png"
        ),
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
