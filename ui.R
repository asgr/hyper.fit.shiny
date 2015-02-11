shinyUI(fluidPage(
    titlePanel(title="hyper.fit"),
    tabsetPanel(
        tabPanel("Plot",
                 sidebarLayout(
                     sidebarPanel(
                         fluidRow(
                             column(6,
                                    actionButton(inputId="hyper_fit_calculate", label=span("Recalculate"), icon("bar-chart-o"))
                                    ),
                             column(6,
                                    uiOutput("hyper_fit_data_used")
                                    )
                             ),
                         tags$hr(),
                         h4("Algorithm"),
                         fluidRow(
                             column(6,
                                    selectInput(inputId="hyper_fit_algo_func",
                                                label="Algorithm",
                                                choices=list(
                                                    "optim"="optim",
                                                    "LA"="LA",
                                                    "LD"="LD"
                                                    ),
                                                selected="optim")
                                    ),
                             column(6,
                                    conditionalPanel(condition="input.hyper_fit_algo_func == 'optim'",
                                                     selectInput(inputId="hyper_fit_optim_method",
                                                                 label="Method",
                                                                 choices=names(algsTable$optim),
                                                                 selected="Nelder-Mead")
                                    ),
                                    conditionalPanel(condition="input.hyper_fit_algo_func == 'LA'",
                                                     selectInput(inputId="hyper_fit_LA_method",
                                                                 label="Method",
                                                                 choices=names(algsTable$LA),
                                                                 selected="NM")
                                    ),
                                    conditionalPanel(condition="input.hyper_fit_algo_func == 'LD'",
                                                     selectInput(inputId="hyper_fit_LD_method",
                                                                 label="Method",
                                                                 choices=names(algsTable$LD),
                                                                 selected="CHARM")
                                    )
                                    )
                             ),
                         uiOutput("hyper_fit_selected_method"),
                         br(),
                         conditionalPanel(condition="input.hyper_fit_algo_func == 'LD'",
                                          checkboxInput(inputId="hyper_fit_show_specs", label="Specs =", value=FALSE),
                                          conditionalPanel(condition="input.hyper_fit_show_specs == true",
                                                           uiOutput("hyper_fit_specs_inputs")
                                          )
                         ),
                         tags$hr(),
                         h4("Plot Options"),
                         fluidRow(
                             column(6,
                                    sliderInput(inputId="hyper_fit_sigscale", span("SigScale", style="color:#3B68B2;"), min=0.1, max=10, value=4, step=0.1, ticks=FALSE)
                             ),
                             column(6,
                                    sliderInput(inputId="hyper_fit_trans", span("Transparency", style="color:#3B68B2;"), min=0, max=1, value=1, step=0.05, ticks=FALSE)
                             )
                         ),
                         checkboxInput(inputId="hyper_fit_doellipse", label="Ellipses", value=TRUE),
                         tags$hr(),
                         h5("2D only"),
                         fluidRow(
                             column(6,
                                    selectInput(inputId="hyper_fit_position",label="Bar Position",
                                                choices=list(
                                                    "Top"="top",
                                                    "Top Right"="topright",
                                                    "Right"="right",
                                                    "Bottom Right"="bottomright",
                                                    "Bottom"="bottom",
                                                    "Bottom Left"="bottomleft",
                                                    "Left"="left",
                                                    "Top Left"="topleft"
                                                ),
                                                selected="topright")
                             ),
                             column(6,
                                    checkboxInput(inputId="hyper_fit_dobar", label="Bar", value=TRUE)
                             )
                         ),
                         tags$hr(),
                         h4("Upload Data"),
                         selectInput(inputId="file1_separator", label="File type", choices=list("csv"=",",
                                                                                                "tsv"="\t",
                                                                                                "ssv"=" "),
                                     selected=" "),
                         fileInput('upload_file1', 'Choose file to upload',
                                   accept = c(
                                       'text/csv',
                                       'text/comma-separated-values',
                                       'text/tab-separated-values',
                                       'text/plain',
                                       '.csv',
                                       '.tsv'
                                   )
                         ),
                         actionButton(inputId="plot_file1", label=span("Use"), icon("file-text")),
                         tags$hr(),
                         h4("Example Data"),
                         actionButton(inputId="example_plot_TFR", label=span("TFR"), icon("file-text")),
                         actionButton(inputId="example_plot_MJB", label=span("MJB"), icon("file-text")),
                         actionButton(inputId="example_plot_GAMAsmVsize", label=span("GAMAsmVsize"), icon("file-text"))
                         ),
                     mainPanel(
                         webGLOutput("hyper_fit_plot3d", width="100%"),
                         plotOutput("hyper_fit_plot2d"),
                         plotOutput("hyper_fit_plotPosterior"),
                         uiOutput("hyper_fit_summary")
                     )
                 ),
                 br(),br(),br()
        ),
        tabPanel("Methods",
                 h3("Methods"),
                 p("Below are the available methods for optim, LA and LD."),
                 h4("Optim :"),
                 dataTableOutput("methods_optim_algs"),
                 h4("LaplaceApproximation (LA) :"),
                 dataTableOutput("methods_LA_algs"),
                 h4("LaplaceApproximation (LD) :"),
                 dataTableOutput("methods_LD_algs"),
                 br(),br(),br()
        ),
        tabPanel("Info",
                 h3("About"),
                 p(span("Welcome to ICRAR's", strong("hyper.fit"), "website!", style="color:#08c"),
                   "This website is written in the programming language", strong("R"), "and uses the library", strong("Shiny"), "to provide
                   the interface."),
                 br(),br(),br()
        )
    ),
    uiOutput("css_output_plots")
))