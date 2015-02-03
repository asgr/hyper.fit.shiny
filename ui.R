shinyUI(fluidPage(
    titlePanel(title="hyper.fit"),
    tabsetPanel(
        tabPanel("Plot",
                 sidebarLayout(
                     sidebarPanel(
                         h4("Options"),
                         textInput("hyper", label = "Text input", value = "0.1"),
                         tags$hr(),
                         h4("Upload Data"),
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
                         actionButton(inputId="plot_file1", label=span("Plot"), icon("bar-chart-o")),
                         tags$hr(),
                         h4("Examples"),
                         actionButton(inputId="example_plot_TFR", label=span("TFR"), icon("bar-chart-o")),
                         actionButton(inputId="example_plot_MJB", label=span("MJB"), icon("bar-chart-o")),
                         actionButton(inputId="example_plot_GAMAsmVsize", label=span("GAMAsmVsize"), icon("bar-chart-o"))
                         ),
                     mainPanel(
                         webGLOutput("hyper_fit_plot3d", width="100%"),
                         plotOutput("hyper_fit_plot2d")
                     )
                 ),
                 br(),
                 uiOutput("hyper_fit_summary")
        ),
        tabPanel("Info",
                 h3("About"),
                 p(span("Welcome to ICRAR's", strong("hyper.fit"), "website!", style="color:#08c"),
                   "This website is written in the programming language", strong("R"), "and uses the library", strong("Shiny"), "to provide
                   the interface.")
        )
    ),
    uiOutput("css_output")
))