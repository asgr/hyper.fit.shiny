shinyUI(fluidPage(
    tags$head(tags$script(src="jquery-1.11.2.min.js")),
    tags$head(tags$script(src="script.js")),
    titlePanel(title="hyper.fit"),
    tabsetPanel(
        tabPanel("Plot",
                 sidebarLayout(
                     sidebarPanel(
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
                         tags$hr(),
                         h4("Examples"),
                         actionButton(inputId="example_plot_FP6dFGS", label=span("FP6dFGS"), icon("bar-chart-o"))
                         ),
                     mainPanel(
                         plotOutput("hyper_fit_plot2d"),
                         webGLOutput("hyper_fit_plot3d", width="100%")
                         )
                 ),
                 br()
        ),
        tabPanel("Info",
                 h3("About"),
                 p(span("Welcome to ICRAR's", strong("hyper.fit"), "website!", style="color:#08c"),
                   "This website is written in the programming language", strong("R"), "and uses the library", strong("Shiny"), "to provide
                   the interface.")
        )
    )
))