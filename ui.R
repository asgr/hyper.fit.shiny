shinyUI(fluidPage(
    
    titlePanel(title="hyper.fit"),
    tabsetPanel(
        tabPanel("Plot",
                 fileInput('upload_testfile', 'Choose file to upload',
                           accept = c(
                               'text/csv',
                               'text/comma-separated-values',
                               'text/tab-separated-values',
                               'text/plain',
                               '.csv',
                               '.tsv'
                           )
                 ),
                 actionButton(inputId="test_button", label=span("test"), icon("random")),
                 # plotOutput("dynamic_plots", height="600px", width="100%"),
                 plotOutput("test_plot2d"),
                 webGLOutput("test_plot3d", width="40%"),
                 br()
        ),
        tabPanel("Info",
                 p("info contents")
        )
    )
))