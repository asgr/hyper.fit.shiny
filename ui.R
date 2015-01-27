shinyUI(fluidPage(
    
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
                         )
                         #actionButton(inputId="test_button", label=span("test"), icon("random"))
                         ),
                     mainPanel(
                         plotOutput("test_plot2d"),
                         webGLOutput("test_plot3d", width="100%")
                         )
                 ),
                 br()
        ),
        tabPanel("Info",
                 p("info contents")
        )
    )
))