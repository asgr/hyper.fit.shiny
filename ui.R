shinyUI(fluidPage(
    
    #scripts here
    
    titlePanel(title="hyper.fit"),
    tabsetPanel(
        tabPanel("Plot",
                 p("tab 1 contents")
        ),
        tabPanel("Info",
                 p("info contents")
        )
    )
))