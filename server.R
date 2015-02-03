shinyServer(function(input, output, session) {
    
    # the program must check which button was last pressed
    actions <- reactiveValues(last = "none")
    observe({
        if (input$plot_file1 != 0 || !is.null(input$upload_file1))
            actions$last <- 'plot_file1'
    })
    observe({
        if (input$example_plot_TFR != 0)
            actions$last <- 'example_plot_TFR'
    })
    observe({
        if (input$example_plot_MJB != 0)
            actions$last <- 'example_plot_MJB'
    })
    observe({
        if (input$example_plot_GAMAsmVsize != 0)
            actions$last <- 'example_plot_GAMAsmVsize'
    })
    
    # function for getting plot data
    getData <- reactive({
        
        # check if example buttons were pressed
        if(actions$last == 'example_plot_TFR') {
            return (hyper.fit(X=TFR[,c("logv", "M_K")],
                              vars=TFR[,c("logv_err", "M_K_err")]^2))
        }
        else if(actions$last == 'example_plot_MJB') {
            return (hyper.fit(X=MJB[,c("logM", "logj", "B.T")],
                              covarray=makecovarray3d(MJB$logM_err, MJB$logj_err, MJB$B.T_err, MJB$corMJ, 0, 0)))
        }
        else if(actions$last == 'example_plot_GAMAsmVsize') {
            return (hyper.fit(X=GAMAsmVsize[,c("logmstar", "logrekpc")],
                              vars=GAMAsmVsize[,c("logmstar_err", "logrekpc_err")]^2,
                              weights=GAMAsmVsize[,"weights"]))
        }
        else {
            inFile <- input$upload_file1
            if (is.null(inFile)) {
                return(NULL)
            }
            
            df <- read.table(inFile$datapath, header=TRUE, sep=" ", dec=".")
            if(!is.null(df$x) && !is.null(df$y) && !is.null(df$z)) {ndims <- 3}
            else if(!is.null(df$x) && !is.null(df$y)) {ndims <- 2}
            else {stop("Input file needs x and y dimensions.")}
            nrows <- nrow(df)
            
            if(ndims == 3) {
                covarray <- makecovarray3d (if(is.null(df$sx)) rep(0, nrows) else df$sx,
                                            if(is.null(df$sy)) rep(0, nrows) else df$sy,
                                            if(is.null(df$sz)) rep(0, nrows) else df$sz,
                                            if(is.null(df$corxy)) rep(0, nrows) else df$corxy,
                                            if(is.null(df$corxz)) rep(0, nrows) else df$corxz,
                                            if(is.null(df$coryz)) rep(0, nrows) else df$coryz)
                X <- matrix(c(df$x, df$y, df$z),nrows,ndims,byrow=FALSE)
                colnames(X) <- c("x", "y", "z")
            }
            else if(ndims == 2) {
                covarray <- makecovarray2d (if(is.null(df$sx)) rep(0, nrows) else df$sx,
                                            if(is.null(df$sy)) rep(0, nrows) else df$sy,
                                            if(is.null(df$corxy)) rep(0, nrows) else df$corxy)
                X <- matrix(c(df$x, df$y),nrows,ndims,byrow=FALSE)
                colnames(X) <- c("x", "y")
            }
            
            return (hyper.fit(X=X, covarray=covarray))
        }
    })
    
    # the 2d plot function
    output$hyper_fit_plot2d <- renderPlot({
        out <- getData()
        if(!is.null(out) && out$dims == 2) {
            plot(out, dobar=TRUE)
        }
    })
    
    # the 3d plot function
    output$hyper_fit_plot3d <- renderWebGL({
        out <- getData()
        if(!is.null(out) && out$dims == 3) {
            plot(out)
        }
        else {
            points3d(1,1,1)
            axes3d()
        }
    })
    
    # the summary output
    output$hyper_fit_summary <- renderUI({
        out <- getData()
        if(!is.null(out)) {
            s <- capture.output(summary(out))
            HTML("<h4>Summary</h4>", paste(as.list(s), collapse="<br/>"))
        }
    })
    
    # css changes to change the layout
    output$css_output <- renderUI({
        out <- getData()
        if(!is.null(out) && out$dims == 2) {
            tags$head(tags$style(HTML("#hyper_fit_plot2d {display:block;} #hyper_fit_plot3d {display:none;}")))
        } else if(!is.null(out) && out$dims == 3) {
            tags$head(tags$style(HTML("#hyper_fit_plot2d {display:none;} #hyper_fit_plot3d {display:block;}")))
        } else {
            tags$head(tags$style(HTML("#hyper_fit_plot2d {display:none;} #hyper_fit_plot3d {display:none;}")))
        }
    })
    
})