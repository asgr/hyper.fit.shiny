shinyServer(function(input, output, session) {
    
    # the program must check which button was last pressed
    actions <- reactiveValues(last = "none")
    observe({
        if (input$plot_file1 != 0 && !is.null(input$upload_file1))
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

        out <- list()
        out[["params"]] <- list() # out$params contains the list of parameters for hyper.fit
        
        # check if example buttons were pressed
        if(actions$last == 'example_plot_TFR') {
            out$params[["X"]] <- TFR[,c("logv", "M_K")]
            out$params[["vars"]] <- TFR[,c("logv_err", "M_K_err")]^2
            out[["ndims"]] <- 2
            return (out)
        }
        else if(actions$last == 'example_plot_MJB') {
            out$params[["X"]] <- MJB[,c("logM", "logj", "B.T")]
            out$params[["covarray"]] <- makecovarray3d(MJB$logM_err, MJB$logj_err, MJB$B.T_err, MJB$corMJ, 0, 0)
            out[["ndims"]] <- 3
            return (out)
        }
        else if(actions$last == 'example_plot_GAMAsmVsize') {
            out$params[["X"]] <- GAMAsmVsize[,c("logmstar", "logrekpc")]
            out$params[["vars"]] <- GAMAsmVsize[,c("logmstar_err", "logrekpc_err")]^2
            out$params[["weights"]] <- GAMAsmVsize[,"weights"]
            out[["ndims"]] <- 2
            return (out)
        }
        
        # otherwise, use uploaded file
        inFile <- input$upload_file1
        if (is.null(inFile)) {
            return(NULL)
        }
        
        df <- read.table(inFile$datapath, header=TRUE, sep=" ", dec=".")
        ndims <- length(which(c(!is.null(df$x), !is.null(df$y), !is.null(df$z))))
        nrows <- nrow(df)
        
        if(ndims == 3) {
            out$params[["covarray"]] <- makecovarray3d(df$sx,
                                                df$sy,
                                                df$sz,
                                                if(is.null(df$corxy)) rep(0,nrows) else df$corxy,
                                                if(is.null(df$corxz)) rep(0,nrows) else df$corxz,
                                                if(is.null(df$coryz)) rep(0,nrows) else df$coryz
                                                )
            out$params[["X"]] <- matrix(c(df$x, df$y, df$z),nrows,ndims,byrow=FALSE)
            colnames(out$params[["X"]]) <- c("x", "y", "z")
        }
        else if(ndims == 2) {
            out$params[["covarray"]] <- makecovarray2d(df$sx,
                                                df$sy,
                                                if(is.null(df$corxy)) rep(0,nrows) else df$corxy
                                                )
            out$params[["X"]] <- matrix(c(df$x, df$y),nrows,ndims,byrow=FALSE)
            colnames(out$params[["X"]]) <- c("x", "y")
        }
        
        out[["ndims"]] <- ndims
        return (out)
    })
    
    # the 2d plot function
    output$hyper_fit_plot2d <- renderPlot({
        out <- getData()
        if(!is.null(out) && out$ndims == 2) {
            plot(do.call(hyper.fit, out$params))
            session$sendCustomMessage(type = "showPlot", message = "hyper_fit_plot2d")
        }
        else {
            session$sendCustomMessage(type = "hidePlot", message ="hyper_fit_plot2d")
        }
    })
    
    # the 3d plot function
    output$hyper_fit_plot3d <- renderWebGL({
        out <- getData()
        if(!is.null(out) && out$ndims == 3) {
            plot(do.call(hyper.fit, out$params))
            session$sendCustomMessage(type = "showPlot", message = "hyper_fit_plot3d")
        }
        else {
            session$sendCustomMessage(type = "hidePlot", message = "hyper_fit_plot3d")
            points3d(1,1,1)
            axes3d()
        }
    })
    
})