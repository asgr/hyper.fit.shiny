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
        
        # make reactive
        input$replot_plot
        
        # get the algortithm/method
        algo.func <- isolate(input$hyper_fit_algo_func)
        algo.method <- isolate(getMethod()$alg)
        
        # get specs
        Specs <- list()
        spec_list <- isolate(getSpecs())
        if(length(spec_list) > 0) {
            for(i in 1:length(spec_list)) {
                spec_name <- spec_list[[i]]$spec
                spec_id <- paste0("hyper_fit_spec_",spec_name)
                spec_text <- isolate(input[[spec_id]])
                if(is.null(spec_text) || nchar(spec_text)==0) {
                    spec_text <- spec_list[[i]]$default
                    updateTextInput(session, spec_id, value=paste0(spec_list[[i]]$default))
                }
                tryCatch({
                    ev <- eval(parse(text=spec_text))
                    if(is.null(ev)) {
                        Specs[spec_name] <- list(NULL)
                    }
                    else {
                        Specs[[spec_name]] <- ev
                    }
                },
                error = function(e){
                    updateTextInput(session, spec_id, value=paste0(spec_text, " - Error!"))
                })
            }
        }
        
        # check if example buttons were pressed
        if(actions$last == 'example_plot_TFR') {
            return (hyper.fit(X=TFR[,c("logv", "M_K")],
                              vars=TFR[,c("logv_err", "M_K_err")]^2,
                              algo.func=algo.func,
                              algo.method=algo.method,
                              Specs=Specs))
        }
        else if(actions$last == 'example_plot_MJB') {
            return (hyper.fit(X=MJB[,c("logM", "logj", "B.T")],
                              covarray=makecovarray3d(MJB$logM_err, MJB$logj_err, MJB$B.T_err, MJB$corMJ, 0, 0),
                              algo.func=algo.func,
                              algo.method=algo.method,
                              Specs=Specs))
        }
        else if(actions$last == 'example_plot_GAMAsmVsize') {
            return (hyper.fit(X=GAMAsmVsize[,c("logmstar", "logrekpc")],
                              vars=GAMAsmVsize[,c("logmstar_err", "logrekpc_err")]^2,
                              weights=GAMAsmVsize[,"weights"],
                              algo.func=algo.func,
                              algo.method=algo.method,
                              Specs=Specs))
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
            
            return (hyper.fit(X=X, covarray=covarray,
                              algo.func=algo.func,
                              algo.method=algo.method,
                              Specs=Specs))
        }
    })
    
    # the 2d plot function
    output$hyper_fit_plot2d <- renderPlot({
        out <- getData()
        if(!is.null(out) && out$dims == 2) {
            
            # plot output
            plot(out,
                 doellipse=input$hyper_fit_doellipse,
                 sigscale=c(0,input$hyper_fit_sigscale),
                 trans=input$hyper_fit_trans,
                 dobar=input$hyper_fit_dobar,
                 position=input$hyper_fit_position) #add extra plot options here
        }
    })
    
    # the 3d plot function
    output$hyper_fit_plot3d <- renderWebGL({
        out <- getData()
        if(!is.null(out) && out$dims == 3) {
            plot(out,
                 doellipse=input$hyper_fit_doellipse,
                 sigscale=c(0,input$hyper_fit_sigscale),
                 trans=input$hyper_fit_trans)
        }
        else {
            points3d(1,1,1)
            axes3d()
        }
    })
    
    # the Posterior1 plot
    output$hyper_fit_plotPosterior <- renderPlot({
        out <- getData()
        if(!is.null(out) && out$args$algo.func=="LD") {
            plot(as.mcmc(out$fit$Posterior1))
        }
    })
    
    # gets the selected method
    getMethod <- reactive ({
        alg <- input$hyper_fit_algo_func
        if(alg=="optim")
            method <- input$hyper_fit_optim_method
        else if(alg=="LA")
            method <- input$hyper_fit_LA_method
        else if(alg=="LD")
            method <- input$hyper_fit_LD_method
        return(algsTable[[alg]][[method]])
    })
    
    # gets the selected specs
    getSpecs <- reactive ({
        return(getMethod()$Specs)
    })
    
    # render the inputs required for the chosen method
    output$hyper_fit_specs_inputs <- renderUI ({
        specs <- getSpecs()
        if(length(specs) > 0) {
            lapply(1:length(specs), function(i) {
                spec_name <- specs[[i]]$spec
                spec_label <- spec_name
                textInput(paste0("hyper_fit_spec_",spec_name),label=spec_label,value=specs[[i]]$default)
            })
        }
    })
    
    # render the method being used
    output$hyper_fit_selected_method <- renderUI({
        HTML("<span style='color:#AAAAAA;'>Using ", getMethod()$name, "</span>")
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
        css <- ""
        if(is.null(out)) {
            css <- "#hyper_fit_plot2d {display:none;} #hyper_fit_plot3d {display:none;} #hyper_fit_plotPosterior {display:none;}"
        } else {
            if(out$dims == 2) {
                css <- "#hyper_fit_plot2d {display:block;} #hyper_fit_plot3d {display:none;}"
            } else if(out$dims == 3) {
                css <- "#hyper_fit_plot2d {display:none;} #hyper_fit_plot3d {display:block;}"
            }
            if(out$args$algo.func=="LD") {
                css <- append(css, "#hyper_fit_plotPosterior {display:block;}")
            } else {
                css <- append(css, "#hyper_fit_plotPosterior {display:none;}")
            }
        }
        tags$head(tags$style(HTML(css)))
    })
    
    # dataTable optim
    output$methods_optim_algs = renderDataTable({
        
        # gather info from main table
        acros <- sapply(algsTable$optim, function(alg) { alg[["alg"]] })
        names <- sapply(algsTable$optim, function(alg) { alg[["name"]] })
        links <- sapply(algsTable$optim, function(alg) { alg[["link"]] })
        name_links <- paste0("<a href='",links,"'>",names,"</a>")
        
        # display data frame
        df <- data.frame(acros, name_links)
        colnames(df) <- c("Acronym","Link")
        df
    }, options = list(paging = FALSE, searching = FALSE))
    
    # dataTable LA
    output$methods_LA_algs = renderDataTable({
        
        # gather info from main table
        acros <- sapply(algsTable$LA, function(alg) { alg[["alg"]] })
        names <- sapply(algsTable$LA, function(alg) { alg[["name"]] })
        links <- sapply(algsTable$LA, function(alg) { alg[["link"]] })
        name_links <- paste0("<a href='",links,"'>",names,"</a>")
        
        # display data frame
        df <- data.frame(acros, name_links)
        colnames(df) <- c("Acronym","Link")
        df
    }, options = list(paging = FALSE, searching = FALSE))
    
    # dataTable LD
    output$methods_LD_algs = renderDataTable({
        
        # gather info from main table
        acros <- sapply(algsTable$LD, function(alg) { alg[["alg"]] })
        names <- sapply(algsTable$LD, function(alg) { alg[["name"]] })
        links <- sapply(algsTable$LD, function(alg) { alg[["link"]] })
        name_links <- paste0("<a href='",links,"'>",names,"</a>")
        
        # display data frame
        df <- data.frame(acros, name_links)
        colnames(df) <- c("Acronym","Link")
        df
    }, options = list(paging = FALSE, searching = FALSE))
    
})