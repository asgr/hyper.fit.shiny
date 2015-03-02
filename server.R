shinyServer(function(input, output, session) {
    
    # Data Selection Listeners #
    ############################
    rvs <- reactiveValues(currentData = "none")
    observe({
        if (input$use_file1 != 0) {
            if(!is.null(isolate(input$upload_file1))) {
                rvs$currentData <- c(isolate(input$upload_file1$name), isolate(input$upload_file1$datapath))
            }
        }
    })
    observe({
        if (input$example_TFR != 0)
            rvs$currentData <- 'TFR'
    })
    observe({
        if (input$example_MJB != 0)
            rvs$currentData <- 'MJB'
    })
    observe({
        if (input$example_GAMAsmVsize != 0)
            rvs$currentData <- 'GAMAsmVsize'
    })
    observe({
        if (input$example_hogg != 0)
            rvs$currentData <- 'hogg'
    })
    observe({
        if (input$example_FP6dFGS != 0)
            rvs$currentData <- 'FP6dFGS'
    })
    
    # Selected Data UI #
    ####################
    output$hyper_fit_data_used <- renderUI ({
        HTML("<span style='color:#888888;'>Using ", rvs$currentData[1], "</span>")
    })
    
    # hyper.fit calculation #
    #########################
    fit_result <- reactive({
        
        # make reactive
        input$hyper_fit_calculate
        
        # get the algortithm/method
        algo.func <- isolate(input$hyper_fit_algo_func)
        algo.method <- isolate(getMethod()$alg)
        
        # get specs
        Specs <- list()
        if(algo.func == "LD") {
            spec_list <- isolate(getSpecs())
            if(length(spec_list) > 0 && isolate(input$hyper_fit_specs_checkbox)==T) {
                for(i in 1:length(spec_list)) {
                    
                    # get spec properties
                    spec_name <- spec_list[[i]]$spec
                    spec_id <- paste0("hyper_fit_spec_",spec_name)
                    spec_text <- isolate(input[[spec_id]])
                    
                    # if spec field is empty, use default value
                    if(nchar(spec_text)==0) {
                        spec_text <- spec_list[[i]]$default
                        updateTextInput(session, spec_id, value=spec_list[[i]]$default)
                    }
                    
                    # parse the input
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
                        stop(paste0(spec_name, " has an invalid argument"))
                    })
                }
            }
            else {
                Specs <- NULL
            }
        }
        
        # get more options
        itermax <- isolate(input$hyper_fit_itermax)
        coord.type <- isolate(input$hyper_fit_coord_type)
        scat.type <- isolate(input$hyper_fit_scat_type)
        doerrorscale <- isolate(input$hyper_fit_doerrorscale)
        
        # check if example buttons were pressed
        if(rvs$currentData[1] == 'hogg') {
            return (hyper.fit(X=hogg[-3,c("x", "y")],
                              covarray=makecovarray2d(hogg[-3,"x_err"], hogg[-3,"y_err"], hogg[-3,"corxy"]),
                              itermax=itermax,
                              coord.type=coord.type,
                              scat.type=scat.type,
                              algo.func=algo.func,
                              algo.method=algo.method,
                              Specs=Specs,
                              doerrorscale=doerrorscale))
        }
        else if(rvs$currentData[1] == 'TFR') {
            return (hyper.fit(X=TFR[,c("logv", "M_K")],
                              vars=TFR[,c("logv_err", "M_K_err")]^2,
                              itermax=itermax,
                              coord.type=coord.type,
                              scat.type=scat.type,
                              algo.func=algo.func,
                              algo.method=algo.method,
                              Specs=Specs,
                              doerrorscale=doerrorscale))
        }
        else if(rvs$currentData[1] == 'MJB') {
            return (hyper.fit(X=MJB[,c("logM", "logj", "B.T")],
                              covarray=makecovarray3d(MJB$logM_err, MJB$logj_err, MJB$B.T_err, MJB$corMJ, 0, 0),
                              itermax=itermax,
                              coord.type=coord.type,
                              scat.type=scat.type,
                              algo.func=algo.func,
                              algo.method=algo.method,
                              Specs=Specs,
                              doerrorscale=doerrorscale))
        }
        else if(rvs$currentData[1] == 'GAMAsmVsize') {
            return (hyper.fit(X=GAMAsmVsize[,c("logmstar", "logrekpc")],
                              vars=GAMAsmVsize[,c("logmstar_err", "logrekpc_err")]^2,
                              weights=GAMAsmVsize[,"weights"],
                              itermax=itermax,
                              coord.type=coord.type,
                              scat.type=scat.type,
                              algo.func=algo.func,
                              algo.method=algo.method,
                              Specs=Specs,
                              doerrorscale=doerrorscale))
        }
        else if(rvs$currentData[1] == 'FP6dFGS') {
            return (hyper.fit(X=FP6dFGS[,c("logIe_J", "logsigma", "logRe_J")],
                              vars=FP6dFGS[,c("logIe_J_err", "logsigma_err", "logRe_J_err")]^2,
                              weights=FP6dFGS[,"weights"],
                              itermax=itermax,
                              coord.type=coord.type,
                              scat.type=scat.type,
                              algo.func=algo.func,
                              algo.method=algo.method,
                              Specs=Specs,
                              doerrorscale=doerrorscale))
        }
        else if(rvs$currentData[1] != "none") {
            
            # read in data from file
            df <- fread(rvs$currentData[2], header=TRUE, sep="auto", data.table=FALSE)
            if(!is.null(df$x) && !is.null(df$y) && !is.null(df$z)) {ndims <- 3}
            else if(!is.null(df$x) && !is.null(df$y)) {ndims <- 2}
            else {stop("Input file needs x and y dimensions.")}
            nrows <- nrow(df)
            
            # construct either 3d data or 2d data
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
            
            # check for weights
            weights <- if(is.null(df$weights)) 1 else df$weights
            
            # return data
            return (hyper.fit(X=X, covarray=covarray, weights=weights,
                              itermax=itermax,
                              coord.type=coord.type,
                              scat.type=scat.type,
                              algo.func=algo.func,
                              algo.method=algo.method,
                              Specs=Specs,
                              doerrorscale=doerrorscale))
        }
        
        # if no data is being used, return NULL
        return (NULL)
    })
    
    # 2d plot function #
    ####################
    output$hyper_fit_plot2d <- renderPlot({
        out <- fit_result()
        if(!is.null(out) && out$dims == 2) {
            plot(out,
                 doellipse=input$hyper_fit_doellipse,
                 sigscale=c(0,input$hyper_fit_sigscale),
                 trans=input$hyper_fit_trans,
                 dobar=input$hyper_fit_use_bar,
                 position=input$hyper_fit_bar_position)
        }
    })
    
    # 3d plot function #
    ####################
    output$hyper_fit_plot3d <- renderWebGL({
        out <- fit_result()
        if(!is.null(out) && out$dims == 3) {
            plot(out,
                 doellipse=input$hyper_fit_doellipse,
                 sigscale=c(0,input$hyper_fit_sigscale),
                 trans=input$hyper_fit_trans)
        }
        else {
            # fixes a shinyRGL bug
            points3d(1,1,1)
            axes3d()
        }
    })
    
    # Posterior1 plot height #
    ##########################
    posterior1_plot_height <- function() {
        fit <- fit_result()
        if(!is.null(fit)) {
            return ((fit$dims+1) * 200)
        }
        return (600)
    }
    
    # Posterior1 plot function (LD only) #
    ######################################
    output$hyper_fit_plotPosterior <- renderPlot({
        out <- fit_result()
        if(!is.null(out) && out$args$algo.func=="LD") {
            plot(as.mcmc(out$fit$Posterior1))
        }
    }, height=posterior1_plot_height)
    
    # function to get the current method #
    ######################################
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
    
    # function to get current spec list #
    #####################################
    getSpecs <- reactive ({
        return(getMethod()$Specs)
    })
    
    # Specs label field #
    #####################
    output$hyper_fit_specs_label <- renderText ({
        if(input$hyper_fit_specs_checkbox==TRUE) {
            if(length(getSpecs()) > 0) {
                "Specs ="
            }
            else {
                "Specs = list( )"
            }
        }
        else {
            "Specs = NULL"
        }
    })
    
    # Specs input fields #
    ######################
    output$hyper_fit_specs_inputs <- renderUI ({
        specs <- getSpecs()
        if(length(specs) > 0) {
            lapply(1:length(specs), function(i) {
                spec_name <- specs[[i]]$spec
                textInput(paste0("hyper_fit_spec_",spec_name),label=spec_name,value=specs[[i]]$default)
            })
        }
    })
    
    # Render current method #
    #########################
    output$hyper_fit_selected_method <- renderUI({
        HTML("<span style='color:#888888;'>Using ", getMethod()$name, "</span>")
    })
    
    # Summary output #
    ##################
    output$hyper_fit_summary <- renderUI({
        out <- fit_result()
        if(!is.null(out)) {
            s <- capture.output(summary(out))
            HTML("<h4>Summary</h4>", paste(as.list(s), collapse="<br/>"))
        }
    })
    
    # CSS output (used to show/hide plots) #
    ########################################
    output$css_output_plots <- renderUI({
        out <- fit_result()
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
    
    # Methods tab - optim table #
    #############################
    output$methods_optim_algs = renderDataTable({
        
        # gather info from main table
        acros <- sapply(algsTable$optim, function(alg) { alg[["alg"]] })
        names <- sapply(algsTable$optim, function(alg) { alg[["name"]] })
        links <- sapply(algsTable$optim, function(alg) { alg[["link"]] })
        name_links <- paste0("<a href='",links,"' target='_blank'>",names,"</a>")
        
        # display data frame
        df <- data.frame(acros, name_links)
        colnames(df) <- c("Acronym","Link")
        df
    }, options = list(paging = FALSE, searching = FALSE))
    
    # Methods tab - LA table #
    ##########################
    output$methods_LA_algs = renderDataTable({
        
        # gather info from main table
        acros <- sapply(algsTable$LA, function(alg) { alg[["alg"]] })
        names <- sapply(algsTable$LA, function(alg) { alg[["name"]] })
        links <- sapply(algsTable$LA, function(alg) { alg[["link"]] })
        name_links <- paste0("<a href='",links,"' target='_blank'>",names,"</a>")
        
        # display data frame
        df <- data.frame(acros, name_links)
        colnames(df) <- c("Acronym","Link")
        df
    }, options = list(paging = FALSE, searching = FALSE))
    
    # Methods tab - LD table #
    ##########################
    output$methods_LD_algs = renderDataTable({
        
        # gather info from main table
        acros <- sapply(algsTable$LD, function(alg) { alg[["alg"]] })
        names <- sapply(algsTable$LD, function(alg) { alg[["name"]] })
        links <- sapply(algsTable$LD, function(alg) { alg[["link"]] })
        name_links <- paste0("<a href='",links,"' target='_blank'>",names,"</a>")
        
        # display data frame
        df <- data.frame(acros, name_links)
        colnames(df) <- c("Acronym","Link")
        df
    }, options = list(paging = FALSE, searching = FALSE))
    
})