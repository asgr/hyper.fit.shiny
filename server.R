shinyServer(function(input, output, session) {
    
    # Data Selection Listeners #
    ############################
    rvs <- reactiveValues(currentData = "example")
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
    observe({
        if (input$use_example != 0)
            rvs$currentData <- 'example'
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
        if(rvs$currentData[1] == 'example') {
            xval <- c(-2,1,5,8,9)
            yval <- c(0,2,5,6,8)
            sx <- c(0.1,0.2,0.1,0.4,0.3)
            sy <- c(0.2,0.3,0.1,0.2,0.5)
            return(hyper.fit(X=cbind(xval,yval),
                             vars=cbind(sx, sy)^2,
                             itermax=itermax,
                             coord.type=coord.type,
                             scat.type=scat.type,
                             algo.func=algo.func,
                             algo.method=algo.method,
                             Specs=Specs,
                             doerrorscale=doerrorscale))
        }
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
        else {
            
            # get column names
            col_x <- isolate(input$hyper_fit_column_x)
            col_y <- isolate(input$hyper_fit_column_y)
            col_z <- isolate(input$hyper_fit_column_z)
            col_sx <- isolate(input$hyper_fit_column_sx)
            col_sy <- isolate(input$hyper_fit_column_sy)
            col_sz <- isolate(input$hyper_fit_column_sz)
            col_corxy <- isolate(input$hyper_fit_column_corxy)
            col_corxz <- isolate(input$hyper_fit_column_corxz)
            col_coryz <- isolate(input$hyper_fit_column_coryz)
            col_weights <- isolate(input$hyper_fit_column_weights)
            
            # read in data from file
            df <- fread(rvs$currentData[2], header=TRUE, sep="auto", data.table=FALSE)
            if(!is.null(df[[col_x]]) && !is.null(df[[col_y]]) && !is.null(df[[col_z]])) {ndims <- 3}
            else if(!is.null(df[[col_x]]) && !is.null(df[[col_y]])) {ndims <- 2}
            else {stop("Input file needs x and y dimensions.")}
            nrows <- nrow(df)
            
            # construct either 3d data or 2d data
            if(ndims == 3) {
                covarray <- makecovarray3d (if(is.null(df[[col_sx]])) rep(0, nrows) else df[[col_sx]],
                                            if(is.null(df[[col_sy]])) rep(0, nrows) else df[[col_sy]],
                                            if(is.null(df[[col_sz]])) rep(0, nrows) else df[[col_sz]],
                                            if(is.null(df[[col_corxy]])) rep(0, nrows) else df[[col_corxy]],
                                            if(is.null(df[[col_corxz]])) rep(0, nrows) else df[[col_corxz]],
                                            if(is.null(df[[col_coryz]])) rep(0, nrows) else df[[col_coryz]])
                X <- matrix(c(df[[col_x]], df[[col_y]], df[[col_z]]),nrows,ndims,byrow=FALSE)
                colnames(X) <- c(col_x, col_y, col_z)
            }
            else if(ndims == 2) {
                covarray <- makecovarray2d (if(is.null(df[[col_sx]])) rep(0, nrows) else df[[col_sx]],
                                            if(is.null(df[[col_sy]])) rep(0, nrows) else df[[col_sy]],
                                            if(is.null(df[[col_corxy]])) rep(0, nrows) else df[[col_corxy]])
                X <- matrix(c(df[[col_x]], df[[col_y]]),nrows,ndims,byrow=FALSE)
                colnames(X) <- c(col_x, col_y)
            }
            
            # check for weights
            weights <- if(is.null(df[[col_weights]])) 1 else df[[col_weights]]
            
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
    
    # Minimal Summary output #
    ##########################
    output$hyper_fit_small_summary <- renderUI({
        out <- fit_result()
        if(!is.null(out)) {
            HTML("<p><b>alpha = ", out$parm[1], "</b></p>",
                 "<p><b>error = ", out$parm.covar[1], "</b></p>
                 <br/>")
        }
    })
    
    # Summary output #
    ##################
    output$hyper_fit_summary <- renderUI({
        out <- fit_result()
        if(!is.null(out)) {
            if(isolate(rvs$currentData[1]) == "example") {
                HTML('<h4>The File:</h4>
<pre>
"x" "y" "sx" "sy"
-2 0 0.1 0.2
1 2 0.2 0.3
5 5 0.1 0.1
8 6 0.4 0.2
9 8 0.3 0.5
</pre>')
            }
            else {
                s <- capture.output(summary(out))
                HTML("<h4>Summary</h4>", paste(as.list(s), collapse="<br/>"))
            }
        }
    })
    
    # Drop-down headers #
    #####################
    
    output$ui_fit_options_header <- renderUI({
        if(input$ui_show_fit_options)
            HTML("<h4>Fit Options <i class='fa fa-chevron-down' style='color:#AAAAAA;'></i></h4>")
        else
            HTML("<h4>Fit Options <i class='fa fa-chevron-up' style='color:#AAAAAA;'></i></h4>")
    })
    output$ui_plot_options_header <- renderUI({
        if(input$ui_show_plot_options)
            HTML("<h4>Plot Options <i class='fa fa-chevron-down' style='color:#AAAAAA;'></i></h4>")
        else
            HTML("<h4>Plot Options <i class='fa fa-chevron-up' style='color:#AAAAAA;'></i></h4>")
    })
    output$ui_upload_data_header <- renderUI({
        if(input$ui_show_upload_data)
            HTML("<h4>Upload Data <i class='fa fa-chevron-down' style='color:#AAAAAA;'></i></h4>")
        else
            HTML("<h4>Upload Data <i class='fa fa-chevron-up' style='color:#AAAAAA;'></i></h4>")
    })
    output$ui_example_data_header <- renderUI({
        if(input$ui_show_example_data)
            HTML("<h4>Example Data <i class='fa fa-chevron-down' style='color:#AAAAAA;'></i></h4>")
        else
            HTML("<h4>Example Data <i class='fa fa-chevron-up' style='color:#AAAAAA;'></i></h4>")
    })
    output$ui_column_names_header <- renderUI({
        if(input$ui_show_column_names)
            HTML("<b>Column Names <i class='fa fa-chevron-down' style='color:#AAAAAA;'></i></b>")
        else
            HTML("<b>Column Names <i class='fa fa-chevron-up' style='color:#AAAAAA;'></i></b>")
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