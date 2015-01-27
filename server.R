require("hyper.fit")

# By default, the upload limit is 5MB. It can be with this:
# options(shiny.maxRequestSize = 9*1024^2)

shinyServer(function(input, output, clientData, session) {
    
    getData <- reactive({
        
        inFile <- input$upload_file1
        out <- list()
        
        if (is.null(inFile))
            return(NULL)
        
        df <- read.table(inFile$datapath, header=TRUE, sep=" ", dec=".")
        ndims <- length(which(c(!is.null(df$x), !is.null(df$y), !is.null(df$z))))
        nrows <- nrow(df)
        
        if(ndims == 3) {
            out[["covarray"]] <- makecovarray3d(df$sx,
                                                df$sy,
                                                df$sz,
                                                if(is.null(df$corxy)) rep(0,nrows) else df$corxy,
                                                if(is.null(df$corxz)) rep(0,nrows) else df$corxz,
                                                if(is.null(df$coryz)) rep(0,nrows) else df$coryz
                                                )
            out[["X"]] <- matrix(c(df$x, df$y, df$z),nrows,ndims,byrow=FALSE)
            colnames(out[["X"]]) <- c("x", "y", "z")
        }
        else if(ndims == 2) {
            out[["covarray"]] <- makecovarray2d(df$sx,
                                                df$sy,
                                                if(is.null(df$corxy)) rep(0,nrows) else df$corxy
                                                )
            out[["X"]] <- matrix(c(df$x, df$y),nrows,ndims,byrow=FALSE)
            colnames(out[["X"]]) <- c("x", "y")
        }
        
        out[["ndims"]] <- ndims
        
        return (out)
    })
    
    #output$dynamic_plots <- renderPlot({
        #layout(matrix(c(1,2,3,4),2,2))
        #plot(1:10,1:10)
        #plot(1:20,1:20)
        #plot(1:30,1:30)
        #plot(1:40,1:40)
    #})
    
    output$test_plot2d <- renderPlot({
        out <- getData()
        if(!is.null(out) && out$ndims == 2) {
            fit <- hyper.fit(X=out$X, covarray=out$covarray)
            plot(fit)
        }
    })
    
    output$test_plot3d <- renderWebGL({
        out <- getData()
        if(!is.null(out) && out$ndims == 3) {
            fit <- hyper.fit(X=out$X, covarray=out$covarray)
            plot(fit)
        }
        else {
            points3d(1,1,1)
            axes3d()
        }
    })
    
})