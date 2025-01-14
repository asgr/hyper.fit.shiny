shinyUI(fluidPage(id="main-page",
    
    tags$head(
        tags$title("hyper.fit"),
        tags$link(rel="shortcut icon", href="favicon.ico"),
        tags$style(HTML("
                        #main-page {
                            padding-left:0px;
                            padding-right:0px;
                        }
                        .dataTables_info {display:none;}
                        .dataTables_wrapper .row-fluid {display:none;}
                        .ui_region {
                            min-height: 20px;
                            padding: 10px;
                            background-color: #f1f1f1;
                            margin-bottom:10px;
                        }
                        .ui_region h4 {
                            margin:0px;
                        }
                        .ui_region label[for=ui_show_fit_options] {
                            padding-left:0px;
                            margin-bottom:0px;
                        }
                        #ui_show_fit_options {
                            display:none;
                        }
                        .ui_region label[for=ui_show_plot_options] {
                            padding-left:0px;
                            margin-bottom:0px;
                        }
                        #ui_show_plot_options {
                            display:none;
                        }
                        .ui_region label[for=ui_show_upload_data] {
                            padding-left:0px;
                            margin-bottom:0px;
                        }
                        #ui_show_upload_data {
                            display:none;
                        }
                        .ui_region label[for=ui_show_example_data] {
                            padding-left:0px;
                            margin-bottom:0px;
                        }
                        #ui_show_example_data {
                            display:none;
                        }
                        .ui_region label[for=ui_show_column_names] {
                            padding-left:0px;
                        }
                        #ui_show_column_names {
                            display:none;
                        }
                        #ui_sidebar {
                            padding:10px;
                            background-color:#FAFAFA;
                        }
                        #ui_header_row h1 {
                            vertical-align:middle;
                            color:white;
                            margin:30px;
                        }
                        #ui_header_row {
                            margin-bottom:10px;
                            background: #000 url('space.gif') repeat 0 0;
                        }
                        #ui_footer {
                            text-align:center;
                            margin-bottom:3px;
                        }
                        #ui_recalculate_div {
                            margin:5px;
                        }
                        .container {
                            text-align:justify;
                        }
                        .well {
                            border:none;
                        }
                        "))
    ),
    
    # title #
    #########
    
    fluidRow(id="ui_header_row",
        h1("hyper.fit")
    ),
    
    # Main Content #
    ################
    
    div(class="container-fluid",
    tabsetPanel(
        
        # Plot Tab #
        ############
        tabPanel("Plot",
                 sidebarLayout(
                     
                     # Side Panel #
                     ##############
                     sidebarPanel(
                         id="ui_sidebar",
                         fluidRow(id="ui_recalculate_div",
                                  column(6,
                                         actionButton(inputId="hyper_fit_calculate", label=span("Recalculate"), icon("bar-chart"))
                                  ),
                                  column(6,
                                         uiOutput("hyper_fit_data_used")
                                  )
                         ),
                         tags$hr(),
                         div(class="ui_region well",
                             checkboxInput(inputId="ui_show_fit_options", label=uiOutput("ui_fit_options_header"), value=FALSE),
                             conditionalPanel(condition="input.ui_show_fit_options == true",
                                              br(),
                                              fluidRow(
                                                  column(5,
                                                         selectInput(inputId="hyper_fit_coord_type",
                                                                     label="Coordinate Type",
                                                                     choices=list("Normal Vector"="normvec",
                                                                                  "Alpha"="alpha",
                                                                                  "Theta"="theta"),
                                                                     selected="alpha")
                                                  ),
                                                  column(7,
                                                         selectInput(inputId="hyper_fit_scat_type",
                                                                     label="Scatter Type",
                                                                     choices=list("Orthogonal to Plane"="orth",
                                                                                  "Along vertical axis"="vert.axis"),
                                                                     selected="vert.axis")
                                                  )
                                              ),
                                              fluidRow(
                                                  column(6,
                                                         checkboxInput(inputId="hyper_fit_doerrorscale", label="Error Scale", value=FALSE)
                                                  ),
                                                  column(6,
                                                         conditionalPanel(condition="input.hyper_fit_algo_func == 'LA' || input.hyper_fit_algo_func == 'LD'",
                                                                          numericInput(inputId="hyper_fit_itermax", label="Max Iterations", value=1e4, min=0)
                                                         )
                                                  )
                                              ),
                                              fluidRow(
                                                  column(6,
                                                         selectInput(inputId="hyper_fit_algo_func",
                                                                     label="Algorithm",
                                                                     choices=list(
                                                                         "Optim"="optim",
                                                                         "LA"="LA",
                                                                         "LD"="LD"
                                                                     ),
                                                                     selected="optim")
                                                  ),
                                                  column(6,
                                                         conditionalPanel(condition="input.hyper_fit_algo_func == 'optim'",
                                                                          selectInput(inputId="hyper_fit_optim_method",
                                                                                      label="Method",
                                                                                      choices=names(algsTable$optim),
                                                                                      selected="Nelder-Mead")
                                                         ),
                                                         conditionalPanel(condition="input.hyper_fit_algo_func == 'LA'",
                                                                          selectInput(inputId="hyper_fit_LA_method",
                                                                                      label="Method",
                                                                                      choices=names(algsTable$LA),
                                                                                      selected="NM")
                                                         ),
                                                         conditionalPanel(condition="input.hyper_fit_algo_func == 'LD'",
                                                                          selectInput(inputId="hyper_fit_LD_method",
                                                                                      label="Method",
                                                                                      choices=names(algsTable$LD),
                                                                                      selected="CHARM")
                                                         )
                                                  )
                                              ),
                                              uiOutput("hyper_fit_selected_method"),
                                              br(),
                                              conditionalPanel(condition="input.hyper_fit_algo_func == 'LD'",
                                                               checkboxInput(inputId="hyper_fit_specs_checkbox", label=textOutput("hyper_fit_specs_label"), value=T),
                                                               conditionalPanel(condition="input.hyper_fit_specs_checkbox == true",
                                                                                uiOutput("hyper_fit_specs_inputs")
                                                               )
                                              )
                             )
                         ),
                         div(class="ui_region well",
                             checkboxInput(inputId="ui_show_plot_options", label=uiOutput("ui_plot_options_header"), value=FALSE),
                             conditionalPanel(condition="input.ui_show_plot_options == true",
                                              br(),
                                              fluidRow(
                                                  column(6,
                                                         sliderInput(inputId="hyper_fit_sigscale", label="Sigma Scale", min=0.1, max=10, value=4, step=0.1, ticks=FALSE)
                                                  ),
                                                  column(6,
                                                         sliderInput(inputId="hyper_fit_trans", label="Transparency", min=0.01, max=1, value=1, step=0.01, ticks=FALSE)
                                                  )
                                              ),
                                              fluidRow(
                                                  column(6,
                                                         checkboxInput(inputId="hyper_fit_doellipse", label="Ellipses", value=TRUE)
                                                  ),
                                                  column(6,
                                                         numericInput(inputId="hyper_fit_sigfigs", label="Sig Figs", value=6,max=15,min=1)
                                                  )
                                              ),
                                              tags$hr(),
                                              fluidRow(
                                                  column(6,
                                                         h5("2D only"),
                                                         checkboxInput(inputId="hyper_fit_use_bar", label="Use Bar", value=TRUE)
                                                  ),
                                                  column(6,
                                                         conditionalPanel(condition="input.hyper_fit_use_bar == true",
                                                                          selectInput(inputId="hyper_fit_bar_position",label="Bar Position",
                                                                                      choices=list(
                                                                                          "Top"="top",
                                                                                          "Top Right"="topright",
                                                                                          "Right"="right",
                                                                                          "Bottom Right"="bottomright",
                                                                                          "Bottom"="bottom",
                                                                                          "Bottom Left"="bottomleft",
                                                                                          "Left"="left",
                                                                                          "Top Left"="topleft"
                                                                                      ),
                                                                                      selected="topleft")
                                                         )
                                                  )
                                              )
                             )
                         ),
                         div(class="ui_region well",
                             checkboxInput(inputId="ui_show_upload_data", label=uiOutput("ui_upload_data_header"), value=TRUE),
                             conditionalPanel(condition="input.ui_show_upload_data == true",
                                              br(),
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
                                              checkboxInput(inputId="ui_show_column_names", label=uiOutput("ui_column_names_header"), value=FALSE),
                                              conditionalPanel(condition="input.ui_show_column_names == true",
                                                               fluidRow(
                                                                   column(4,
                                                                          textInput(inputId="hyper_fit_column_x", label="x :", value="x"),
                                                                          textInput(inputId="hyper_fit_column_y", label="y :", value="y"),
                                                                          textInput(inputId="hyper_fit_column_z", label="z :", value="z")
                                                                   ),
                                                                   column(4,
                                                                          textInput(inputId="hyper_fit_column_sx", label="sx :", value="sx"),
                                                                          textInput(inputId="hyper_fit_column_sy", label="sy :", value="sy"),
                                                                          textInput(inputId="hyper_fit_column_sz", label="sz :", value="sz")
                                                                   ),
                                                                   column(4,
                                                                          textInput(inputId="hyper_fit_column_corxy", label="corxy :", value="corxy"),
                                                                          textInput(inputId="hyper_fit_column_corxz", label="corxz :", value="corxz"),
                                                                          textInput(inputId="hyper_fit_column_coryz", label="coryz :", value="coryz")
                                                                   )
                                                               ),
                                                               textInput(inputId="hyper_fit_column_weights", label="weights :", value="weights")
                                              ),
                                              actionButton(inputId="use_file1", label=span("Use"), icon("file-text")),
                                              actionButton(inputId="use_example", label=span("Show Example")) 
                             )
                         ),
                         div(class="ui_region well",
                             checkboxInput(inputId="ui_show_example_data", label=uiOutput("ui_example_data_header"), value=TRUE),
                             conditionalPanel(condition="input.ui_show_example_data == true",
                                              br(),
                                              actionButton(inputId="example_hogg", label=span("hogg"), icon("file-text")),
                                              actionButton(inputId="example_GAMAsmVsize", label=span("GAMAsmVsize"), icon("file-text")),
                                              actionButton(inputId="example_TFR", label=span("TFR"), icon("file-text")),
                                              actionButton(inputId="example_FP6dFGS", label=span("FP6dFGS"), icon("file-text")),
                                              actionButton(inputId="example_MJB", label=span("MJB"), icon("file-text"))
                             )
                         )
                     ),
                    
                    # Main Panel (outputs) #
                    ########################
                     mainPanel(
                         rglwidgetOutput("hyper_fit_plot3d", width="100%"),
                         plotOutput("hyper_fit_plot2d"),
                         uiOutput("hyper_fit_small_summary"),
                         plotOutput("hyper_fit_plotPosterior", height="auto"),
                         uiOutput("hyper_fit_summary")
                     )
                 )
        ),
        
        # Methods Tab #
        ###############
        tabPanel("Methods", class="container-fluid",
                 h3("Methods"),
                 p("Below are the available methods for optim, LA and LD."),
                 br(),
                 h4("Optim :"),
                 DT::DTOutput("methods_optim_algs"),
                 br(),
                 h4("LaplaceApproximation (LA) :"),
                 DT::DTOutput("methods_LA_algs"),
                 br(),
                 h4("LaplacesDemon (LD) :"),
                 DT::DTOutput("methods_LD_algs")
        ),
        
        # Info Tab #
        ############
        tabPanel("Info", class="container-fluid",
                 h3("About"),
                 div(class="container",
                     p(
                         "hyper.fit is a top level line fitting function that uses downhill searches (optim/LaplaceApproximation) or MCMC (LaplacesDemon) to search out the best fitting parameters for a hyperplane (minimum a 1D line for 2D data), including the intrinsic scatter as part of the fit."
                     )
                 ),
                 br(),
                 h3("Usage"),
                 div(class="container",
                     p(
                         "Specify data first by using", span("Example Data", style="text-decoration:underline;"),
                         "or", span("Uploaded Data.", style="text-decoration:underline;"),
                         "Change the fitting parameters under", span("Fit Options", style="text-decoration:underline;"),
                         "and click", span("Recalculate", style="text-decoration:underline;"),
                         "to produce a different fit.",
                         "The appearance of the plot can be changed under", span("Plot Options.", style="text-decoration:underline;")
                     )
                 ),
                 br(),
                 p(strong("Example Data", style="text-decoration:underline;")),
                 div(class="container",
                     p(
                         "Example data can be used by clicking an example under", strong("Example Data."),
                         "Once an example is clicked, the example data will be used in all future calculations."
                     )
                 ),
                 br(),
                 p(strong("Upload Data", style="text-decoration:underline;")),
                 div(class="container",
                     p(
                         "Data may be uploaded under the", strong("Upload Data"), "section.",
                         "The file format accepted has a header with quoted column names, and with the values separated by a separator in the set [ , tab space | ; : ].",
                         "Once the data is uploaded, it can be used in all future calculations by pressing the", span("Use", style="text-decoration:underline;"), "button.",
                         "The uploaded data may either be 2D or 3D data, and below are some rules and examples of the 2D and 3D data:"
                     )
                 ),
                 br(),
                 fluidRow(
                     column(3,
                            p(
                                span("Required,", style='color:blue;'),
                                span("Ignore,", style='color:red;'),
                                span("Optional.", style='color:green;')
                            ),
                            HTML(
                                "<table class='table table-condensed'><tbody>
                                <tr>
                                <th>2D</th>
                                <th>3D</th>
                                </tr>
                                <tr>
                                <td style='color:blue;'>x</th>
                                <td style='color:blue;'>x</th>
                                </tr>
                                <tr>
                                <td style='color:blue;'>y</th>
                                <td style='color:blue;'>y</th>
                                </tr>
                                <tr>
                                <td style='color:red;'>z</th>
                                <td style='color:blue;'>z</th>
                                </tr>
                                <tr>
                                <td style='color:green;'>sx</th>
                                <td style='color:green;'>sx</th>
                                </tr>
                                <tr>
                                <td style='color:green;'>sy</th>
                                <td style='color:green;'>sy</th>
                                </tr>
                                <tr>
                                <td style='color:red;'>sz</th>
                                <td style='color:green;'>sz</th>
                                </tr>
                                <tr>
                                <td style='color:green;'>corxy</th>
                                <td style='color:green;'>corxy</th>
                                </tr>
                                <tr>
                                <td style='color:red;'>corxz</th>
                                <td style='color:green;'>corxz</th>
                                </tr>
                                <tr>
                                <td style='color:red;'>coryz</th>
                                <td style='color:green;'>coryz</th>
                                </tr>
                                <tr>
                                <td style='color:green;'>weights</th>
                                <td style='color:green;'>weights</th>
                                </tr>
                                </tbody></table>")
                            ),
                     column(4,
                            p("Example of 2D data (space-separated)", style="text-decoration:underline;"),
                            HTML('<pre>
"x" "y" "sx" "sy" "corxy" "weights"
0.2695 0.0724 0.065 0.03 0.85 1.0
0.1615 0.0147 0.065 0.03 0.85 1.0
-0.0865 -0.151 0.065 0.03 0.85 1.0
0.8808 0.5284 0.065 0.03 0.85 1.5
-0.8177 -0.9546 0.065 0.03 0.85 1.5
0.4069 0.0901 0.065 0.03 0.85 1.0
0.4118 0.242 0.065 0.03 0.85 1.0
0.4425 -0.0621 0.065 0.03 0.85 1.0
</pre>')
                            ),
                     column(5,
                            p("Example of 3D data (CSV)", style="text-decoration:underline;"),
                            HTML('<pre>
"x","y","z","sx","sy","sz","corxy","corxz","coryz"
0.2695,0.0724,0.0394,0.065,0.03,0.02,0.85,0,0
0.1615,0.0147,0.0529,0.065,0.03,0.02,0.85,0,0
-0.0865,-0.151,0.0224,0.065,0.03,0.02,0.85,0,0
0.8808,0.5284,0.1042,0.065,0.03,0.02,0.85,0,0
-0.8177,-0.9546,0,0.065,0.03,0.02,0.85,0,0
0.4069,0.0901,0.0191,0.065,0.03,0.02,0.85,0,0
0.4118,0.242,0.0292,0.065,0.03,0.02,0.85,0,0
0.4425,-0.0621,0.1424,0.065,0.03,0.02,0.85,0,0
</pre>')
                     )
                     ),
                 br(),
                 p(strong("Fit Options", style="text-decoration:underline;")),
                 div(class="container",
                     p(
                         "Under",strong("Fit Options,"), "the following parameters can be specified for the fit calculation. The names in",
                         span("(grey)", style="color:#999999"), "represent the parameter names for the hyper.fit function."
                     )
                 ),
                 br(),
                 fluidRow(
                     column(2,
                            strong("Coordinate Type"), p("(coord.type)", style="color:#999999")
                            ),
                     column(10,
                            p('This specifies whether the fit should be done in terms of the normal vector to the hyperplane (coord.type="normvec") gradients defined to produce values along the vert.axis dimension (coord.type="alpha") or by the values of the angles that form the gradients (coord.type="theta"). "theta" is the default since it will tend to produce a more numerically stable fit since changes in one angle will not impact the others as much as a change in the unit vector components.')
                            )
                     ),
                 fluidRow(
                     column(2,
                            strong("Scatter Type"), p("(scat.type)", style="color:#999999")
                     ),
                     column(10,
                            p('This specifies whether the intrinsic scatter should be defined orthogonal to the plane (orth) or along the vert.axis of interest (vert.axis).')
                     )
                 ),
                 fluidRow(
                     column(2,
                            strong("Error Scale"), p("(doerrorscale)", style="color:#999999")
                     ),
                     column(10,
                            p('If FALSE then the provided covariance are treated as it. If TRUE then the likelihood function is also allowed to rescale all errors by a uniform multiplicative value.')
                     )
                 ),
                 fluidRow(
                     column(2,
                            strong("Max Iterations"), p("(itermax)", style="color:#999999")
                     ),
                     column(10,
                            p('The maximum iterations to use for either the LaplaceApproximation function or LaplacesDemon function. (LA and LD only)')
                     )
                 ),
                 fluidRow(
                     column(2,
                            strong("Algorithm"), p("(algo.func)", style="color:#999999")
                     ),
                     column(10,
                            p('If algo.func="optim" (default) hyper.fit will optimise using the R base optim function. If algo.func="LA" will optimise using the LaplaceApproximation function. If algo.func="LD" will optimise using the LaplacesDemon function. For both algo.func="LA" and algo.func="LD" the LaplacesDemon package will be used (see http://www.bayesian-inference.com/software).')
                     )
                 ),
                 fluidRow(
                     column(2,
                            strong("Method"), p("(algo.method)", style="color:#999999")
                     ),
                     column(10,
                            p('Specifies the "method" argument of optim function when using algo.func="optim" (if not specified hyper.fit will use "Nelder-Mead" for optim). Specifies "Method" argument of LaplaceApproximation function when using algo.func="LA" (if not specified hyper.fit will use "NM" for LaplaceApproximation). Specifies "Algorithm" argument of LaplacesDemon function when using algo.func="LD" (if not specified hyper.fit will use "CHARM" for LaplacesDemon). When using algo.func="LD" the user can also specify further options via the Specs argument below.')
                     )
                 ),
                 fluidRow(
                     column(2,
                            strong("Specs"), p("(Specs)", style="color:#999999")
                     ),
                     column(10,
                            p('Inputs to pass to the LaplacesDemon function. Default Specs=list(alpha.star = 0.44) option is for the default CHARM algorithm (see algo.method above). Specs can be set to NULL, but not all methods will accept this. (LD only)')
                     )
                 ),
                 br(),
                 p(strong("Plot Options", style="text-decoration:underline;")),
                 div(class="container",
                     p(
                         "Under",strong("Plot Options,"), "the following parameters can change the appearance of the plot. The names in",
                         span("(grey)", style="color:#999999"), "represent the parameter names for the plot.hyper.fit function."
                     )
                 ),
                 br(),
                 fluidRow(
                     column(2,
                            strong("Sigma Scale"), p("(sigscale)", style="color:#999999")
                     ),
                     column(10,
                            p('Changes the colour range of the points.')
                     )
                 ),
                 fluidRow(
                     column(2,
                            strong("Transparency"), p("(trans)", style="color:#999999")
                     ),
                     column(10,
                            p('Sets the alpha of the points.')
                     )
                 ),
                 fluidRow(
                     column(2,
                            strong("Ellipses"), p("(doellipse)", style="color:#999999")
                     ),
                     column(10,
                            p('If set to TRUE, points will be displayed as ellipses. If FALSE, points are displayed as dots. Using ellipses may reduce the performance of the program.')
                     )
                 ),
                 fluidRow(
                     column(2,
                            strong("Sig Figs")
                     ),
                     column(10,
                            p('The number of significant figures to show in the main output.')
                     )
                 ),
                 fluidRow(
                     column(2,
                            strong("Use Bar"), p("(dobar)", style="color:#999999")
                     ),
                     column(10,
                            p('If set to TRUE, a bar depicting the SigScale will appear on the plot. (2D only)')
                     )
                 ),
                 fluidRow(
                     column(2,
                            strong("Bar Position"), p("(position)", style="color:#999999")
                     ),
                     column(10,
                            p('Sets the position of the SigScale bar on the plot.',
                            span("Use Bar", style="text-decoration:underline"),
                            'must be set to TRUE to enable this option. (2D only)')
                     )
                 ),
                 br(),
                 h4("Acknowledgements"),
                 div(class="container",
                     p(
                         "This website was written in the programming language", strong("R"), "by", strong("Joseph Dunne"), "and", strong("Aarom Robotham."),
                         "It uses the library", strong("Shiny"), "to provide the interface."
                     )
                 )
        )
    )),
    br(),
    div(
        span(a("ICRAR", href="http://www.icrar.org/", target="_blank"), "2015, written by Joseph Dunne, Aaron Robotham", style="color:grey;font-size:12px;"),
        id="ui_footer"
    ),
    uiOutput("css_output_plots")
))