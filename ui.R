shinyUI(fluidPage(
    
    tags$head(
        tags$title("hyper.fit"),
        tags$link(rel="shortcut icon", href="favicon.ico"),
        tags$style(HTML("
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
                        "))
    ),
    
    # title #
    #########
    
    fluidRow(id="ui_header_row",
        h1("hyper.fit")
    ),
    
    # Main Content #
    ################
    
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
                                         actionButton(inputId="hyper_fit_calculate", label=span("Recalculate"), icon("bar-chart-o"))
                                  ),
                                  column(6,
                                         uiOutput("hyper_fit_data_used")
                                  )
                         ),
                         tags$hr(),
                         div(class="ui_region well",
                             checkboxInput(inputId="ui_show_fit_options", label=h4("Fit Options"), value=TRUE),
                             conditionalPanel(condition="input.ui_show_fit_options == true",
                                              br(),
                                              fluidRow(
                                                  column(6,
                                                         selectInput(inputId="hyper_fit_coord_type",
                                                                     label="coord.type",
                                                                     choices=list("normvec"="normvec",
                                                                                  "alpha"="alpha",
                                                                                  "theta"="theta"),
                                                                     selected="alpha")
                                                  ),
                                                  column(6,
                                                         selectInput(inputId="hyper_fit_scat_type",
                                                                     label="scat.type",
                                                                     choices=list("orth"="orth",
                                                                                  "vert.axis"="vert.axis"),
                                                                     selected="vert.axis")
                                                  )
                                              ),
                                              fluidRow(
                                                  column(6,
                                                         checkboxInput(inputId="hyper_fit_doerrorscale", label="doerrorscale", value=FALSE)
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
                                                                         "optim"="optim",
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
                                                               checkboxInput(inputId="hyper_fit_specs_checkbox", label=textOutput("hyper_fit_specs_label"), value=FALSE),
                                                               conditionalPanel(condition="input.hyper_fit_specs_checkbox == true",
                                                                                uiOutput("hyper_fit_specs_inputs")
                                                               )
                                              )
                             )
                         ),
                         div(class="ui_region well",
                             checkboxInput(inputId="ui_show_plot_options", label=h4("Plot Options"), value=TRUE),
                             conditionalPanel(condition="input.ui_show_plot_options == true",
                                              br(),
                                              fluidRow(
                                                  column(6,
                                                         sliderInput(inputId="hyper_fit_sigscale", span("SigScale", style="color:#3B68B2;"), min=0.1, max=10, value=4, step=0.1, ticks=FALSE)
                                                  ),
                                                  column(6,
                                                         sliderInput(inputId="hyper_fit_trans", span("Transparency", style="color:#3B68B2;"), min=0.01, max=1, value=1, step=0.01, ticks=FALSE)
                                                  )
                                              ),
                                              checkboxInput(inputId="hyper_fit_doellipse", label="Ellipses", value=TRUE),
                                              tags$hr(),
                                              h5("2D only"),
                                              fluidRow(
                                                  column(6,
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
                                                                                      selected="topright")
                                                         )
                                                  )
                                              )
                             )
                         ),
                         div(class="ui_region well",
                             checkboxInput(inputId="ui_show_upload_data", label=h4("Upload Data"), value=TRUE),
                             conditionalPanel(condition="input.ui_show_upload_data == true",
                                              br(),
                                              selectInput(inputId="file1_separator", label="File type", choices=list("csv"=",",
                                                                                                                     "tsv"="\t",
                                                                                                                     "ssv"=" "),
                                                          selected=" "),
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
                                              actionButton(inputId="use_file1", label=span("Use"), icon("file-text"))      
                             )
                         ),
                         div(class="ui_region well",
                             checkboxInput(inputId="ui_show_example_data", label=h4("Example Data"), value=TRUE),
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
                         webGLOutput("hyper_fit_plot3d", width="100%"),
                         plotOutput("hyper_fit_plot2d"),
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
                 dataTableOutput("methods_optim_algs"),
                 br(),
                 h4("LaplaceApproximation (LA) :"),
                 dataTableOutput("methods_LA_algs"),
                 br(),
                 h4("LaplacesDemon (LD) :"),
                 dataTableOutput("methods_LD_algs")
        ),
        
        # Info Tab #
        ############
        tabPanel("Info", class="container-fluid",
                 h3("About"),
                 p(span("Welcome to ICRAR's", strong("hyper.fit"), "website!", style="color:#08c")),
                 p(
                     "hyper.fit is a package used to ....."
                     ),
                 p(
                     "This website was written in the programming language", strong("R"), "by", strong("Joseph Dunne"), "and", strong("Aarom Robotham."),
                     "It uses the library", strong("Shiny"), "to provide the interface."
                   ),
                 br(),
                 h3("Usage"),
                 p(
                     "To use this website, data must first be specified.",
                     "Either example data (see", span("Example Data", style="text-decoration:underline;"), ")",
                     "or custom data (see", span("Uploading Data", style="text-decoration:underline;"), ") can be used.",
                     "Once the data has been specified, the fitting line/plane can be calculated and plotted.",
                     "To change the calculation method, see", span("Changing the Fit method.", style="text-decoration:underline;"),
                     "To change the appearance of the plot, see", span("Changing the Plot style.", style="text-decoration:underline;")
                 ),
                 br(),
                 p(strong("Example Data", style="text-decoration:underline;")),
                 p(
                     "Example data can be used by selecting an example under", strong("Example Data."),
                     "Once an example is clicked, the example data will be used in all future calculations."
                 ),
                 br(),
                 p(strong("Uploading Data", style="text-decoration:underline;")),
                 p(
                     "Data may be uploaded under the", strong("Upload Data"), "section.",
                     "Before uploading a file, the file type should be specified using the",span("File Type", style="text-decoration:underline;"), "menu.",
                     "The menu entries include: CSV = comma-separated value, TSV = tab-separated values, SSV = space-separated values.",
                     "Once the data is uploaded, it can be used in all future calculations by pressing the", actionButton(inputId="dud", label=span("Use"), icon("file-text"))   ,"button.",
                     "The uploaded data may either be 2D or 3D data, and below are some rules and examples of the 2D and 3D data:"
                 ),
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
                            p("Example of 2D data (SSV)", style="text-decoration:underline;"),
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
"x","y","z","sx","sy","sz","corxy","corxz","coryz",
0.2695,0.0724,0.0394,0.065,0.03,0.02,0.85,0,0,
0.1615,0.0147,0.0529,0.065,0.03,0.02,0.85,0,0,
-0.0865,-0.151,0.0224,0.065,0.03,0.02,0.85,0,0,
0.8808,0.5284,0.1042,0.065,0.03,0.02,0.85,0,0,
-0.8177,-0.9546,0,0.065,0.03,0.02,0.85,0,0,
0.4069,0.0901,0.0191,0.065,0.03,0.02,0.85,0,0,
0.4118,0.242,0.0292,0.065,0.03,0.02,0.85,0,0,
0.4425,-0.0621,0.1424,0.065,0.03,0.02,0.85,0,0,
</pre>')
                     )
                     ),
                 br(),
                 p(strong("Changing the Fit method", style="text-decoration:underline;")),
                 p(
                     "The method used to calculate the fit can be changed under", strong("Fit Options."),
                     "Under", span("Algorithm,", style="text-decoration:underline;"), "the algorithm (optim, LA, or LD) may be chosen.",
                     "When LA or LD is chosen, the max iterations can be set.",
                     "When LD is chosen, the Specs can be set by clicking the", span("Specs", style="text-decoration:underline;"), "label.",
                     "The Specs may be left as NULL, however not all methods accept a NULL value for the Specs."
                     ),
                 br(),
                 p(strong("Changing the Plot style", style="text-decoration:underline;")),
                 p(
                     "The appearance of the plot can be changed under", strong("Plot Options."),
                     span("SigScale", style="text-decoration:underline;"), "will change the colour scale of the points, and",
                     span("Transparency", style="text-decoration:underline;"), "will change the transparency of the points.",
                     span("Ellipses", style="text-decoration:underline;"), "will determine whether to use ellipses or points in the plot (ellipses may reduce performance).",
                     "The", strong("2D only"), "section is for 2D plots only where options exist to include a SigScale bar."
                 )
        )
    ),
    br(),
    div(
        span(a("ICRAR", href="http://www.icrar.org/", target="_blank"), "2015, written by Joseph Dunne, Aaron Robotham", style="color:grey;font-size:12px;"),
        id="ui_footer"
    ),
    uiOutput("css_output_plots")
))