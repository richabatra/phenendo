library(shinydashboard)
library(shinydashboardPlus)
library(shinyalert)
library(plotly)

shinyUI(
  dashboardPagePlus(
    dashboardHeaderPlus(title = "PhenEndo"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Home",  icon = shiny::icon("home"),tabName = "about"),
        menuItem("Questionnaire Data",  icon = shiny::icon("bar-chart"),tabName = "about",
                 menuSubItem("User Guide",  icon = shiny::icon("info-circle"),tabName = "guide"),
                 menuSubItem("Upload data", icon = shiny::icon("cloud-upload"),tabName = "upload"),
                 menuSubItem("Raw Data Visualization", icon = icon("bar-chart"),tabName = "viz"),
                 menuSubItem("Unsupervised Clustering", icon = icon("code-fork"), tabName = "clus"),
                 menuSubItem("Cluster Signature", icon = icon("eye"), tabName = "sign")),
        menuItem("Longitudinal Data",  icon = shiny::icon("line-chart"),tabName = "about",
                 menuSubItem("User Guide",  icon = shiny::icon("info-circle"),tabName = "guidelong"),
                 menuSubItem("Upload data", icon = shiny::icon("cloud-upload"),tabName = "upload_long"),
                 menuSubItem("Dimension Reduction", icon = shiny::icon("cut"),tabName = "famd_long"),
                 menuSubItem("Longitudinal Clustering", icon = icon("code-fork"), tabName = "clus_long")),
        menuItem("Contact", icon = icon("envelope"),href = "https://www.helmholtz-muenchen.de/icb/institute/staff/staff/ma/5034/Dr.-Batra/index.html")
      )
    ),
    dashboardBody(
      tags$head(tags$style(HTML('
                                .shiny-output-error { visibility: hidden; }
                                .shiny-output-error:before { visibility: hidden; }
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #FFFFFF;
                                color: #ED1443;
                                }
                               
                            
                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #FFFFFF;
                                }
                                
                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #FFFFFF;
                                }        
                                
                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #d2d6de;
                                }
                                
                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #bec4d0;
                                color: #b00d31;
                                border-left-color: #b00d31;
                                }
                                
                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #d2d6de;
                                border-left-color: #d2d6de;
                                color: #000000;
                                }
                                
                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #bec4d0;
                                border-left-color:#ED1443;
                                color: #ED1443;
                                }
                                /* toggle button when hovered  */                    
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #FFFFFF;
                                color: #b00d31;
                                }
                   
                                .skin-blue .main-header .navbar .sidebar-toggle{
                                background-color: #FFFFFF;
                                color: #ED1443;
                                }
                                 
                                .nav-tabs-custom .nav-tabs li.active {
                                     border-top-color:  #ED1443;
                                }
                                .progress-bar{background-color:#ED1443;}
                                .irs-bar,
                                .irs-bar-edge,
                                .irs-single,
                                .irs-grid-pol {
                                  background: #ED1443;
                                  border-color: #ED1443;}
                                .box.box-solid.box-primary>.box-header {
                                color:#fff;
                                background:#ED1443
                                                  }
                              .boxhead a {
                                    color: #FFFFFF;
                                    text-decoration: none;
                                }
                                .box.box-solid.box-primary{
                              border-bottom-color:#ED1443;
                              border-left-color:#ED1443;
                              border-right-color:#ED1443;
                              border-top-color:#ED1443;
                              }
                                '))),
     
      tabItems(
        tabItem(tabName = "about",
                fluidPage(
                  box(width = 12,
                      h2("Welcome to the PhenEndo Tool!"),
                      h4('PhenEndo is the first of its kind to explore mixed, categorical and numerical data.
                          We built this for our colaborators at the Dermatology department, Technical University of Munich. We hope that
                          other medical professionals will also find it useful.'),
                      h4("To use our tool follow the steps given below."),
                      tags$br(),
                      HTML(paste0('
                        <div class="tab-pane" id="timeline" style="width:50%;">
                         <!-- The timeline -->
                         <ul class="timeline timeline-inverse">
                         <!-- timeline time label -->
                         <li class="time-label">
                         <span class="bg-yellow">
                         Upload Your Data!
                         </span>
                         </li>
                         <!-- /.timeline-label -->
                         <!-- timeline item -->
                         <li>
                         <i class="fa fa-cloud-upload bg-gray"></i>
                         
                         <div class="timeline-item">

                         <h3 class="timeline-header">Endotype Data</h3>
                         
                         <div class="timeline-body">
                         This is the medical data to be analysed. It should be a complete dataset with NO missing values. If missing values are found, we will use simple mean/mode imputation method to fill it.
                         </div>
                         </div>
                         </li>
                         <!-- END timeline item -->
                         <!-- timeline item -->
                         <li>

                         <div class="timeline-item">

                         <h3 class="timeline-header">Feature Information
                         </h3>
                        <div class="timeline-body">
                         Additionally, you can provide the data type (categorical or numerical) for every feature in the dataset.
                         </div>
                         </div>
                         </li>
                         
                         <li class="time-label">
                         <span class="bg-green">
                         Visualize!
                         </span>
                         </li>
                         <!-- /.timeline-label -->
                         <!-- timeline item -->
                         <li>
                         <i class="fa fa-bar-chart bg-gray"></i>
                         
                         <div class="timeline-item">
                         <h3 class="timeline-header">Data Visualization</h3>
                         
                         <div class="timeline-body"> <p>To get some first impressions of your data, try out our visualization tools. PhenEndo is equipped with summary plots, e.g. heatmaps and FAMD analysis.</p>
                           </div>
                         </div>
                         </li>
                        <li class="time-label">
                         <span class="bg-maroon">
                         Cluster!
                         </span>
                         </li>
                         <li>
                         <i class="fa fa-code-fork bg-marvel"></i>
                         <div class="timeline-item">
                         <h3 class="timeline-header">Unsupervised Clustering</h3>
                         
                         <div class="timeline-body">By means of the Consenus Clustering methods, you can easily find out which clustering best describes your data.  <p></p>
                         </div>
                         </div>
                         </li>

                        <li class="time-label">
                         <span class="bg-purple">
                         Analyse further!
                         </span>
                         </li>
                         <li>
                         <i class="fa fa-eye bg-marvel"></i>
                         <div class="timeline-item">
                         <h3 class="timeline-header">Clustering Signature</h3>
                         
                         <div class="timeline-body">We also provide Multivariate Analysis tools, to gain more insights into the chosen clustering.
                         </div>
                         </div>
                         </li>
                        <li>
                    <i class="fa fa-check bg-gray"></i>
                  </li>
                         </ul>
                         </div>
                         ')),
                    tags$br(),
    
                h6('Powered by Theis Lab'),
                tags$hr(),
                tags$img(
                  src = "icb.jpg",
                  height = 25,
                  width = 200
                ),
                tags$img(
                  src = "hmgu.jpg",
                  height = 35,
                  width = 300
                )
              ))
          
        ),
        tabItem(tabName = "guide",
    
                 fluidPage(
                    box(
                      width = 12,
                     uiOutput('mymarkdown')
                     )
                   )
               
                   
        ),
        tabItem(tabName = "upload",
                useShinyalert(),
                fluidRow(
                  box(width = 12,  shiny::h4("Click",
                                             shiny::a("User Guide", href="#shiny-tab-guide", "data-toggle" = "tab"),
                                             " in the sidepanel to check the requirements for the data format."
                  ),
                   fileInput(
                    'file1',
                    'Upload dataset as a CSV or an Excel File',
                    multiple = FALSE,
                    accept = c('text/csv',
                               'text/comma-separated-values,text/plain', '.csv','.xls',
                               '.xlsx')
                  ),
          
                 column(4, radioButtons("disp", "Display",
                                        choices = c(Head = "head",
                                                    All = "all"),
                                        selected = "head"),
                        checkboxInput("header", "Header", TRUE)
                        ),
                  
                  # Input: Select separator ----
                 column(4,radioButtons("sep", "Separator",
                               choices = c(Comma = ",",
                                           Semicolon = ";",
                                           Tab = "\t"),
                               selected = ",")),
                  
                  # Input: Select quotes ----
                 column(4,radioButtons("quote", "Quote",
                               choices = c(None = "",
                                           "Double Quote" = '"',
                                           "Single Quote" = "'"),
                               selected = '"'))
                                    ),
                 box(width = 12,  
                 
                 fileInput(
                   'file2',
                   'Upload categorical variables as a CSV or an Excel File',
                   multiple = FALSE,
                   accept = c('text/csv',
                              'text/comma-separated-values,text/plain', '.csv','.xls',
                              '.xlsx')
                 ),
                 column(4, radioButtons("disp2", "Display",
                                        choices = c(Head = "head",
                                                    All = "all"),
                                        selected = "head"),
                        checkboxInput("header2", "Header", TRUE)
                 ),
                 
                 # Input: Select separator ----
                 column(4,radioButtons("sep2", "Separator",
                                       choices = c(Comma = ",",
                                                   Semicolon = ";",
                                                   Tab = "\t"),
                                       selected = ",")),
                 
                 # Input: Select quotes ----
                 column(4,radioButtons("quote2", "Quote",
                                       choices = c(None = "",
                                                   "Double Quote" = '"',
                                                   "Single Quote" = "'"),
                                       selected = '"'))
                 ),

                  box(width = 12,title="Uploaded Data",status = "primary",
                      solidHeader = T,
                    DT::dataTableOutput('contents')
                  )
                )),
        tabItem(tabName = "viz",
                fluidRow(
                  tabBox(width = 12, 
                         id = "tabsetviz", 
                         tabPanel("Heatmap Plot",h4("The heatmap data entries are scaled between 1/(number of unique feature values) and 1.
The y-axis represents the features and the x-axis the data items. "), 
                                  fluidRow( column(3,align="center"),
                                    column(6,align="center",plotlyOutput("Heatmap", height=600),downloadButton('downloadHeatmap', 'Download Plot')),
                                  column(3,align="center"))
                                  ),
                         tabPanel("FAMD Plots", 
                                  plotOutput("PCA",height=800),
                                  tags$br(),
                                  downloadButton('downloadPCA', 'Download Plot')),
                  tabPanel("Feature Distribution Plots", 
                           selectInput(inputId = "attributesbp", label = "Select an Attribute", choices = NULL),
                           tags$hr(), fluidRow( column(3,align="center"),
                                                column(6,align="center",plotlyOutput("Barplot"),downloadButton('downloadBarplot', 'Download Plot')),
                                                column(3,align="center"))
                           )
                           
                  
                  ))),
        tabItem(tabName = "clus",
                fluidPage(
                  column(width=12, 
                         tabBox(
                           width = 12,
                           id = "tabset1", 
                           tabPanel("Cluster Selection", 
                                    h4("Tipps for obtatining the optimal number of clusters. Click",
                                                       shiny::a("User Guide", href="#shiny-tab-guide", "data-toggle" = "tab"),
                                       " in the sidepanel for more information. The optimal number of clusters is reached, when"),
                                    tags$ul(
                                      tags$li("CDF Plot: No change on the CDF shape is observed."), 
                                      tags$li("Delta Plot: Delta curve is getting horizontal to the x-axis."),
                                      style = "font-size: 16px"
                                    ),
                                    
                                    numericInput("maxK", "Maximum Cluster Number (maxK):", 5, min = 2, max = 50),
                        
                                   fluidRow( column(6,align="center",plotlyOutput("cdf"),downloadButton('downloadcdf', 'Download Plot')
                                                    ),
                                             column(6,align="center",plotlyOutput("delta"),downloadButton('downloaddelta', 'Download Plot')
                                                    )
                                             )
                                   
                                    
                                    
                                    ),
                           tabPanel("Clusters", h4("Visualize the chosen clusters directly on the dataset heatmap. The coloring of the cells represents the feature values."),
                                    tags$hr(),
                                    uiOutput("secondSelection"),
                                    plotlyOutput("clusteringHeatmap",height = 900), br(),
                                    downloadButton('downloadclusteringHeatmap', 'Download Plot')
                                    )
                         ))
                  
                  
                )
               
          
        ),
        tabItem(tabName = "sign",
                fluidRow(
                  tabBox(width = 12,
                         id = "tabsetsign", 
                         tabPanel("Multivariate Regression",h4("The obtained model parameters are visualized in a heatmap. The colors in the blue scale correspond to negative values and in the yellow scale - to positive values."),

                                  numericInput("numClusters", "Choose Number of Clusters:", 5, min = 2, max = 100),
                                  tags$hr(),
                                  fluidRow( column(3,align="center"),
                                            column(6,align="center", plotlyOutput("signiture", height=600),                                    
                                                   downloadButton('downloadsigniture', 'Download Plot')
),
                                            column(3,align="center"))
                              
                                  ),
                         tabPanel("Univariate Analysis", h4("For each attribute in the dataset a conditional bar plot (for categorical) or conditional histogram (for numerical data) is provided."),
                                  selectInput(inputId = "attribute", label = "Select an Attribute", choices = NULL),
                                  tags$hr(), 
                                  fluidRow( column(3,align="center"),
                                            column(6,align="center",plotlyOutput("univariate"),br(),downloadButton('downloadunivariate', 'Download Plot')),
                                            column(3,align="center")))
                  )) 
              ),
        tabItem(tabName = "upload_long",
                useShinyalert(),
                fluidRow(
                  box(width = 12,  shiny::h4("Click",
                                             shiny::a("User Guide", href="#shiny-tab-guidelong", "data-toggle" = "tab"),
                                             " in the sidepanel to check the requirements for the data format."
                  ),
                  fileInput(
                    'file1_long',
                    'Upload dataset as a CSV or an Excel File',
                    multiple = FALSE,
                    accept = c('text/csv',
                               'text/comma-separated-values,text/plain', '.csv','.xls',
                               '.xlsx')
                  ),
                  
                  column(4, radioButtons("disp_long", "Display",
                                         choices = c(Head = "head",
                                                     All = "all"),
                                         selected = "head"),
                         checkboxInput("header_long", "Header", TRUE)
                  ),
                  
                  # Input: Select separator ----
                  column(4,radioButtons("sep_long", "Separator",
                                        choices = c(Comma = ",",
                                                    Semicolon = ";",
                                                    Tab = "\t"),
                                        selected = ",")),
                  
                  # Input: Select quotes ----
                  column(4,radioButtons("quote_long", "Quote",
                                        choices = c(None = "",
                                                    "Double Quote" = '"',
                                                    "Single Quote" = "'"),
                                        selected = '"'))
                  ),
                  box(width = 12,
                      
                      fileInput(
                        'file2_long',
                        'Upload data type as a CSV or an Excel File',
                        multiple = FALSE,
                        accept = c('text/csv',
                                   'text/comma-separated-values,text/plain', '.csv','.xls',
                                   '.xlsx')
                      ),
                      column(4, radioButtons("disp2_long", "Display",
                                             choices = c(Head = "head",
                                                         All = "all"),
                                             selected = "head"),
                             checkboxInput("header2_long", "Header", TRUE)
                      ),
                      
                      # Input: Select separator ----
                      column(4,radioButtons("sep2_long", "Separator",
                                            choices = c(Comma = ",",
                                                        Semicolon = ";",
                                                        Tab = "\t"),
                                            selected = ",")),
                      
                      # Input: Select quotes ----
                      column(4,radioButtons("quote2_long", "Quote",
                                            choices = c(None = "",
                                                        "Double Quote" = '"',
                                                        "Single Quote" = "'"),
                                            selected = '"'))
                  ),
                  
                  box(width = 12,title="Uploaded Data",status = "primary",
                      solidHeader = T,
                      DT::dataTableOutput('contents_long')
                  )
                )),
        tabItem(tabName = "famd_long",
                useShinyalert(),
                fluidRow(
                  box(width = 12,  shiny::h4("Click",
                                             shiny::a("User Guide", href="#shiny-tab-guide", "data-toggle" = "tab"),
                                             " in the sidepanel to check the requirements for the data format."
                  ),
                  fileInput(
                    'file3_long',
                    'Upload variable groupings as a CSV or an Excel File',
                    multiple = FALSE,
                    accept = c('text/csv',
                               'text/comma-separated-values,text/plain', '.csv','.xls',
                               '.xlsx')
                  ),
                  
                  column(4, radioButtons("disp3_long", "Display",
                                         choices = c(Head = "head",
                                                     All = "all"),
                                         selected = "head"),
                         checkboxInput("header3_long", "Header", TRUE)
                  ),
                  
                  # Input: Select separator ----
                  column(4,radioButtons("sep3_long", "Separator",
                                        choices = c(Comma = ",",
                                                    Semicolon = ";",
                                                    Tab = "\t"),
                                        selected = ",")),
                  
                  # Input: Select quotes ----
                  column(4,radioButtons("quote3_long", "Quote",
                                        choices = c(None = "",
                                                    "Double Quote" = '"',
                                                    "Single Quote" = "'"),
                                        selected = '"'))
                  ),
                  tabBox(width = 12, 
                         id = "tabsetfamd", 
                         tabPanel("Loadings Table", 
                                  fluidRow(
                                  column(width=4,selectInput(inputId = "groupsfamd", label = "Select a Group", choices = NULL)),
                                  column(width=4,selectInput(inputId = "famddim", label = "Select Number of Dimensions", choices = c(1,2,3,4,5)))),
                                  tags$hr(), DT::dataTableOutput("loading")
                                                  
                         ),
                         tabPanel("Individual Factor Plot",  
                                  
                                  fluidRow(
                                    column(width=4,selectInput(inputId = "visit", label = "Select a Visit", choices = NULL)),
                                    column(width=4,selectInput(inputId = "groupsfactor", label = "Select a Group", choices = NULL))
                                  ),
                                  tags$hr(),
                                  plotlyOutput("ifp",height = 800),
                                  downloadButton('downloadifp', 'Download Plot'))
                        
                         
                         
                  )
                )),
        tabItem(tabName = "guidelong",
                
                fluidPage(
                  box(
                    width = 12,
                    uiOutput('mymarkdown_long')
                  )
                )
                
                
        ),
        tabItem(tabName = "clus_long",
                fluidPage(
                  column(width=12,
                         tabBox(
                           width = 12,
                           id = "tabsetclus_long",
                           tabPanel("Cluster Selection",
                                    h4("Tipps for obtatining the optimal number of clusters. Click",
                                       shiny::a("User Guide", href="#shiny-tab-guide", "data-toggle" = "tab"),
                                       " in the sidepanel for more information."),

                                    
                                    fluidRow( 
                                      column(3,numericInput("maxKlong", "Maximum Cluster Number:", 5, min = 3, max = 50),
                                               actionButton("goButton","Cluster!")),
                                      column(3,numericInput("optKlong", "Choose Optimal Cluster Number:", 5, min = 3, max = 50),
                                             downloadButton("clusAssignTable","Download Cluster Assignments!"))
                                    
                                    ),
                                    
                                    fluidRow( column(3,align="center"),
                                              column(6,align="center", plotlyOutput("cluslongchoice", height=600),downloadButton('downloadcluslongchoice', 'Download Plot')),
                                              column(3,align="center")
                                    )
                               



                           ),
                           tabPanel("Sankey Plot", 
                                    plotOutput("sankey",height = 900),
                                    tags$br(),
                                    downloadButton('downloadSankey', 'Download Plot')
                                    
                           ),
                           tabPanel("Spaghetti Plot", 
                                    fluidRow(
                                    column(width=4,selectInput(inputId = "cluscritera", label = "Select an Clustering Criterion", choices = c("ICL","AIC","BIC"))),
                                    column(width=4,selectInput(inputId = "groupsfactor2", label = "Select a Group", choices = NULL)),
                                    column(width=4,selectInput(inputId = "famddim2", label = "Select a PCA/FAMD Component", choices = NULL))),
                                    tags$hr(),
                                    plotOutput("spaghetti",height = 900),
                                    tags$br(),
                                    downloadButton('downloadSpaghetti', 'Download Plot')
                                    
                           )
                         ))


                ))
      )
    )
  )
)