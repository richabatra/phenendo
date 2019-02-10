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
        menuItem("User Guide",  icon = shiny::icon("info-circle"),tabName = "guide"),
        menuItem("Upload data", icon = shiny::icon("cloud-upload"),tabName = "upload"),
        menuItem("Raw Data Visualization", icon = icon("bar-chart"),tabName = "viz"),
        menuItem("Unsupervised Clustering", icon = icon("code-fork"), tabName = "clus"),
        menuItem("Cluster Signature", icon = icon("eye"), tabName = "sign"),
        menuItem("Contact", icon = icon("envelope"),tabName = "contact")
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
                         Additionally, you can provide a list of names of the categorical features in your dataset.
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
                         <h3 class="timeline-header">Clustering Signiture</h3>
                         
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
                                    column(6,align="center",plotlyOutput("Heatmap", height=600)),
                                  column(3,align="center"))
                                  #downloadButton('downloadHeat', 'Download Plot')
                                  ),
                         tabPanel("FAMD Plots", 
                                  plotOutput("PCA",height=800),
                                  tags$br(),
                                  downloadButton('downloadPCA', 'Download Plot')),
                  tabPanel("Feature Distribution Plots", 
                           selectInput(inputId = "attributesbp", label = "Select an Attribute", choices = NULL),
                           tags$hr(), fluidRow( column(3,align="center"),
                                                column(6,align="center",plotlyOutput("Barplot")),
                                                column(3,align="center"))#,downloadButton('downloadBoxPlot', 'Download Box/Histrogram Plot')
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
                        
                                   fluidRow( column(6,align="center",plotlyOutput("cdf")
                                                    ),
                                             column(6,align="center",plotlyOutput("delta")
                                                    )
                                             )
                                   
                                    
                                    
                                    ),
                           tabPanel("Clusters", h4("Visualize the chosen clusters directly on the dataset heatmap. The coloring of the cells represents the feature values."),
                                    tags$hr(),
                                    uiOutput("secondSelection"),
                                    plotlyOutput("clusteringHeatmap",height = 900)
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
                                            column(6,align="center", plotlyOutput("signiture", height=600)),
                                            column(3,align="center"))
                              
                                  ),
                         tabPanel("Univariate Analysis", h4("For each attribute in the dataset a conditional bar plot (for categorical) or conditional histogram (for numerical data) is provided."),
                                  selectInput(inputId = "attribute", label = "Select an Attribute", choices = NULL),
                                  tags$hr(), 
                                  fluidRow( column(3,align="center"),
                                            column(6,align="center",plotlyOutput("univariate")),
                                            column(3,align="center")))
                  )) 
              ),
       tabItem(tabName = "contact",
               fluidRow(
                 box(width = 12,
                     br(),
                    column(width = 6,boxProfile(
                       src = "http://www.mfa.gov.rs/en/images/slike/no_photo_female.jpg",
                       title = "Max Mustermann",
                       subtitle = "Worker at ICB",
                       boxProfileItemList(
                         bordered = T,
                         boxProfileItem(title = "Email",
                                        description = "max.mustermann@helmholtz-muenchen.de"),
                         boxProfileItem(title = "Github",
                                        description = "https://github.com/max.mustermann"))
                    )),
                    column(width = 6,boxProfile(
                      src = "http://www.mfa.gov.rs/en/images/slike/no_photo_female.jpg",
                      title = "Max Mustermann",
                      subtitle = "Worker at ICB",
                      boxProfileItemList(
                        bordered = T,
                        boxProfileItem(title = "Email",
                                       description = "max.mustermann@helmholtz-muenchen.de"),
                        boxProfileItem(title = "Github",
                                       description = "https://github.com/max.mustermann"))
                    )),
                    column(width = 12, h6('Powered by Theis Lab'),
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
                     ))
                    
                  )
                ))
      )
    )
  )
)