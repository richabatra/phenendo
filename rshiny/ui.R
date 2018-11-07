library(shinydashboard)
library(shinydashboardPlus)
library(shinyalert)
library(rintrojs)
shinyUI(
  dashboardPagePlus(
    dashboardHeaderPlus(title = "ExMMeD"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Home",  icon = shiny::icon("home"),tabName = "about"),
        menuItem("Upload data", icon = shiny::icon("cloud-upload"),tabName = "upload"),
        menuItem(" Data Visualization", icon = icon("bar-chart"),tabName = "viz"),
        menuItem("Unsupervised Clustering", icon = icon("code-fork"), tabName = "clus"),
        menuItem("Cluster Signiture", icon = icon("eye"), tabName = "sign"),
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
                              
                              .box.box-solid.box-primary{
                              border-bottom-color:#ED1443;
                              border-left-color:#ED1443;
                              border-right-color:#ED1443;
                              border-top-color:#ED1443;
                              }
                                '))),
      introjsUI(),
      tabItems(
        tabItem(tabName = "about",
                fluidPage(
                  box(width = 12,
                      h2("Welcome to the ExMMeD Tool!"),
                      h4('Exploring Mixed Medical Data (ExMMeD) is the first of its kind to explore mixed, categorical and numerical data.
                          We built this for our colaborators at the Dermatology department, Technical University of Munich. We hope that
                          other medical professionals will also find it useful.'),
                      h4("To use our tool follow the steps given below."),
                      tags$br(),
                      #HTML('<center><img src="workflow.png" width="350"></center>'),
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
                         
                         <div class="timeline-body"> <p>To get some first impressions of your data, try out our visualization tools. ExMMeD is equipped with summary plots, e.g. heatmaps and FAMD analysis.</p>
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
                         
                         <div class="timeline-body">By means of the Consenus Clustering methods, you can easily find out which clustering fits your data the most.  <p></p>
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
      
        tabItem(tabName = "upload",
                useShinyalert(),
                fluidRow(
                  box(width = 12,
                      fileInput(
                    'file1',
                    'Upload dataset as a CSV File',
                    multiple = FALSE,
                    accept = c('text/csv',
                               'text/comma-separated-values,text/plain', '.csv')
                  ),
                  fileInput(
                    'file2',
                    'Upload factor variables as a TXT File',
                    multiple = FALSE,
                    accept = c('text/csv',
                               'text/comma-separated-values,text/plain', '.csv')
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
                    DT::dataTableOutput('contents')
                  )
                )),
        tabItem(tabName = "viz",
                fluidRow(
                  tabBox(width = 12,
                         id = "tabsetviz", 
                         tabPanel("Heatmap Plot",  h4("Here, we present a heatmap of the standardized data entries. The y-axis represents the features and the x-axis the data items."),tags$hr(),plotOutput("Heatmap"), tags$br(),
                                  downloadButton('downloadHeat', 'Download Plot')),
                         tabPanel("FAMD Plots", 
                                  h4("Factor Analysis for Mixed Data (FAMD) is a version of PCA, which considers the categorical features in the dataset. The generated plots study the relationship between the different features, projected on the two components, which have the greatest eigenvalues. "),
                                  
                                  HTML("<h4><ul><li><b>Individual Graph:</b> Every entry is represented by the two components.</li>
                                       <li><b>Quantitative Variables:</b> A correlation circle for the continuous features.</li>
                                       <li><b>Qualitative Variables:</b>  A correlation plot of the categorical features.</li>
                                       <li><b>Graph of the Variables:</b> An assotiation plot for all features.</li></ul></h4>"),
                                  tags$hr(),plotOutput("PCA"),
                                  tags$br(),
                                  downloadButton('downloadPCA', 'Download Plot'))))),
        tabItem(tabName = "clus",
                fluidPage(
                column(width=12,
                         box(width = 12, 
                             column(width=6,h4("Consensus Clustering is used to determine the number of clusters. For this experiment, hierarchical clustering with Ward.D2 clustering criterion is chosen. For each K (K varies from 2 to maxK) the data items are subsampled according to user's input and, afterwards, clustered into K groups. 
                                Thus, for each clustering a consensus matrix is obtained, which stores, for each pair of items, the proportion of clustering runs in which two items are clustered together. According to the measures derived from the consensus matrix, the optimal number of clusters can be determined."), 
                                    h4("Note that the clustering distance is the Gower distance, which considers the presence of categorical features in the dataset."),
                                    h4("To understand the meaining of the input parameters, please press the 'Help' button.")),
                             column(width=6, numericInput("maxK", "Maximum Cluster Number (maxK):", 5, min = 2, max = 50),
                                    numericInput("reps", "Number of Subsamples:", 10, min = 2, max = 100),
                                    sliderInput("pItem", "Proportion of Items:",min = 0, max = 1, value = 0.8),
                                    actionButton("ccIntro", "Help"),
                                    tags$style(type='text/css', "#ccIntro { display: block; margin: 0 auto; }")),
                             title = "Consensus Clustering",  
                             status = "primary",
                             solidHeader = T)),
                  column(width=12, 
                         tabBox(
                           width = 12,
                           id = "tabset1", 
                           tabPanel("Consensus Matrices", h4("The estimated consensus matrix for each clustering, ranging from 0 (never clustered together) to 1 (every time clustered together)."),
                                    tags$hr(),
                                    uiOutput("secondSelection"),
                                    plotOutput("clusteringHeatmap"),
                                    downloadButton('downloadConsensusMatrix', 'Download Plot')),
                           tabPanel("CDF Plot", h4("The CDF plot shows the cummulative distrbution of the consensus matrix entries given the number of clusters. Ideally, the entries should be centered around 0 and 1."),
                                    tags$hr(),
                                    plotOutput("cdf"),
                                    downloadButton('downloadCDF', 'Download Plot')),
                           tabPanel("Delta Plot", h4("The Delta plot allows the user to observe the relative increase in consensus (y-axis) and determine the number of clusters K at which there is no appreciable increase. After the true K is reached there should be no sinificant change in the area under the CDF."),
                                    tags$hr(),
                                    plotOutput("delta"),
                                    downloadButton('downloadDelta', 'Download Plot'))
                         ))
                  
                  
                )
               
          
        ),
        tabItem(tabName = "sign",
                fluidRow(
                  box(width = 4,h4("By means of Consensus Clustering the user can determine the optimal number of clusters. Here, we do further analysis, as we fit a Multinomial GLM with elasticnet regularization to the dataset with targets the estimated cluster centroids."),
                      h4("Please provide the chosen number of clusters."),
                      numericInput("numClusters", "Optimal Number of Clusters:", 5, min = 2, max = 100)),
                  tabBox(width = 8,
                         id = "tabsetsign", 
                         tabPanel("Multivariate Regression", 
                                  tags$hr(),
                                  plotOutput("signiture"),
                                  tags$br(),
                                  downloadButton('downloadMulti', 'Download Plot')),
                         tabPanel("Univariate hypothesis testing", 
                                  tags$hr(), 
                                  DT::dataTableOutput('table')),
                                  tags$br())
                  )
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