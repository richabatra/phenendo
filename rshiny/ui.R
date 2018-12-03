library(shinydashboard)
library(shinydashboardPlus)
library(shinyalert)
library(rintrojs)
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
      introjsUI(),
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
                  box(width = 12, h4("This tool supports numerical as well as categorical data, such as survey data. For this purpose, we recommend the user to upload two data files. 
                                     The first one contains the attributes (or features) which are going to be analyzed for every biomedical record.
                                    The second one annotates the categorical features in the first dataset."
                  ), HTML("<h4>To check the data format of the required data please download a sample <a download='sampleData.csv' href='data:text/csv;base64,ImF0dHJpYnV0ZV8xIiwiYXR0cmlidXRlXzIiLCJhdHRyaWJ1dGVfMyIsImF0dHJpYnV0ZV80IiwiYXR0cmlidXRlXzUiLCJhdHRyaWJ1dGVfNiIsImF0dHJpYnV0ZV83IiwiYXR0cmlidXRlXzgiLCJhdHRyaWJ1dGVfOSIsImF0dHJpYnV0ZV8xMCIsImF0dHJpYnV0ZV8xMSIsImF0dHJpYnV0ZV8xMiIsImF0dHJpYnV0ZV8xMyIsImF0dHJpYnV0ZV8xNCIsImF0dHJpYnV0ZV8xNSIsImF0dHJpYnV0ZV8xNiIsImF0dHJpYnV0ZV8xNyIsImF0dHJpYnV0ZV8xOCIsImF0dHJpYnV0ZV8xOSIsImF0dHJpYnV0ZV8yMCIsImF0dHJpYnV0ZV8yMSIsImF0dHJpYnV0ZV8yMiIsImF0dHJpYnV0ZV8yMyIsImF0dHJpYnV0ZV8yNCIsImF0dHJpYnV0ZV8yNSIsImF0dHJpYnV0ZV8yNiIsImF0dHJpYnV0ZV8yNyIsImF0dHJpYnV0ZV8yOCIsImF0dHJpYnV0ZV8yOSIsImF0dHJpYnV0ZV8zMCIsImF0dHJpYnV0ZV8zMSIsImF0dHJpYnV0ZV82MyIsImF0dHJpYnV0ZV82NCIKIjAiLCIxIiwzLjM4MDIxMTI0MTcxMTYxLCIxIiw1LCIwIiwiMSIsMywyNy4yLCIxIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsMS4wMDg2MDAxNzE3NjE5MiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIgoiMCIsIjEiLDMuNjY3NDUyOTUyODg5OTUsIjEiLDIwLCIwIiwiMSIsMyw2OS4yLCIwIiwiMSIsIjAiLCIxIiwiMCIsIjAiLCIwIiwiMSIsIjEiLCIxIiwiMSIsIjEiLCIxIiwiMSIsMi41OTEwNjQ2MDcwMjY1LCIxIiwiMCIsIjEiLCIwIiwiMSIsIjAiLCIwIiwiMSIsIjAiCiIwIiwiMSIsMy41MzAxOTk2OTgyMDMwOCwiMSIsMTIsIjEiLCIxIiwzLDE2LjEsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwwLjA0MTM5MjY4NTE1ODIyNTEsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMSIKIjAiLCIxIiwzLjQ5NjkyOTY0ODA3MzIxLCIxIiwxMiwiMSIsIjEiLDMsNDQuNSwiMCIsIjEiLCIxIiwiMCIsIjAiLCIxIiwiMSIsIjEiLCIwIiwiMSIsIjEiLCIwIiwiMCIsIjAiLDAuODg1MzYxMjIwMDMxNTEyLCIwIiwiMCIsIjAiLCIwIiwiMSIsIjAiLCIwIiwiMCIsIjEiCiIwIiwiMCIsMy41MTE4ODMzNjA5Nzg4NywiMSIsOSwiMCIsIjEiLDEsMjcuMiwiMCIsIjEiLCIwIiwiMCIsIjEiLCIxIiwiMSIsIjAiLCIwIiwiMSIsIjAiLCIxIiwiMCIsIjAiLDIuOTQxMDE0MjQzNzA1NTcsIjEiLCIxIiwiMSIsIjEiLCIxIiwiMCIsIjAiLCIxIiwiMCIKIjEiLCIwIiwzLjU3NDAzMTI2NzcyNzcyLCIxIiw4LCIwIiwiMSIsMiwyNC43LCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMSIsMi4xMTcyNzEyOTU2NTU3NiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIxIgoiMCIsIjEiLDMuNTY4MjAxNzI0MDY2OTksIjEiLDYsIjAiLCIxIiwzLDQxLjcsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwwLjQ3NzEyMTI1NDcxOTY2MiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIxIgoiMCIsIjAiLDMuMzExNzUzODYxMDU1NzUsIjEiLDMsIjAiLCIxIiwyLDE2LjcsIjAiLCIxIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIwIiwwLjY0NzM4Mjk3MDExNDYyLCIxIiwiMCIsIjAiLCIwIiwiMSIsIjEiLCIwIiwiMSIsIjAiCiIwIiwiMSIsMy40NzcxMjEyNTQ3MTk2NiwiMSIsNiwiMCIsIjEiLDYsNTAuOSwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLDEuNTQ3Nzc0NzA1Mzg3ODIsIjEiLCIxIiwiMCIsIjAiLCIxIiwiMSIsIjAiLCIwIiwiMCIKIjAiLCIwIiwzLjU0NDA2ODA0NDM1MDI4LCIxIiw2LCIwIiwiMSIsMSw2OC40LCIwIiwiMCIsIjAiLCIxIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMSIsMS44MjkzMDM3NzI4MzEwMiwiMSIsIjEiLCIxIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIwIgoiMCIsIjEiLDMuNTI1MDQ0ODA3MDM2ODUsIjEiLDksIjAiLCIxIiw1LDQwLjIsIjEiLCIxIiwiMSIsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIxIiwxLjc3ODE1MTI1MDM4MzY0LCIxIiwiMCIsIjAiLCIxIiwiMSIsIjAiLCIwIiwiMCIsIjEiCiIwIiwiMSIsMy41Mzc4MTkwOTUwNzMyNywiMSIsMywiMCIsIjEiLDMsMzYuOCwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMSIsIjAiLCIxIiwiMCIsIjAiLDEuNzU1MTEyMjY2Mzk1MDcsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIKIjAiLCIxIiwzLjQ3NzEyMTI1NDcxOTY2LCIxIiw0LCIxIiwiMCIsMjQsMjUuMSwiMSIsIjEiLCIwIiwiMCIsIjAiLCIxIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLDEuNDI2NTExMjYxMzY0NTgsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMCIsIjAiLCIxIiwiMSIKIjAiLCIxIiwzLjYxMDY2MDE2MzA4OTg4LCIwIiwwLCIxIiwiMSIsMyw1MS43LCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMSIsMS4wODYzNTk4MzA2NzQ3NSwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIgoiMCIsIjEiLDMuNTU2MzAyNTAwNzY3MjksIjEiLDI0LCIwIiwiMSIsNSw0OS41LCIwIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMSIsMi4zMTU5NzAzNDU0NTY5MiwiMSIsIjAiLCIxIiwiMCIsIjEiLCIwIiwiMCIsIjEiLCIwIgoiMCIsIjEiLDMuNDExNjE5NzA1OTYzMjMsIjEiLDEyLCIwIiwiMSIsMTIsMzMuNywiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMSIsIjAiLCIwIiwiMCIsIjEiLCIwIiwiMCIsIjAiLDAuOTIwNjQ1MDAxNDA2Nzg4LCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIwIiwiMSIsIjAiCiIwIiwiMSIsMy4zNDI0MjI2ODA4MjIyMSwiMSIsNCwiMCIsIjAiLDYsMzMuMiwiMSIsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMSIsIjAiLCIxIiwiMCIsIjAiLCIwIiwiMCIsIjEiLDIuMTY0MzUyODU1Nzg0NDQsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMCIKIjAiLCIxIiwzLjU1NjMwMjUwMDc2NzI5LCIxIiw4LCIwIiwiMSIsNiw2NC45LCIxIiwiMSIsIjAiLCIxIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMCIsIjAiLCIxIiwiMCIsMS41MDEwNTkyNjIyMTc3NSwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMCIsIjEiLCIwIgoiMCIsIjEiLDMuNjgxMjQxMjM3Mzc1NTksIjEiLDEsIjEiLCIwIiwyLDI0LjUsIjAiLCIxIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIxIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwxLjMwMTAyOTk5NTY2Mzk4LCIxIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiCiIxIiwiMSIsMy42MjMyNDkyOTAzOTc5LCIwIiwwLCIxIiwiMSIsMTIsMjYuNSwiMSIsIjEiLCIwIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLDIuMDc1NTQ2OTYxMzkyNTMsIjAiLCIwIiwiMSIsIjAiLCIwIiwiMSIsIjAiLCIxIiwiMSIKIjEiLCIxIiwzLjQ1OTM5MjQ4Nzc1OTIzLCIxIiwxMiwiMSIsIjEiLDMsNTQuMiwiMSIsIjEiLCIxIiwiMSIsIjAiLCIxIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLDIuMzcxMDY3ODYyMjcxNzQsIjAiLCIxIiwiMCIsIjAiLCIxIiwiMCIsIjAiLCIxIiwiMSIKIjEiLCIwIiwzLjQzMTM2Mzc2NDE1ODk5LCIxIiwxNCwiMCIsIjEiLDUsMjQuNCwiMCIsIjEiLCIxIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLDAuOTU5MDQxMzkyMzIxMDk0LCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiCiIwIiwiMSIsMy41NTgxMDgzMDE2MzA1NSwiMSIsNSwiMSIsIjEiLDYsMzMuOSwiMCIsIjEiLCIwIiwiMCIsIjAiLCIxIiwiMCIsIjAiLCIwIiwiMSIsIjAiLCIwIiwiMCIsIjEiLDEuMDI1MzA1ODY1MjY0NzcsIjEiLCIwIiwiMSIsIjAiLCIwIiwiMSIsIjAiLCIxIiwiMSIKIjAiLCIxIiwzLjUxODUxMzkzOTg3Nzg5LCIwIiwwLCIwIiwiMCIsMzYsMzMuNiwiMCIsIjEiLCIxIiwiMCIsIjAiLCIwIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLDIuNTc2MzQxMzUwMjA1NzksIjAiLCIwIiwiMCIsIjEiLCIxIiwiMCIsIjAiLCIwIiwiMCIKIjEiLCIxIiwzLjUwMzc5MDY4MzA1NzE4LCIwIiwwLCIwIiwiMSIsOCwxMi41LCIwIiwiMSIsIjEiLCIxIiwiMCIsIjAiLCIxIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsMS41NDI4MjU0MjY5NTkxOCwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIgoiMCIsIjEiLDMuNTY4MjAxNzI0MDY2OTksIjEiLDcsIjAiLCIxIiw1LDI5LjIsIjEiLCIxIiwiMCIsIjEiLCIwIiwiMCIsIjAiLCIxIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwyLjMwMTAyOTk5NTY2Mzk4LCIxIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIwIiwiMSIsIjAiCiIwIiwiMSIsMy41OTk4ODMwNzIwNzM2OSwiMSIsMSwiMSIsIjAiLDMsMzcuOCwiMSIsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLDEuNTY0NjY2MDY0MjUyMDksIjAiLCIwIiwiMCIsIjAiLCIxIiwiMCIsIjAiLCIxIiwiMSIKIjAiLCIxIiwzLjUwNTE0OTk3ODMxOTkxLCIxIiw3LCIxIiwiMSIsMyw1Ni4zLCIxIiwiMSIsIjEiLCIxIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMSIsIjEiLCIxIiwiMSIsMS4zMzY0NTk3MzM4NDg1MywiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIwIgoiMCIsIjEiLDMuNTI1MDQ0ODA3MDM2ODUsIjEiLDIsIjEiLCIxIiwzNiw2OS45LCIxIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMSIsMS44OTkyNzMxODczMTc2LCIxIiwiMCIsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiCiIwIiwiMSIsMy40NTkzOTI0ODc3NTkyMywiMSIsOCwiMCIsIjEiLDQsMzUuNiwiMSIsIjEiLCIxIiwiMSIsIjAiLCIwIiwiMCIsIjEiLCIwIiwiMSIsIjAiLCIxIiwiMSIsIjEiLDIuMTUyMjg4MzQ0MzgzMDYsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMSIsIjEiLCIxIiwiMCIKIjAiLCIwIiwzLjQ5MTM2MTY5MzgzNDI3LCIxIiwxMCwiMSIsIjEiLDMsMzkuOCwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLDAuOTg2MzIzNzc3MDUwNzY1LCIxIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiCiIwIiwiMSIsMy41OTEwNjQ2MDcwMjY1LCIxIiwzLCIxIiwiMSIsMTAsNTcuOSwiMSIsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLDEuNjYxODEyNjg1NTM3MjYsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMSIsIjAiLCIwIiwiMCIKIjAiLCIwIiwzLjQzMTM2Mzc2NDE1ODk5LCIxIiw0LCIwIiwiMCIsNCwxNy40LCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMCIsIjEiLCIxIiwiMCIsIjAiLCIwIiwiMCIsMS4xNDYxMjgwMzU2NzgyNCwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIwIgoiMCIsIjEiLDMuNDg4NTUwNzE2NTAwNDQsIjEiLDcsIjAiLCIwIiw4LDEyLjksIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwxLjE3ODk3Njk0NzI5MzE3LCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiCiIwIiwiMSIsMy41NTk5MDY2MjUwMzYxMSwiMSIsMiwiMCIsIjAiLDEsNDAuMiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMSIsIjAiLCIwIiwiMSIsIjAiLCIwIiwiMCIsIjEiLDAuODY5MjMxNzE5NzMwOTc2LCIxIiwiMCIsIjEiLCIwIiwiMCIsIjAiLCIxIiwiMCIsIjAiCiIwIiwiMSIsMy41ODgyNzE3MDY4NDIzMywiMSIsNSwiMSIsIjEiLDQsNTIuNSwiMSIsIjAiLCIxIiwiMSIsIjAiLCIxIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMSIsIjAiLDIuMTE3MjcxMjk1NjU1NzYsIjEiLCIwIiwiMCIsIjAiLCIxIiwiMCIsIjAiLCIxIiwiMCIKIjAiLCIxIiwzLjU5OTg4MzA3MjA3MzY5LCIxIiw2LCIxIiwiMSIsMTMsMjQuOCwiMCIsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMSIsIjEiLDAuODAyMDg5MjU3ODgxNzMzLCIwIiwiMCIsIjAiLCIwIiwiMSIsIjAiLCIwIiwiMCIsIjAiCiIwIiwiMCIsMy40NDcxNTgwMzEzNDIyMiwiMSIsNiwiMSIsIjAiLDI2LDQ1LCIwIiwiMSIsIjAiLCIwIiwiMCIsIjEiLCIxIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIwIiwiMSIsMS45MDIwMDI4OTEzNTA3MywiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIxIgoiMCIsIjAiLDMuNjEyNzgzODU2NzE5NzQsIjEiLDYsIjAiLCIxIiwyLDQwLjQsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIwIiwiMCIsIjAiLCIwIiwwLjY3OTQyNzg5NjYxMjExOSwiMSIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMCIsIjAiLCIxIgoiMCIsIjEiLDMuNDk5Njg3MDgyNjE4NCwiMSIsOCwiMCIsIjEiLDIsMzYuNSwiMCIsIjEiLCIxIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMSIsIjEiLCIxIiwiMCIsIjAiLDIuMDU2OTA0ODUxMzM2NDcsIjEiLCIxIiwiMSIsIjAiLCIxIiwiMSIsIjEiLCIwIiwiMCIKIjAiLCIxIiwzLjUyMzc0NjQ2NjgxMTU2LCIxIiwxMCwiMCIsIjAiLDUsMzQuNSwiMCIsIjEiLCIwIiwiMSIsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLDEuNjY5MzE2ODgwNTY2MTEsIjEiLCIwIiwiMSIsIjEiLCIwIiwiMCIsIjAiLCIxIiwiMCIKIjAiLCIxIiwzLjU2MzQ4MTA4NTM5NDQxLCIxIiwzLCIwIiwiMCIsMyw1Ni43LCIxIiwiMSIsIjAiLCIxIiwiMCIsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsMi4wNTMwNzg0NDM0ODM0MiwiMSIsIjEiLCIxIiwiMCIsIjAiLCIxIiwiMCIsIjEiLCIwIgoiMSIsIjEiLDMuNDk5Njg3MDgyNjE4NCwiMSIsMTIsIjEiLCIxIiw2LDQ4LjEsIjEiLCIxIiwiMCIsIjAiLCIxIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwxLjQ3Mjc1NjQ0OTMxNzIxLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiCiIxIiwiMSIsMy41NTE0NDk5OTc5NzI4OCwiMSIsNiwiMSIsIjEiLDExLDE0LjgsIjAiLCIxIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMSIsIjAiLCIwIiwxLjMzMjQzODQ1OTkxNTYxLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIwIiwiMSIsIjAiCiIxIiwiMCIsMy41MjUwNDQ4MDcwMzY4NSwiMCIsMCwiMSIsIjEiLDMsMzQuMywiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMSIsIjEiLDEuMjQwNTQ5MjQ4MjgyNiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIgoiMCIsIjEiLDMuNjA0MjI2MDUzMDg0NDcsIjEiLDQsIjAiLCIxIiw0LDI2LjIsIjEiLCIxIiwiMCIsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIxIiwyLjAzMzQyMzc1NTQ4Njk1LCIxIiwiMCIsIjAiLCIxIiwiMCIsIjAiLCIwIiwiMSIsIjAiCiIwIiwiMSIsMy41MTE4ODMzNjA5Nzg4NywiMCIsMCwiMSIsIjEiLDMsMjUuNSwiMSIsIjEiLCIxIiwiMCIsIjAiLCIxIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLDIuMDM3NDI2NDk3OTQwNjIsIjEiLCIwIiwiMCIsIjEiLCIxIiwiMCIsIjAiLCIwIiwiMCIKIjAiLCIwIiwzLjUxODUxMzkzOTg3Nzg5LCIxIiw2LCIxIiwiMSIsMiwyMC44LCIwIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMCIsIjAiLCIwIiwiMCIsMS4zMjgzNzk2MDM0Mzg3NCwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMSIsIjAiLCIxIgoiMCIsIjAiLDMuNTM3ODE5MDk1MDczMjcsIjEiLDksIjAiLCIxIiwxOCw2MC41LCIwIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMSIsMi4wMjkzODM3Nzc2ODUyMSwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMCIsIjAiLCIwIgoiMCIsIjEiLDMuNTU2MzAyNTAwNzY3MjksIjEiLDMsIjAiLCIxIiwxLDYwLjUsIjAiLCIxIiwiMSIsIjAiLCIwIiwiMSIsIjEiLCIwIiwiMCIsIjEiLCIwIiwiMCIsIjAiLCIwIiwyLjU1MDIyODM1MzA1NTA5LCIxIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMSIsIjEiCiIwIiwiMSIsMy40NDcxNTgwMzEzNDIyMiwiMSIsNSwiMSIsIjAiLDMsMjMuMywiMCIsIjAiLCIxIiwiMCIsIjAiLCIwIiwiMSIsIjEiLCIwIiwiMCIsIjAiLCIxIiwiMCIsIjAiLDIuNjYxODEyNjg1NTM3MjYsIjEiLCIxIiwiMCIsIjEiLCIxIiwiMSIsIjEiLCIwIiwiMSIKIjAiLCIxIiwzLjUwNzg1NTg3MTY5NTgzLCIxIiwxLCIwIiwiMSIsMSw1Mi4xLCIxIiwiMSIsIjEiLCIxIiwiMCIsIjAiLCIxIiwiMCIsIjAiLCIxIiwiMCIsIjEiLCIwIiwiMCIsMi45OTY1MTE2NzIxNTQxOCwiMCIsIjAiLCIwIiwiMCIsIjEiLCIxIiwiMSIsIjEiLCIwIgoiMCIsIjAiLDMuNTYzNDgxMDg1Mzk0NDEsIjEiLDYsIjEiLCIxIiwxLDU3LCIxIiwiMSIsIjEiLCIxIiwiMCIsIjEiLCIxIiwiMSIsIjAiLCIxIiwiMCIsIjEiLCIwIiwiMSIsMi40Nzg1NjY0OTU1OTM4NCwiMSIsIjEiLCIwIiwiMSIsIjEiLCIwIiwiMCIsIjEiLCIwIgoiMCIsIjEiLDMuNjUzMjEyNTEzNzc1MzQsIjEiLDgsIjAiLCIwIiwyLDM4LjEsIjAiLCIxIiwiMCIsIjEiLCIwIiwiMCIsIjEiLCIwIiwiMCIsIjEiLCIxIiwiMSIsIjAiLCIwIiwyLjA0NTMyMjk3ODc4NjY2LCIxIiwiMSIsIjEiLCIxIiwiMCIsIjAiLCIxIiwiMCIsIjAiCiIwIiwiMCIsMy41NDQwNjgwNDQzNTAyOCwiMSIsMywiMCIsIjEiLDMsNzQuMSwiMCIsIjAiLCIwIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLDEuNDYwODk3ODQyNzU2NTUsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIKIjAiLCIxIiwzLjUxODUxMzkzOTg3Nzg5LCIxIiwzLCIwIiwiMSIsNCwzOS45LCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIxIiwiMCIsIjAiLCIxIiwiMSIsIjEiLCIwIiwiMSIsMi4zMDMxOTYwNTc0MjA0OSwiMSIsIjEiLCIxIiwiMCIsIjAiLCIxIiwiMCIsIjEiLCIxIgoiMCIsIjAiLDMuNTA1MTQ5OTc4MzE5OTEsIjAiLDAsIjAiLCIwIiw2LDM0LjcsIjEiLCIxIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwxLjYxMzg0MTgyMTg3NjA3LCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiCiIwIiwiMSIsMy42MTQ4OTcyMTYwMzMxMywiMSIsNiwiMSIsIjEiLDMsOS42LCIwIiwiMCIsIjEiLCIxIiwiMSIsIjAiLCIxIiwiMCIsIjAiLCIwIiwiMSIsIjEiLCIwIiwiMCIsMi41NjM0ODEwODUzOTQ0MSwiMSIsIjEiLCIxIiwiMSIsIjEiLCIwIiwiMCIsIjEiLCIxIgoiMCIsIjEiLDMuNTA1MTQ5OTc4MzE5OTEsIjAiLDAsIjAiLCIwIiwxLDMyLjUsIjAiLCIwIiwiMSIsIjEiLCIxIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIxIiwiMSIsIjAiLCIxIiwxLjc5Nzk1OTY0MzczNzIsIjEiLCIwIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMSIKIjAiLCIxIiwzLjU0MjgyNTQyNjk1OTE4LCIxIiw2LCIxIiwiMSIsMiw3MC40LCIxIiwiMSIsIjEiLCIwIiwiMSIsIjAiLCIxIiwiMCIsIjAiLCIwIiwiMSIsIjEiLCIwIiwiMSIsMC40NzcxMjEyNTQ3MTk2NjIsIjAiLCIxIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIwIiwiMSIKIjEiLCIxIiwzLjU3OTc4MzU5NjYxNjgxLCIxIiw5LCIwIiwiMCIsMTcsMTMuNywiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMSIsIjAiLDIuMTEwNTg5NzEwMjk5MjUsIjAiLCIwIiwiMCIsIjEiLCIwIiwiMCIsIjAiLCIxIiwiMCIKIjAiLCIxIiwzLjUxNTIxMTMwNDMyNzgsIjEiLDE0LCIwIiwiMSIsMiw0Ni4xLCIxIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMSIsIjAiLCIwIiwiMCIsMS4yNTc2Nzg1NzQ4NjkxOCwiMSIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMSIsIjEiLCIwIgoiMCIsIjAiLDMuMzUwMjQ4MDE4MzM0MTYsIjEiLDQsIjEiLCIxIiwzLDI5LjksIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIwIiwyLjEzNjcyMDU2NzE1NjQxLCIxIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMSIsIjAiCiIwIiwiMCIsMy41NjgyMDE3MjQwNjY5OSwiMSIsNiwiMCIsIjEiLDEsMzkuNiwiMCIsIjAiLCIxIiwiMSIsIjEiLCIxIiwiMSIsIjEiLCIwIiwiMSIsIjAiLCIwIiwiMCIsIjAiLDEuNjY3NDUyOTUyODg5OTUsIjEiLCIxIiwiMCIsIjAiLCIwIiwiMSIsIjAiLCIxIiwiMCIKIjAiLCIwIiwzLjUxODUxMzkzOTg3Nzg5LCIxIiw2LCIwIiwiMSIsMTgsMzIuMiwiMCIsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIxIiwiMCIsIjAiLCIwIiwiMCIsIjEiLDIuMjgxMDMzMzY3MjQ3NzMsIjEiLCIwIiwiMCIsIjEiLCIxIiwiMCIsIjAiLCIwIiwiMCIKIjAiLCIxIiwzLjUwNjUwNTAzMjQwNDg3LCIxIiwxMCwiMCIsIjEiLDIsMzMuNywiMCIsIjEiLCIxIiwiMCIsIjEiLCIxIiwiMSIsIjEiLCIwIiwiMSIsIjAiLCIxIiwiMSIsIjAiLDMuMjU1MjcyNTA1MTAzMzEsIjEiLCIxIiwiMCIsIjEiLCIxIiwiMSIsIjEiLCIxIiwiMCIKIjAiLCIxIiwzLjU0Nzc3NDcwNTM4NzgyLCIxIiw1LCIwIiwiMSIsMiw0Mi43LCIwIiwiMSIsIjAiLCIwIiwiMCIsIjEiLCIxIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMSIsMS4yNTA0MjAwMDIzMDg4OSwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIwIgoiMSIsIjEiLDMuNDQ3MTU4MDMxMzQyMjIsIjEiLDExLCIwIiwiMSIsMywxMS41LCIxIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMSIsIjEiLCIwIiwiMCIsMi40ODcxMzgzNzU0NzcxOSwiMSIsIjEiLCIxIiwiMCIsIjEiLCIwIiwiMCIsIjAiLCIxIgoiMSIsIjEiLDMuNTIxMTM4MDgzNzA0MDQsIjEiLDQsIjAiLCIxIiw0LDU1LjYsIjEiLCIxIiwiMSIsIjEiLCIwIiwiMCIsIjAiLCIxIiwiMSIsIjAiLCIxIiwiMSIsIjAiLCIxIiwxLjcxMDk2MzExODk5NTI4LCIxIiwiMSIsIjEiLCIwIiwiMCIsIjAiLCIxIiwiMSIsIjAiCiIwIiwiMCIsMy41MTMyMTc2MDAwNjc5NCwiMSIsNSwiMCIsIjAiLDI0LDIzLjEsIjEiLCIxIiwiMSIsIjEiLCIwIiwiMCIsIjEiLCIwIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIxIiwxLjU0OTAwMzI2MjAyNTc5LCIwIiwiMCIsIjAiLCIwIiwiMSIsIjAiLCIxIiwiMSIsIjAiCiIwIiwiMSIsMy40Njk4MjIwMTU5NzgxNiwiMSIsMTIsIjAiLCIwIiw0LDQ2LCIwIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMSIsMC40NDQwNDQ3OTU5MTgwNzYsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMCIKIjAiLCIwIiwzLjQyMzI0NTg3MzkzNjgxLCIwIiwwLCIwIiwiMCIsMyw1OS41LCIxIiwiMSIsIjEiLCIwIiwiMSIsIjEiLCIwIiwiMCIsIjEiLCIxIiwiMCIsIjAiLCIwIiwiMSIsMS42ODMwNDcwMzgyMzg4NSwiMSIsIjEiLCIwIiwiMCIsIjEiLCIxIiwiMCIsIjAiLCIxIgoiMCIsIjAiLDMuNDg0Mjk5ODM5MzQ2NzksIjAiLDAsIjAiLCIxIiwxLDQ1LjcsIjAiLCIwIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwyLjE5ODY1NzA4Njk1NDQyLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIwIiwiMSIsIjAiCiIxIiwiMSIsMy41NTYzMDI1MDA3NjcyOSwiMSIsNiwiMCIsIjEiLDksMTYuOCwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMSIsIjAiLCIwIiwiMCIsIjAiLDEuOTE4MDMwMzM2Nzg0ODgsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMSIKIjAiLCIxIiwzLjYyNjM0MDM2NzM3NTA0LCIxIiwxMSwiMCIsIjEiLDMsNDMuNiwiMSIsIjEiLCIxIiwiMSIsIjEiLCIwIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMSIsIjEiLDEuNzUwNTA4Mzk0ODUxMzUsIjAiLCIxIiwiMSIsIjAiLCIxIiwiMCIsIjAiLCIxIiwiMCIKIjAiLCIwIiwzLjI0NTUxMjY2NzgxNDE1LCIxIiwxLCIxIiwiMSIsNiwyNy44LCIxIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMSIsMS40MTE2MTk3MDU5NjMyMywiMSIsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIgoiMCIsIjEiLDMuNDYyMzk3OTk3ODk4OTYsIjEiLDE4LCIwIiwiMSIsNywxNC4zLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsMC41OTIxNzY3NTczOTU4NjcsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMSIsIjAiLCIwIiwiMSIKIjAiLCIxIiwzLjQ4ODU1MDcxNjUwMDQ0LCIxIiwyLCIxIiwiMSIsNCwxMS4zLCIwIiwiMSIsIjAiLCIxIiwiMCIsIjAiLCIxIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMCIsMS45ODA5MTE5Mzc3NzY4NCwiMSIsIjAiLCIwIiwiMCIsIjEiLCIxIiwiMCIsIjEiLCIwIgoiMCIsIjAiLDMuNTQ0MDY4MDQ0MzUwMjgsIjAiLDAsIjAiLCIxIiwzLDEzLjksIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwyLjYyMTE3NjI4MTc3NTA0LCIxIiwiMSIsIjAiLCIwIiwiMSIsIjEiLCIwIiwiMSIsIjEiCiIwIiwiMSIsMy41MDUxNDk5NzgzMTk5MSwiMSIsMTIsIjEiLCIxIiwxOCwzNSwiMSIsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIwIiwiMSIsIjAiLCIwIiwiMCIsIjAiLDIuMjQzMDM4MDQ4Njg2MjksIjEiLCIwIiwiMCIsIjAiLCIxIiwiMSIsIjAiLCIwIiwiMCIKIjAiLCIwIiwzLjU1NjMwMjUwMDc2NzI5LCIxIiwxOCwiMSIsIjEiLDEzLDI3LjksIjAiLCIxIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIxIiwiMSIsIjAiLCIwIiwxLjk1Mzc1OTY5MTczMzIzLCIxIiwiMSIsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMSIsIjEiCiIwIiwiMSIsMy42NTMyMTI1MTM3NzUzNCwiMSIsMTIsIjAiLCIwIiwxMiwzMy44LCIxIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMSIsMS4zMTgwNjMzMzQ5NjI3NiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIxIgoiMCIsIjEiLDMuNjE4MDQ4MDk2NzEyMDksIjAiLDAsIjEiLCIxIiwxMCwxNi42LCIwIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMCIsMi40MjMyNDU4NzM5MzY4MSwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIgoiMCIsIjEiLDMuMzY5MjE1ODU3NDEwMTQsIjEiLDcsIjEiLCIxIiwyNCwxMSwiMSIsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLDEuNTA5MjAyNTIyMzMxMSwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMCIsIjAiLCIxIgoiMCIsIjAiLDMuNDExNjE5NzA1OTYzMjMsIjAiLDAsIjAiLCIwIiw1LDMxLjksIjEiLCIwIiwiMSIsIjAiLCIxIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwxLjY1ODk2NDg0MjY2NDQ0LCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIwIiwiMCIsIjAiCiIwIiwiMCIsMy40OTEzNjE2OTM4MzQyNywiMSIsMiwiMSIsIjEiLDEyLDE4LjcsIjEiLCIxIiwiMSIsIjAiLCIwIiwiMSIsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwxLjIzMDQ0ODkyMTM3ODI3LCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIwIiwiMSIsIjAiCiIxIiwiMCIsMy41NDQwNjgwNDQzNTAyOCwiMSIsOSwiMCIsIjEiLDMsNDAuMywiMSIsIjEiLCIwIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMSIsIjAiLCIwIiwiMCIsIjAiLDEuODcxNTcyOTM1NTQ1ODgsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMCIKIjAiLCIxIiwzLjUxODUxMzkzOTg3Nzg5LCIwIiwwLCIwIiwiMSIsMywyMC4zLCIwIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMSIsMC45ODQ5NzcxMjY0MTU0OTMsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMSIKIjAiLCIxIiwzLjUxNDU0Nzc1MjY2MDI5LCIxIiw5LCIwIiwiMSIsMywyNy40LCIxIiwiMSIsIjEiLCIxIiwiMSIsIjEiLCIxIiwiMCIsIjAiLCIwIiwiMSIsIjAiLCIwIiwiMSIsMS41Mjg5MTY3MDAyNzc2NSwiMCIsIjAiLCIwIiwiMCIsIjEiLCIwIiwiMCIsIjEiLCIxIgoiMSIsIjAiLDMuNTM1Mjk0MTIwMDQyNzcsIjAiLDAsIjAiLCIxIiwzLDMwLjMsIjAiLCIxIiwiMCIsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIwIiwiMCIsIjAiLCIwIiwyLjE0MzAxNDgwMDI1NDA5LCIxIiwiMCIsIjEiLCIxIiwiMCIsIjAiLCIwIiwiMSIsIjEiCiIwIiwiMSIsMy40NjIzOTc5OTc4OTg5NiwiMSIsNSwiMSIsIjEiLDEsMjMuNSwiMSIsIjAiLCIwIiwiMCIsIjEiLCIwIiwiMSIsIjAiLCIxIiwiMCIsIjAiLCIwIiwiMCIsIjAiLDIuMDM3NDI2NDk3OTQwNjIsIjEiLCIxIiwiMSIsIjAiLCIwIiwiMSIsIjAiLCIxIiwiMCIKIjAiLCIxIiwzLjYyMTE3NjI4MTc3NTA0LCIxIiw5LCIwIiwiMSIsMSwxMS4zLCIxIiwiMSIsIjEiLCIxIiwiMCIsIjEiLCIxIiwiMCIsIjAiLCIxIiwiMSIsIjEiLCIwIiwiMSIsMS4yMjI3MTY0NzExNDc1OCwiMCIsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIwIgoiMCIsIjEiLDMuNDQwOTA5MDgyMDY1MjIsIjEiLDMsIjEiLCIxIiwxLDc5LjMsIjEiLCIxIiwiMSIsIjEiLCIwIiwiMCIsIjEiLCIxIiwiMCIsIjEiLCIxIiwiMCIsIjAiLCIxIiwyLjYyNDI4MjA5NTgzNTY3LCIxIiwiMCIsIjEiLCIwIiwiMSIsIjAiLCIwIiwiMCIsIjAiCiIwIiwiMSIsMy40NDcxNTgwMzEzNDIyMiwiMCIsMCwiMCIsIjAiLDUsOCwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLDAuNjk3MjI5MzQyNzU5NzE4LCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiCiIwIiwiMSIsMy41MzQwMjYxMDYwNTYxMywiMSIsOCwiMCIsIjEiLDUsMzguMywiMSIsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLDIuNDIxNjAzOTI2ODY5ODMsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMSIKIjAiLCIxIiwzLjQ4NTcyMTQyNjQ4MTU4LCIxIiw4LCIxIiwiMSIsOSwxNi45LCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsMi4xNzMxODYyNjg0MTIyNywiMCIsIjAiLCIwIiwiMSIsIjAiLCIwIiwiMCIsIjEiLCIxIgoiMCIsIjEiLDMuNjIzMjQ5MjkwMzk3OSwiMCIsMCwiMSIsIjEiLDI0LDExLjcsIjEiLCIxIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIwIiwwLjc0MDM2MjY4OTQ5NDI0NCwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMCIsIjAiLCIwIgoiMCIsIjEiLDMuNDk4MzEwNTUzNzg5NiwiMSIsMTIsIjAiLCIxIiwxMiwzOC43LCIxIiwiMSIsIjEiLCIxIiwiMCIsIjEiLCIxIiwiMCIsIjEiLCIwIiwiMCIsIjAiLCIxIiwiMCIsMi40NTkzOTI0ODc3NTkyMywiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMSIsIjEiLCIxIgoiMCIsIjAiLDMuNTQ0MDY4MDQ0MzUwMjgsIjEiLDMsIjAiLCIxIiwxLDM1LjEsIjAiLCIxIiwiMCIsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIwIiwwLjU4ODgzMTcyNTU5NDIwNywiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMCIsIjAiLCIwIgoiMCIsIjEiLDMuNjM1NDgzNzQ2ODE0OTEsIjEiLDksIjAiLCIxIiwzLDU1LjgsIjEiLCIxIiwiMCIsIjEiLCIwIiwiMSIsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIxIiwxLjU5NDM5MjU1MDM3NTQzLCIxIiwiMSIsIjEiLCIxIiwiMCIsIjAiLCIxIiwiMSIsIjAiCiIwIiwiMSIsMy41MTE4ODMzNjA5Nzg4NywiMSIsOSwiMSIsIjEiLDIsNDEuMSwiMCIsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMSIsIjAiLCIxIiwiMCIsIjAiLDEuNDM3NzUwNTYyODIwMzksIjEiLCIwIiwiMCIsIjAiLCIwIiwiMSIsIjAiLCIxIiwiMSIKIjAiLCIxIiwzLjEyODcyMjI4NDMzODQzLCIwIiwwLCIwIiwiMSIsMTAsNTAuMiwiMSIsIjEiLCIxIiwiMSIsIjAiLCIxIiwiMSIsIjAiLCIwIiwiMCIsIjEiLCIwIiwiMCIsIjEiLDIuNDAzMTIwNTIxMTc1ODIsIjEiLCIxIiwiMSIsIjAiLCIxIiwiMCIsIjEiLCIxIiwiMCIKIjAiLCIwIiwzLjU0MDMyOTQ3NDc5MDg3LCIxIiwyLCIxIiwiMSIsNCwxOS42LCIxIiwiMSIsIjEiLCIxIiwiMCIsIjEiLCIxIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMSIsMi4zNTAyNDgwMTgzMzQxNiwiMSIsIjEiLCIxIiwiMSIsIjEiLCIwIiwiMSIsIjAiLCIwIgoiMSIsIjEiLDMuNTY4MjAxNzI0MDY2OTksIjEiLDcsIjAiLCIxIiwxOCw1Mi42LCIwIiwiMCIsIjAiLCIwIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsMi4yNDc5NzMyNjYzNjE4MSwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMCIsIjEiLCIwIgoiMSIsIjAiLDMuNjEyNzgzODU2NzE5NzQsIjAiLDAsIjEiLCIxIiw4LDIyLjEsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIwIiwxLjgxMTU3NTAwNTg3MDU5LCIxIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiCiIwIiwiMSIsMy41MzE0Nzg5MTcwNDIyNSwiMSIsMzYsIjAiLCIxIiw2LDUwLjUsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwwLjcwMTU2Nzk4NTA1NTkyNywiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMSIsIjEiLCIwIgoiMCIsIjAiLDMuNTQ0MDY4MDQ0MzUwMjgsIjEiLDEyLCIwIiwiMSIsMjgsNDQsIjAiLCIxIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwxLjc0MzUwOTc2NDcyODQzLCIxIiwiMCIsIjAiLCIxIiwiMSIsIjEiLCIwIiwiMCIsIjEiCiIxIiwiMSIsMy41MzE0Nzg5MTcwNDIyNSwiMSIsMywiMCIsIjEiLDMsMzcuNSwiMCIsIjEiLCIwIiwiMCIsIjAiLCIxIiwiMSIsIjAiLCIwIiwiMSIsIjEiLCIwIiwiMCIsIjAiLDAuMjc0MTU3ODQ5MjYzNjgsIjEiLCIwIiwiMSIsIjAiLCIwIiwiMCIsIjEiLCIxIiwiMSIKIjAiLCIxIiwzLjQ2MjM5Nzk5Nzg5ODk2LCIxIiw0LCIwIiwiMSIsMiwzNC4xLCIxIiwiMSIsIjEiLCIwIiwiMSIsIjEiLCIxIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIxIiwiMCIsMS41ODMxOTg3NzM5Njg2MiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIxIgoiMCIsIjEiLDMuNDI5NzUyMjgwMDAyNDEsIjEiLDE0LCIxIiwiMCIsMTIsNTcuOCwiMCIsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLDEuMTM5ODc5MDg2NDAxMjQsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIKIjEiLCIxIiwzLjU0NDA2ODA0NDM1MDI4LCIxIiwyMCwiMSIsIjEiLDUsMTYuNCwiMCIsIjAiLCIwIiwiMSIsIjEiLCIxIiwiMSIsIjAiLCIxIiwiMCIsIjAiLCIxIiwiMSIsIjAiLDAuMzAxMDI5OTk1NjYzOTgxLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIwIiwiMSIsIjEiCiIwIiwiMCIsMy40NDI0Nzk3NjkwNjQ0NSwiMSIsOSwiMCIsIjEiLDMsNzAsIjEiLCIwIiwiMCIsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIwIiwxLjE3ODk3Njk0NzI5MzE3LCIxIiwiMCIsIjEiLCIwIiwiMSIsIjEiLCIwIiwiMCIsIjAiCiIwIiwiMSIsMy4zMzA0MTM3NzMzNDkxOSwiMSIsMTMsIjAiLCIxIiwxMCw0NC45LCIwIiwiMSIsIjEiLCIxIiwiMSIsIjEiLCIxIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMSIsMS41Mjc2Mjk5MDA4NzEzNCwiMCIsIjAiLCIwIiwiMCIsIjEiLCIwIiwiMSIsIjAiLCIwIgoiMSIsIjAiLDMuNTE4NTEzOTM5ODc3ODksIjEiLDEsIjEiLCIxIiwxLDQyLjEsIjEiLCIxIiwiMSIsIjAiLCIwIiwiMSIsIjEiLCIwIiwiMSIsIjEiLCIxIiwiMCIsIjAiLCIxIiwwLjgwNDgyMDY3ODcyMTE2MiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMSIsIjEiLCIwIgoiMCIsIjEiLDMuNTUyNjY4MjE2MTEyMTksIjEiLDEyLCIxIiwiMSIsNywxOS42LCIxIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMSIsMy40MDIyNjEzODI0NTQ2OCwiMSIsIjAiLCIwIiwiMSIsIjAiLCIwIiwiMCIsIjEiLCIwIgoiMCIsIjEiLDMuNTk3Njk1MTg1OTI1NTEsIjEiLDMwLCIwIiwiMCIsMiw0MC43LCIxIiwiMSIsIjAiLCIxIiwiMCIsIjAiLCIxIiwiMCIsIjAiLCIxIiwiMCIsIjEiLCIwIiwiMSIsMi44ODg3NDA5NjA2ODI4OSwiMSIsIjEiLCIxIiwiMSIsIjAiLCIwIiwiMCIsIjEiLCIwIgoiMCIsIjEiLDMuNDc3ODQ0NDc2MzM4NzYsIjEiLDgsIjAiLCIwIiw2LDE5LjIsIjAiLCIxIiwiMCIsIjAiLCIxIiwiMSIsIjEiLCIxIiwiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwxLjExMDU4OTcxMDI5OTI1LCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIwIiwiMSIsIjEiCiIwIiwiMCIsMy41MDUxNDk5NzgzMTk5MSwiMSIsMSwiMSIsIjEiLDE4LDcwLjEsIjEiLCIxIiwiMCIsIjAiLCIwIiwiMSIsIjEiLCIxIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwxLjY1NzA1NTg1Mjg1NzEsIjEiLCIxIiwiMCIsIjAiLCIxIiwiMCIsIjAiLCIxIiwiMSIKIjAiLCIxIiwzLjUwNzg1NTg3MTY5NTgzLCIxIiwzNiwiMCIsIjEiLDIsNjIuMywiMSIsIjEiLCIxIiwiMSIsIjAiLCIxIiwiMSIsIjEiLCIwIiwiMSIsIjAiLCIxIiwiMCIsIjAiLDEuMDE3MDMzMzM5Mjk4NzgsIjEiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjEiLCIxIiwiMCIKIjAiLCIxIiwzLjYxMTcyMzMwODAwNzM0LCIxIiwxNCwiMSIsIjEiLDQsNjYuMSwiMSIsIjEiLCIwIiwiMCIsIjAiLCIxIiwiMSIsIjEiLCIwIiwiMCIsIjAiLCIxIiwiMCIsIjAiLDIuNzY3MTU1ODY2MDgyMTgsIjEiLCIwIiwiMSIsIjEiLCIwIiwiMCIsIjAiLCIxIiwiMCIKIjAiLCIwIiwzLjUxMTg4MzM2MDk3ODg3LCIxIiw2LCIxIiwiMSIsMywyMC44LCIxIiwiMSIsIjEiLCIxIiwiMCIsIjAiLCIwIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsMS44Njc0Njc0ODc4NTkwNSwiMSIsIjAiLCIxIiwiMSIsIjEiLCIxIiwiMSIsIjEiLCIxIgoiMSIsIjEiLDMuNTQ3Nzc0NzA1Mzg3ODIsIjEiLDExLCIxIiwiMSIsMTIsNjkuOSwiMSIsIjEiLCIwIiwiMSIsIjEiLCIwIiwiMSIsIjAiLCIwIiwiMCIsIjAiLCIwIiwiMCIsIjAiLDEuNjIyMjE0MDIyOTY2MywiMCIsIjAiLCIwIiwiMCIsIjAiLCIxIiwiMCIsIjAiLCIxIg=='>here</a>.</h4>"),
                    HTML('<h4>The second dataset can be uploaded in txt format like  <a download="var.txt" href="data:text/csv;base64,ImF0dHJpYnV0ZV8xIgoiYXR0cmlidXRlXzIiCiJhdHRyaWJ1dGVfNCIKImF0dHJpYnV0ZV82IgoiYXR0cmlidXRlXzciCiJhdHRyaWJ1dGVfMTAiCiJhdHRyaWJ1dGVfMTEiCiJhdHRyaWJ1dGVfMTIiCiJhdHRyaWJ1dGVfMTMiCiJhdHRyaWJ1dGVfMTQiCiJhdHRyaWJ1dGVfMTUiCiJhdHRyaWJ1dGVfMTYiCiJhdHRyaWJ1dGVfMTciCiJhdHRyaWJ1dGVfMTgiCiJhdHRyaWJ1dGVfMTkiCiJhdHRyaWJ1dGVfMjAiCiJhdHRyaWJ1dGVfMjEiCiJhdHRyaWJ1dGVfMjIiCiJhdHRyaWJ1dGVfMjMiCiJhdHRyaWJ1dGVfMjUiCiJhdHRyaWJ1dGVfMjYiCiJhdHRyaWJ1dGVfMjciCiJhdHRyaWJ1dGVfMjgiCiJhdHRyaWJ1dGVfMjkiCiJhdHRyaWJ1dGVfMzAiCiJhdHRyaWJ1dGVfMzEiCiJhdHRyaWJ1dGVfNjMiCiJhdHRyaWJ1dGVfNjQi">this example file.</a></h4>'),   
                   fileInput(
                    'file1',
                    'Upload dataset as a CSV or an Excel File',
                    multiple = FALSE,
                    accept = c('text/csv',
                               'text/comma-separated-values,text/plain', '.csv','.xls',
                               '.xlsx')
                  ),
                  fileInput(
                    'file2',
                    'Upload categorical variables as a TXT File',
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

                  box(width = 12,title="Uploaded Data",status = "primary",
                      solidHeader = T,
                    DT::dataTableOutput('contents')
                  )
                )),
        tabItem(tabName = "viz",
                fluidRow(
                  tabBox(width = 12, 
                         id = "tabsetviz", 
                         tabPanel("Heatmap Plot",h4("Here, we present a heatmap of the data entries, which are scaled between 1/(number of unique feature values) and 1.
The y-axis represents the features and the x-axis the data items. "), 
                                  fluidRow( column(3,align="center"),
                                    column(6,align="center",plotOutput("Heatmap", height=600)),
                                  column(3,align="center")),
                                  downloadButton('downloadHeat', 'Download Plot')),
                         tabPanel("FAMD Plots", 
                                  h4("Factor Analysis for Mixed Data (FAMD) is a version of PCA, which considers the categorical features in the dataset. The generated plots study the relationship between the different features, projected on the two components, which have the greatest eigenvalues. "),
                                  
                                  HTML("<h4><ul><li><b>Individual Graph:</b> Every entry is represented by the two components.</li>
                                       <li><b>Quantitative Variables:</b> A correlation circle for the continuous features.</li>
                                       <li><b>Qualitative Variables:</b>  A correlation plot of the categorical features.</li>
                                       <li><b>Graph of the Variables:</b> An assotiation plot for all features.</li></ul></h4>"),
                                  tags$hr(),plotOutput("PCA",height=800),
                                  tags$br(),
                                  downloadButton('downloadPCA', 'Download Plot')),
                  tabPanel("Box Plots", 
                           h4("A barplot or a histogram of every attribute is provided, to describe its distribution structure. For example,  in many of these plots one could detect  imbalance in the binary variables. These highly biased sets add to the noise in the data. Keep in mind that if they co-occur they can be the deciding factor for the clustering algorithms."),
                           selectInput(inputId = "attributesbp", label = "Select Attribute", choices = NULL),
                           tags$hr(), fluidRow( column(3,align="center"),
                                                column(6,align="center",plotOutput("Barplot")),
                                                column(3,align="center")),downloadButton('downloadBoxPlot', 'Download Box/Histrogram Plot'))
                           
                  
                  ))),
        tabItem(tabName = "clus",
                fluidPage(
                  column(width=12, 
                         tabBox(
                           width = 12,
                           id = "tabset1", 
                           tabPanel("Input Parameters",fluidRow(column(8,h4("To determine the number of clusters, Consensus Clustering is used.  The data first is subsampled and afterwards is clustered several times with number of clusters ranging from 2 to a number given by the user. Each clustering  is evaluated by generating a consensus matrix, whose entries store the proportion of clustering runs in which two items are clustered together. Here, we won't consider the consensus matrix itself, but its summary statistics.
Note that for this experiment, hierarchical clustering with Ward.D2 clustering criterion is chosen. Moreover, the clustering distance is the Gower distance, which considers the presence of categorical features in the dataset.
The output plots of this procedure help the user optimally determine the number of clusters from the dataset."),
                                                               h4("To understand the meaining of the input parameters, please press the 'Help' button. Changing any of the parameters invokes automatically the clustering.") ,  
                                                               tags$br(),
                                                               
                                                               div(style="display:inline-block;width:48%;text-align: right;padding:10px;",actionButton("ccIntro", "Help"))
                                                              # div(style="display:inline-block;width:48%;text-align: left;",actionButton("cluster", "Cluster"))
                                                                                                                    
                                                              
                           ),

                            column(4,numericInput("maxK", "Maximum Cluster Number (maxK):", 5, min = 2, max = 50),
                             numericInput("reps", "Number of Subsamples:", 10, min = 2, max = 100),
                                  sliderInput("pItem", "Proportion of Items:",min = 0, max = 1, value = 0.8))
                                  )),
                           
                           # tabPanel("CDF Plot", h4("The CDF plot shows the cummulative distrbution of the consensus matrix entries given the number of clusters. Ideally, the entries should be centered around 0 and 1."),
                           #          tags$hr(),
                           #          plotOutput("cdf"),
                           #          ),
                           tabPanel("Cluster Selection", h4("The cummulative distrbution of the consensus matrix entries (CDF) is a useful statistic for evaluating the calculated clusters. The shape of each CDF curve gives information about the presence of a cluster. In the ideal case the CDF corresponding to the optimal clustering should approximate a step function. Alternatively, the clustering assessment can be done by inspecting the delta plot, which shows the area under the CDF curve for each clustering.  It allows the user to observe the relative increase in consensus (y-axis) and determine the number of clusters K at which there is no appreciable increase. After the true K is reached there should be no significant change in the area under the CDF."),
                                    tags$hr(),
                                    fluidRow( column(6,align="center",plotOutput("cdf"),downloadButton('downloadCDF', 'Download CDF Plot')),
                                              column(6,align="center",plotOutput("delta"),downloadButton('downloadDelta', 'Download Delta Plot')))
                                    
                                    ),
                           tabPanel("Clustering", h4("Additionally, the user can visualize the chosen clusters directly on the dataset heatmap. As in the Raw Data Visualization section the coloring of the cells represents the feature values."),
                                    tags$hr(),
                                    uiOutput("secondSelection"),
                                    plotOutput("clusteringHeatmap",height = 1000),
                                    downloadButton('downloadClustering', 'Download Plot'))
                         ))
                  
                  
                )
               
          
        ),
        tabItem(tabName = "sign",
                fluidRow(
#                   box(width = 4,h4("The carried-out cluster analysis helps the user to decide on the optimal number of clusters. To study what separates one cluster from the other, we suggest the last functionality in PhenEndo.
# We fit a mutlinomial GLM with covariates the data and response the assigned clusters. The number of labels should match the optimal number of clusters given by the user below.
# "),
#                       h4("Please provide the chosen number of clusters."),
#                       numericInput("numClusters", "Optimal Number of Clusters:", 5, min = 2, max = 100)),
                  tabBox(width = 12,
                         id = "tabsetsign", 
                         tabPanel("Multivariate Regression", h4("The carried-out cluster analysis helps the user to decide on the optimal number of clusters. To study what separates one cluster from the other, we suggest the last functionality in PhenEndo.
We fit a mutlinomial GLM with covariates the data and response the assigned clusters. The number of labels should match the optimal number of clusters given by the user below.
"),                         h4("The obtained model parameters are visualized in a heatmap. The colors in the blue scale correspond to negative values and in the red scale - to positive values."),

                                  h4("Please provide the chosen number of clusters."),
                                  numericInput("numClusters", "Optimal Number of Clusters:", 5, min = 2, max = 100),
                                  tags$hr(),
                                  plotOutput("signiture", height=600),
                                  tags$br(),
                                  downloadButton('downloadMulti', 'Download Plot')),
                         tabPanel("Univariate hypothesis testing", h4("We also run univariate hypothesis test for each attribute to analyze its parameter significance in the model structure. 
                                                                      You can assume that an attribute is significant if its p-value is less than 0.05."),
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