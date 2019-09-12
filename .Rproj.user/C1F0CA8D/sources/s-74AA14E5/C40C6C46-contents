suppressMessages({library(StatMatch) # gower.dist
  library(FactoMineR) # famd
  library(tidyverse) # %>% functions
  library(cluster) # PAM
  library(ggplot2)
  library(DT) # fancy data tables
  library(ConsensusClusterPlus)
  library(xlsx) # read excel data
  library(scales)
  library(caret) 
  library(missMDA) # data imputation
  library(heatmaply) # heatmaps with plotly
  library(factoextra)
  library(flexmix) # longitudinal clustering
  library(riverplot)
  library(RColorBrewer)
  library(shinyjs)
  library(raster)
  library(webshot)
  library(htmlwidgets)
})

source("source/srcFunctions.R")

shinyServer(function(session,input, output) {
  
  #########################################################
  ################ Questionnaire Data #####################
  #########################################################
  
  source("tabs/questionnaire.R", local = TRUE)
  
  #########################################################
  ################ Longitudinal Data ######################
  #########################################################
  

  source("tabs/longitudinal.R", local = TRUE)

})

