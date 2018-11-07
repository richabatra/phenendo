suppressMessages({library(StatMatch) # gower.dist
  library(pheatmap) # pheatmap
 # library(NbClust) # cindex
  library(FactoMineR) # famd
  library(tidyverse) # %>% functions
  library(dendextend) # dendrogram
  library(cluster) # PAM
  #library(Rtsne) # tsne
  library(ggplot2)
  library(glmnet) # ridge regression
  #library(pvclust) # robust clusters
  library(broom)
  library(DT)
  library(ConsensusClusterPlus)
})

cbPalette <<- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

source("srcFunctions.R")

ccHelp <-
  data.frame(
    step = c(1, 2,3),
    intro = c(
      "The maximal number of clusters, which are going to be evaluated. The maximum default value is 50.",
      "The number of data resampling.The maximum default value is 100.",
      "The proportion of data entries, which are going to be randomly selected at each iteration."
    ),
    element = c(
      "#maxK",
      "#reps",
      "#pItem"
    )
  )
shinyServer(function(session,input, output) {
  # readInput <- eventReactive(input$do,{
  #  runif(input$file1 & input$file2)
  #   dataFile <- read.csv(input$file1$datapath, check.names = F)
  #   featureFile <- read.csv(input$file2$datapath, check.names = F)
  #   file.list <- list(dataFile, featureFile)
  #   names(file.list) <- c("dataFile", "featureFile")
  #   return(file.list)
  # })
  # output$txt <- renderText({
  #   paste("You chose", input$rb)
  # })
  output$contents <-  DT::renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    df <-quesData()
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  }, options = list(scrollX = TRUE))
  quesData <- reactive({
    req(input$file1)
    tryCatch(
      {
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
   
    if(!is.null(input$file2)){
      var = read.table(input$file2$datapath,
                       col.names = F,
                           sep = ",")
      for(n in levels(var[,1])){
        df[,n] = as.factor(df[,n])
      }
    }
      },
    error = function(e) {
      # return a safeError if a parsing error occurs
      shinyalert(title = "Something went wrong! Check your data!", type = "error")
      stop(safeError(e))
    }
    )
    return(standardizeMixedData(df))
    
  })
  

  getPlotmat<- eventReactive(input$do, {
    runif(input$file1 & input$file2)
    dsetFile <- input$file1
    dtypeFile <- input$file2
    if (is.null(dsetFile))
      return(NULL)
    
    dset.mat <- read.csv(dsetFile$datapath, header = input$header1, check.names = F)
    dtype.mat <- read.csv(dtypeFile$datapath, header = input$header2, check.names = F)
    plot.list <- apply(data.mat, 2, function(x) trim(x[which(x!="")]))
    #xx <- sample(1:100, 100, replace=T)
  })
  getFAMD <- function(dataMat){
    par(mfrow=c(2,2))
    plotMat <- nameMixedData(dataMat)
    quesFAMD <- FAMD(plotMat, graph = F)
    plot(quesFAMD, choix="ind", title="Individual graph")
    plot(quesFAMD, choix="quanti", title="Quantitative variables")
    plot(quesFAMD, choix="quali", title="Qualitative variables")
    plot(quesFAMD, choix="var")
  }
   plotType <- function(dataMat, type) {
     switch(type,
     PCA = getFAMD(dataMat),# na_col = 'brown', scale = "none", cluster_rows = F, 
           #cluster_cols = F, color = colorRampPalette(rev(brewer.pal(n = 3, name ="RdYlBu")))(100)),
      Heatmap = pheatmap(data.matrix(dataMat), cluster_rows = F, cluster_cols = F, scale = "column", show_rownames = F)
     )
   }
   plotHeat<-reactive({
     dataMat = quesData()
     pheatmap(data.matrix(dataMat), cluster_rows = F, cluster_cols = F, scale = "column", show_rownames = F)
     
   })
   plotPCA<-reactive({
     dataMat = quesData()
     getFAMD(dataMat)     
   })
   output$Heatmap <-renderPlot({
     plotHeat()
   })
   output$PCA <-renderPlot({
     dataMat = quesData()
     plotPCA()
   })

   output$downloadHeat <- downloadHandler(
     filename = function() {
       "plot_heat.png"
     },
     content = function(file) {
       ggsave(file, plotHeat(), width = 16, height = 10.4)
     },
     contentType = "image/png"
   )
   
   output$downloadPCA <- downloadHandler(
     filename = function() {
       "plot_pca.png"
     },
     content = function(file) {
       ggsave(file, plotPCA(), width = 16, height = 10.4)
     },
     contentType = "image/png"
   )
   
   # Clustering
   cluster <-  reactive( {
     ConsensusClusterPlus(d = as.dist(gower.dist(quesData())), input$reps,pItem = input$pItem, maxK = input$maxK, clusterAlg = "hc", distance = gower.dist,finalLinkage = "ward.D2",innerLinkage = "ward.D2")
   })
   
   output$downloadDelta <- downloadHandler(
     filename = function() {
       "plot_delta.png"
     },
     content = function(file) {
       ggsave(file, plotDelta(), width = 16, height = 10.4)
     },
     contentType = "image/png"
   )
   plotDelta <-reactive({
     clustering = cluster()
     ml = list()
     k = input$maxK
     ml[[1]] = list()
     for(i in 2:k){
       ml[[i]] = clustering[[i]]$ml
     }
     areaK = c()
     for (i in 2:k) {
       v = ConsensusClusterPlus:::triangle(ml[[i]], mode = 1)
       h = hist(v, plot = FALSE, breaks = seq(0, 1, by = 1/100))
       h$counts = cumsum(h$counts)/sum(h$counts)
       thisArea = 0
       for (bi in 1:(length(h$breaks) - 1)) {
         thisArea = thisArea + h$counts[bi] * (h$breaks[bi + 1] - h$breaks[bi])
         bi = bi + 1
       }
       areaK = c(areaK, thisArea)
     }
     deltaK = areaK[1]
     for (i in 2:(length(areaK))) {
       deltaK = c(deltaK, (areaK[i] - areaK[i - 1])/areaK[i -  1])
     }
     
     df = data.frame(x=1 + (1:length(deltaK)), y = deltaK)
     ggplot(df,aes(x=x,y=y)) + geom_line(size = 1, color="#ED1443")+ geom_point(size =3)+
       labs(x = "k", y = "Relative Change in Area under CDF Curve")+ theme_bw()
     
   })
   output$delta <- renderPlot({
     req(input$maxK)
     plotDelta()
   })
   output$downloadCDF <- downloadHandler(
     filename = function() {
       "plot_cdf.png"
     },
     content = function(file) {
       ggsave(file, plotCDF(), width = 16, height = 10.4)
     },
     contentType = "image/png"
   )
   plotCDF <-reactive({
     clustering = cluster()
     ml = list()
     k = input$maxK
     ml[[1]] = list()
     for(i in 2:k){
       ml[[i]] = clustering[[i]]$ml
     }
     df <- NULL
     areaK = c()
     for (i in 2:k) {
       v = ConsensusClusterPlus:::triangle(ml[[i]], mode = 1)
       h = hist(v, plot = FALSE, breaks = seq(0, 1, by = 1/100))
       h$counts = cumsum(h$counts)/sum(h$counts)
       thisArea = 0
       for (bi in 1:(length(h$breaks) - 1)) {
         thisArea = thisArea + h$counts[bi] * (h$breaks[bi + 
                                                          1] - h$breaks[bi])
         bi = bi + 1
       }
       areaK = c(areaK, thisArea)
       temp_df <- data.frame(x = h$mids, y=h$counts, col=rep(i:i, each=10))
       df <- rbind(df,temp_df)
     }
     
     ggplot(df,aes(x=x,y=y,group=col,colour=factor(col))) + geom_line(size = 1)+  scale_color_brewer(palette = 'Set3')+
       labs(x = "Consensus Index", y = "CDF", color = "Clusters")+ theme_bw()
     
     
   })
   output$cdf <- renderPlot({
     req(input$maxK)
     plotCDF()  
     })
   output$boxHeatmap <- renderUI({
     
     box(title = paste("Consensus Matrix for",input$k, "clusters"), width = 8, status = "primary", solidHeader = TRUE,plotOutput("clusteringHeatmap"))
   })
   output$downloadConsensusMatrix <- downloadHandler(
     filename = function() {
       "plot_ConsensusMatrix.png"
     },
     content = function(file) {
       ggsave(file, plotConsensusMatrix(), width = 16, height = 10.4)
     },
     contentType = "image/png"
   )
   plotConsensusMatrix <- reactive({
     k = as.numeric(input$k)
     
     clustering = cluster()
     thisPal <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", 
                  "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", 
                  "#6A3D9A", "#FFFF99", "#B15928", "#bd18ea", "#2ef4ca", 
                  "#f4cced", "#f4cc03", "#05188a", "#e5a25a", "#06f106", 
                  "#85848f", "#000000", "#076f25", "#93cd7f", "#4d0776", 
                  "#ffffff")
     hc = clustering[[k]]$consensusTree
     pc = clustering[[k]]$consensusMatrix
     pc = pc[hc$order,]
     
     colorList = list()
     for(tk in 2:k){
       ct = cutree(hc, tk)
       colorList = ConsensusClusterPlus:::setClusterColors(clustering[[tk - 1]][[3]], ct, 
                                                           thisPal, colorList)
     }
     
     colBreaks = 10
     tmyPal = ConsensusClusterPlus:::myPal(colBreaks)
     ct = cutree(hc, k)
     heatmap(pc, Colv = as.dendrogram(hc), Rowv = NA, 
             symm = FALSE, scale = "none", col = tmyPal, na.rm = TRUE, 
             labRow = F, labCol = F, mar = c(5, 5), ColSideCol = colorList[[1]])
     legend("topright", legend = unique(ct), fill = unique(colorList[[1]]),
            horiz = F, box.lwd = 0)
     
   })
   output$clusteringHeatmap <- renderPlot({
     req(input$k,input$maxK)
     plotConsensusMatrix()
   })
   output$downloadMulti <- downloadHandler(
     filename = function() {
       "plotMultivariateRegression.png"
     },
     content = function(file) {
       ggsave(file, plotMulti(), width = 16, height = 10.4)
     },
     contentType = "image/png"
   )
   plotMulti <- reactive({
     numClusters = input$numClusters
     quesDist <- gower.dist(quesData())
     pamfit <- pam(quesDist, k = numClusters)
     cluster.ids <- pamfit$clustering
     
  
       
       design.mat <- model.matrix(cluster.ids ~ data.matrix(quesData()))
       glmfit.cv <- cv.glmnet(design.mat, cluster.ids, family = "multinomial", alpha=0.5, standardize = FALSE)
       coef.mat <- do.call(cbind, coef(glmfit.cv, s=glmfit.cv$lambda.1se))
       
       plotMat <- coef.mat[-c(1:2), ]
       rownames(plotMat) <- names(quesData())
       colnames(plotMat) <- 1:numClusters
       if(sum(data.matrix(plotMat))>0) {
         pheatmap(plotMat, cluster_rows = F, cluster_cols = F, display_numbers = T)
       } else {
         shinyalert(title = "Oops! no variables found here! Check in univariate section!", type = "warning")
         
       }
     
     
   })
   output$signiture <-renderPlot({
     req(input$numClusters)
     plotMulti()
   })
   output$table <- DT::renderDataTable({
   
       data = quesData()
       numClusters = input$numClusters
       quesDist <- gower.dist(data)
       pamfit <- pam(quesDist, k = numClusters)
       data$clusterIDs <- pamfit$clustering
       df <- data %>% gather(key, value, -clusterIDs) %>%
         group_by(key) %>%
         do(tidy(kruskal.test(x= .$value, g= .$clusterIDs)))
       df
       
     
     
   })
   output$secondSelection <- renderUI({
     req(input$maxK)
     selectInput('k', 'Choose number of clusters', choices = 2:input$maxK)
   })
    clusterType <- function(maxK, reps, pItem){
       ConsensusClusterPlus(d = as.dist(gower.dist(quesData())), reps = reps,pItem = pItem, maxK = maxK, clusterAlg = "hc", distance = gower.dist,finalLinkage = "ward.D2",innerLinkage = "ward.D2")
    }
    
    observeEvent(input$ccIntro,
                 rintrojs::introjs(session,options = list(steps = ccHelp))
    )
 
  gowerDist <- function(mat){
    as.dist(gower.dist(data.frame(t(mat))))
  }
})