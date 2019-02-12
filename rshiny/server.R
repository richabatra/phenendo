suppressMessages({library(StatMatch) # gower.dist
  library(FactoMineR) # famd
  library(tidyverse) # %>% functions
  #library(dendextend) # dendrogram
  library(cluster) # PAM
  library(ggplot2)
  library(glmnet) # ridge regression
  library(DT)
  library(ConsensusClusterPlus)
  library(xlsx)
  library(scales)
  library(caret)
  library(missMDA)#data imputation
  library(heatmaply)
})


source("srcFunctions.R")

shinyServer(function(session,input, output) {

  output$mymarkdown <- renderUI({  
     shiny::includeHTML('UserGuide_adj.html') 
  }) 

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
    fileext = tolower(tools::file_ext(input$file1$datapath)) 
    if(fileext=="xlsx" || fileext=="xls"){
      df = read.xlsx2(input$file1$datapath, sheetIndex = 1, header = input$header)
    } 
    else{    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    }
    if(!is.null(input$file2)){
      fileext = tolower(tools::file_ext(input$file2$datapath)) 
      
      if(fileext=="xlsx" || fileext=="xls"){
        df = read.xlsx2(input$file2$datapath, sheetIndex = 1, header = input$header)
      } 
      else{ 
      var = read.csv(input$file2$datapath,
                     header = input$header2,
                     sep = input$sep2,
                     quote = input$quote2)
      }
      for(n in var[,1]){
        if(var[var==n,][2]=="categorical"){
          df[,n]=as.factor( df[,n])
        }
      }
      
    }
    if(any(is.na(df))){
      df.imp <- imputeFAMD(df)
      df <- df.imp$completeObs
      shinyalert(title = "Data is not complete! Missing values are imputed.", type = "warning")
    }
      },
    error = function(e) {
      # return a safeError if a parsing error occurs
      shinyalert(title = "Something went wrong! Check your data!", type = "error")
      stop(safeError(e))
    }
    )
    return(df)
    
  })
  
  choices_attributes <- reactive({
    choices_attributes <- colnames(quesData())
  })
  
  observe({
    updateSelectInput(session = session, inputId = "attributesbp", choices = choices_attributes())
  })
  observe({
    updateSelectInput(session = session, inputId = "attribute", choices = choices_attributes())
  })


  getFAMD <- function(dataMat){
    par(mfrow=c(2,2))
    plotMat <- nameMixedData(dataMat)
    quesFAMD <- FAMD(plotMat, graph = F)
    plot(quesFAMD, choix="ind", title="Individual graph: Every entry is represented by the two components")
    plot(quesFAMD, choix="quanti", title="Quantitative variables: A correlation circle for the continuous features")
    plot(quesFAMD, choix="quali", title="Qualitative variables:  A correlation plot of the categorical features")
    plot(quesFAMD, choix="var", title = "Graph of the Variables: An assotiation plot for all features")
  }
   
   plotHeat<-reactive({
     dataMat = quesData()
     dataMat = apply(dataMat, 2, FUN=function(x) rescale(as.numeric(as.matrix(x)), to=c((1/length(unique(x))),1)))
     plot_ly(z = data.matrix(dataMat), type = "heatmap", x = colnames(dataMat), 
                      colorbar = list(title = "Attribute Value"))
     
   })
   output$downloadPCA <- downloadHandler(
     filename = function() {
       "plot_pca.png"
     },
     content = function(file) {
       ggsave(file, plotPCA(), width = 16, height = 10.4)
     },
     contentType = "image/png"
   )
   plotPCA<-reactive({
     dataMat = quesData()
     getFAMD(dataMat)     
   })
   output$Heatmap <-renderPlotly({
     plotHeat()
   })
   output$PCA <-renderPlot({
     plotPCA()
   })

   plotBox <-reactive({
     attr = input$attributesbp
     dat=quesData()
     var = dat[,attr]
     if (is.factor(var)){
       pl = ggplot(dat, aes(x=var)) +
         geom_bar(stat="count",fill='#8DD3C7')+  
         theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
               panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.title.x=element_blank())
     }
     else{
       pl =  ggplot(dat, aes(x=var)) +
         geom_histogram(fill='#8DD3C7')+ 
         theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
               panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.title.x=element_blank())
     }
     ggplotly(pl)
   })
   output$Barplot<-renderPlotly({
     plotBox()
   })
   
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
     dist_data=as.dist(gower.dist(quesData()))
     set.seed(1)
     ConsensusClusterPlus(d = dist_data, 10,pItem = 0.8, maxK = input$maxK, clusterAlg = "hc", distance = gower.dist,finalLinkage = "ward.D2",innerLinkage = "ward.D2")
   })
   
   plotDelta <-reactive({
     i=0
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
     pp=ggplot(df,aes(x=x,y=y)) + geom_line(size = 1, color="#ED1443")+ geom_point(size =3)+
       labs(x = "k", y = "Relative Change in Area under CDF Curve")+ theme_bw()
     ggplotly(pp)
     
     
   })
   output$delta <- renderPlotly({
     req(input$maxK)
     plotDelta()
   })
 
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
     
     p=ggplot(df,aes(x=x,y=y,group=col,colour=factor(col))) + geom_line(size = 1)+  scale_color_brewer(palette = 'Set3')+
       labs(x = "Consensus Index", y = "CDF", color = "Clusters")+ theme_bw()
     ggplotly(p)
     
     
     
   })
   output$cdf <- renderPlotly({
     req(input$maxK)
     plotCDF()  
     })
 

   plotClustering <-reactive({
     k = as.numeric(input$k)
     clustering = cluster()
     dat = quesData()
     plotMat <- dat[clustering[[k]]$consensusTree$order, ]
     names = rownames(plotMat)
     plotMat <- apply(plotMat, 2, FUN=function(x)rescale(as.numeric(as.matrix(x)), to=c((1/length(unique(x))),1)))
     rownames(plotMat) = names
     hc = clustering[[k]]$consensusTree
     ct = cutree(hc, k)
     
     clusterIDs <- data.frame(Cluster=factor(ct[clustering[[k]]$consensusTree$order]))
     rownames(clusterIDs)=rownames(plotMat)
     heatmaply(plotMat, seriate = "none",
               Rowv=NULL,Colv = "Rowv",mode="plotly", 
               row_side_colors =clusterIDs)
   })
   output$clusteringHeatmap <- renderPlotly({
     req(input$k,input$maxK)
     plotClustering()
   })
  
   plotMulti <- reactive({
     numClusters = input$numClusters
     quesDist <- gower.dist(quesData())
     hc <- hclust(as.dist(quesDist), method="ward.D2")
     #pamfit<-cutree(hc, k = numClusters)
     cluster.ids <- cutree(hc, k = numClusters)
     #cluster.ids <-as.factor(cluster.ids)
     lower_bound_obs = ceiling(0.1*nrow(quesData()))
  
       if(any(table(cluster.ids)<lower_bound_obs)){
         shinyalert(title = "One or more cluters contains fewer than 10% of all data observations and, therefore, will be removed!", type = "warning")
         dat=quesData()
         idx=which(table(cluster.ids)<lower_bound_obs)
         idxx=-which(cluster.ids%in%idx)
         dt=dat[idxx,]
         cl.ids=cluster.ids[idxx]
         design.mat <- model.matrix(cl.ids ~ data.matrix(dt))
         foldmin=min(5,min(table(cl.ids)))
         foldid  <- createFolds(factor(cl.ids), k = foldmin, list = FALSE)
         
         glmfit.cv <- cv.glmnet(design.mat, cl.ids, family = "multinomial", alpha=0.5, standardize = FALSE,foldid=foldid)
         coef.mat <- do.call(cbind, coef(glmfit.cv, s=glmfit.cv$lambda.1se))
         
         plotMat <- coef.mat[-c(1:2), ]
         rownames(plotMat) <- names(quesData())
         colnames(plotMat) <- paste0("cluster ", levels(factor(cl.ids)))
        # pheatmap(plotMat, cluster_rows = F, cluster_cols = F, display_numbers = T)
         
         
       }
     else{
       design.mat <- model.matrix(cluster.ids ~ data.matrix(quesData()))
       glmfit.cv <- cv.glmnet(design.mat, cluster.ids, family = "multinomial", alpha=0.5, standardize = FALSE)
       coef.mat <- do.call(cbind, coef(glmfit.cv, s=glmfit.cv$lambda.1se))
       
       plotMat <- coef.mat[-c(1:2), ]
       rownames(plotMat) <- names(quesData())
       colnames(plotMat) <- paste0("cluster ", levels(factor(cl.ids)))
      # if(sum(data.matrix(plotMat))>0) {#TODO
        # pheatmap(plotMat, cluster_rows = F, cluster_cols = F, display_numbers = T)
       #} else {
        # shinyalert(title = "Oops! no variables found here! Check in univariate section!", type = "warning")
         
       }
     plot_ly(z = data.matrix(plotMat), type = "heatmap", x = colnames(plotMat), 
             colorbar = list(title = "Coefficients"),xgap=2,ygap=2,y=names(quesData()))
     
   })
   output$signiture <-renderPlotly({
     req(input$numClusters)
     plotMulti()
   })
   plotUni<-reactive({
     numClusters = input$numClusters
     att=input$attribute
     dat=quesData()
     quesDist <- gower.dist(dat)
     hc <- hclust(as.dist(quesDist), method="ward.D2")
     #pamfit<-cutree(hc, k = numClusters)
     cluster.ids <- cutree(hc, k = numClusters)
     lower_bound_obs = ceiling(0.1*nrow(quesData()))
     idx=which(cluster.ids%in%which(table(cluster.ids)<lower_bound_obs))
     if (length(idx)!=0){
       dat=dat[-which(cluster.ids%in%which(table(cluster.ids)<lower_bound_obs)),]
       cluster.ids=cluster.ids[-which(cluster.ids%in%which(table(cluster.ids)<lower_bound_obs))]
     }
     dat$clusters=as.factor(cluster.ids)
     if (is.factor(dat[,att])){
       pl<- qplot(dat[,att], data=dat, geom="bar", fill=clusters)+  scale_fill_brewer(palette = 'Set3')+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))+ labs(x = att)
       
     }else{
       pl<- ggplot(dat, aes(clusters, dat[,att], fill = clusters)) + 
         geom_boxplot()+ labs(y = att)+ scale_fill_brewer(palette = 'Set3')+
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                       panel.background = element_blank(), axis.line = element_line(colour = "black"))
       
           }
     ggplotly(pl)
     
   })
   output$univariate <-renderPlotly({
     req(input$numClusters,input$attribute)
     plotUni()


   })
   
   output$secondSelection <- renderUI({
     req(input$maxK)
     selectInput('k', 'Choose number of clusters', choices = 2:input$maxK)
   })
   
  gowerDist <- function(mat){
    as.dist(gower.dist(data.frame(t(mat))))
  }
})