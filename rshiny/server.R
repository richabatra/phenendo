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
  library(factoextra)
  #library(kml3d)
  library(flexmix)
  library(riverplot)
  library(RColorBrewer)
  
})

source("srcFunctions.R")

shinyServer(function(session,input, output) {
  
  output$mymarkdown <- renderUI({  
     shiny::includeHTML('UserGuide_adj.html') 
  }) 
  output$mymarkdown_long <- renderUI({  
    shiny::includeHTML('UserGuideLongiClustering.html') 
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
  
  #########################################################
  ################ Longitudinal Data ######################
  #########################################################
  output$contents_long <-  DT::renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1_long)
    
    df <-longiData()
    
    if(input$disp_long == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  }, options = list(scrollX = TRUE))
  
  longiData <- reactive({
    req(input$file1_long)
    tryCatch(
      {
        fileext = tolower(tools::file_ext(input$file1_long$datapath)) 
        if(fileext=="xlsx" || fileext=="xls"){
          df = read.xlsx2(input$file1_long$datapath, sheetIndex = 1, header = input$header)
        } 
        else{    
          df <- read.csv(input$file1_long$datapath,
                         header = input$header_long,
                         sep = input$sep_long,
                         quote = input$quote_long)
        }
        if(!is.null(input$file2_long)){
          fileext = tolower(tools::file_ext(input$file2_long$datapath)) 
          
          if(fileext=="xlsx" || fileext=="xls"){
            df = read.xlsx2(input$file2_long$datapath, sheetIndex = 1, header = input$header2_long)
          } 
          else{ 
            var = read.csv(input$file2_long$datapath,
                           header = input$header2_long,
                           sep = input$sep2_long,
                           quote = input$quote2_long)
          }
          for(n in var[,1]){
            if(var[var==n,][2]=="categorical"){
              df[,n]=as.factor( df[,n])
            }
            else if (var[var==n,][2]=="ordered"){
              df[,n]=factor( df[,n],ordered = TRUE)
            }
            else{
              df[,n]=as.numeric( df[,n])
            }
          }
          ids_copy=as.character(df[,ncol(df)-1])
          ids_names=names(table(ids_copy))
          for (id in ids_names){
            ids_copy[which(ids_copy==id)]=paste0(id,1:length(which(ids_copy==id)))
          }
          rownames(df)=ids_copy
          
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
  longDataFAMD<- reactive({
    req(input$file3_long)
    if(!is.null(input$file3_long)){
      fileext = tolower(tools::file_ext(input$file3_long$datapath)) 
      
      if(fileext=="xlsx" || fileext=="xls"){
        groups = read.xlsx2(input$file3_long$datapath, sheetIndex = 1, header = input$header3_long)
      } 
      else{ 
        groups = read.csv(input$file3_long$datapath,
                       header = input$header3_long,
                       sep = input$sep3_long,
                       quote = input$quote3_long)
      }
      groups[,2]=as.numeric(groups[,2])
      group_names=as.numeric(names(table(groups[,2])))
      dathet=longiData()
      ggroup=groups[!is.na(groups[,2]),]
      cur_dim=c()
      cur_loadings=list()
      model=list()
      for(i in group_names){
        dnames = ggroup[ggroup[,2]==i,1]
        subfamd <- dathet[, as.character(dnames)]
        if(all(unlist(sapply(subfamd, class)) == 'numeric')){
          famd <- PCA(subfamd, graph = F) # have to use pca instead of famd - only continuous variables
          cur_dim <- cbind(cur_dim,famd$ind$coord[, 1])
          cur_loadings[[i]] <- famd$var$coord[, 1]
          model[[i]]=famd
        }
        else{
          famd <- FAMD(subfamd, graph = F, ncp = 10)
          cur_dim <- cbind(cur_dim,famd$ind$coord[, 1])
          cur_loadings[[i]] <- famd$var$coord[, 1]
          model[[i]]=famd
        }
      }
      return(list(loadings=cur_loadings,dims=cur_dim,models=model))
    }
    
  })
  observe({
    updateSelectInput(session = session, inputId = "groupsfamd", choices = choices_groups())
  })
  observe({
    updateSelectInput(session = session, inputId = "groupsfactor", choices = choices_groups())
  })
  observe({
    updateSelectInput(session = session, inputId = "groupsfactor2", choices = choices_groups())
  })
  observe({
    updateSelectInput(session = session, inputId = "visit", choices = choices_visit())
  })
  choices_visit <- reactive({
    req(input$file3_long)
    
    dat=longiData()
    1:nlevels(dat[,ncol(dat)])
  })
  choices_groups <- reactive({
    req(input$file3_long)
    dat=longDataFAMD()
    1:ncol(dat[[2]])
  })
  output$loading <-  DT::renderDataTable({
    req(input$groupsfamd)
    famd=longDataFAMD()
    group=as.integer(input$groupsfamd)
    Loadings=famd[[1]][[group]]
    return(as.data.frame(t(Loadings)))
  },options = list(scrollX = TRUE))
  
  output$ifp<-renderPlotly({
    plotifp()
  })
  plotifp <- reactive({
    req(input$visit)
    gr=as.integer(input$groupsfactor)
    vis=as.character(input$visit)
    dat=longiData()
    famd_out=longDataFAMD()
    ids_copy=rownames(dat)
    ids_v=ids_copy[substr(ids_copy,nchar(ids_copy),nchar(ids_copy)) %in% c(vis)]
    pl=fviz_pca_ind(famd_out[[3]][[gr]],select.ind = list(name=ids_v),geom.ind = "text")
    ggplotly(pl)
  })
  output$cluslongchoice<-renderPlot({
    plotcluslongchoice()
  })
  clusterData<-reactive({
    dat=longiData()
    var=ncol(dat)
    datFAMD=longDataFAMD()
    rotx=datFAMD[[2]]
    ndim <- ncol(rotx)
    lst <- split(rotx[,c(1:ndim)], dat[,var-1])
    visits=nlevels(dat[,var])
    fulllst <- t(sapply(lst[sapply(lst, length) == visits*ndim], function(i) matrix(i, nrow = visits)))
    colnames(fulllst) <- as.vector(outer(paste0("visit", 1:visits), paste0("_PC", 1:ndim), paste, sep=""))
    fulllst<-fulllst[,order(colnames(fulllst))]
    
    lstv <- split(dat[,var], dat[,var-1])
    
    thr_vis <- t(sapply(lstv[sapply(lstv, length) == visits-1], function(i) matrix(i, nrow = visits-1)))
    thr_vis1 <- t(sapply(lst[sapply(lst, length) == (visits-1)*ndim], function(i) matrix(i, nrow = visits-1)))
    thr_gr=thr_vis[!duplicated(thr_vis),]
    for(i in 1:nrow(thr_gr)){
      min_length=round(0.05*nrow(thr_vis))
      v_sel=apply(thr_vis, 1, function(x) all(x %in% thr_gr[i,]))
      thr_sel<- cbind(thr_vis1[rownames(t(sapply(lst[sapply(lst, length) == (visits-1)*ndim], function(i) matrix(i, nrow = visits-1)))) %in% names(which(v_sel)), ],
                      matrix(NA, nrow = sum(v_sel), ncol = ndim))
      nn=as.vector(outer(thr_gr[i,], paste0("_PC", 1:ndim), paste, sep=""))
      nn=nn[order(nn)]
      not_gr=as.character(unique(dat[,var][ !dat[,var]%in%thr_gr[i,]]))
      nn=c(nn,paste0(not_gr,"_PC", 1:ndim))
      colnames(thr_sel)=nn
      thr_sel<-thr_sel[,order(colnames(thr_sel))]
      if(sum(v_sel)>=min_length){
        fulllst <- rbind(fulllst, thr_sel)
      }
    }
    return(fulllst)
  })
  clusterLong<-reactive({
    fulllst = clusterData()
    datFAMD=longDataFAMD()
    rotx=datFAMD[[2]]
    ndim <- ncol(rotx)
    flx_clust <- flex_psy(fulllst, ndim,nclus=input$maxKlong) 
    # ar <- array(dim = c(nrow(fulllst), visits, ndim))
    # for(i in 1:ndim) ar[, , i] <- fulllst[, seq(i, visits*ndim, ndim)]
    # tw.cld3d <- cld3d(ar, idAll = rownames(fulllst))
    # clusttry <- 2:input$maxKlong
    # restarts <- 500
    # cld.res <- kml3d(tw.cld3d, nbClusters = clusttry, nbRedrawing = restarts, toPlot = "none")
    # return(tw.cld3d)
    return(flx_clust)
  })
  plotcluslongchoice<-reactive({
    req(input$maxKlong)
    fitted_models=clusterLong()
    plot(fitted_models)

  })
  output$downloadClusLong <- downloadHandler(
    filename = function() {
      "plot_clus.png"
    },
    content = function(file) {
      ggsave(file, plotcluslongchoice(), width = 16, height = 10.4)
    },
    contentType = "image/png"
  )
  output$sankey<-renderPlot({
    fulllst = clusterData()
    fitted_models=clusterLong()
    custom_river(fitted_models,fulllst)
  })
  output$downloadSankey<- downloadHandler(
    filename = function() {
      "plot_sankey.png"
    },
    content = function(file) {
      fulllst = clusterData()
      fitted_models=clusterLong()
      custom_river(fitted_models,fulllst)
      ggsave(file, custom_river(fitted_models,fulllst), width = 16, height = 10.4)
    },
    contentType = "image/png"
  )
  plotSpaghetti <- reactive({
    fulllst = clusterData()
    fitted_models=clusterLong()
    req(input$cluscritera)
    criterion=input$cluscritera
    if (criterion =="ICL")  nclust_flx <- as.numeric(names(which.min(ICL(fitted_models))))
    if (criterion =="AIC")  nclust_flx <- as.numeric(names(which.min(AIC(fitted_models))))
    if (criterion =="BIC")  nclust_flx <- as.numeric(names(which.min(BIC(fitted_models))))
    fitted_model <- getModel(fitted_models, as.character(nclust_flx))
    cluster_flx <- clusters(fitted_model)[1:nrow(fulllst)]
    datFAMD=longDataFAMD()
    rotx=datFAMD[[2]]
    ndim <- ncol(rotx)
    dat=longiData()
    var=ncol(dat)
    visits=nlevels(dat[,var])
    i=input$groupsfactor2
    long.psych.plot(to_plot = fulllst[, seq(i, visits*ndim, ndim)], paste0('dimension extracted from ', i), cluster = cluster_flx)
    
  })
  output$spaghetti<-renderPlot({
      plotSpaghetti()
      })
  output$downloadSpaghetti<- downloadHandler(
    filename = function() {
      "plot_spaghetti.png"
    },
    content = function(file) {
      ggsave(file, plotSpaghetti(), width = 16, height = 10.4)
    },
    contentType = "image/png"
  )
})
custom_river <- function(fitted_models,fulllst, levels = 8){
    nodes <- 'start'
    
    for(i in 2:levels)
      nodes <- c(nodes, paste0('Cl_', i, LETTERS[1:i]))
    a <- as.data.frame(table(factor(clusters(getModel(fitted_models, as.character(2)))[1:nrow(fulllst)], 
                                    levels = order(table(factor(clusters(getModel(fitted_models, '2'))[1:nrow(fulllst)], levels = 1:2)),decreasing = T),
                                    labels = 1:2)))[, 2]
    mov = a
    edges <- list( start= list( Cl_2A = mov[1], Cl_2B = mov[2]))
    for(i in 2:(levels-1)){
      
      a <- factor(clusters(getModel(fitted_models, as.character(i)))[1:nrow(fulllst)], 
                  levels = order(table(factor(clusters(getModel(fitted_models, as.character(i)))[1:nrow(fulllst)], levels = 1:i)),decreasing = T),
                  labels = 1:i)
      b <- factor(clusters(getModel(fitted_models, as.character(i+1)))[1:nrow(fulllst)], 
                  levels = order(table(factor(clusters(getModel(fitted_models, as.character(i+1)))[1:nrow(fulllst)], levels = 1:(i+1))),decreasing = T),
                  labels = 1:(i+1))
      mov <- t(table(a, b)) 
      listlist <- NULL
      for(j in 1:i) listlist[j] <- paste0('Cl_', i+1, LETTERS[1:(i+1)], '=mov[', 1:(i+1),',', j, ']', collapse=',')
      eval(parse(text = paste0(paste0('edges$Cl_', i, LETTERS[1:i]), '=list(', listlist, ')')))
    }
    # i think from here we can write outside of flexmix if-check
    node_xpos <- unlist(sapply(1:levels, function(i) rep(i, each = i)))
    
    ind <- 1
    for(i in 2:(levels)) ind <- c(ind, 2:(i+1))
    cols <- brewer.pal(levels+1, 'Set1')[ind]
    
    node_styles= list( start = list( col= cols[1] ))
    for(i in 2:length(nodes))
      eval(parse(text = paste0(paste0('node_styles$', nodes[i], ' =list(col=cols[', i, '])'))))
    
    
    r <- makeRiver( nodes, edges, node_xpos= node_xpos, node_styles= node_styles)
    ds <- default.style()
    ds[['textcex']] <- .7
    ds[['srt']] <- 45
    
    plot(r, lty = 0, default_style = ds, gravity = "bottom", 
         #plot_area = 1.2, 
         nsteps = 101, fix.pdf = F)
   # mtext(paste(subset, method, dimred))

  
  
}
add.alpha <- function(col, alpha=1){
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}
long.psych.plot <- function(to_plot, ylab, cluster){
  plot(0, t = "n", xlim = c(1, 4), ylim = range(-1*to_plot, na.rm = T), xaxt = "n",
       xlab = "time", ylab = ylab)
  axis(1, at = 1:4, labels = c("first visit", "6m follow-up", "12m follow-up", "18m follow-up"))
  for(i in 1:nrow(to_plot))
    lines(na.omit(cbind(x = seq_along(-1*to_plot[i, ]), y = -1*to_plot[i, ])), col = add.alpha(as.numeric(cluster)[i] + 1, .2))
  cl_means <- apply(-1*to_plot, 2, function(i) tapply(i, cluster, mean, na.rm = T))
  cl_sds <- apply(-1*to_plot, 2, function(i) tapply(i, cluster, sd, na.rm = T))
  for(i in 1:length(unique(cluster))){
    polygon(c(1:4, 4:1), c(cl_means[i, ] + cl_sds[i, ], rev(cl_means[i, ] - cl_sds[i, ])), density = 100,
            col = add.alpha((2:(length(unique(cluster)) + 1))[i], .4), border = add.alpha((2:(length(unique(cluster)) + 1))[i], .4))
  }
  for(i in 1:length(unique(cluster))){
    lines(cl_means[i, ], col = 1, lwd = 9)
    lines(cl_means[i, ], col = (2:(length(unique(cluster)) + 1))[i], lwd = 5)
  }
  points(rep(1:4, 2)[1:length(unique(cluster))], y = c(diag(cl_means), diag(cl_means[length(unique(cluster)):1, ]))[1:length(unique(cluster))], pch = 19, cex = 5, col = "black")
  points(rep(1:4, 2)[1:length(unique(cluster))], y = c(diag(cl_means), diag(cl_means[length(unique(cluster)):1, ]))[1:length(unique(cluster))], pch = 19, cex = 4, col = "white")
  text(rep(1:4, 2)[1:length(unique(cluster))], y = c(diag(cl_means), diag(cl_means[length(unique(cluster)):1, ]))[1:length(unique(cluster))], labels = LETTERS[1:length(unique(cluster))],
       cex = 1.7, col = (2:(length(unique(cluster)) + 1)))
}
flex_psy <- function(fulllst, ndim, nclus=8, ntime = 4){
  
  Y1 <- unclass(as.vector(fulllst[, seq(1, ntime*ndim, ndim)])) # dim 1
  Y <- Y1
  for(i in 2:ndim){
    Y2 <- unclass(as.vector(fulllst[, seq(i, ntime*ndim, ndim)]))
    Y <- cbind(Y, Y2)
  }
  nay <- is.na(Y1)
  Y <- Y[!nay, ]
  X <- 1:ntime
  X <- rep(X, each = nrow(fulllst))
  X <- X[!nay]
  grp <- rep(1:nrow(fulllst), ntime)
  grp <- grp[!nay]
  cls_dat <- data.frame(Y = Y, X = X, grp = grp)
  colnames(cls_dat)[1:ndim] <- paste0('Y', 1:ndim)
  mlist <- list()
  for(i in 1:ndim){
    eval(parse(text = paste0('mlist[[', i, ']] <- FLXMRmgcv(Y', i, '~ .)')))
  }
  eval(parse(text = paste0('fitted_models <- stepFlexmix(~s(X, k = ', ntime, ') | grp, data = cls_dat, k = 2:',nclus,', model=mlist, nrep = 5)')))
  return(fitted_models)
  
  #save(fitted_models, file = paste0('data/intermediate/', subset, '_flexmix_output_', method, '.Rdata'))
}
