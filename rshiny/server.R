suppressMessages({library(StatMatch) # gower.dist
  library(FactoMineR) # famd
  library(tidyverse) # %>% functions
  library(cluster) # PAM
  library(ggplot2)
  library(glmnet) # ridge regression
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
  
})

source("srcFunctions.R")

shinyServer(function(session,input, output) {
  
  #########################################################
  ################ Questionnaire Data #####################
  #########################################################
  
  # User Guide phenendo
  output$mymarkdown <- renderUI({  
     shiny::includeHTML('UserGuide_adj.html') 
  }) 

  # load questionnaire data from files
  quesData <- reactive({
    req(input$file1)
    tryCatch(
      { 
        # read data file
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
        # read feature file
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
          # cast variables to numeric or categorical
          for(n in var[,1]){
            if(var[var==n,][2]=="categorical"){
              df[,n]=as.factor( df[,n])
            }
          }
          
        }
        # check if data is complete
        if(any(is.na(df))){
          # impute missing values
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
  

  # update ui
  choices_attributes <- reactive({
    choices_attributes <- colnames(quesData())
  })
  
  observe({
    updateSelectInput(session = session, inputId = "attributesbp", choices = choices_attributes())
  })
  observe({
    updateSelectInput(session = session, inputId = "attribute", choices = choices_attributes())
  })
  output$secondSelection <- renderUI({
    req(input$maxK)
    selectInput('k', 'Choose number of clusters', choices = 2:input$maxK)
  })
  
  
  ## RAW DATA VISUALIZATION TAB
  # Data heatmap plot
  output$Heatmap <-renderPlotly({
    plotHeat()
  })
  
  plotHeat<-reactive({
     dataMat = quesData()
     dataMat = apply(dataMat, 2, FUN=function(x) rescale(as.numeric(as.matrix(x)), to=c((1/length(unique(x))),1)))
     plot_ly(z = data.matrix(dataMat), type = "heatmap", x = colnames(dataMat), 
                      colorbar = list(title = "Attribute Value"))
     
   })
  
  # Data FAMD plot
  output$PCA <-renderPlot({
    print(plotPCA())
  })
  plotPCA<-reactive({
    dataMat = quesData()
    p <- getFAMD(dataMat)     
  })
   output$downloadPCA <- downloadHandler(
     
     filename = function() {
       "plot_pca.png"
     },
     content = function(file) {
       #dataMat = quesData()
       #p <- getFAMD(dataMat)
      p <- plotPCA() 
      ggsave(file, p, width = 16, height = 10.4)
     }#,
     #contentType = "image/png"
   )
   
  # Data Bar plot
  output$Barplot<-renderPlotly({
     plotBar()
   })
   
  plotBar <-reactive({
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

   
   ## UNSUPERVISED CLUSTERING TAB
   
  # Clustering with the package ConsensusClusterPlus
   cluster <-  reactive( {
     dist_data=as.dist(gower.dist(quesData()))
     set.seed(1)
     ConsensusClusterPlus(d = dist_data, 10,pItem = 0.8, maxK = input$maxK, clusterAlg = "hc", distance = gower.dist,finalLinkage = "ward.D2",innerLinkage = "ward.D2")
   })
   
   # compute delta area plot and consensus cdf
   # Note that the code is copied from  'ConsensusClusterPlus' 
   computeDeltaAndCDF <- reactive({
     clustering = cluster()
     ml = list()
     k = input$maxK
     ml[[1]] = list()
     for(i in 2:k){
       ml[[i]] = clustering[[i]]$ml
     }
     cdf <-NULL
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
       temp_cdf <- data.frame(x = h$mids, y=h$counts, col=rep(i:i, each=10))
       cdf <- rbind(cdf,temp_cdf)
     }
     deltaK = areaK[1]
     for (i in 2:(length(areaK))) {
       deltaK = c(deltaK, (areaK[i] - areaK[i - 1])/areaK[i -  1])
     }
     
     delta = data.frame(x=1 + (1:length(deltaK)), y = deltaK)
     return(list(delta,cdf))
   })
   
   # Delta area plot
   output$delta <- renderPlotly({
     req(input$maxK)
     df = computeDeltaAndCDF()[[1]]
     pp=ggplot(df,aes(x=x,y=y)) + geom_line(size = 1, color="#ED1443")+ geom_point(size =3)+
       labs(x = "k", y = "Relative Change in Area under CDF Curve")+ theme_bw()
     ggplotly(pp)
     })
   

   # CDF Plot
   output$cdf <- renderPlotly({
     req(input$maxK)
     df = computeDeltaAndCDF()[[2]]
     p=ggplot(df,aes(x=x,y=y,group=col,colour=factor(col))) + geom_line(size = 1)+  scale_color_brewer(palette = 'Set3')+
       labs(x = "Consensus Index", y = "CDF", color = "Clusters")+ theme_bw()
     ggplotly(p)
   })

   # Clustering Heatmap
   output$clusteringHeatmap <- renderPlotly({
     req(input$k,input$maxK)
     plotClustering()
   })
   plotClustering <-reactive({
     k = as.numeric(input$k)
     clustering = cluster()
     dat = quesData()
     plotMat <- dat[clustering[[k]]$consensusTree$order, ] # order data according to cluster id
     names = rownames(plotMat)
     plotMat <- apply(plotMat, 2, FUN=function(x)rescale(as.numeric(as.matrix(x)), to=c((1/length(unique(x))),1))) # rescale columns to 0-1
     rownames(plotMat) = names
     hc = clustering[[k]]$consensusTree
     ct = cutree(hc, k)
     
     clusterIDs <- data.frame(Cluster=factor(ct[clustering[[k]]$consensusTree$order]))
     rownames(clusterIDs)=rownames(plotMat)
     heatmaply(plotMat, seriate = "none",
               Rowv=NULL,Colv = "Rowv",mode="plotly", 
               row_side_colors =clusterIDs)
   })

   ## CLUSTER SIGNATURE TAB
   # Heatmap to Multivariate Analysis
   output$signiture <-renderPlotly({
     req(input$numClusters)
     plotMulti()
   })
   plotMulti <- reactive({
     numClusters = input$numClusters
     quesDist <- gower.dist(quesData())
     hc <- hclust(as.dist(quesDist), method="ward.D2")
     cluster.ids <- cutree(hc, k = numClusters)
     # Require a min number of observation per cluster
     # Each cluster should be at least  of size 10%  of the whole data otherwise it is removed from the further analysis
     lower_bound_obs = ceiling(0.1*nrow(quesData()))
  
       if(any(table(cluster.ids)<lower_bound_obs)){
         shinyalert(title = "One or more cluters contains fewer than 10% of all data observations and, therefore, will be removed!", type = "warning")
         dat=quesData()
         idx=which(table(cluster.ids)<lower_bound_obs)
         idxx=-which(cluster.ids%in%idx)
         dt=dat[idxx,]
         cluster.ids=cluster.ids[idxx]
       }
      else{
       dt <-quesData()
     }
     
     design.mat <- model.matrix(cluster.ids ~ data.matrix(dt))
     foldmin=min(5,min(table(cluster.ids)))
     foldid  <- createFolds(factor(cluster.ids), k = foldmin, list = FALSE)
     # fit a cross-validated glm model
     glmfit.cv <- cv.glmnet(design.mat, cluster.ids, family = "multinomial", alpha=0.5, standardize = FALSE,foldid=foldid)
     coef.mat <- do.call(cbind, coef(glmfit.cv, s=glmfit.cv$lambda.1se))
     
     plotMat <- coef.mat[-c(1:2), ]
     rownames(plotMat) <- names(quesData())
     colnames(plotMat) <- paste0("cluster ", levels(factor(cluster.ids)))
     plot_ly(z = data.matrix(plotMat), type = "heatmap", x = colnames(plotMat), 
             colorbar = list(title = "Coefficients"),xgap=2,ygap=2,y=names(quesData()))
     
   })
   
   # Bar/Histogram plot of data features conditioned on cluster ID
   output$univariate <-renderPlotly({
     req(input$numClusters,input$attribute)
     plotUni()
     })
  
   plotUni<-reactive({
     numClusters = input$numClusters
     att=input$attribute
     dat=quesData()
     quesDist <- gower.dist(dat)
     hc <- hclust(as.dist(quesDist), method="ward.D2")
     cluster.ids <- cutree(hc, k = numClusters)
     lower_bound_obs = ceiling(0.1*nrow(quesData()))
     idx=which(cluster.ids%in%which(table(cluster.ids)<lower_bound_obs))
     if (length(idx)!=0){
       dat=dat[-which(cluster.ids%in%which(table(cluster.ids)<lower_bound_obs)),]
       cluster.ids=cluster.ids[-which(cluster.ids%in%which(table(cluster.ids)<lower_bound_obs))]
     }
     dat$clusters=as.factor(cluster.ids)
     # bar plot
     if (is.factor(dat[,att])){
       pl<- qplot(dat[,att], data=dat, geom="bar", fill=clusters)+  scale_fill_brewer(palette = 'Set3')+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))+ labs(x = att)
       
     }
     # histogram plot
     else{
       pl<- ggplot(dat, aes(clusters, dat[,att], fill = clusters)) + 
         geom_boxplot()+ labs(y = att)+ scale_fill_brewer(palette = 'Set3')+
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                       panel.background = element_blank(), axis.line = element_line(colour = "black"))
       
           }
     ggplotly(pl)
     
   })

   
   
  
  #########################################################
  ################ Longitudinal Data ######################
  #########################################################
  
  # User Guide for Longitudinal Clustering
  output$mymarkdown_long <- renderUI({  
    shiny::includeHTML('UserGuideLongiClustering.html') 
  }) 
  
  # update UI
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
     req(input$famddim)
     updateSelectInput(session = session, inputId = "famddim2", choices = 1:input$famddim)
   })
   observe({
     updateSelectInput(session = session, inputId = "visit", choices = choices_visit())
   })
   choices_visit <- reactive({
     req(input$file3_long)
     
     dat=longiData()
     1:nlevels(dat[,ncol(dat)])
   })
   getNumberOfVisits<- reactive({
     dat=longiData()
     nlevels(dat[,ncol(dat)])
   })
   getNumberOfComponents <- reactive({
     datFAMD=longDataFAMD()
     rotx=datFAMD[[2]]
     ndim <- ncol(rotx)
   })
   choices_groups <- reactive({
     req(input$file3_long,input$famddim)
     dat=longDataFAMD()
     ngroups=ncol(dat[[2]])/as.numeric(input$famddim)
     1:ngroups
   })
   
  # download buttons
   output$downloadSankey<- downloadHandler(
     filename = function() {
       "plot_sankey.pdf"
     },
     content = function(file) {
       pdf(file)
       plotSankey()
       dev.off()
         # device <- function(..., width=16, height=10.4) {
        #   grDevices::png(..., width = width, height = height, res = 300, units = "in")
         # }
        #  plot <- plotSankey()
        #  ggsave(file, plot, device = device)
       #ggsave(file,  plotSankey(), width = 16, height = 10.4)
     }#,
    # contentType = "image/png"
    #contentType = "application/pdf"
   )
   output$downloadSpaghetti<- downloadHandler(
     filename = function() {
       "plot_spaghetti.png"
     },
     content = function(file) {
       ggsave(file, plotSpaghetti(), width = 16, height = 10.4)
     },
     contentType = "image/png"
   )
   
  # Load Data from files and cast features
   longiData <- reactive({
     req(input$file1_long)
     tryCatch(
       {
         fileext = tolower(tools::file_ext(input$file1_long$datapath)) 
         # load longitudinal data
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
           # load feature type data
           if(fileext=="xlsx" || fileext=="xls"){
             df = read.xlsx2(input$file2_long$datapath, sheetIndex = 1, header = input$header2_long)
           } 
           else{ 
             var = read.csv(input$file2_long$datapath,
                            header = input$header2_long,
                            sep = input$sep2_long,
                            quote = input$quote2_long)
           }
           # cast columns to factor, ordered and numeric values
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
           #NB: in the last but one column the patient id number is stored
           ids_copy=as.character(df[,ncol(df)-1])
           ids_names=names(table(ids_copy))
           for (id in ids_names){
             ids_copy[which(ids_copy==id)]=paste0(id,1:length(which(ids_copy==id)))
           }
           # row names=PatiendID+Visit, e.g. AAAA1,AAAA2,AAAA3,...
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
  # Output data
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
  
  ## DIMENSION REDUCTION TAB
  
  #FAMD data
  longDataFAMD<- reactive({
    ncomp=as.numeric(input$famddim)
    req(input$file3_long) 
    # read file containing the variables divided by group
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
      # load untransformed data
      dathet=longiData()
      ggroup=groups[!is.na(groups[,2]),]
      cur_dim=c()
      cur_loadings=list()
      model=list()
      # iterate groups
      for(i in group_names){
        # extract variables from group i
        dnames = ggroup[ggroup[,2]==i,1]
        # subset data wrt dnames
        subfamd <- dathet[, as.character(dnames)]
        # perform PCA(numeric data) FAMD (mixed data) with 10 components
        if(all(unlist(sapply(subfamd, class)) == 'numeric')){
          famd <- PCA(subfamd, graph = F) # have to use pca instead of famd - only continuous variables
        }
        else{
          famd <- FAMD(subfamd, graph = F, ncp = 10)
        }
        # for each group save the FAMD/PCA components in a data frame
        cur_dim <- cbind(cur_dim,famd$ind$coord[, 1:ncomp])
        # save loadings
        cur_loadings[[i]] <- famd$var$coord[, 1:ncomp]
        # save model per group
        model[[i]]=famd
      
      }
      return(list(loadings=cur_loadings,dims=cur_dim,models=model))
    }
    
  })
  # output loading table
  output$loading <-  DT::renderDataTable({
    req(input$groupsfamd)
    famd=longDataFAMD()
    group=as.integer(input$groupsfamd)
    Loadings=famd[[1]][[group]]
    return(as.data.frame(t(Loadings)))
  },options = list(scrollX = TRUE))
  
  # individual factor plot per visit and per group
  output$ifp<-renderPlotly({
    req(input$visit)
    # group id
    gr=as.integer(input$groupsfactor)
    # visit id
    vis=as.character(input$visit)
    dat=longiData()
    famd_out=longDataFAMD()
    ids_copy=rownames(dat)
    ids_v=ids_copy[substr(ids_copy,nchar(ids_copy),nchar(ids_copy)) %in% c(vis)]
    pl=fviz_pca_ind(famd_out[[3]][[gr]],select.ind = list(name=ids_v),geom.ind = "text")
    ggplotly(pl)
  })

  ## LONGITUDINAL CLUSTERING TAB
  # cluster selection plot
  output$cluslongchoice<-renderPlotly({
    plotcluslongchoice()
  })
  plotcluslongchoice<-reactive({
    req(input$maxKlong)
    fitted_models=clusterLong()
    icl=ICL(fitted_models)
    aic=AIC(fitted_models)
    bic=BIC(fitted_models)
    y=c(icl,aic,bic)
    x=rep(2:input$maxKlong,3)
    gr=c(rep("ICL",input$maxKlong-1),rep("AIC",input$maxKlong-1),rep("BIC",input$maxKlong-1))
    df=data.frame(x=x,y=y,gr=gr)
    
    p=ggplot(df,aes(x=x,y=y,group=gr,colour=factor(gr))) + geom_line(size = 1)+  scale_color_brewer(palette = 'Dark2')+
      labs(x = "Clusters", y = "", color = "Score")+ geom_point()+theme_bw()
    ggplotly(p)
    
    })
  
  # prepare data for clustering
  clusterData<-reactive({
    dat=longiData()
    var=ncol(dat)
    datFAMD=longDataFAMD()
    components=data.matrix(datFAMD[[2]])
    ndim <- ncol(components) 
    visits=getNumberOfVisits()
    # get all combinations patientIDxVisitID
    t =rep(unique(dat[,var-1]),visits)
    t=t[order(t)]
    t=as.character(t)
    allpatientVisits = paste0(t,1:visits)
    # get combinations patientIDxVisitID from the data
    patientVisits=rownames(components)
    # select the patient which missed at least a visit
    missedpatientvisits=allpatientVisits[which(!allpatientVisits%in%patientVisits)]
    
    # extend the component matrix with NA for the visits which are missed by patient
    # output: components is a npatients*nvisits X ncomponents
    if(!is_empty(missedpatientvisits)){
      missinginfomat = matrix(NA, nrow = length(missedpatientvisits), ncol = ncol(components))
      rownames(missinginfomat)=missedpatientvisits
      components=rbind(components,missinginfomat)
      
    }
    #order the rows alphabetically
    components=components[order(rownames(components)),]
    components=data.matrix(components)
    #if(ndim>1){
      colnames(components)=paste0("_PC",1:ndim)
    #} else {
    #  colnames(components)="_PC1"
    #}
    
    # In the following we want to transform 'components' into npatients X ncomponents*nvisits matrix
    # i.e. each row corresponds to a patient ID and the columns are visit_i_component_j with i=1,...,nvisits, and  j=1,...,ncomponents
    gr=rep(1:visits,length(unique(dat[,var-1])))
    ind=split(1:nrow(components),gr)
    
    clusdata=data.matrix(components[ind[[1]],])
    colnames(clusdata)=paste("visits",1,colnames(components),sep="")
    rownames(clusdata)=unique(dat[,var-1])
    for(i in 2:visits){
      t= data.matrix(components[ind[[i]],])
      colnames(t)=paste("visits",i,colnames(components),sep="")
      rownames(t)=unique(dat[,var-1])
      clusdata=cbind(clusdata,t) 
    }
    
    # select only the patients who haven't missed at least 50% of the visits
    row_sel=apply(clusdata,1,function(x) sum(is.na(x))<(visits%/%2)*ndim)
    clusdata=clusdata[row_sel,]

    return(clusdata)
  })
  
  # longitudinal clustering
  clusterLong<-reactive({
    fulllst = clusterData()
    flx_clust <- flex_psy(fulllst, ndim = getNumberOfComponents(),nclus=input$maxKlong, ntime = getNumberOfVisits()) 
    #flx_clust=fitted_models
    return(flx_clust)
  })

  # sankey plot
  output$sankey<-renderPlot({
    plotSankey()
  })
  
  plotSankey <-reactive({
    fulllst = clusterData()
    fitted_models=clusterLong()
    custom_river(fitted_models,fulllst,levels = input$maxKlong)
  })
  
  # spaghetti plot
  output$spaghetti<-renderPlot({
    plotSpaghetti()
  })
  
  plotSpaghetti <- reactive({
    fulllst = clusterData()
    fitted_models=clusterLong()
    req(input$cluscritera)
    criterion=input$cluscritera
    # obtain optimal number of clusters wrt selected criterion
    if (criterion =="ICL")  nclust_flx <- as.numeric(names(which.min(ICL(fitted_models))))
    if (criterion =="AIC")  nclust_flx <- as.numeric(names(which.min(AIC(fitted_models))))
    if (criterion =="BIC")  nclust_flx <- as.numeric(names(which.min(BIC(fitted_models))))
    
    fitted_model <- getModel(fitted_models, as.character(nclust_flx))
    cluster_flx <- clusters(fitted_model)[1:nrow(fulllst)]
    ndim <- getNumberOfComponents()
    visits=getNumberOfVisits()
    i=as.numeric(input$groupsfactor2)
    dim=as.numeric(input$famddim2)
    long.psych.plot(to_plot = fulllst[, seq(i+dim-1, visits*ndim, ndim)], paste('Dimension',dim,'extracted from group', i), cluster = cluster_flx,v=visits)
    
  })


})

