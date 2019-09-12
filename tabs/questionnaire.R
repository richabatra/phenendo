# User Guide phenendo
output$mymarkdown <- renderUI({  
  shiny::includeHTML('guide/UserGuide_adj.html') 
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
output$boxes<-renderUI({
  req(input$file1)
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
  )
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
output$secondSelectionSankey <- renderUI({
  req(input$maxK)
  selectInput('optk', 'Choose number of clusters', choices = 2:input$maxK)
})

## RAW DATA VISUALIZATION TAB
# Data heatmap plot
output$Heatmap <-renderPlotly({
  plotHeat()
})
output$downloadHeatmap<- downloadHandler("plot_heatmap.pdf", function(theFile) {
  
  makePdf <- function(filename){
    pdf(file = filename)
    
    export(plotHeat(), file = "plot_heatmap.pdf")
    
    r <- brick(file.path(getwd(), "plot_heatmap.pdf"))
    plotRGB(r)
    
    dev.off()
  }
  
  makePdf(theFile)
})



plotHeat<-reactive({
  dataMat = quesData()
  dataMat = apply(dataMat, 2, FUN=function(x) rescale(as.numeric(as.matrix(x)), to=c((1/length(unique(x))),1)))
  plot_ly(z = data.matrix(dataMat), type = "heatmap", x = colnames(dataMat), 
          colorbar = list(title = "Attribute Value"))
  
})

# Data FAMD plot
output$PCA <-renderPlot({
  plotPCA()
})
plotPCA<-reactive({
  dataMat = quesData()
  getFAMD(dataMat)     
  
})

output$downloadunivariate<- downloadHandler(
  
  filename = function() {
    "plot_univariate.pdf"
  },
  content = function(file) {
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
    
    ggsave(file, pl, width = 16, height = 10.4,device = "pdf")
  },
  contentType = "application/pdf"
)
output$downloadcdf<- downloadHandler(
  
  filename = function() {
    "plot_cdf.pdf"
  },
  content = function(file) {
    df = computeDeltaAndCDF()[[2]]
    p=ggplot(df,aes(x=x,y=y,group=col,colour=factor(col))) + geom_line(size = 1)+  scale_color_brewer(palette = 'Set3')+
      labs(x = "Consensus Index", y = "CDF", color = "Clusters")+ theme_bw()
    
    ggsave(file, p, width = 16, height = 10.4,device = "pdf")
  },
  contentType = "application/pdf"
)
output$downloaddelta <- downloadHandler(
  
  filename = function() {
    "plot_delta.pdf"
  },
  content = function(file) {
    df = computeDeltaAndCDF()[[1]]
    pp=ggplot(df,aes(x=x,y=y)) + geom_line(size = 1, color="#ED1443")+ geom_point(size =3)+
      labs(x = "k", y = "Relative Change in Area under CDF Curve")+ theme_bw()
    ggsave(file, pp, width = 16, height = 10.4,device = "pdf")
  },
  contentType = "application/pdf"
)
output$downloadPCA <- downloadHandler(
  
  filename = function() {
    "plot_pca.pdf"
  },
  content = function(file) {
    dataMat = quesData()
    #p <- getFAMD(dataMat)
    #p <- plotPCA() 
    ggsave(file, getFAMD(dataMat), width = 16, height = 10.4,device = "pdf")
  },
  contentType = "application/pdf"
)
output$downloadBarplot <- downloadHandler(
  
  filename = function() {
    "plot_bar.pdf"
  },
  content = function(file) {
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
    ggsave(file, pl, width = 16, height = 10.4,device = "pdf")
  },
  contentType = "application/pdf"
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
output$downloadclusteringHeatmap <- downloadHandler("plot_ClusteringHeatmap.pdf", function(theFile) {
  
  makePdf <- function(filename){
    pdf(file = filename)
    
    export(plotClustering(), file = "plot_ClusteringHeatmap.pdf")
    
    r <- brick(file.path(getwd(), "plot_ClusteringHeatmap.pdf"))
    plotRGB(r)
    
    dev.off()
  }
  
  makePdf(theFile)
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


output$clusteringSankey<-renderPlot({
  plotclusteringSankey()
})

plotclusteringSankey<-reactive({
  clustering=cluster()
  custom_river2(clustering,levels = as.numeric(input$optk))
})
output$downloadclusteringSankey<- downloadHandler(
  filename = function() {
    "plot_sankey.pdf"
  },
  content = function(file) {
    clustering=cluster()
    pdf(file)
    custom_river2(clustering,levels = as.numeric(input$optk))
    dev.off()
    
  }#,
  
)