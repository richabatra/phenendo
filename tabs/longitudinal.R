# User Guide for Longitudinal Clustering
output$mymarkdown_long <- renderUI({  
  shiny::includeHTML('guide/UserGuideLongiClustering.html') 
}) 
output$boxes_long<-renderUI({
  req(input$file1_long)
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
  )
  
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
    fulllst = clusterData()
    fitted_models=clusterLong()
    fitted_models=fitted_models[[1]]
    
    pdf(file)
    custom_river(fitted_models,fulllst,levels = input$optKlong)
    dev.off()
    
  }#,
  
)
output$downloadcluslongchoice <- downloadHandler(
  filename = function() {
    "plot_optimal_K.pdf"
  },
  content = function(file){
    fitted_models=clusterLong()
    fitted_models=fitted_models[[1]]
    
    
    icl=ICL(fitted_models)
    aic=AIC(fitted_models)
    bic=BIC(fitted_models)
    y=c(icl,aic,bic)
    x=rep(2:input$maxKlong,3)
    gr=c(rep("ICL",input$maxKlong-1),rep("AIC",input$maxKlong-1),rep("BIC",input$maxKlong-1))
    df=data.frame(x=x,y=y,gr=gr)
    
    p=ggplot(df,aes(x=x,y=y,group=gr,colour=factor(gr))) + geom_line(size = 1)+  scale_color_brewer(palette = 'Dark2')+
      labs(x = "Clusters", y = "", color = "Score")+ geom_point()+theme_bw()
    ggsave(file, p ,width = 16, height = 10.4,device = "pdf")
    
  },
  contentType = "application/pdf"
)
output$downloadifp <- downloadHandler(
  
  filename = function() {
    "plot_ifp.pdf"
  },
  content = function(file) {
    
    req(input$visit)
    # group id
    gr=as.integer(input$groupsfactor)
    # visit id
    vis=as.character(input$visit)
    dat=longiData()
    famd_out=longDataFAMD()
    ids_copy=rownames(dat)
    ids_v=ids_copy[substr(ids_copy,nchar(ids_copy),nchar(ids_copy)) %in% c(vis)]
    
    ggsave(file, fviz_pca_ind(famd_out[[3]][[gr]],select.ind = list(name=ids_v),geom.ind = "text"),width = 16, height = 10.4,device = "pdf")
    
  },
  contentType = "application/pdf"
)
output$downloadSpaghetti<- downloadHandler(
  filename = function() {
    "plot_spaghetti.pdf"
  },
  content = function(file) {
    fulllst = clusterData()
    fitted_models=clusterLong()
    fitted_models=fitted_models[[1]]
    
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
    
    ggsave(file, long.psych.plot(to_plot = fulllst[, seq(i+dim-1, visits*ndim, ndim)], paste('Dimension',dim,'extracted from group', i), cluster = cluster_flx,v=visits), width = 16, height = 10.4,device = "pdf")
  },
  contentType = "application/pdf"
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
        # first anonymize
        nl=nlevels(df[,ncol(df)-1])
        levels(df[,ncol(df)-1])=paste0(paste0("patient_",1:nl),"_")
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
  if(is.null(input$file1_long)){
    shinyalert(title = "Longitudinal Data is missing!", type = "error")
  }
  ncomp=as.numeric(input$famddim)
  req(input$file1_long, input$file3_long) 
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
  fitted_models=fitted_models[[1]]
  
  if(!is.null(fitted_models)){
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
  }
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
  rownames(clusdata)=levels(dat[,var-1])
  for(i in 2:visits){
    t= data.matrix(components[ind[[i]],])
    colnames(t)=paste("visits",i,colnames(components),sep="")
    rownames(t)=levels(dat[,var-1])
    clusdata=cbind(clusdata,t) 
  }
  
  # select only the patients who haven't missed at least 50% of the visits
  row_sel=apply(clusdata,1,function(x) sum(is.na(x))<(visits%/%2)*ndim)
  clusdata=clusdata[row_sel,]
  
  return(clusdata)
})

# longitudinal clustering
clusterLong<-reactive({
  if (is.null(input$goButton) || input$goButton == 0){return()}
  isolate({
    input$goButton
    # Disable a button
    fulllst = clusterData()
    flx_clust <- flex_psy(fulllst, ndim = getNumberOfComponents(),nclus=input$maxKlong, ntime = getNumberOfVisits()) 
    #flx_clust=fitted_models
    #return(flx_clust)
    # Enable a button again
    enable("goButton")
    flx_clust
  })
  
})

# sankey plot
output$sankey<-renderPlot({
  plotSankey()
})

plotSankey <-reactive({
  fulllst = clusterData()
  fitted_models=clusterLong()
  custom_river(fitted_models[[1]],fulllst,levels = input$optKlong)
})
output$clusAssignTable <- downloadHandler(
  
  filename = function() {
    paste("table-assignements", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    mod_gr=clusterLong()
    ids=clusters(mod_gr[[1]]@models[[input$optKlong-1]])
    gr=mod_gr[[2]]
    data=data.frame(name=gr,clusterID=ids)
    write.csv(data, file, row.names = FALSE)
  }
)
output$clusLoadingsTable <- downloadHandler(
  filename = function() {
    paste("table-loadings", "_group_", input$groupsfamd,  "_numdim_", input$famddim, ".csv", sep="")
  },
  content = function(file) {
    famd=longDataFAMD()
    group=as.integer(input$groupsfamd)
    Loadings=famd[[1]][[group]]
    data = as.data.frame(t(Loadings))
    write.csv(data, file, row.names = FALSE)
  }
)
# spaghetti plot
output$spaghetti<-renderPlot({
  plotSpaghetti()
})

plotSpaghetti <- reactive({
  fulllst = clusterData()
  fitted_models=clusterLong()
  fitted_models=fitted_models[[1]]
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