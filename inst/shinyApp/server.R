
shinyServer(function(input, output, session) {
  # Set maxiumum upload file size to 500MB
  options(shiny.maxRequestSize=500*1024^2)
  # disable tabs Exposure, Covariate, and Construct on page load
  shinyjs::js$disableTab("Exposure")
  shinyjs::js$disableTab("Covariates")
  shinyjs::js$disableTab("Construct")
  
################################################
################# Cohort code
################################################
  
  observeEvent(input$navbarPage, {
    removeNotification("cohortWarning")
    removeNotification("cohortError")
    removeNotification("cohortEW")
    removeNotification("cohortWE")
    removeNotification("exposureWarning")
    removeNotification("exposureError")
    removeNotification("exposureEW")
    removeNotification("exposureWE")
    removeNotification("covariateWarning")
    removeNotification("covariateError")
    removeNotification("covariateWE")
    removeNotification("covariateEW")
    
    removeNotification("constructWarning")
    removeNotification("constructError")
    removeNotification("constructWE")
    removeNotification("constructEW")
    })

  cohort.data <- reactive({
    inFile.cohort <- input$cohort.file
    if (is.null(inFile.cohort))
      return(NULL)
    if(stringr::str_detect(inFile.cohort$datapath, ".sas7bdat")) df <- data.table::setDT(haven::read_sas(inFile.cohort$datapath))
    else df <- data.table::fread(inFile.cohort$datapath,sep = ',')
    return(df)
  })
  
  rv <- reactiveValues(cohort.data = NULL)
  rv <- reactiveValues(cohort.id = NULL)
  rv <- reactiveValues(cohort.index.date = NULL)
  rv <- reactiveValues(cohort.eof.date = NULL)
  rv <- reactiveValues(cohort.eof.type = NULL)
  rv <- reactiveValues(cohort.no.time.indep = NULL)
  rv <- reactiveValues(cohort.index.sas.date = NULL)
  rv <- reactiveValues(cohort.eof.sas.date = NULL)

  observeEvent(input$cohort.file, rv$cohort.data <- cohort.data())
  
  output$cohort.table <- DT::renderDataTable({
    df <- cohort.data()
    DT::datatable(df,options=list(scrollX=TRUE, scrollCollapse=TRUE))
  })
  
  observe({
    value <- c("",names(cohort.data()))
    
    observeEvent(req(cohort.data()),{
      updateSelectInput(session,"cohort.IDvar",choices = value)
    })
    
    observeEvent(req(cohort.data(),input$cohort.IDvar),{
      cohort.index.values <- value[!value%in%c(isolate(input$cohort.IDvar))] 
      updateSelectInput(session,"cohort.index.date",choices = isolate(cohort.index.values))
    })
    
    observeEvent(req(cohort.data(),input$cohort.IDvar,input$cohort.index.date),{
      cohort.EOF.date.values <- value[!value%in%c(isolate(input$cohort.IDvar),isolate(input$cohort.index.date))]
      updateSelectInput(session,"cohort.EOF.date",choices = isolate(cohort.EOF.date.values))
    })
    
    observeEvent(req(cohort.data(),input$cohort.IDvar,input$cohort.index.date,input$cohort.EOF.date),{
      cohort.EOF.type.values <- value[!value%in%c(isolate(input$cohort.IDvar),isolate(input$cohort.index.date),isolate(input$cohort.EOF.date))]
      updateSelectInput(session,"cohort.EOF.type",choices = isolate(cohort.EOF.type.values))
    })
    
    observeEvent(req(cohort.data(),input$cohort.IDvar,input$cohort.index.date,input$cohort.EOF.date,input$cohort.EOF.type),{
      cohort.L0.values <- value[!value%in%c(isolate(input$cohort.IDvar),isolate(input$cohort.index.date),isolate(input$cohort.EOF.date),isolate(input$cohort.EOF.type))] 
      updateSelectInput(session,"cohort.L0",choices = isolate(cohort.L0.values))
    })

    observeEvent(req(cohort.data(),input$cohort.IDvar,input$cohort.index.date,input$cohort.EOF.date,input$cohort.EOF.type,input$cohort.L0),{
      cohort.L0.timeIndep.values <- value[!value%in%c(isolate(input$cohort.IDvar),isolate(input$cohort.index.date),isolate(input$cohort.EOF.date),isolate(input$cohort.EOF.type))]
      cohort.L0.timeIndep.values <- input$cohort.L0
      updateSelectInput(session,"cohort.L0.timeIndep",choices = cohort.L0.timeIndep.values)
    })
  })

  observeEvent(req(input$cohort.L0.timeIndep), {
    output$covariateop <- renderUI({  
      lapply(input$cohort.L0.timeIndep, function(x){
        
        tags$div(id = paste0("extra_criteria_for_", x),
                 h4(x),
                 selectInput(paste0(x,"_categorical"), "Categorical",
                             choices = c("",TRUE,FALSE)),
                 selectInput(paste0(x,"_impute"), "Impute",
                             choices = c("","default","mean","mode","median")), 
                 textInput(paste0(x,"_impute_default_level"), "Impute default level"),
                 tags$hr()
                 )
        })
      })
    })
  
  observe({
    lapply(input$cohort.L0.timeIndep, function(x){
      updateSelectInput(session, paste0(x,"_impute"), selected = ifelse(as.logical(input[[paste0(x,"_categorical")]])==TRUE,"mode","mean"))
    })
  })
  
  timeIndepL0 <- eventReactive(input$set.cohort.button,{
    cov <- input$cohort.L0.timeIndep
    timeIndep.list <- NULL
    for(L0.i in seq_along(cov)){
      timeIndep.list[[L0.i]] <- list("categorical"=as.logical(reactiveValuesToList(input)[paste0(cov[L0.i],"_categorical")]),
                                     "impute"=ifelse(as.character(reactiveValuesToList(input)[paste0(cov[L0.i],"_impute")])=="",NA,as.character(reactiveValuesToList(input)[paste0(cov[L0.i],"_impute")])),
                                     "impute_default_level"=ifelse(as.character(reactiveValuesToList(input)[paste0(cov[L0.i],"_impute")])!="default" || as.character(reactiveValuesToList(input)[paste0(cov[L0.i],"_impute_default_level")])=="",NA,as.character(reactiveValuesToList(input)[paste0(cov[L0.i],"_impute_default_level")])))
    }
    names(timeIndep.list) <- cov
    return(timeIndep.list)
  })

  timeIndepL0.Rcode <- eventReactive(input$set.cohort.button,{
    `%+%` <- function(a, b) paste0(a, b)
    cov <- input$cohort.L0.timeIndep
    timeIndep.list <- NULL
    for(L0.i in seq_along(cov)){
      timeIndep.list[L0.i] <- paste0("'"%+%cov[L0.i]%+%"'","=list('categorical'=",reactiveValuesToList(input)[paste0(cov[L0.i],"_categorical")],",'impute'=",ifelse(as.character(reactiveValuesToList(input)[paste0(cov[L0.i],"_impute")])=="",NA,"'"%+%as.character(reactiveValuesToList(input)[paste0(cov[L0.i],"_impute")])%+%"'"),",'impute_default_level'=",ifelse(as.character(reactiveValuesToList(input)[paste0(cov[L0.i],"_impute")])!="default" || as.character(reactiveValuesToList(input)[paste0(cov[L0.i],"_impute_default_level")])=="",NA,"'"%+%as.character(reactiveValuesToList(input)[paste0(cov[L0.i],"_impute_default_level")])%+%"'"),")")
    }
    timeIndep.list <- paste0(timeIndep.list,collapse = ",")
    timeIndep.list <- "list("%+%timeIndep.list%+%")"
    return(timeIndep.list)
  })
  
  observeEvent(input$cohort.EOF.type,{
    if(input$cohort.EOF.type==""){
      updateSelectInput(session,"cohort.Y.name",choices = "")
      return(NULL)
    }
    updateSelectInput(session,"cohort.Y.name",choices = unique(cohort.data()[,get(input$cohort.EOF.type)]))
    })
  observeEvent(input$cohort.IDvar, {
    if(input$cohort.IDvar=="")
      return(NULL)
    rv$cohort.id <- cohort.data()[,get(input$cohort.IDvar)]
  })
  observeEvent(input$cohort.index.date, {
    if(input$cohort.index.date=="")
      return(NULL)
    rv$cohort.index.date <- cohort.data()[,get(input$cohort.index.date)]
  })
  observeEvent(input$cohort.EOF.date, {
    if(input$cohort.EOF.date=="")
      return(NULL)
    rv$cohort.eof.date <- cohort.data()[,get(input$cohort.EOF.date)]
  })
  observeEvent(input$cohort.EOF.type, {
    if(input$cohort.EOF.type=="")
      return(NULL)
    rv$cohort.eof.type <- cohort.data()[,get(input$cohort.EOF.type)]
  })
  
  cohort.data.final <- eventReactive(input$set.cohort.button,{
    rv$cohort.data[,input$cohort.IDvar] <- as.character(rv$cohort.id)
    rv$cohort.index.sas.date <- is.numeric(rv$cohort.data[,get(input$cohort.index.date)])
    rv$cohort.eof.sas.date <- is.numeric(rv$cohort.data[,get(input$cohort.EOF.date)])
    SAS.file <- isTRUE(stringr::str_detect(input$cohort.file$datapath, ".sas7bdat"))
    if(SAS.file){
      if(rv$cohort.index.sas.date) rv$cohort.data[,input$cohort.index.date] <- lubridate::as_date(rv$cohort.index.date, origin="1960-01-01") else rv$cohort.data[,input$cohort.index.date] <- lubridate::as_date(lubridate::parse_date_time(rv$cohort.index.date, c("Ymd","Ydm","mdY","mYd","dmY","dYm")))
      if(rv$cohort.eof.sas.date) rv$cohort.data[,input$cohort.EOF.date] <- lubridate::as_date(rv$cohort.eof.date, origin="1960-01-01") else rv$cohort.data[,input$cohort.EOF.date] <- lubridate::as_date(lubridate::parse_date_time(rv$cohort.eof.date, c("Ymd","Ydm","mdY","mYd","dmY","dYm")))
    } else{
      rv$cohort.data[,input$cohort.index.date] <- lubridate::as_date(lubridate::parse_date_time(rv$cohort.index.date, c("Ymd","Ydm","mdY","mYd","dmY","dYm"))) 
      rv$cohort.data[,input$cohort.EOF.date] <- lubridate::as_date(lubridate::parse_date_time(rv$cohort.eof.date, c("Ymd","Ydm","mdY","mYd","dmY","dYm"))) 
    }
    rv$cohort.data[,input$cohort.EOF.type] <- as.character(rv$cohort.eof.type)
    col.to.num <- names(rv$cohort.data)[sapply(rv$cohort.data,class)%in%"integer"]
    if(length(col.to.num)>0) rv$cohort.data[,eval(col.to.num) := as.numeric(get(col.to.num))]
    return(rv$cohort.data)
    })
  
  set.cohort <- eventReactive(input$set.cohort.button,{
    #LtAtStructuR::setCohort(data.table::as.data.table(cohort.data.final()), input$cohort.IDvar, input$cohort.index.date, input$cohort.EOF.date, input$cohort.EOF.type, input$cohort.Y.name, input$cohort.L0)
    x <- simsalapar::tryCatch.W.E(LtAtStructuR::setCohort(cohort.data.final(), input$cohort.IDvar, input$cohort.index.date, input$cohort.EOF.date, input$cohort.EOF.type, input$cohort.Y.name, input$cohort.L0, timeIndepL0())) #timeIndepL0()
    if (inherits(x$value, "simpleError") & inherits(x$warning, "simpleWarning")) {
      mess.error <- x$value$message
      mess.warning <- x$warning$message
      showNotification(mess.error,duration=NULL,id="cohortEW",type="error")
      showNotification(mess.warning,duration=NULL,id="cohortWE",type="warning")
    }
    else if(inherits(x$value, "simpleError")){
      mess.error <- x$value$message
      showNotification(mess.error,duration=NULL,id="cohortError",type="error")
    }
    else if(inherits(x$warning, "simpleWarning")){
      mess.warning <- x$warning$message
      showNotification(mess.warning,duration=NULL,id="cohortWarning",type="warning")
      return(x$value)
    }
    return(x$value)
  })
  
  cohort.code <- eventReactive(input$set.cohort.button,{
    SAS.file <- isTRUE(stringr::str_detect(input$cohort.file$datapath, ".sas7bdat"))
    pdt.index.date.method <- paste0("cohortDT[,",input$cohort.index.date,":=as_date(parse_date_time(",input$cohort.index.date,", c('Ymd','Ydm','mdY','mYd','dmY','dYm')))]"," \n")
    pdt.eof.date.method <- paste0("cohortDT[,",input$cohort.EOF.date,":=as_date(parse_date_time(",input$cohort.EOF.date,", c('Ymd','Ydm','mdY','mYd','dmY','dYm')))]"," \n")
    sas.index.date.method <- paste0("cohortDT[,",input$cohort.index.date,":=as_date(",input$cohort.index.date,", origin='1960-01-01')]"," \n")
    sas.eof.date.method <- paste0("cohortDT[,",input$cohort.EOF.date,":=as_date(",input$cohort.EOF.date,", origin='1960-01-01')]"," \n")
    if(SAS.file){
      if(rv$cohort.index.sas.date) index.date.code <- sas.index.date.method else index.date.code <- pdt.index.date.method
      if(rv$cohort.eof.sas.date) EOF.date.code <- sas.eof.date.method else EOF.date.code <- pdt.eof.date.method
    } else{
      index.date.code <- pdt.index.date.method
      EOF.date.code <- pdt.eof.date.method
    }
    `%+%` <- function(a, b) paste0(a, b)
    paste0("library(lubridate)"," \n",
           "library(data.table)"," \n",
           "library(LtAtStructuR)"," \n\n",
           "cohortDT <- fread(file='",input$cohort.file$name,"')"," \n",
           "cohortDT[,",input$cohort.IDvar,":=as.character(",input$cohort.IDvar,")]"," \n",
           index.date.code,
           EOF.date.code,
           "cohortDT[,",input$cohort.EOF.type,":=as.character(",input$cohort.EOF.type,")]"," \n",
           "col.to.num <- names(cohortDT)[sapply(cohortDT,class)%in%'integer']"," \n",
           "cohortDT[,eval(col.to.num) := as.numeric(get(col.to.num))]"," \n",
           "cohort <- setCohort(data = cohortDT, IDvar = '",input$cohort.IDvar,"', index_date = '",input$cohort.index.date,"', EOF_date = '",input$cohort.EOF.date,"', EOF_type = '",input$cohort.EOF.type,"', Y_name = '",input$cohort.Y.name,"', L0 = c(",paste0("'"%+%input$cohort.L0%+%"'",collapse=","),"), L0_timeIndep = ",timeIndepL0.Rcode(),")")
  })
  
  observeEvent(input$set.cohort.button, {
    shinyjs::disable("set.cohort.button")
    rv$cohort.no.time.indep <- all(input[['cohort.L0']]%in%input[['cohort.L0.timeIndep']])
    })
  
  observeEvent(input$cohort.file, {
    shinyjs::disable("set.cohort.button")
  })

  observeEvent({
    #req(input$cohort.IDvar, input$cohort.index.date, input$cohort.EOF.date, input$cohort.EOF.type, input$cohort.Y.name, input$cohort.L0, input$cohort.L0.timeIndep)
    eval(parse(text = paste0("req(input$cohort.IDvar, 
                             input$cohort.index.date, 
                             input$cohort.EOF.date, 
                             input$cohort.EOF.type, 
                             input$cohort.Y.name, 
                             input$cohort.L0, 
                             input$cohort.L0.timeIndep, ",
                             paste0(unlist(lapply(input$cohort.L0.timeIndep, function(x){paste0("input$",x,"_categorical")})),collapse = ","),",",
                             paste0(unlist(lapply(input$cohort.L0.timeIndep, function(x){paste0("input$",x,"_impute")})),collapse = ","),")")))
  }, {
    shinyjs::enable("set.cohort.button")
  }
  )
  
  # output$cohort.code <- renderText({
  #   cohort.code()
  # })
  # 
  # observeEvent(req(set.cohort(),rv$cohort.button.disabled), {
  #   insertUI(
  #     selector = '#cohort_r_template',
  #     ui = tags$div(id = "cohort_insertUI",
  #                   h3("R Template Code"),
  #                   verbatimTextOutput("cohort.code"))
  #   )
  # })
  
  observeEvent({
    input$cohort.file
    input$cohort.IDvar
    input$cohort.index.date
    input$cohort.EOF.date
    input$cohort.EOF.type
    input$cohort.Y.name
    input$cohort.L0
    input$cohort.L0.timeIndep
  }, {
    removeUI(
      selector = '#cohort_insertUI'
    )
    shinyjs::js$disableTab("Exposure")
    shinyjs::js$disableTab("Covariates")
    shinyjs::js$disableTab("Construct")
    removeNotification("cohortEW")
    removeNotification("cohortWE")
    removeNotification("cohortWarning")
    removeNotification("cohortError")
  })
  
  observe({
    req(any(class(set.cohort())%in%c("LtAtObject")))
    shinyjs::js$enableTab("Exposure")
  })
  
################################################    
################ Exposure code  
################################################
  
  exposure.data <- reactive({
    inFile.exposure <- input$exposure.file
    if (is.null(inFile.exposure))
      return(NULL)
    if(stringr::str_detect(inFile.exposure$datapath, ".sas7bdat")) df <- data.table::setDT(haven::read_sas(inFile.exposure$datapath))
    else df <- data.table::fread(inFile.exposure$datapath,sep = ',')
    return(df)
  })
  
  rv <- reactiveValues(exposure.data = NULL)
  rv <- reactiveValues(exposure.id = NULL)
  rv <- reactiveValues(exposure.start.date = NULL)
  rv <- reactiveValues(exposure.end.date = NULL)
  rv <- reactiveValues(exposure.start.sas.date = NULL)
  rv <- reactiveValues(exposure.end.sas.date = NULL)

  observeEvent(input$exposure.file, rv$exposure.data <- exposure.data())
  
  output$exposure.table <- DT::renderDataTable({
    df <- exposure.data()
    DT::datatable(df,options=list(scrollX=TRUE, scrollCollapse=TRUE))
  })  
  
  observe({
    value <- c("",names(exposure.data()))
    
    observeEvent(req(exposure.data()),{
      updateSelectInput(session,"exposure.IDvar",choices = value)
    })
    
    observeEvent(req(exposure.data(),input$exposure.IDvar),{
      exposure.start.date.values <- value[!value%in%c(isolate(input$exposure.IDvar))]
      updateSelectInput(session,"exposure.start.date",choices = exposure.start.date.values)
    })
    
    observeEvent(req(exposure.data(),input$exposure.IDvar,input$exposure.start.date),{
      exposure.end.date.values <- value[!value%in%c(isolate(input$exposure.IDvar),isolate(input$exposure.start.date))]
      updateSelectInput(session,"exposure.end.date",choices = exposure.end.date.values)
    })
    
    observeEvent(req(exposure.data(),input$exposure.IDvar,input$exposure.start.date,input$exposure.end.date),{
      exposure.exp.level.values <- value[!value%in%c(isolate(input$exposure.IDvar),isolate(input$exposure.start.date),isolate(input$exposure.end.date))]
      updateSelectInput(session,"exposure.exp.level",choices = exposure.exp.level.values)
    })
  })
  
  observeEvent(input$exposure.IDvar, {
    if(input$exposure.IDvar=="")
      return(NULL)
    rv$exposure.id <- exposure.data()[,get(input$exposure.IDvar)]
  })
  observeEvent(input$exposure.start.date, {
    if(input$exposure.start.date=="")
      return(NULL)
  rv$exposure.index.date <- exposure.data()[,get(input$exposure.start.date)]
  })
  observeEvent(input$exposure.end.date, {
    if(input$exposure.end.date=="")
    return(NULL)
    rv$exposure.eof.date <- exposure.data()[,get(input$exposure.end.date)]
  })
  
  exposure.data.final <- eventReactive(input$set.exposure.button,{
    #browser()
    rv$exposure.data[,input$exposure.IDvar] <- as.character(rv$exposure.id)
    SAS.file <- isTRUE(stringr::str_detect(input$exposure.file$datapath, ".sas7bdat"))
    rv$exposure.start.sas.date <- is.numeric(rv$exposure.data[,get(input$exposure.start.date)])
    rv$exposure.end.sas.date <- is.numeric(rv$exposure.data[,get(input$exposure.end.date)])
    if(SAS.file){
      if(rv$exposure.start.sas.date) rv$exposure.data[,input$exposure.start.date] <- lubridate::as_date(rv$exposure.index.date, origin="1960-01-01") else rv$exposure.data[,input$exposure.start.date] <- lubridate::as_date(lubridate::parse_date_time(rv$exposure.index.date, c("Ymd","Ydm","mdY","mYd","dmY","dYm")))
      if(rv$exposure.end.sas.date) rv$exposure.data[,input$exposure.end.date] <- lubridate::as_date(rv$exposure.eof.date, origin="1960-01-01") else rv$exposure.data[,input$exposure.end.date] <- lubridate::as_date(lubridate::parse_date_time(rv$exposure.eof.date, c("Ymd","Ydm","mdY","mYd","dmY","dYm")))
    } else{
      rv$exposure.data[,input$exposure.start.date] <- lubridate::as_date(lubridate::parse_date_time(rv$exposure.index.date, c("Ymd","Ydm","mdY","mYd","dmY","dYm")))
      rv$exposure.data[,input$exposure.end.date] <- lubridate::as_date(lubridate::parse_date_time(rv$exposure.eof.date, c("Ymd","Ydm","mdY","mYd","dmY","dYm")))
    }
    return(rv$exposure.data)
  })
  
  set.exposure <- eventReactive(input$set.exposure.button,{
    #LtAtStructuR::setExposure(data.table::as.data.table(exposure.data.final()), input$exposure.IDvar, input$exposure.start.date, input$exposure.end.date)
    x <- simsalapar::tryCatch.W.E(LtAtStructuR::setExposure(exposure.data.final(), input$exposure.IDvar, input$exposure.start.date, input$exposure.end.date, ifelse(input$exposure.exp.level=="",NA,input$exposure.exp.level), ifelse(input$exposure.exp.ref=="",NA,input$exposure.exp.ref)))
    if (inherits(x$value, "simpleError") & inherits(x$warning, "simpleWarning")) {
      mess.error <- x$value$message
      mess.warning <- x$warning$message
      showNotification(mess.error,duration=NULL,id="exposureEW",type="error")
      showNotification(mess.warning,duration=NULL,id="exposureWE",type="warning")
    }
    else if(inherits(x$value, "simpleError")){
      mess.error <- x$value$message
      showNotification(mess.error,duration=NULL,id="exposureError",type="error")
    }
    else if(inherits(x$warning, "simpleWarning")){
      mess.warning <- x$warning$message
      showNotification(mess.warning,duration=NULL,id="exposureWarning",type="warning")
      return(x$value)
    }
    return(x$value)
  })
  
  # exposure.code <- eventReactive(input$set.exposure.button,{
  #   `%+%` <- function(a, b) paste0(a, b)
  #   paste0("exposureDT <- fread(file='",input$exposure.file$name,"')"," \n",
  #          "exposureDT[,",input$exposure.IDvar,":=as.character(",input$exposure.IDvar,")]","\n",
  #          "exposureDT[,",input$exposure.start.date,":=as_date(parse_date_time(",input$exposure.start.date,", c('Ymd','Ydm','mdY','mYd','dmY','dYm')))]","\n",
  #          "exposureDT[,",input$exposure.end.date,":=as_date(parse_date_time(",input$exposure.end.date,", c('Ymd','Ydm','mdY','mYd','dmY','dYm')))]","\n",
  #          "exposure <- setExposure(data = exposureDT, IDvar = '",input$exposure.IDvar,"', start_date = '",input$exposure.start.date,"', end_date = '",input$exposure.end.date,"', exp_level = ",ifelse(input$exposure.exp.level=="",NA,"'"%+%input$exposure.exp.level%+%"'"),", exp_ref = ",ifelse(input$exposure.exp.ref=="",NA,"'"%+%input$exposure.exp.ref%+%"'"),")")
  # })
  
  exposure.code <- eventReactive(input$set.exposure.button,{
    SAS.file <- isTRUE(stringr::str_detect(input$cohort.file$datapath, ".sas7bdat"))
    pdt.start.date.method <- paste0("exposureDT[,",input$exposure.start.date,":=as_date(parse_date_time(",input$exposure.start.date,", c('Ymd','Ydm','mdY','mYd','dmY','dYm')))]","\n")
    pdt.end.date.method <- paste0("exposureDT[,",input$exposure.end.date,":=as_date(parse_date_time(",input$exposure.end.date,", c('Ymd','Ydm','mdY','mYd','dmY','dYm')))]","\n")
    sas.start.date.method <- paste0("exposureDT[,",input$exposure.start.date,":=as_date(",input$exposure.start.date,", origin='1960-01-01')]","\n")
    sas.end.date.method <- paste0("exposureDT[,",input$exposure.end.date,":=as_date(",input$exposure.end.date,", origin='1960-01-01')]","\n")
    if(SAS.file){
      if(rv$exposure.start.sas.date) start.date.code <- sas.start.date.method else start.date.code <- pdt.start.date.method
      if(rv$exposure.end.sas.date) end.date.code <- sas.end.date.method else end.date.code <- pdt.end.date.method
    } else{
      start.date.code <- pdt.start.date.method
      end.date.code <- pdt.end.date.method
    }
    `%+%` <- function(a, b) paste0(a, b)
    paste0("exposureDT <- fread(file='",input$exposure.file$name,"')"," \n",
           "exposureDT[,",input$exposure.IDvar,":=as.character(",input$exposure.IDvar,")]","\n",
           start.date.code,
           end.date.code,
           "exposure <- setExposure(data = exposureDT, IDvar = '",input$exposure.IDvar,"', start_date = '",input$exposure.start.date,"', end_date = '",input$exposure.end.date,"', exp_level = ",ifelse(input$exposure.exp.level=="",NA,"'"%+%input$exposure.exp.level%+%"'"),", exp_ref = ",ifelse(input$exposure.exp.ref=="",NA,"'"%+%input$exposure.exp.ref%+%"'"),")")
  })
  
  observeEvent(input$set.exposure.button, {
    shinyjs::disable("set.exposure.button")
  })
  
  observeEvent(input$exposure.file, {
    shinyjs::disable("set.exposure.button")
  })

  observeEvent({
    req(input$exposure.IDvar, input$exposure.start.date, input$exposure.end.date) #input$exposure.exp.level, input$exposure.exp.ref
  }, {
    shinyjs::enable("set.exposure.button")
  }
  )
  
  # output$exposure.code <- renderText({
  #   exposure.code()
  # })
  # 
  # observeEvent(req(set.exposure(),rv$exposure.button.disabled), {
  #   insertUI(
  #     selector = '#exposure_r_template',
  #     ui = tags$div(id = "exposure_insertUI",
  #                   h3("R Template Code"),
  #                   verbatimTextOutput("exposure.code"))
  #   )
  # })
  
  observeEvent({
    input$exposure.file
    input$exposure.IDvar
    input$exposure.start.date
    input$exposure.end.date
    input$exposure.exp.level
    input$exposure.exp.ref
  }, {
    removeUI(
      selector = '#exposure_insertUI'
    )
    shinyjs::js$disableTab("Covariates")
    shinyjs::js$disableTab("Construct")
    removeNotification("exposureEW")
    removeNotification("exposureWE")
    removeNotification("exposureWarning")
    removeNotification("exposureError")
  })
  
  observe({
    req(any(class(set.cohort())%in%c("LtAtObject")),any(class(set.exposure())%in%c("LtAtObject")))
    if(!rv$cohort.no.time.indep)shinyjs::js$enableTab("Covariates") else shinyjs::js$enableTab("Construct")
  })

################################################
############ Covariate(s) code
################################################  
  
  covariate.data <- reactive({
    inFile.covariate <- input$covariate.file
    if (is.null(inFile.covariate))
      return(NULL)
    if(stringr::str_detect(inFile.covariate$datapath, ".sas7bdat")) df <- data.table::setDT(haven::read_sas(inFile.covariate$datapath))
    else df <- data.table::fread(inFile.covariate$datapath,sep = ',')
    return(df)
  })
  
  rv <- reactiveValues(covariate.data = NULL)
  rv <- reactiveValues(covariate.id = NULL)
  rv <- reactiveValues(covariate.l.date = NULL)
  rv <- reactiveValues(covariate.l.name = NULL)
  rv <- reactiveValues(set.covariate.list = NULL)
  rv <- reactiveValues(set.covariate.list.names = NULL)
  rv <- reactiveValues(covariate.R.code = NULL)
  rv <- reactiveValues(covariate.l.sas.date = NULL)

  observeEvent(input$covariate.file, rv$covariate.data <- covariate.data())
  
  output$covariate.table <- DT::renderDataTable({
    df <- covariate.data()
    DT::datatable(df,options=list(scrollX=TRUE, scrollCollapse=TRUE))
  })  
  
  observe({
    value <- c("",names(covariate.data()))
    
    observeEvent(req(covariate.data()),{
      updateSelectInput(session,"covariate.IDvar",choices = value)
    })
    
    observeEvent(req(covariate.data(),input$covariate.IDvar),{
      covariate.L.date.values <- value[!value%in%c(isolate(input$covariate.IDvar))]
      updateSelectInput(session,"covariate.L.date",choices = covariate.L.date.values)
    })
    
    observeEvent(req(covariate.data(),input$covariate.IDvar,input$covariate.L.date),{
      covariate.L.name.values <- value[!value%in%c(isolate(input$covariate.IDvar),isolate(input$covariate.L.date))]
      updateSelectInput(session,"covariate.L.name",choices = covariate.L.name.values)
    })
  })
  
  observeEvent(input$covariate.IDvar, {
    if(input$covariate.IDvar=="")
      return(NULL)
    rv$covariate.id <- covariate.data()[,get(input$covariate.IDvar)]
  })
  observeEvent(input$covariate.L.date, {
    if(input$covariate.L.date=="")
      return(NULL)
    rv$covariate.l.date <- covariate.data()[,get(input$covariate.L.date)]
  })
  observeEvent(input$covariate.L.name, {
    if(input$covariate.L.name=="")
      return(NULL)
    rv$covariate.l.name <- covariate.data()[,get(input$covariate.L.name)]
  })
  
  observeEvent(input$covariate.file, {
    updateSelectInput(session,"covariate.type",selected = "")
    updateSelectInput(session,"covariate.L.categorical",selected = "")
    updateSelectInput(session,"covariate.L.impute",selected = "")
    updateSelectInput(session,"covariate.L.acute.change",selected = "")
  })
  
  observeEvent(input$covariate.L.categorical,{
    updateSelectInput(session, "covariate.L.impute", selected = ifelse(as.logical(input$covariate.L.categorical)==TRUE,"mode","mean"))
  })
  
  covariate.data.final <- eventReactive(input$set.covariate.button,{
    rv$covariate.data[,input$covariate.IDvar] <- as.character(rv$covariate.id)
    SAS.file <- isTRUE(stringr::str_detect(input$covariate.file$datapath, ".sas7bdat"))
    rv$covariate.l.sas.date <- is.numeric(rv$covariate.data[,get(input$covariate.L.date)])
    if(SAS.file){
      if(rv$covariate.l.sas.date) rv$covariate.data[,input$covariate.L.date] <- lubridate::as_date(lubridate::as_date(rv$covariate.l.date, origin="1960-01-01")) else rv$covariate.data[,input$covariate.L.date] <- lubridate::as_date(lubridate::parse_date_time(rv$covariate.l.date, c("Ymd","Ydm","mdY","mYd","dmY","dYm")))
    }
    else{
      rv$covariate.data[,input$covariate.L.date] <- lubridate::as_date(lubridate::parse_date_time(rv$covariate.l.date, c("Ymd","Ydm","mdY","mYd","dmY","dYm")))
    }
    return(rv$covariate.data)
  })

  output$test <- renderPrint({
    lapply(rv$covariate.data,class)
    lapply(covariate.data.final(),class)
  })
  
  set.covariate <- eventReactive(input$set.covariate.button,{
    #LtAtStructuR::setCovariate(data.table::as.data.table(covariate.data.final()), input$covariate.type, input$covariate.IDvar, input$covariate.L.date, input$covariate.L.name, ifelse(input$covariate.L.class=="character",TRUE,FALSE))#input$covariate_categorical)
    x <- simsalapar::tryCatch.W.E(LtAtStructuR::setCovariate(covariate.data.final(), input$covariate.type, input$covariate.IDvar, input$covariate.L.date, input$covariate.L.name, as.logical(input$covariate.L.categorical), ifelse(input$covariate.L.impute=="",NA,input$covariate.L.impute), ifelse(input$covariate.L.impute.default.level=="",NA,input$covariate.L.impute.default.level), as.logical(input$covariate.L.acute.change))) #ifelse(input$covariate.L.class=="character",TRUE,FALSE)
    if (inherits(x$value, "simpleError") & inherits(x$warning, "simpleWarning")) {
      mess.error <- x$value$message
      mess.warning <- x$warning$message
      showNotification(mess.error,duration=NULL,id="covariateEW",type="error")
      showNotification(mess.warning,duration=NULL,id="covariateWE",type="warning")
    }
    else if(inherits(x$value, "simpleError")){
      mess.error <- x$value$message
      showNotification(mess.error,duration=NULL,id="covariateError",type="error")
    }
    else if(inherits(x$warning, "simpleWarning")){
      mess.warning <- x$warning$message
      showNotification(mess.warning,duration=NULL,id="covariateWarning",type="warning")
      return(x$value)
    }
    return(x$value)
  })
  
  ## ATTENTION: THE BUG OCCURS BECAUSE OF set.covariate()
  observeEvent(input$set.covariate.button,{
    rv$set.covariate.list[[input$covariate.file$name]] <- set.covariate()
    rv$set.covariate.list.names <- c(rv$set.covariate.list.names, input$covariate.file$name)
  })
  
  output$covariate.count <- renderText({
    paste0("Number of covariates uploaded: ",length(unique(rv$set.covariate.list.names)))
  })
  
  output$covariate.uploaded <- renderText({
    if(is.null(rv$set.covariate.list.names)) return("Uploaded covariates will appear here")
    unique(rv$set.covariate.list.names)
  })
  
  covariate.code <- eventReactive(input$set.covariate.button,{
    `%+%` <- function(a, b) paste0(a, b)
    number <- which(unique(rv$set.covariate.list.names)==input$covariate.file$name)
    paste0("covariateDT",number," <- fread(file='",input$covariate.file$name,"')"," \n","covariate",number," <- setCovariate(data = covariateDT",number,", type = '",input$covariate.type,"', IDvar = '",input$covariate.IDvar,"', L_date = '",input$covariate.L.date,"', L_name = '",input$covariate.L.name,"', categorical = ",as.logical(input$covariate.L.categorical),", impute = ",ifelse(input$covariate.L.impute=="",NA,"'"%+%input$covariate.L.impute%+%"'"),", impute_default_level = ",ifelse(input$covariate.L.impute.default.level=="",NA,"'"%+%input$covariate.L.impute.default.level%+%"'"),", acute_change = ",input$covariate.L.acute.change,")")
  })

  # observeEvent(input$set.covariate.button,{
  #   `%+%` <- function(a, b) paste0(a, b)
  #   number <- which(unique(rv$set.covariate.list.names)==input$covariate.file$name)
  #   rv$covariate.R.code[number] <- paste0("covariateDT",number," <- fread(file='",input$covariate.file$name,"')"," \n",
  #                                         "covariateDT",number,"[,",input$covariate.IDvar,":=as.character(",input$covariate.IDvar,")]"," \n",
  #                                         "covariateDT",number,"[,",input$covariate.L.date,":=as_date(parse_date_time(",input$covariate.L.date,", c('Ymd','Ydm','mdY','mYd','dmY','dYm')))]"," \n",
  #                                         "covariate",number," <- setCovariate(data = covariateDT",number,", type = '",input$covariate.type,"', IDvar = '",input$covariate.IDvar,"', L_date = '",input$covariate.L.date,"', L_name = '",input$covariate.L.name,"', categorical = ",as.logical(input$covariate.L.categorical),", impute = ",ifelse(input$covariate.L.impute=="",NA,"'"%+%input$covariate.L.impute%+%"'"),", impute_default_level = ",ifelse(input$covariate.L.impute.default.level=="",NA,"'"%+%input$covariate.L.impute.default.level%+%"'"),", acute_change = ",input$covariate.L.acute.change,")")
  # })
  
  observeEvent(input$set.covariate.button,{
    SAS.file <- isTRUE(stringr::str_detect(input$covariate.file$datapath, ".sas7bdat"))
    number <- which(unique(rv$set.covariate.list.names)==input$covariate.file$name)
    pdt.cov.date.method <- paste0("covariateDT",number,"[,",input$covariate.L.date,":=as_date(parse_date_time(",input$covariate.L.date,", c('Ymd','Ydm','mdY','mYd','dmY','dYm')))]"," \n")
    sas.cov.date.method <- paste0("covariateDT",number,"[,",input$covariate.L.date,":=as_date(",input$covariate.L.date,", origin='1960-01-01')]"," \n")
    if(SAS.file){
      if(rv$covariate.l.sas.date) cov.date.code <- sas.cov.date.method else cove.date.code <- pdt.cov.date.method
    } else{
      cov.date.code <- pdt.cov.date.method
    }
    `%+%` <- function(a, b) paste0(a, b)
    rv$covariate.R.code[number] <- paste0("covariateDT",number," <- fread(file='",input$covariate.file$name,"')"," \n",
                                          "covariateDT",number,"[,",input$covariate.IDvar,":=as.character(",input$covariate.IDvar,")]"," \n",
                                          cov.date.code,
                                          "covariate",number," <- setCovariate(data = covariateDT",number,", type = '",input$covariate.type,"', IDvar = '",input$covariate.IDvar,"', L_date = '",input$covariate.L.date,"', L_name = '",input$covariate.L.name,"', categorical = ",as.logical(input$covariate.L.categorical),", impute = ",ifelse(input$covariate.L.impute=="",NA,"'"%+%input$covariate.L.impute%+%"'"),", impute_default_level = ",ifelse(input$covariate.L.impute.default.level=="",NA,"'"%+%input$covariate.L.impute.default.level%+%"'"),", acute_change = ",input$covariate.L.acute.change,")")
  })
  
  observeEvent(input$set.covariate.button, {
    shinyjs::disable("set.covariate.button")
  })
  
  observeEvent(input$covariate.file, {
    shinyjs::disable("set.covariate.button")
  })
  
  observeEvent({
    req(input$covariate.type, input$covariate.IDvar, input$covariate.L.date, input$covariate.L.name, input$covariate.L.categorical, input$covariate.L.impute, input$covariate.L.acute.change) #input$covariate.L.class
  }, {
    shinyjs::enable("set.covariate.button")
  }
  )
  
  # output$covariate.code <- renderText({
  #   paste0(rv$covariate.R.code, collapse = " \n\n")
  # })
  # 
  # observeEvent(req(set.covariate(),rv$covariate.button.disabled), {
  #   insertUI(
  #     selector = '#covariate_r_template',
  #     ui = tags$div(id = "covariate_insertUI",
  #                   h3("R Template Code"),
  #                   verbatimTextOutput("covariate.code"))
  #   )
  # })
  
  observeEvent({
    input$covariate.type
    input$covariate.IDvar
    input$covariate.L.date
    input$covariate.L.name
    input$covariate.L.class
    input$covariate.L.categorical
    input$covariate.L.impute
    input$covariate.L.acute.change
  }, {
    removeUI(
      selector = '#covariate_insertUI'
    )
    shinyjs::js$disableTab("Construct")
    removeNotification("covariateWarning")
    removeNotification("covariateError")
    removeNotification("covariateWE")
    removeNotification("covariateEW")
  })
  
  observe({
    req(any(class(set.cohort())%in%c("LtAtObject")),any(class(set.exposure())%in%c("LtAtObject")),any(class(set.covariate())%in%c("LtAtObject")))
    shinyjs::js$enableTab("Construct")
  })

################################################
############ Construct code
################################################
  
  specification <- eventReactive(input$set.construct.button,{
    if(rv$cohort.no.time.indep){
      x <- simsalapar::tryCatch.W.E(eval(parse(text=paste0("set.cohort()+set.exposure()"))))
      if (inherits(x$value, "simpleError") & inherits(x$warning, "simpleWarning")) {
        mess.error <- x$value$message
        mess.warning <- x$warning$message
        showNotification(mess.error,duration=NULL,type="error")
        showNotification(mess.warning,duration=NULL,type="warning")
      }
      else if(inherits(x$value, "simpleError")){
        mess.error <- x$value$message
        showNotification(mess.error,duration=NULL,type="error")
      }
      else if(inherits(x$warning, "simpleWarning")){
        mess.warning <- x$warning$message
        showNotification(mess.warning,duration=NULL,type="warning")
        return(x$value)
      }
      return(x$value)
    } else{
      for(i in 1:length(rv$set.covariate.list)){
        if(i==1){
          list.i <- paste0("rv$set.covariate.list[[",i,"]]")
        }
        else{
          list.i <- c(list.i,paste0("rv$set.covariate.list[[",i,"]]"))
        } 
      }
      covariates.string <- paste0(list.i,collapse="+")
      ## Create reactive values for set.cohort() and set.exposure() and see if it works
      #LtAt.specification <- eval(parse(text=paste0("set.cohort()+set.exposure()+",covariates.string)))
      x <- simsalapar::tryCatch.W.E(eval(parse(text=paste0("set.cohort()+set.exposure()+",covariates.string))))
      if (inherits(x$value, "simpleError") & inherits(x$warning, "simpleWarning")) {
        mess.error <- x$value$message
        mess.warning <- x$warning$message
        showNotification(mess.error,duration=NULL,type="error")
        showNotification(mess.warning,duration=NULL,type="warning")
      }
      else if(inherits(x$value, "simpleError")){
        mess.error <- x$value$message
        showNotification(mess.error,duration=NULL,type="error")
      }
      else if(inherits(x$warning, "simpleWarning")){
        mess.warning <- x$warning$message
        showNotification(mess.warning,duration=NULL,type="warning")
        return(x$value)
      }
      return(x$value)
    }
  })
  
  construct <- eventReactive(input$set.construct.button,{
    if(input$m.cores){
      library(future)
      options(mc.cores=input[['multiple_cores_slider']])
      plan(multiprocess)
    }
    x <- simsalapar::tryCatch.W.E(LtAtStructuR::construct(specification(), time_unit = input$time.unit, first_exp_rule = as.numeric(input$first.exp.rule), exp_threshold = input$exp.threshold, format = input$format, dates = as.logical(input$dates)))
    if (inherits(x$value, "simpleError") & inherits(x$warning, "simpleWarning")) {
      mess.error <- x$value$message
      mess.warning <- x$warning$message
      showNotification(mess.error,duration=NULL,id="constructEW",type="error")
      showNotification(mess.warning,duration=NULL,id="constructWE",type="warning")
    }
    else if(inherits(x$value, "simpleError")){
      mess.error <- x$value$message
      showNotification(mess.error,duration=NULL,id="constructError",type="error") #mess.error #removes "attempt to apply non function" error when a covariate is not specified as baseline in cohort tab but is uploaded to the covariates tab; have to make sure this doesn't stop other needed error messages from being blocked
    }
    else if(inherits(x$warning, "simpleWarning")){
      mess.warning <- x$warning$message
      showNotification(mess.warning,duration=NULL,id="constructWarning",type="warning")
      return(x$value)
    }
    return(x$value)
  })
  
  output$final.table <- DT::renderDataTable({
    if(data.table::is.data.table(construct())){
      df <- construct()
      DT::datatable(df,options=list(scrollX=TRUE, scrollCollapse=TRUE))
    }
  })

  observeEvent(input$set.construct.button, {
    shinyjs::disable("set.construct.button")
  })
  
  observeEvent({
    req(input$cohort.IDvar,
        input$cohort.index.date,
        input$cohort.EOF.date,
        input$cohort.EOF.type,
        input$cohort.Y.name,
        input$cohort.L0,
        input$exposure.IDvar,
        input$exposure.start.date,
        input$exposure.end.date,
        input$covariate.type,
        input$covariate.IDvar,
        input$covariate.L.date, 
        input$covariate.L.name,
        input$covariate.L.class,
        input$time.unit,
        input$first.exp.rule,
        input$exp.threshold)
  }, {
    shinyjs::enable("set.construct.button")
  }
  )
  
  observeEvent(req(data.table::is.data.table(construct())), { #input$set.construct.button
    insertUI(
      selector = '#construct_final_table',
      ui = tags$div(id = "construct_table_insertUI", 
                    DT::dataTableOutput("final.table")
      )
    )
  })
  
  observeEvent(req(data.table::is.data.table(construct())), { #input$set.construct.button
    insertUI(
      selector = '#download',
      ui = tags$div(id = "download_insertUI", downloadButton("downloadData", "Download data"), downloadButton("downloadR", "Download R code"))
    )
  })
  
  construct.code <- eventReactive(input$set.construct.button,{
    if(rv$cohort.no.time.indep){
      paste0("LtAt.specification <- cohort + exposure \n","LtAt.data <- construct(LtAt.specification, time_unit = ",input$time.unit,", first_exp_rule = ",input$first.exp.rule,", exp_threshold = ",input$exp.threshold,") \n","fwrite(LtAt.data, file = 'final_dataset.csv', row.names = FALSE)")
    } else{
      paste0("LtAt.specification <- cohort + exposure + ",paste0('covariate',c(1:length(unique(rv$set.covariate.list.names))),collapse = " + ")," \n","LtAt.data <- construct(LtAt.specification, time_unit = ",input$time.unit,", first_exp_rule = ",input$first.exp.rule,", exp_threshold = ",input$exp.threshold,") \n","fwrite(LtAt.data, file = 'final_dataset.csv', row.names = FALSE)")
    }
  })
  
  output$construct.code <- renderText({
    `%+%` <- function(a, b) paste0(a, b)
    if(rv$cohort.no.time.indep){
      paste0(cohort.code()%+%" \n\n",exposure.code()%+%" \n\n",construct.code())
    } else{
      paste0(cohort.code()%+%" \n\n",exposure.code()%+%" \n\n",paste0(rv$covariate.R.code, collapse = " \n\n")%+%" \n\n",construct.code())
    }
  })
  
  final.R.code <- eventReactive(input$set.construct.button,{
    `%+%` <- function(a, b) paste0(a, b)
    paste0(cohort.code()%+%" \n\n",exposure.code()%+%" \n\n",paste0(rv$covariate.R.code, collapse = " \n\n")%+%" \n\n",construct.code())
  })
  
  observeEvent(req(data.table::is.data.table(construct())), {
    insertUI(
      selector = '#construct_r_template',
      ui = tags$div(id = "construct_r_insertUI", 
                    h3("R Template Code"),
                    verbatimTextOutput("construct.code"))
    )
  })
  
  observeEvent({
    input$cohort.IDvar
    input$cohort.index.date
    input$cohort.EOF.date
    input$cohort.EOF.type
    input$cohort.Y.name
    input$cohort.L0
    input$exposure.IDvar
    input$exposure.start.date
    input$exposure.end.date
    input$covariate.type
    input$covariate.IDvar
    input$covariate.L.date 
    input$covariate.L.name
    input$covariate.L.class
    input$time.unit
    input$first.exp.rule
    input$exp.threshold
    input$format
    input$dates
  }, {
    removeUI(
      selector = '#download_insertUI'
    )
    removeUI(
      selector = '#construct_r_insertUI'
    )
    removeUI(
      selector = '#construct_table_insertUI'
    )
    shinyjs::enable("set.construct.button")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("final_dataset.csv", sep = "")
    },
    content = function(file) {
      data.table::fwrite(construct(), file, row.names = FALSE)
    }
  )
  
  output$downloadR <- downloadHandler(
    filename = function() {
      paste("LtAt_code.R", sep = "")
    },
    content = function(file) {
      writeLines(paste(final.R.code(), collapse = ", "), file)
    }
  )
  
  UI_exist = FALSE
  observeEvent({input$m.cores}, {
    if(!UI_exist && sum(input$m.cores) == 1){
      insertUI(
        selector = "#multiple_cores",
        ui = tags$div(id = "multiple_cores_UI", sliderInput("multiple_cores_slider", "Number of cores to use for computation", min = 1, max = length(future::availableWorkers("system")), value = 1))
      )  
      UI_exist <<- TRUE
    }
    
    if(UI_exist && !input$m.cores){ # if UI exists and 
      removeUI(selector = "div#multiple_cores > div")
      UI_exist <<- FALSE
    }
    
  })
  
})