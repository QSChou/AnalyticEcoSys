library(mdatools)
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(readr)
options(shiny.fullstacktrace = TRUE)
##  cannot open file 'rplots.pdf'
##  Solution: pdf(NULL)
pdf(NULL)

## UDF

colSQLStr <- function(colList) {paste(lapply(colList,function (x) {gsub("^\\s+|\\s+$", "", x)}),collapse = ",")}
inSQLStr <- function(inList) {paste0("(",paste(lapply(inList,function(x) {paste0("'",x,"'")}),collapse = ","),")")}

shinyServer(function(input, output, session) {
  
  print(Sys.time())
  
  ## Gen Meta
  #cred_str1 <- expression(paste0("\"",input$UserName,"\""))
  cred_str1 <- expression(paste0(input$UserName))
  cred_str2 <- expression(paste0("\"",input$Passward,"\""))
  conn_td   <- expression(dbConnect(drv_td,"jdbc:teradata://edw05/TMODE=ANSI",eval(cred_str1),eval(cred_str2),encoding="ASCII"))
  conn_hive_lp <- expression(dbConnect(drv_hive,"jdbc:hive2://10.55.1.185:10000/td_pdata_eda?hive.execution.engine=tez",eval(cred_str1),eval(cred_str2)))
  conn_hive_dl <- expression(dbConnect(drv_hive,"jdbc:hive2://pahdpm01:10000/td_pdata_eda",eval(cred_str1),eval(cred_str2)))
  
  try_conn <- eventReactive(input$connDB,{
    switch(input$data_source,
           "EDW" = dbGetQuery(eval(conn_td),"help table PEDAVIEW.RPT_EDA_PTCH_M1"),
           "DataLake" = dbGetQuery(eval(conn_hive_dl),"show databases"),
           "Lab+" = dbGetQuery(eval(conn_hive_lp),"show databases"))
          })
  
  output$value <- renderText({
    switch(
      input$data_source,
      "EDW" = paste(conn_td,try_conn()),
      "Lab+" = paste(conn_hive_lp,try_conn()),
      "DataLake" = paste(conn_hive_dl,try_conn())
    )
  })

  # render ui - choice the data for analyze. ver 0.01 use fixed csv meta stored on sever ana.tbl.csv, ana_tbl is import in global.R
  defaultwd <- getwd()
  setwd("/srv/shiny-server/pilot-anapp/edc_analysis")
  ana_tbl <- read.table("./meta/ana_tbl.csv",header = TRUE,sep = ",", quote = "\"",stringsAsFactors = FALSE)
  setwd(defaultwd)
  output$ChoiceData <- renderUI({
    selectInput("ChoiceData", "Choice Data for Analyze: ", ana_tbl[,1] ,selected = ana_tbl[2,1] , multiple = FALSE)})
  
  #anaDataMeta <- eventReactive(!is.null(input$ChoiceData),{
  anaDataMeta <- eventReactive(input$connDB, {  
  #anaDataMeta <- reactive({ 
    #anaDataMeta <- ana_tbl %>% dplyr::filter(alias_name %in% input$ChoiceData && data_source %in% input$data_source)})
      subset(ana_tbl, alias_name == input$ChoiceData & data_source == input$data_source)})
  
  output$DataMeta <- renderPrint({
    anaDataMeta()
  })
  
  meta_list <- eventReactive(input$connDB,{
    sqlstr <- as.character(anaDataMeta()$meta_sql)
    sqlstr <- paste0(sqlstr," where TXN_DTTM between (timestamp '",input$inDateRange[1]," 00:00:00')"," and (timestamp '",input$inDateRange[2]," 23:59:59')")
    switch(input$data_source,
      "EDW" = dbGetQuery(eval(conn_td),sqlstr),
      "Lab+" = dbGetQuery(eval(conn_hive_lp),sqlstr),
      "DataLake" = dbGetQuery(eval(conn_hive_dl),sqlstr))
  })
  
  output$meta_list <- renderDataTable(
    meta_list(),options = list(pageLength = 20)
  )
  
  prod_list <- eventReactive(input$fixProd,{
    tmp_list <- meta_list()
    #tmp_list <-meta_tbl()
    if(length(input$opra)>=1 & input$fixOpra) {tmp_list <- tmp_list %>% filter(OPRA_ID %in% input$opra)}
    if(length(input$line)>=1 & input$fixLine) {tmp_list <- tmp_list %>% filter(LINE_EQPT_ID %in% input$line)}
    if(length(input$equip)>=1 & input$fixEquip) {tmp_list <- tmp_list %>% filter(EQPT_ID %in% input$equip)}
    prod_list <- tmp_list %>% select(starts_with("PROD_ID"))
    remove(tmp_list)
    sort(distinct(prod_list)[,1],decreasing = FALSE) 
  })
  
  opra_list <- eventReactive(input$fixOpra,{
    tmp_list <- meta_list()
    #tmp_list <-meta_tbl()
    if(length(input$prod)>=1 & input$fixProd) {tmp_list <- tmp_list %>% filter(PROD_ID %in% input$prod)}
    if(length(input$line)>=1 & input$fixLine) {tmp_list <- tmp_list %>% filter(LINE_EQPT_ID %in% input$line)}
    if(length(input$equip)>=1 & input$fixEquip) {tmp_list <- tmp_list %>% filter(EQPT_ID %in% input$equip)}
    opra_list <- tmp_list %>% select(starts_with("OPRA_ID"))
    remove(tmp_list)
    sort(distinct(opra_list)[,1],decreasing = FALSE)
  })
  
  line_list <- eventReactive(input$fixLine,{
    tmp_list <- meta_list()
    #tmp_list <-meta_tbl()
    if(length(input$prod)>=1 & input$fixProd) {tmp_list <- tmp_list %>% filter(PROD_ID %in% input$prod)}
    if(length(input$opra)>=1 & input$fixOpra) {tmp_list <- tmp_list %>% filter(OPRA_ID %in% input$opra)}
    if(length(input$equip)>=1 & input$fixEquip) {tmp_list <- tmp_list %>% filter(EQPT_ID %in% input$equip)}
    line_list <- tmp_list %>% select(starts_with("LINE_EQPT_ID"))
    remove(tmp_list)
    sort(distinct(line_list)[,1],decreasing = FALSE)
  })
  
  equip_list <- eventReactive(input$fixEquip,{
    tmp_list <- meta_list()
    #tmp_list <-meta_tbl()
    if(length(input$prod)>=1 & input$fixProd) {tmp_list <- tmp_list %>% filter(PROD_ID %in% input$prod)}
    if(length(input$opra)>=1 & input$fixOpra) {tmp_list <- tmp_list %>% filter(OPRA_ID %in% input$opra)}
    if(length(input$line)>=1 & input$fixLine) {tmp_list <- tmp_list %>% filter(LINE_EQPT_ID %in% input$line)}
    equip_list <- tmp_list %>% select(starts_with("EQPT_ID"))
    remove(tmp_list)
    sort(distinct(equip_list)[,1],decreasing = FALSE)
  })
  
  # render ui -- get data selection
  
  output$daterangeui <- renderUI({
    dateRangeInput("inDateRange", "Date range:",
                   start = Sys.Date()-32,end = Sys.Date(), 
                   min = Sys.Date()-365*2,max = Sys.Date(), format = "yyyy/mm/dd")})
  
  output$produi <- renderUI({
    selectInput("prod", NULL,prod_list(),multiple = TRUE)})
  
  output$opraui <- renderUI({
    selectInput("opra", NULL,opra_list(),multiple = TRUE)})
  
  output$lineui <- renderUI({
    selectInput("line", NULL,line_list(),multiple = TRUE)})
  
  output$eqpui <- renderUI({
    selectInput("equip", NULL,equip_list(),multiple = TRUE)})
  
  selCrit <- reactive({
    paste(input$prod,input$opra,input$line,input$equip,input$inDateRange)
  })
  
  output$selCrit <- renderPrint({
    selCrit()
  })
  
  df <- eventReactive(input$getData,{
    querySql <- as.character(anaDataMeta()$main_sql)
    querySql <- paste0(querySql," where a.TXN_DTTM between (timestamp '",input$inDateRange[1]," 00:00:00')"," and (timestamp '",input$inDateRange[2]," 23:59:59')")
    if(length(input$prod) > 0) querySql <- paste(querySql," and a.PROD_ID in ", inSQLStr(input$prod))
    if(length(input$opra) > 0) querySql <- paste(querySql," and a.OPRA_ID in ", inSQLStr(input$opra))
    if(length(input$line) > 0) querySql <- paste(querySql," and a.LINE_EQPT_ID in ", inSQLStr(input$line))
    if(length(input$equip) > 0) querySql <- paste(querySql," and a.EQPT_ID in ", inSQLStr(input$equip))
    
    switch(input$data_source,
           "EDW" =  dbGetQuery(eval(conn_td),querySql),
           "Lab+" = dbGetQuery(eval(conn_hive_lp),querySql),
           "DataLake" = dbGetQuery(eval(conn_hive_dl),querySql))
    })
  
  output$dfTbl <- renderDataTable({
    df <- df() %>% tbl_df()
    if(anaDataMeta()$alias_name == "Bonding_EDC_M1") {
      names(df) <- c("plant_lot_id","TXN_DTTM", "GLS_ID", "eqpt_id","POS", "X","Y","Q","TCP_PTCH","CELL_PTCH","GAUGE","HEIGHT","NG_FLG")
      df <- df %>% 
        dplyr::mutate(ID = c(1:nrow(df))) %>% 
        dplyr::mutate(date = substr(TXN_DTTM, 1, 9)) %>%
        dplyr::mutate(time = substr(TXN_DTTM, 11, length(TXN_DTTM))) %>%
        dplyr::mutate(distance = (X^2 + Y^2)^0.5) %>%
        dplyr::mutate(NG_FLG = as.character(NG_FLG)) %>%  
        select(ID, plant_lot_id, eqpt_id, date, time, TXN_DTTM, GLS_ID, POS, X, Y, distance, Q, 
        TCP_PTCH, CELL_PTCH, GAUGE, HEIGHT, NG_FLG) %>% tbl_df()}
      return(df)
    })
  
  numdat <- reactive({
    as.data.frame(df()[,sapply(tbl_df(),is.numeric)])
  })
  
  output$numdatbl <- renderDataTable(numdat())
  
  output$data_summary <- renderPrint({summary(df())})
  setwd("/tmp/shiny-data")
  edc_sample <- read.csv("simca-sample.csv",encoding = "latin1") 
  colnames(edc_sample)[1] <- "glass"
  edc_sample[,1:18] <- lapply(edc_sample[,1:18],as.factor)
  edc_dat <- edc_sample %>% 
    dplyr::mutate(lr = as.numeric(gsub("%","",LQBubbleLOSS,fixed = TRUE))/100) %>% 
    dplyr::mutate(rank = cut(lr,breaks = c(0,0.1,0.5,1),lables = c("G","M","B"),include.lowest = TRUE))
  
  # Normal Plot
  plot_normall <- reactive({
    numdat <- numdat()
    lapply(1:dim(numdat)[2],function(i) {
      output[[paste0('dist-',names(numdat)[i])]] <- renderPlot({
        x <- numdat[,i]
        bins <- seq(min(x),max(x),length.out = input$bining)
        hist(x,breaks = bins,col='blue',border = 'white',main = paste("Histogram of",names(numdat)[i]))})})
  
  # Insert the right number of plot output objects into the web page
    output$num_plots <- renderUI({
      plot_output_list <- lapply(1:dim(numdat)[2], function(i) {
      plotname <- paste0("dist-", names(numdat)[i])
      plotOutput(plotname)})   
    })
  })
  
  
  
  num_type <- sapply(edc_dat,is.numeric)
  edc_ix <- dplyr::select(edc_dat,1)
  edc_num  <- edc_dat[,num_type]
  edc_meta <- edc_dat[,!num_type]
  
  # render ui -- PCA Selection
  
  output$edcmXSelector <- renderUI({
    selectInput('edcmX', 'edc multiple X', choices = names(edc_num), selected = names(edc_num),multiple = TRUE)
  })
  
  output$edcsXSelector <- renderUI({
    selectInput('edcsX', 'edc Single X', choices = input$edcmX, multiple = FALSE)
  })
  
  output$edcYSelector <- renderUI({
    selectInput('edcY', 'edc Y', choices = names(edc_meta)[-1],selected = NULL,multiple = FALSE)
  })
  
  output$edc_num_summary <- renderPrint(
    summary(edc_num)
  )
  
  output$edc_meta_summary <- renderPrint(
    summary(edc_meta)
  )
  
  observeEvent(input$RunPCA, {
    cat(input$edcmX)
  })
  
  observeEvent(!is.null(event_data("plotly_selected",source = "pcaLoading")),
  {
    e <- event_data("plotly_selected",source = "pcaLoading")
    cat(e$key)
  })
  
  
  edc_pca <- eventReactive(input$RunPCA,
    {
      e <- event_data("plotly_selected",source = "pcaLoading")
      if (is.null(e)) {selcol <- which(names(edc_num) %in% input$edcmX)} else {
        selcol <- which(names(edc_num) %in% e$key)}
        edc_pca <- pca(edc_num[,selcol],scale = TRUE, cv = 1, info = 'Simple PCA model')
     })
  
  edc_pcaC <- reactive({
    selcol <- which(names(edc_meta) %in% input$edcY)
    #pcaC <- dplyr::select(edc_meta,selcol)
    pcaC <- edc_meta[,selcol]
  })
  
  output$pca_score <- renderPlotly({
    d <- event_data("plotly_selected",source = "singleValDtl")
    edc_pca <- edc_pca()
    edc_color <- edc_pcaC()
    d1 <- cbind(edc_ix,as.data.frame(edc_pca$scores[,1:2]),edc_color)
    if(is.null(d) == "FALSE") {d1 <- dplyr::filter(d1,glass %in% d$key)}
    key <- d1$glass
    plot_ly(d1,type = "scatter",x=V1,y=V2,key=key,color = edc_color ,mode = "markers",source = "pcaScore") %>%
    layout(title = "Observation Scores", dragmode = "select")
  })
  
  output$pca_loading <- renderPlotly({
    edc_pca <- edc_pca()
    pca_loading <- as.data.frame(edc_pca$loadings)
    loading <- pca_loading %>% mutate("var" = rownames(pca_loading))
    key <- loading$var
    plot_ly(loading,type = "scatter",x= `Comp 1`,y=`Comp 2`,text = paste(var),key=key,mode = "markers",source = "pcaLoading") %>% 
      layout(title = "Variables Loading (PC1 & PC2)",dragmode = "select")
  })
  
  output$pc1_loading <- renderPlotly({
    edc_pca <- edc_pca()
    pca_loading <- as.data.frame(edc_pca$loadings)
    loading <- pca_loading %>% mutate("var" = rownames(pca_loading)) %>% dplyr::arrange(desc(`Comp 1`))
    key <- loading$var
    plot_ly(loading,type = "bar", x= var,y=`Comp 1`, key=key,mode = "markers",source = "singleCompLoading") %>% 
      layout(title = "Single Principle Component Loading", dragmode = "select")
  })
  
  output$hover <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) edc_pca() else d
  })
  
  edcsX <- reactive({
    edcsX <- input$edcsX
  })
  
  output$singleVarDtl <- renderPlotly({
    if(is.null(edc_pca())) return
    d <- event_data("plotly_selected",source = "pcaScore")
    e <- event_data("plotly_click",source = "singleCompLoading")
    if (is.null(e)) {
      edc_xcol <- which(names(edc_num) %in% edcsX())
      edc_name <- edcsX()} else {
      edc_xcol <- which(names(edc_num) %in% e$key)
      edc_name <- e$key
    }
    edc_xdat <- edc_num[,edc_xcol]
    edc_color <- edc_pcaC()
    plot_data <- cbind(edc_ix,edc_xdat,edc_color)
    if (is.null(d))  {
      key <- plot_data$glass
      plot_ly(plot_data, x=plot_data[,1], y=plot_data[,2],key=key, color = plot_data[,3] , mode = "markers",source = "singleValDtl") %>%
      layout(title = paste("All observation of ",edc_name), dragmode = "select")  }
    else {
      plot_data <- dplyr::filter(plot_data, glass %in% d$key)
      key <- plot_data$glass
      plot_ly(plot_data, x=plot_data[,1],y=plot_data[,2] ,key=key,color = plot_data[,3] ,mode = "markers",source = "singleValDtl") %>%
      layout(title = paste("Selected observation of ",edc_name),dragmode = "select")}
  })
  
  output$brush <- renderPrint({
    d <- event_data("plotly_selected")
    e <- event_data("plotly_click",source = "singleCompLoading")
    if (is.null(e)) {edc_xcol <- which(names(edc_num) %in% edcsX())} else {
      edc_xcol <- e$pointNumber
    }
    edc_xdat <- dplyr::select(edc_num,edc_xcol)
    edc_color <- edc_pcaC()
    plot_data <- cbind(edc_ix,edc_xdat,edc_color)
    paste("selected Glass:",e$key,"  ",plot_data$glass,sep="-")
    })
  
  output$varsel <- renderPrint({
    e <- event_data("plotly_selected",source = "pcaLoading")
    if (is.null(e)) {
      selcol <- which(names(edc_num) %in% input$edcmX)
      paste("according to sidebar:",selcol)
    } else {
      selcol <- e$pointNumber
      edc_pca <- edc_pca()
      pca_loading <- as.data.frame(edc_pca$loadings)
      loading <- pca_loading %>% mutate("var" = rownames(pca_loading))
      paste("according to plot select:",selcol,loading$`Comp 1`)
    }
  })
  
  #output$click <- renderPrint({
  #  d <- event_data("plotly_click")
  #  if (is.null(d)) "Click events appear here (double-click to clear)" else d
  #})
  
  # output$brush <- renderPrint({
  #    d <- event_data("plotly_selected")
  #  if (is.null(d)) "Click and drag events (i.e., select/lasso) appear here (double-click to clear)" else edc_dat[d$pointNumber,1:10]
  # })
  
  output$zoom <- renderPrint({
    d <- event_data("plotly_relayout")
    if (is.null(d)) "Relayout (i.e., zoom) events appear here" else d
  })
  
})
