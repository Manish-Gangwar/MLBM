if (!require(shiny)) {install.packages("shiny")};  library(shiny)
if (!require(tidyverse)) {install.packages("tidyverse")};  library(tidyverse)
if (!require(DT)) {install.packages("DT")};  library(DT)
if (!require(tibble)) {install.packages("tibble")};  library(tibble)
if (!require(descriptr)) {install.packages("descriptr")};  library(descriptr)
if (!require(dplyr)) {install.packages("dplyr")};  library(dplyr)
if (!require(purrr)) {install.packages("purrr")};  library(purrr)
if (!require("mice")) {install.packages("mice")}; library(mice)
if(!require("shinyBS")) {install.packages("shinyBS")}; library(shinyBS)
if(!require("DT")) {install.packages("DT")}; library(DT)
if (!require("shinycssloaders")) {install.packages("shinycssloaders")}; library(shinycssloaders)

options(qwraps2_markup = "markdown")

shinyServer(function(input, output,session) {
  Datasetf0 <- reactive({
    if (is.null(input$file)) { 
      return(NULL)
    }else if (1==1) {
    #}else if (input$id_col==FALSE){
      df <- read.csv(input$file$datapath,
                     stringsAsFactors = TRUE,
                     header = TRUE)
      return(data.frame(df))
    }else{
      df <- read.csv(input$file$datapath,
                     stringsAsFactors = TRUE,
                     header = TRUE)
      df <- tibble::rowid_to_column(df, "ID")
      df$ID = paste0('ID_', df$ID)
      return(data.frame(df))
      }
    })
  
  output$imputemiss <- renderUI({
    if (is.null(input$file)) {return(NULL)}
    else {
        selectInput("imputemiss", "Impute missing vaulues or drop missing value rows", 
                    c("do not impute or drop rows", "impute missing values", "drop missing value rows"), 
                    selected = "drop missing value rows")
      }
  })
  
  output$imout <- renderUI({
    if (is.null(input$file)) {return(NULL)}
    if (input$imputemiss == "do not impute or drop rows") {
      p("Note: to impute or drop missing values (if any) check options in the panel on the left.",style="color:black")}
    else if ((input$imputemiss == "impute missing values")) {
      p("Note: missing values imputed (if any), check options in the panel on the left.",style="color:black")
    }
    else { p("Note: missing value rows dropped (if any), check options in the panel on the left.",style="color:black") }
  })
  
  data0 = reactive({
    if (input$imputemiss == "do not impute or drop rows") 
    { mydataimp=Datasetf0() }
    else if (input$imputemiss == "impute missing values") 
    { mydata = Datasetf0()
    mice_mod = mice(mydata, printFlag=FALSE)
    mydataimp <- complete(mice_mod) }
    else # (input$imputemiss == "drop missing value rows") 
    { mydata = Datasetf0()
    mydataimp = na.omit(mydata)  }
    return(mydataimp)
  })
  
  # output$summ <- renderText(summary_table(mtcars))
  
  output$sel_id_var <- renderUI({
    if(is.null(input$file)){
      return(NULL)
    }else{
      #print(colnames(data()))
      selectInput("id","Select Identity column",
                  choices = colnames(data0()),
                  multiple = FALSE,
                  selected = colnames(data0())[1])
    }
  })
  
  data1 = reactive({
    df=data0()
    df[,input$id]<-as.factor(df[,input$id])
    return(df)
  })
  
  if(!require("descriptr")) {install.packages("descriptr")}
  library(descriptr)
  output$screen_summary <- renderPrint({
    if (is.null(input$file)) {return(NULL)}
    #else {  ds_screener(mydata())}
    else {  str(data1())} 
  })
  
  output$sample_data <- renderDataTable({
    if (is.null(input$file)) {return(NULL)}
    (data1())
    })
  
  nu0.data = reactive({
    if (is.null(input$file)) {return(NULL)}
    else {
      coln=setdiff(colnames(data1()),input$id)
      data = data1()[,coln]
      Class = NULL
      for (i in 1:ncol(data)){
        c1 = (class(data[,i]))
        Class = c(Class, c1)
      }
      nu = which(Class %in% c("numeric","integer","double"))
      nu.data = data.frame(data[,nu])
      return(nu.data)
    }
  })
  
  
  output$colList <- renderUI({
    if (is.null(input$file)) {return(NULL)}
    else {
      coln=setdiff(colnames(data1()),input$id)
      varSelectInput("selVar",label = "Select X variables for adjacency",
                     data = data1()[,coln],multiple = TRUE,selectize = TRUE,selected = colnames(nu0.data()))
    }
  })
  
  filtered_dataset11 <- reactive({if (is.null(input$file)) { return(NULL) }
    else{
      coln=setdiff(colnames(data1()),input$id)
      Dataset <- data1() %>% dplyr::select(!!!c(input$id,input$selVar))
      return(Dataset)
    }})
  
  nu1.data = reactive({
    if (is.null(input$file)) {return(NULL)}
    else {
      #coln=setdiff(colnames(data1()),input$id)
      x01 = filtered_dataset11()
      #x01 = fastDummies::dummy_cols(x01, remove_first_dummy = FALSE,remove_selected_columns = TRUE)
    return(x01)
    }
  })
  
  data = reactive({
    #df=data1()[,c(input$id,colnames(nu1.data()))]
    df=nu1.data()
    #df=data.frame(c(data1()[,input$id],nu1.data()))
    return(df)
  })
  
  nu.data = reactive({
    if (is.null(input$file)) {return(NULL)}
    else {
      coln=setdiff(colnames(data()),input$id)
      data = data()[,coln]
      Class = NULL
      for (i in 1:ncol(data)){
        c1 = (class(data[,i]))
        Class = c(Class, c1)
      }
      nu = which(Class %in% c("numeric","integer","double"))
      nu.data = data.frame(data[,nu])
      return(nu.data)
    }
  })
  
  output$summ <- renderDataTable(
    if (is.null(input$file)) { 
      return(NULL)
    }else{
      data=nu.data()
      maxc=min(ncol(data),100)
      summry_df(data[,1:maxc])
    },options = list(lengthMenu = c(20,50,100), pageLength = 20)
  )
  
  output$df_size <- renderText({
    if (is.null(input$file)) {return(NULL)}
    paste0("data for adjacency matrix calculations; data has ",dim(nu.data())[1],
           " observations (rows) and ", dim(nu.data())[2]," columns (shown below)")
  })
  

  output$node_attr <- renderUI({
    if(is.null(input$file)){
      return(NULL)
    }else{
      #print(colnames(data()))
      coln=setdiff(colnames(data1()),input$id)
      df <- data1()[,coln] # data1 for all columns ..
      df <- df %>%
        as_tibble() %>%
        mutate_if(is.character, factor) 
      factor_var <- df %>%  select_if(function(col) is.factor(col) &
                                                 nlevels(col) < 4) %>% colnames()
      
      selectInput("attr","Select Node Attr",
                  choices = colnames(df)[!colnames(df) %in% input$id],
                  multiple = TRUE,
                  selected = factor_var[!colnames(df) %in% input$id])
    }
  })
  
  values <- reactive({ 
    input_df <- data()
    adj0 = df2adjacency(input_df,
                        cutoff_percentile = input$cut_off,
                        id_var = input$id)
    values <- adj0
  })
  
  
  output$sample_adj <- renderDataTable({
    if (is.null(input$file)) { return(NULL) }
    
    datatable(values(), rownames = TRUE )
    
  })
  
  
  
  # sample data download
  
  output$downloadData1 <- downloadHandler(
    filename = function() { "mtcars.csv" },
    content = function(file) {
      write.csv(read.csv("data/mtcars.csv"), file,row.names = FALSE)
    }
  )
  
  
  # adjaceny download
  output$download_adj_mat <- downloadHandler(
    filename = function() { paste(str_split(input$file$name,"\\.")[[1]][1],"_adj_mat.csv",collapse = " ") },
    content = function(file) {
      #df_csv <- values$df_data
      #rownames(df_csv) <- colnames(df_csv)
      write.csv(values(), file,row.names=TRUE)
    }
  )
  
  attr_df <- reactive({
    df1 <- data()
    rownames(df1) <-  make.names(df1[,input$id], unique=TRUE)
    df1 <- df1[,input$attr]
    df1 <- tibble::rownames_to_column(df1, input$id)
    df1 = purrr::imap_dfc(dplyr::select(df1, everything()), function(item, id){
      paste(id, ": ", item)
    })
    #df1[,input$id] <- rownames(df1)
    df1
  })
  

  output$download_node_attr <- downloadHandler(
    filename = function() { paste(str_split(input$file$name,"\\.")[[1]][1],"_node_attr.csv",collapse = " ") },
    content = function(file) {
      #df_csv <- values$df_data
      #rownames(df_csv) <- colnames(df_csv)
      write.csv(attr_df(), file,row.names=FALSE)
    }
  )
  
})