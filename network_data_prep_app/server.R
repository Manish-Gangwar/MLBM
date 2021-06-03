if (!require(shiny)) {install.packages("shiny")};  library(shiny)
if (!require(tidyverse)) {install.packages("tidyverse")};  library(tidyverse)
if (!require(DT)) {install.packages("DT")};  library(DT)
if (!require(tibble)) {install.packages("tibble")};  library(tibble)
if (!require(descriptr)) {install.packages("descriptr")};  library(descriptr)
if (!require(dplyr)) {install.packages("dplyr")};  library(dplyr)
if (!require(purrr)) {install.packages("purrr")};  library(purrr)
if (!require("mice")) {install.packages("mice")}; library(mice)


options(qwraps2_markup = "markdown")

shinyServer(function(input, output,session) {
  Datasetf0 <- reactive({
    if (is.null(input$file)) { 
      return(NULL)
    }else if(input$id_col==FALSE){
      df <- read.csv(input$file$datapath,
                     stringsAsFactors = TRUE,
                     header = TRUE)
      
      return(df)
    }else{
      df <- read.csv(input$file$datapath,
                     stringsAsFactors = TRUE,
                     header = TRUE)
      df <- tibble::rowid_to_column(df, "ID")
      df$ID = paste0('ID_', df$ID)
      return(df)
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
  
  data = reactive({
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
  
  if(!require("descriptr")) {install.packages("descriptr")}
  library(descriptr)
  output$screen_summary <- renderPrint({
    if (is.null(input$file)) {return(NULL)}
    #else {  ds_screener(mydata())}
    else {  str(data())} 
  })
  
  output$sample_data <- renderDataTable({
    if (is.null(input$file)) {return(NULL)}
    (data())})
  output$df_size <- renderText({
    if (is.null(input$file)) {return(NULL)}
    paste0("Uploaded data has ",dim(data())[1]," rows and ", dim(data())[2]," columns")})
  
  output$summ <- renderDataTable(
    if (is.null(input$file)) { 
      return(NULL)
    }else{
      summry_df(data())
      },options = list(lengthMenu = c(20, 50,100), pageLength = 20)
    )
 # output$summ <- renderText(summary_table(mtcars))
  
  output$sel_id_var <- renderUI({
    if(is.null(input$file)){
      return(NULL)
    }else{
    #print(colnames(data()))
      selectInput("id","Select Identity column",
                  choices = colnames(data()),
                  multiple = FALSE,
                  selected = colnames(data()))
    }
  })
  
  
  output$node_attr <- renderUI({
    if(is.null(input$file)){
      return(NULL)
    }else{
      #print(colnames(data()))
      df <- data()#[,-(input$id)]
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
  
  values <- reactiveValues(df_data = NULL) 
  
  observeEvent(input$apply,{
    input_df <- data()
    adj0 = df2adjacency(input_df,
                        cutoff_percentile = input$cut_off,
                        id_var = input$id)
    values$df_data <- adj0
  })
  
  
  output$sample_adj <- renderDataTable({
    datatable(values$df_data[1:8,1:8], rownames = TRUE )
    
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
      write.csv(values$df_data, file,row.names=TRUE)
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