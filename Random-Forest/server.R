if (!require('randomForest')){install.packages('randomForest')}; library(randomForest)
if (!require('datasets')){install.packages('datasets')}; library(datasets)
if (!require('caret')){install.packages('caret')}; library(caret)
if (!require('ROCR')){install.packages('ROCR')}; library(ROCR)
if (!require('magrittr')){install.packages('magrittr')}; library(magrittr)
if (!require('shiny')){install.packages('shiny')}; library(shiny)
if (!require('markdown')){install.packages('markdown')}; library(markdown)
if (!require('shinyWidgets')){install.packages('shinyWidgets')}; library(shinyWidgets)
if (!require('DT')){install.packages('DT')}; library(DT)
if (!require('Amelia')){install.packages('Amelia')}; library(Amelia)
if (!require('ggplot2')){install.packages('ggplot2')}; library(ggplot2)
if (!require('e1071')){install.packages('e1071')}; library(e1071)
if (!require('dplyr')){install.packages('dplyr')}; library(dplyr)
if (!require('tidyr')){install.packages('tidyr')}; library(tidyr)
if(!require("shinyBS")) {install.packages("shinyBS")}; library(shinyBS)
if(!require("corrplot")) {install.packages("corrplot")};library(corrplot)

server <- function(input, output,session) {
  
  tr_data1 <-  reactive({
    req(input$tr_data$datapath)
    df <- read.csv(input$tr_data$datapath,stringsAsFactors = FALSE)
    return(df)
  })
  
  test_data <-  reactive({
    req(input$test_data$datapath)
    df <- read.csv(input$test_data$datapath,stringsAsFactors = FALSE)
    return(df)
  })
  
  tr_cols <- reactive({
    req(input$tr_data$datapath)
    return(colnames(tr_data1()))
    })
 
  
  #----Tab-2 Data Summary----#
  
  output$samp <- DT::renderDataTable({
    req(input$tr_data$datapath)
    DT::datatable(tr_data1(),
                  #filter = "top"
                  options = list(lengthMenu = list(c(5,25,50,-1),c("5","25","50","All")),
                                autoWidth = TRUE),
                  #caption = "Table 1: Sample of Data"
                  )
  })
  
  output$sampp <- DT::renderDataTable({
    req(input$test_data$datapath)
    DT::datatable(test_data(),
                  #filter = "top"
                  options = list(lengthMenu = list(c(5,25,50,-1),c("5","25","50","All")),
                                 autoWidth = TRUE),
                  #caption = "Table 1: Sample of Data"
    )
  })
  

  #-------------#
  output$y_ui <- renderUI({
    req(input$tr_data$datapath)
    selectInput(inputId = 'sel_y',label = "Select Y",choices = tr_cols(),multiple = FALSE)
  })
  
  x_col <- reactive({
    req(input$tr_data$datapath)
    x <- match(input$sel_y,tr_cols())
    y_col <- tr_cols()[-x]
    return(y_col)
  })
  
  output$x_ui <- renderUI({
    req(input$tr_data$datapath)
    selectInput(inputId = "sel_x",label="Select X",choices = x_col(),multiple = TRUE,selected = x_col())
  })
  
  tr_data <- reactive({
    tr_data1()[,c(input$sel_y,input$sel_x)]
  })
  
  output$data_str <- renderPrint({
    str(tr_data())
  })
  
  output$miss_plot1 <- renderPlot({
    req(input$tr_data$datapath)
    Amelia::missmap(tr_data())
  })
  
  if(!require("descriptr")) {install.packages("descriptr")}
  library(descriptr)
  output$miss_plot <- renderPrint({
    if (is.null(input$tr_data)) {return(NULL)}
    else {  ds_screener(  tr_data() )} 
  })
 
  output$chry <- renderUI({ 
  if (class(tr_data()[,input$sel_y]) %in% c("numeric")) {
    radioButtons("task","Select task",choices = c("Regression" = "reg"))
    }
  else if (class(tr_data()[,input$sel_y]) %in% c("integer")) {
  radioButtons("task","Select task",choices = c("Regression" = "reg","Classification" = 'clf'))
  }
  else {
      radioButtons("task","Select task",choices = c("Classification" = 'clf'))
    }
  })
  
  output$yout <- renderUI({
    if (identical(tr_data(), '') || identical(tr_data(),data.frame())) return(NULL)
    p("selected Y = ",input$sel_y,style="color:red")
  })
  
  output$yout21 <- renderUI({
    if (identical(tr_data(), '') || identical(tr_data(),data.frame())) return(NULL)
    if (input$task == 'clf') {
      p("Note the 'Positive' Class for Senstivity and Specificity")
    }
    })
  
  output$pca_plot <- renderPlot({
    if(input$task=="clf"){
      y <- tr_data()[,input$sel_y]
      X <- tr_data()[,input$sel_x]
      pca_plot(y,X)
    }else{
      return(NULL)
    }
    
  })
  
  values <- reactiveValues()
  
  data <- eventReactive(input$apply, {
    y <- tr_data()[,input$sel_y]
    X <- tr_data()[,input$sel_x]
    df0 <- data.frame(y,X)
    df0 <- df0 %>% tidyr::drop_na()
    #df0 
    train_test_data <- train_test_split(df0,classifn = input$task,(100-input$tr_per)/100)
    train_data <- train_test_data[[1]]
    test_data <- train_test_data[[2]]
    withProgress(message = 'Training in progress. Please wait ...',value = 0.5,
    rf <- randomForest(y ~ ., data=train_data, ntree = input$n_tree, proximity=TRUE,na.action = na.omit))
    p1 <- predict(rf, train_data)
    p2 <- predict(rf, test_data)
    if (input$task == 'clf') {
      train_conf = caret::confusionMatrix(p1, train_data[,1])
      test_conf = caret::confusionMatrix(p2, test_data[,1])
      output$roc <- renderPlot({
        suppressWarnings({
                          plot_roc(rf,test_data$y,test_data[,-1])
        })
                    })
      output$roc_val <- renderPrint({
                        auc_l <- print_roc(rf,test_data$y,test_data[,-1])
                        auc_l
                        })
      return(list(rf,train_conf,test_conf))
    }else{
      rmse_train <- RMSE(p1,train_data[,1])
      rmse_test <- RMSE(p2,test_data[,1])
      return(list(rf,rmse_train,rmse_test))
    }
    
  })
  
  
  corplot1 = renderPlot({
    if (identical(tr_data(), '') || identical(tr_data(),data.frame())) return(NULL)
    if (is.null(input$tr_data)) {return(NULL)}
    else {
    data1 <- na.omit(data.frame(tr_data()[,c(input$sel_y,input$sel_x)]))
    if (nrow(data1)>1000){ samp= sample(1:nrow(data1), 1000 )
    data = data1[samp,]}
    Class = NULL
    for (i in 1:ncol(data)){
      c1 = class(data[,i])
      Class = c(Class, c1)
    }
    nu = which(Class %in% c("numeric","integer"))
    #fa = which(Class %in% c("factor","character"))
    my_data = (data[,nu])
    cor.mat <- round(cor(my_data),3)
    corrplot.mixed(cor.mat, lower.col="black", number.cex=0.7)
    #          type = "upper",    # upper triangular form
    #          order = "hclust",  # ordered by hclust groups
    #          tl.col = "black",  # text label color
    #          tl.srt = 45)  
    }
  })
    
    
  
  output$mod_sum <- renderPrint({
    return(data()[[1]])
  })
  
  output$train_res <- renderUI({
    if(input$task=="clf"){
      plotOutput("conf_train_plot")
    }else{
      return(NULL)
    }
  })
  
  output$test_res <- renderUI({
    if(input$task=="clf"){
      plotOutput('conf_test_plot')
    }else{
      return(NULL)
    }
  })
  
  output$conf_train_plot <- renderPlot({
    fourfoldplot(data()[[2]]$table,
                 color = c("#CC6666", "#99CC99"),
                 conf.level = 0,
                 main="Confusion Matrix (Train Set)")
  })
 
  
  output$conf_test_plot <- renderPlot({
    fourfoldplot(data()[[3]]$table,
                 color = c("#CC6666", "#99CC99"),
                 conf.level = 0,
                 main="Confuison Matrix (Test Set)")
  })
  
  output$conf_train <- renderPrint({
    if (is.null(input$tr_data)) {return(NULL)}
    else {
    if(input$task=="clf"){
      return(data()[[2]])
    }else{
      return(cat("RMSE on Train data is ",data()[[2]]))
    }
    }
  })

  output$conf_test <- renderPrint({
    if (is.null(input$tr_data)) {return(NULL)}
    else {
    if(input$task=="clf"){
      return(data()[[3]])
    }else{
      return(cat("RMSE on Test data is ",data()[[3]]))
    }
    }
  })
  
  #----RF Plot output tab ------#
  output$err_rate <- renderPlot({
    req(data()[[1]])
    plot(data()[[1]],main="Error Rate")
  })
  
  # output$roc <- renderPlot({
  #   if(input$task=="clf"){
  #     data()[[4]]
  #   }else{
  #     return(NULL)
  #   }
  #      
  # })
  # 
  #-----Var Imp Plot ----#
  
  output$n_tree <- renderPlot({
    hist(treesize(data()[[1]]),
         main = "No. of Nodes for the Trees",
         col = "green",xlab="Tree Size")
  })
  
  output$var_imp <- renderPlot({
    varImpPlot(data()[[1]],
               sort = T,
               n.var = length(input$sel_x),
               main = "Variable Importance"
               )
  })
  
  output$var_imp_tb <- DT::renderDataTable({
    if (is.null(input$tr_data)) {return(NULL)}
    else {
    if(input$task=="clf"){
      imp_df = data.frame("Features" = names(importance(data()[[1]])[,1]),
                          "MeanDecreaseGini"=round(importance(data()[[1]])[,1],2))
      a0 = sort(imp_df$MeanDecreaseGini, decreasing = TRUE, index.return=TRUE)$ix
      rownames(imp_df) = NULL
      # names(imp_df) <- c("Features","MeanDecreaseGini")
      imp_df[a0,]
    }else{
      imp_df = data.frame("Features" = names(importance(data()[[1]])[,1]),
                          "Mean_Decrease_Residual_Sum_Of_Sqr"=round(importance(data()[[1]])[,1],2))
      imp_df$Mean_Decrease_Residual_Sum_Of_Sqr <- format(imp_df$Mean_Decrease_Residual_Sum_Of_Sqr,big.mark = ",",scientific=FALSE)
     # a0 = sort(imp_df$Mean_Residual_Sum_Of_Sqr, decreasing = TRUE, index.return=TRUE)$ix
      rownames(imp_df) = NULL
      # names(imp_df) <- c("Features","MeanDecreaseGini")
      imp_df
    }
    }
  
   
  })
  
  # Prediction Tab----#
  out_pred_df <- reactive({ 
    req(test_data())
    pred_data <- test_data()[,input$sel_x]
    p3 = predict(data()[[1]], pred_data)
    if(input$task!="clf"){
      p3 <- round(p3,3)
    }
    
    out_pred_df = data.frame("prediction" = p3, pred_data)
    })# downloadable file. })
  
  output$test_op <- DT::renderDataTable({
       head(out_pred_df(), 25) # display 10 rows of this as HTML tbl
  })
  
  output$download_pred <- downloadHandler(
    filename = function() { "predictions.csv" },
    content = function(file) {
      write.csv(out_pred_df(), file,row.names=FALSE)
    }
  )
  
}