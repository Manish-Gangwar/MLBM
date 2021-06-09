# Try to connect to existing cluster. If it does not exist then initialize.
initialize_h20 <- function(x){
  tryCatch({
  h2o.init(startH2O = FALSE)
}, error = function(err) {
  errorStatus <- err[1]$message
  message(paste0(errorStatus,"\n Initializing new H2O cluster..."))
  # Inititialize H2o cluster
  try({h2o.shutdown(prompt = FALSE)}, silent=TRUE)
  h2o.init(ip = 'localhost', port = 54321, nthreads= -1, max_mem_size = '4g')
  return(errorStatus)
}) # END tryCatch
}

h2o_automl <- function(df0, classifn = 'clf', 
                       train_propn_ui=train_propn_ui,max_runtime_secs_ui=30){
  
  # Partition input Data
  set.seed(222)
  ind <- sample(2, nrow(df0), replace = TRUE, 
                prob = c(train_propn_ui, (1 - train_propn_ui))) # from UI
  train <- df0[ind==1,]
  test <- df0[ind==2,]
  
  # build H2O frames
  train_h2o = as.h2o(train)
  test_h2o = as.h2o(test)
  #pred_h2o = as.h2o(pred_data)
  
  y <- "y"  # select DV variable name by user
  x <- setdiff(names(train), y)
  
  'discern classifn vs regn for autoML'
  if (classifn == 'clf'){
    train_h2o[,y] <- as.factor(train_h2o[,y]) 
  }
  
  # Create automl obj by passing the mandatory parms.
  system.time({
    aml <- h2o.automl(x = x, y = y,
                      training_frame = train_h2o,
                      max_runtime_secs = max_runtime_secs_ui,seed=222) # from UI.
  }) # 46 s
  
  return(aml)
}

# Shut down H2O cluster on app exit
onStop(function() {
  try({h2o.shutdown(prompt = FALSE)}, silent=TRUE)
})

#---------Staring Server code--------#

server <- function(input, output,session) {
  progress <- Progress$new(session, min=1, max=10)
  progress$set(value=3,"Initializing h2o")
  initialize_h20()
  progress$set(value=5,"h2o setup done")
  Sys.sleep(1)
  on.exit(progress$close())
  
  tr_data <-  reactive({
    req(input$tr_data$datapath)
    #progress$set(value=2,"reading data")
    df <- read.csv(input$tr_data$datapath,stringsAsFactors = TRUE)
    return(df)
  })
  
  test_data <-  reactive({
    req(input$test_data$datapath)
    df <- read.csv(input$test_data$datapath,stringsAsFactors = TRUE)
    return(df)
  })
  
  tr_cols <- reactive({
    req(input$tr_data$datapath)
    return(colnames(tr_data()))
  })
  
  
  #----Tab-2 Data Summary----#
  
  output$samp <- DT::renderDataTable({
    req(input$tr_data$datapath)
    DT::datatable(tr_data(),
                  #filter = "top"
                  options = list(lengthMenu = list(c(10,25,50,-1),c("5","25","50","All")),
                                 autoWidth = TRUE),
                  caption = "Table 1: Sample of Data"
    )
  })
  
  output$data_str <- renderPrint({
    str(tr_data())
  })
  
  # output$data_summ_tb <- DT::renderDataTable({
  #   req(input$tr_data$datapath)
  #   progress$set(value=4,"generating summary report")
  #   h2o_df <- as.h2o(tr_data())
  #   temp <-  as.data.frame(h2o.describe(h2o_df))
  #   temp <- temp %>% mutate_if(is.numeric, round, digits=3)
  #   on.exit(progress$close())
  #   DT::datatable(temp)
  #   
  # })
  
  output$miss_plot <- renderPlot({
    req(input$tr_data$datapath)
    Amelia::missmap(tr_data())
  })
  
  
  #----------X & Y Sel UI-------------#
  output$y_ui <- renderUI({
    req(input$tr_data$datapath)
    selectInput(inputId = 'sel_y',label = "Select Y (Target Variable)",choices = tr_cols(),multiple = FALSE)
  })
  
  x_col <- reactive({
    req(input$tr_data$datapath)
    x <- match(input$sel_y,tr_cols())
    y_col <- tr_cols()[-x]
    return(y_col)
  })
  
  output$x_ui <- renderUI({
    req(input$tr_data$datapath)
    selectInput(inputId = "sel_x",label="Select X (Features)",choices = x_col(),multiple = TRUE,selected = x_col())
  })
  
  
  # output$pca_plot <- renderPlot({
  #   y <- tr_data()[,input$sel_y]
  #   X <- tr_data()[,input$sel_x]
  #   X <- select_if(X,is.numeric)
  #   pca_plot(y,X)
  # })
  # 

#--- Tab-2 leader board ------#
  output$h2o_flow <- renderUI({
    url <- a("H2o Flow", href="http://localhost:54321/flow/index.html",target="_blank")
    tagList("For more details on trained models go to", url)
  })
  
  model <- eventReactive(input$apply, {
    withProgress(message="Reading data",
                 {
                   
                 
                 y <- tr_data()[,input$sel_y]
                 X <- tr_data()[,input$sel_x]
                 df0 <- data.frame(y,X)
    
    #df0 
    incProgress(message = 'Training in progress. Please wait ...',amount = 0.3)
                 mod_list <- h2o_automl(df0 = df0,
                                        classifn = input$task,
                                        train_propn_ui = input$tr_per,
                                        max_runtime_secs_ui = input$max_tr_time
                                        )
    incProgress(message = 'Models Trained',amount = 0.8)
                 }
    )
    
    return(mod_list)
   
  })
  
  output$lb_table <- DT::renderDataTable({
    lb <- model()@leaderboard; lb
    lb_df <- as.data.frame(lb)
    lb_df <- lb_df %>% dplyr::mutate_if(is.numeric, round, digits=3)
    DT::datatable(lb_df)  # show as HTML table
    
  })
  
  output$lb_auc_plot <- renderPlot({
    lb <- model()@leaderboard; lb
    lb_df = as.data.frame(lb)
    if (input$task == 'clf'){
      if (length(unique(tr_data()[,input$sel_y])) > 2){
        plot1 = ggplot(data=lb_df, aes(seq(1:nrow(lb_df)), mean_per_class_error)) + 
          geom_line(colour = 'red') + 
          geom_point(colour = "blue") + 
          xlab("model number in leaderboard")
      }else{
        plot1 = ggplot(data=lb_df, aes(seq(1:nrow(lb_df)), auc)) + 
          geom_line(colour = 'red') + 
          geom_point(colour = "blue") + 
          xlab("model number in leaderboard")
      }
     
    }else {
      plot1 = ggplot(data=lb_df, aes(seq(1:nrow(lb_df)), rmse)) + 
        geom_line(colour = 'red') + 
        geom_point(colour = "blue") + 
        xlab("model number in leaderboard")
    }
    plot1
  })
  
  test_predict <- reactive(
    {
      req(model())
      pred <- predict(model()[[1]],model()[[2]][,input$sel_x])
      pred
    }
  )
  output$conf_test_plot <- renderPlot({
    cnf <- confusionMatrix(test_predict(),model()[[2]]$y)
    fourfoldplot(cnf$table,
                 color = c("#CC6666", "#99CC99"),
                 conf.level = 0,
                 main="")
  })
  
  # output$conf_train <- renderPrint({
  #   data()[[2]]
  # })
  # 
  output$conf_test <- renderPrint({
    model()[[1]]
  })
  
  #----Best Model output tab ------#
output$top_model <- renderPrint({
  best_model = model()@leader
  cat("==== +++++++ Auto-ML recommended top model is: +++++++ ====\n\n")
  best_model
})

output$ith_model <- renderUI({
  req(model())
  lb <- model()@leaderboard
  lb_df = as.data.frame(lb)
  max_i <- dim(lb_df)[1]
  numericInput("ith", "Select i^th model from the leaderboard", value = 1, min = 1, max = max_i)
})


output$ith_model_op <- renderPrint({
  req(model())
  i <- as.numeric(input$ith)
  lb <- model()@leaderboard
  oth_model = h2o.getModel(as.vector(lb$model_id[i]))
  cat("===== Auto-ML recommended", i,"th model is: =====\n\n")
  oth_model
})

  # Prediction Tab----#
  out_pred_df <- reactive({
    req(test_data())
    best_model = model()@leader
    pred_data <- test_data()[,input$sel_x]
    pred_h2o <- as.h2o(pred_data)
    pred_out = h2o.predict(best_model, pred_h2o)
    pred_out_df = as.data.frame(pred_out)
    pred_out_df <- pred_out_df %>% mutate_if(is.numeric, round, digits=3)
    
  })# downloadable file. })
  # 
  output$test_op <- DT::renderDataTable({
    head(out_pred_df(), 25) # display 10 rows of this as HTML tbl
  })
  # 
  output$download_pred <- downloadHandler(
    filename = function() { "predictions.csv" },
    content = function(file) {
      write.csv(out_pred_df(), file,row.names=FALSE)
    }
  )
  # 
  
  # code to run while closing shiny session
  # session$onSessionEnded(function() {
  #   h2o.shutdown()
  #})
}