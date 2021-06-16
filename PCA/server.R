
  if (!require('plotly')){install.packages("plotly")}; library("plotly")
  if (!require('viridis')){install.packages("viridis")}; library("viridis")
  if (!require('markdown')){install.packages("markdown")}; library("markdown")
  if (!require('shiny')){install.packages("shiny")}; library("shiny")
  if (!require('ggplot2')){install.packages("ggplot2")}; library("ggplot2")
  if (!require('DT')){install.packages("DT")}; library("DT")
  if (!require('dplyr')){install.packages("dplyr")}; library("dplyr")
  if (!require('tidyr')){install.packages("tidyr")}; library("tidyr")


#---------Staring Server code--------#

server <- function(input, output,session) {
  #-----Data upload----#
  df_data <- reactive({
    req(input$file)
    Dataset1 = read.csv(input$file$datapath)
    rownames(Dataset1) = Dataset1[,1]
    #Dataset = Dataset1[,2:ncol(Dataset1)]
    return(Dataset1)
  })
  
  #--1.Main Panel O/P
  # 1. dimension
  output$dim <- renderPrint({
    cat("Uploaded dataset has ",dim(df_data())[1],"rows and ",dim(df_data())[2]," columns")
  })
  
  output$yout <- renderPrint({
    if (is.null(input$file)) { return(NULL) }
    p("dropped missing value rows (if any) and removed factor variables from the analysis",style="color:red")
  })
  
  # 2. sample data
  output$samp_data <- DT::renderDataTable({
    (df_data())
  })
  
  nu.Dataset = reactive({
    if (is.null(input$file)) {return(NULL)}
    data = df_data()[,2:ncol(df_data())]
    Class = NULL
    for (i in 1:ncol(data)){
      c1 = class(data[,i])
      Class = c(Class, c1)
    }
    nu = which(Class %in% c("numeric","integer"))
    nu.data = data[,nu] 
    return(nu.data)
  })
  
  output$colList <- renderUI({
    if (is.null(input$file)) {return(NULL)}
    else {
      varSelectInput("selVar",label = "Select only numerical X variables",
                     data = nu.Dataset(),multiple = TRUE,selectize = TRUE,selected = colnames(nu.Dataset()))
    }
  })
  
  # 3. missing plot
  output$miss_plot1 <- renderPlot({
    req(input$file)
    df_data1 <- df_data() %>% dplyr::select(!!!input$selVar)
    Amelia::missmap(df_data1)
  })
  
  if(!require("descriptr")) {install.packages("descriptr")}
  library(descriptr)
  output$miss_plot <- renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {  
      df_data1 <- df_data() %>% dplyr::select(!!!input$selVar)
      ds_screener(  df_data1 )} 
  })
  
  #----build rmse mat -----#
  
  list0 <- reactive({
    suppressWarnings({ 
      df_data1 <- df_data() %>% dplyr::select(!!!input$selVar) %>% drop_na()
      list0 = build_rmse_mat(df_data1) 
      return(list0)
    })
  })
  
  output$summ <- renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {  
      data=df_data() %>% dplyr::select(!!!input$selVar) %>% drop_na()
      str(  data  )} 
  })
  
  output$dimk <- renderUI({
    data=df_data() %>% dplyr::select(!!!input$selVar) %>% drop_na()
    numericInput("k","Select number of components",min = 2,max=dim(data)[2],value=dim(data)[2])
#    cat("final dataset has ",dim(data)[1],"rows and ",dim(data)[2]," columns")
  })
  
  #---PCA Loadings Tab---#
 loadings_dt <- reactive({
    df1 = list0()[[2]]
    loadings <- pca_outputs(df1)[[1]]
    loadings
  })
  
  output$loadings_dt <- DT::renderDataTable({
    loadings_dt()[, 1:input$k]
  })
  
  output$loading_hm <- renderPlotly({
    df1 = list0()[[2]]
    build_pca_loadings_heatmap(df1, k=input$k)
  })
  #----PCA Scores Tab--#
  scores_dt <- reactive({
    df1 = list0()[[2]]
    scores <- pca_outputs(df1)[[2]]
    sc1=data.frame(scores)
    #sc1=data.frame(scores[, 1:input$k])
    #colnames(sc1)[1]="Obs_id"
    return(sc1)
  })
  
  output$scores_dt <- DT::renderDataTable({
    scores_dt()
  })
  
  output$downloadDataX <- downloadHandler(
    filename = function() { "pca_scores.csv" },
    content = function(file) {
      if (is.null(input$file)) { return(NULL) }
      write.csv(scores_dt(), file, row.names = F)
    }
  )
  
  output$bi_plot <- renderPlotly({
    X = list0()[[2]]
    build_display_biplot(X)
  })
  #--- Variance exp Tab---#
  output$var_exp <- renderPlotly({
    X = list0()[[2]]
    varExpl = var_expl(X); # dim(varExpl)
    # now plot-ly the var explained %s
    plot01 = ggplot(data = varExpl, aes(component, compt.var)) + geom_col() + ggtitle("% variance explained by PCA compts")
    fig01 = ggplotly(plot01)
    fig01  # show in Var Explained tab
  })
  
  output$cum_var_exp <- renderPlotly({
    X = list0()[[2]]
    varExpl = var_expl(X); # dim(varExpl)
    plot02 = ggplot(data = varExpl, aes(component, cumul.var)) + 
      geom_col() + 
      ggtitle("% Cumul variance explained by PCA compts")
    fig02 = ggplotly(plot02)
    fig02  # show in Var Explained Tab
  })
  
  
}