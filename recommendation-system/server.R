if (!require(shinyWidgets)) {install.packages("shinyWidgets")};  library(shinyWidgets)
if (!require(shiny)) {install.packages("shiny")};  library(shiny)
if (!require(quanteda)) {install.packages("quanteda")};  library(quanteda)
if (!require(ggplot2)) {install.packages("ggplot2")};  library(ggplot2)
if (!require(magrittr)) {install.packages("magrittr")};  library(magrittr)
if (!require(text2vec)) {install.packages("text2vec")};  library(text2vec)
if(!require("shinyBS")) {install.packages("shinyBS")}; library(shinyBS)
if(!require("DT")) {install.packages("DT")}; library(DT)
if (!require("shinycssloaders")) {install.packages("shinycssloaders")}; library(shinycssloaders)

shinyServer(function(input, output,session) {
  

#-------Data Upload---------
dataset <- reactive({
    if (is.null(input$file)) {return(NULL)}
    else {
      Document = read.csv(input$file$datapath,header = TRUE)
      rownames(Document) = Document[,1]
      Document = Document[,2:ncol(Document)]
      return(Document)
      }
  })
  
#----Selecting Focal User-------  
output$focal_list <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  else{
    
    users_list <- rownames(dataset())
    pickerInput(
      inputId = "user",
      label = "Select Focal User", 
      choices = users_list,
      options = list(`live-search` = TRUE)
    )
  }
})

output$yout <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  else{
    (p(paste0("recommendation for - ",input$user),style="color:red"))
  }
}) 
output$yout1 <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  else{
    (p(paste0("recommendation for - ",input$user),style="color:red"))
  }
})
output$yout2 <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  else{
    (p(paste0("similar users like - ",input$user),style="color:red"))
  }
})
  
#-----Descriptive Tab-----

output$dim <- renderText({
  if (is.null(input$file)) {return(NULL)}
  else{
    size <- dim(dataset())
    return(paste0("uploaded dataset has ",size[1]," users (rows) "," X ",size[2]," items (columns)"))
  }
})  
  

output$dtm_head <- renderDataTable({
  return(dataset())
},options = list(pageLength = 10))


output$freq_table <- renderDataTable({
  if (is.null(input$file)) {return(NULL)}
  else{
    dtm <- dataset()
   
    
    a0 = colSums(dtm)
    a1 = sort(a0, decreasing=TRUE, index.return=TRUE)
    a2 = as.matrix(a0[a1$ix])
    token_freqs = data.frame(freq = a2)
    token_freqs$item = rownames(token_freqs)
    # reorder by column name
    token_freqs <- token_freqs[c("item", "freq")]  #return(as.data.frame(head(token_freqs, 10))) # 2nd output. Sorted freqs
    rownames(token_freqs) <- NULL
    (token_freqs)

  }
},options = list(pageLength = 10))
#----------IBFC Recommendation-----------
  
  output$ibfc_re <- DT::renderDataTable({
    if (is.null(input$file)) {return(NULL)}
    else{
      system.time({ CF.list = dtm2CF(dataset(), input$user, 12) })
      #CF.list = dtm2CF(dataset(), input$user, 12)
      ibcf.brands = CF.list[[1]]
      DT::datatable(ibcf.brands, options = list(pageLength = 10))
    }
   
  })
  
output$ubfc_re <- DT::renderDataTable({
  if (is.null(input$file)) {return(NULL)}
  else{
    system.time({ CF.list = dtm2CF(dataset(), input$user, 12) })
    #CF.list = dtm2CF(dataset(), input$user, 12)
    ibcf.brands = CF.list[[2]]
    DT::datatable(ibcf.brands, options = list(pageLength = 10))
  }

})

output$sim_usr <- DT::renderDataTable({
  if (is.null(input$file)) {return(NULL)}
  else{
    system.time({ CF.list = dtm2CF(dataset(), input$user, 12) })
    #CF.list = dtm2CF(dataset(), input$user, 12)
    simil.users = CF.list[[3]]
    DT::datatable(simil.users, options = list(pageLength = 10))
  }
  
})




output$downloadData1 <- downloadHandler(
  filename = function() { "recommendation_system_input.csv" },
  content = function(file) {
    write.csv(read.csv("data/B2C brands pgp21_dtm.csv"), file,row.names = FALSE)
  }
)

  
})


  
  
  
  
  
