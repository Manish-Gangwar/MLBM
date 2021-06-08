#################################################
#      Binary App                      #
#################################################
if(!require("shiny")) {install.packages("shiny")}
if(!require("pastecs")){install.packages("pastecs")}
if(!require("RColorBrewer")){install.packages("RColorBrewer")}
if(!require("Hmisc")){install.packages("Hmisc")}
if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("reshape2")){install.packages("reshape2")}
if (!require("corrplot")) {install.packages("corrplot")}
if (!require("ROCR")) {install.packages("ROCR")}
if (!require("caret")) {install.packages("caret")}
if (!require("Rfast")) {install.packages("Rfast")}
if (!require("e1071")) {install.packages("e1071")}
if (!require("dplyr")) {install.packages("dplyr")}
if (!require("fastDummies")) {install.packages("fastDummies")}
if (!require("mice")) {install.packages("mice")}
if (!require("shinycssloaders")) {install.packages("shinycssloaders")};  
if(!require("DescTools")) {install.packages("DescTools")}

library(shinycssloaders)
library(shiny)
library(e1071)
library(pastecs)
library(RColorBrewer)
library(Hmisc)
library(ggplot2)
library(reshape2)
library(corrplot)
library(ROCR)
library(caret)
library(Rfast)
library(dplyr)
library(fastDummies)
library(mice)
library(DescTools)

shinyServer(function(input, output,session) {
  
Datasetf0 <- reactive({
  if (is.null(input$file)) { return(NULL) }
  else{
    Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
    for (i in 1:ncol(Dataset)){  if (class(Dataset[,i])==c("character")) {Dataset[,i]=factor(Dataset[,i])}  }
    return(Dataset)
  }
})

output$readdata <- renderDataTable({
  if (is.null(input$file)) {return(NULL)}
  else {
    Datasetf0()
  }
}, options = list(lengthMenu = c(5, 30, 50, 100), pageLength = 5))

pred.readdata <- reactive({
  if (is.null(input$filep)) { return(NULL) }
  else{
    readdata <- as.data.frame(read.csv(input$filep$datapath ,header=TRUE, sep = ","))
    return(readdata)
  }
})

output$readdatap <- renderDataTable({
  if (is.null(input$file)) {return(NULL)}
  else {
    pred.readdata()
  }
}, options = list(lengthMenu = c(5, 30, 50,100), pageLength = 5))

Dataset.temp = reactive({
  mydata = Datasetf0()#[,c(input$yAttr,input$xAttr)]
  return(mydata)
})

nu.Dataset = reactive({
  if (is.null(input$file)) {return(NULL)}
  else {
  data = Dataset.temp()
  Class = NULL
  for (i in 1:ncol(data)){
    c1 = class(data[,i])
    Class = c(Class, c1)
  }
  nu = which(Class %in% c("numeric","integer"))
  nu.data = data[,nu] 
  return(nu.data)}
})

num.Dataset = reactive({
  data = nu.Dataset()
  Class = NULL
  for (i in 1:ncol(data)){
    c1 = class(data[,i])
    Class = c(Class, c1)
  }
  nu = which(Class %in% c("numeric"))
  num.data = data[,nu] 
  return(num.data)
})

# Select variables:
output$yvarselect <- renderUI({
  if (is.null(input$file)) {return(NULL)}
 # if (identical(Datasetf(), '') || identical(Datasetf(),data.frame())) return(NULL)
  else {
  selectInput("yAttr", "Select Y variable (categorical)", 
              setdiff(colnames(Dataset.temp()),colnames(num.Dataset())), 
              setdiff(colnames(Dataset.temp()),colnames(nu.Dataset())))
  }
})

output$xvarselect <- renderUI({
#  if (identical(Datasetf(), '') || identical(Datasetf(),data.frame())) return(NULL)
  if (is.null(input$file)) {return(NULL)}
  else {
  checkboxGroupInput("xAttr", "Select X variables",
                     setdiff(colnames(Dataset.temp()),input$yAttr), 
                     setdiff(colnames(Dataset.temp()[,-1]),input$yAttr))
  }
})

output$fxvarselect <- renderUI({
#  if (identical(Datasetf(), '') || identical(Datasetf(),data.frame())) return(NULL)
  if (is.null(input$file)) {return(NULL)}
  else {
  checkboxGroupInput("fxAttr", "Select factor (categorical) variables in X",
                #     setdiff(colnames(Dataset.temp()),input$yAttr),"" )
                setdiff(colnames(Dataset.temp()[,c(input$xAttr)]), colnames(num.Dataset())),    
                setdiff(colnames(Dataset.temp()[,c(input$xAttr)]), colnames(nu.Dataset())) )
  }
})


output$fxvarselect1 <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  else {
    selcol=setdiff(input$xAttr,colnames(num.Dataset()))
    pickcol=setdiff(input$xAttr,c(colnames(nu.Dataset())))
    varSelectInput("fxAttr",label = "Select factor (categorical) variables in X",
                   data = Dataset.temp()[,selcol], multiple = TRUE, selectize = TRUE, selected = pickcol  )
  }
})

Datasetf = reactive({
  mydata = Dataset.temp()#[,c(input$yAttr,input$xAttr)]
  return(mydata)
})

basealt <- reactive({
  if (is.null(input$file)) {return(NULL)}
  else {
    basealt = (as.data.frame(t(as.matrix(table(factor(Datasetf()[,input$yAttr]) )))))
    #funname = function(x) {gsub(" ", ".",x)}; Datasetf()[,input$yAttr] = sapply(Datasetf()[,input$yAttr], funname)
    funname = function(x) {gsub(" ", ".",x)}; names(basealt) = sapply(names(basealt), funname)
    funname = function(x) {gsub("-", ".",x)}; names(basealt) = sapply(names(basealt), funname)
    #funname = function(x) {gsub("_", ".",x)}; names(basealt) = sapply(names(basealt), funname)
    #basealt = (as.data.frame(t(as.matrix(table(Datasetf()[,input$yAttr])))))
   # for (j in 1:length(basealt)){ names(basealt)[j] = sub(" ", ".", names(basealt)[j])  }
  return(basealt)}
})

#select base alternative
output$BaseAlternativeselect <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  else {
    #if (identical(base(), '') || identical(base(),data.frame())) return(NULL)
    #y=t(as.matrix(table(Dataset()$AlternativesAttr))) 
    selectInput("BaseAlternative", "Set positive class (Y=1) equal to", # choices = colnames(basealt()) )
               # selected = colnames(basealt())[2],multiple = FALSE)
                colnames(basealt()), colnames(basealt())[2] )
  }
})

output$yout <- renderUI({
  if (identical(Datasetf(), '') || identical(Datasetf(),data.frame())) return(NULL)
  p("Model predicts log-odds of",input$yAttr,"=",input$BaseAlternative,style="color:red")
})

output$yout1 <- renderUI({
  if (identical(Datasetf(), '') || identical(Datasetf(),data.frame())) return(NULL)
  p(" 'Postive' Class : 1 is",input$yAttr,"=",input$BaseAlternative,style="color:red")
})

output$yout2 <- renderUI({
  if (identical(Datasetf(), '') || identical(Datasetf(),data.frame())) return(NULL)
  p("Model predicts probability of",input$yAttr,"=",input$BaseAlternative,style="color:red")
})

output$yout3 <- renderUI({
  if (identical(Datasetf(), '') || identical(Datasetf(),data.frame())) return(NULL)
  p("Model predicts probability of",input$yAttr,"=", input$BaseAlternative,style="color:red")
})

filtered_dataset11 <- reactive({if (is.null(input$file)) { return(NULL) }
  else{
    #Dataset <- Datasetf() %>% dplyr::select(!!!input$fxAttr)
    Dataset = Datasetf()[,input$fxAttr]
    return(Dataset)
  }})

Datasetf1 = reactive({
  ydata = factor(Datasetf()[,c(input$yAttr)])
  Y=as.data.frame(dummy_cols(ydata))[,-1]
  names(Y) = colnames(basealt())
  Y1=factor(Y[,input$BaseAlternative])
 
  mydata=cbind(Y1, Datasetf()[,c(input$yAttr,input$xAttr)])
  names(mydata)[1]=paste0(input$yAttr,"_",input$BaseAlternative)
  
  fxAttr = input$fxAttr
  #fxAttr = colnames(filtered_dataset11())
  if (length(fxAttr) >= 1){
  for (j in 1:length(fxAttr)){
      mydata[,fxAttr[j]] = factor(mydata[,fxAttr[j]])
  }}
  mydata[,input$yAttr] = factor(mydata[,input$yAttr])
  return(mydata)
  
})


output$samsel <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  else {
    selectInput("obs", "Select sub sample", c("quick run, random 2,000 obs", "random 10,000 obs", "full dataset"), 
                selected = "quick run, random 2,000 obs")
  }
})

Datasetf2 <- reactive({
  if (is.null(input$file)) {return(NULL)}
  if (input$obs=="full dataset") { return(Datasetf1()) }
  else if(input$obs=="random 10,000 obs") 
  {
    if (nrow(Datasetf1())>10000){
      set.seed(1234)
      testsample= sample(1:nrow(Datasetf1()), 10000 )
      Dataset1=Datasetf1()[testsample,]
      return(Dataset1)}
    else {return(Datasetf1())}
  }
  else (input$obs=="quick run, random 2,000 obs")
  {
    if (nrow(Datasetf1())>2000){
      set.seed(1234)
      testsample= sample(1:nrow(Datasetf1()), 2000 )
      Dataset1=Datasetf1()[testsample,]
      return(Dataset1)}
    else {return(Datasetf1())}
  }  
})

output$imputemiss <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  else {
    selectInput("imputemiss", "Impute missing values or drop missing value rows", 
                c("do not impute or drop rows", "impute missing values", "drop missing value rows"), 
                selected = "do not impute or drop rows")
  }
})

mydata = reactive({
  if (is.null(input$imputemiss)) {return(Datasetf2())}
  else {
    if (input$imputemiss == "do not impute or drop rows") 
    { mydataimp=Datasetf2() }
    else if (input$imputemiss == "impute missing values") 
    { mydata = Datasetf2()
    mice_mod = mice(mydata, printFlag=FALSE)
    mydataimp <- complete(mice_mod) }
    else # (input$imputemiss == "drop missing value rows") 
    { mydata = Datasetf2()
    mydataimp = na.omit(mydata)  }
    
    #mydataimp[,input$yAttr] = factor(mydataimp[,input$yAttr])
    return(mydataimp) 
  }
})

output$imout <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  if (input$imputemiss == "do not impute or drop rows") {
    p("Note: to impute or drop missing values (if any) check options in the panel on the left.",style="color:black")}
  else if ((input$imputemiss == "impute missing values")) {
    p("Note: missing values imputed (if any) check options in the panel on the left.",style="color:black")
  }
  else { p("Note: missing value rows dropped (if any) check options in the panel on the left.",style="color:black") }
})

Dataset.Predict <- reactive({
#  fxc = setdiff(input$fxAttr, input$yAttr)
  fxc=input$fxAttr
 # fxc = colnames(filtered_dataset11())
  mydata = pred.readdata()
  
  if (length(fxc) >= 1){
    for (j in 1:length(fxc)){
      mydata[,fxc[j]] = as.factor(mydata[,fxc[j]])
    }
  }
  return(mydata)
})

output$inputobs = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    dim(mydata())
  }
})

output$newobs = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    dim(Dataset.Predict())
  }
})

if(!require("descriptr")) {install.packages("descriptr")}
library(descriptr)
output$screen_summary <- renderPrint({
  if (is.null(input$file)) {return(NULL)}
#  else {  ds_screener(mydata())}
  else {  str(mydata())} 
})

out = reactive({
data = mydata()
Missing1=(data[!complete.cases(data),])
Missing=head(Missing1)
mscount=nrow(Missing1)
Dimensions = dim(data)
Head = head(data)
Tail = tail(data)
Class = NULL
for (i in 1:ncol(data)){
  c1 = class(data[,i])
  Class = c(Class, c1)
}

nu = which(Class %in% c("numeric","integer"))
fa = which(Class %in% c("factor","character"))
nu.data = data[,nu]; 
#nu.data = nu.data[,-1]
factor.data = data[,fa] 
Summary = list(Factor.data = describe(factor.data),Numeric.data = round(stat.desc(nu.data)[c(4,5,6,8,9,12,13),] ,3))
# Summary = list(Numeric.data = round(stat.desc(nu.data)[c(4,5,6,8,9,12,13),] ,4), factor.data = describe(fa.data))

a = seq(from = 0, to=200,by = 4)
j = length(which(a < ncol(nu.data)))
out = list(Dimensions = Dimensions, Summary =Summary, Tail=Tail, factor.data, nu.data, 
           a, j, Head=Head, MissingDataRows=Missing, missing.data.rows.count=mscount)
return(out)
})

output$misswarn <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  else { 
    if (out()[[10]]>0) { 
  p('remove missing data variable(s) if any, or impute or drop rows - check "Data Summary" tab',style="color:red")
  }}
})

output$misswarn1 <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  else { 
    if (out()[[10]]>0) { 
      p('remove missing data variable(s) if any, or impute or drop rows - check "Data Summary" tab',style="color:red")
    }}
})

output$head = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    out()[8]
  }
})

output$tail = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    out()[3]
  }
})

output$ontr = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
  list(Input_Data_Dimension=out()[[1]],Missing_Data_Rows=out()[[10]])
  }
}) #verbatimTextOutput("ontr"),

output$missing = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    out()[9]
  }
})

output$summary = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    out()[1:2]
      }
})

output$hist = renderPlot({
  if (is.null(input$file)) {return(NULL)}
  else {
    hist(mydata())#[,input$xAttr],main=paste("Histogram of" ,input$xAttr), xlab=input$xAttr)
  }
})

output$dens = renderPlot({
  if (is.null(input$file)) {return(NULL)}
  else {
    datadensity(out()[[5]])#[,input$xAttr],main=paste("Histogram of" ,input$xAttr), xlab=input$xAttr)
  }
})

output$heatmap = renderPlot({ 
    qplot(x=Var1, y=Var2, data=melt(cor(out()[[5]], use = "pairwise.complete.obs")), fill=value, geom="tile") +
    scale_fill_gradient2(limits=c(-1, 1))
})

plotsample =  reactive({
  sample(1:nrow(mydata()), round( if (nrow(mydata()>100)) {100} else {nrow(mydata())}  ))
})

plot_data = reactive({
  my_data = out()[[5]]
  my_data[plotsample(),]
})


ols = reactive({
    rhs = paste(input$xAttr, collapse = "+")
    formula= as.formula(paste0(input$yAttr,"_",input$BaseAlternative,"~", rhs))
    ols = glm(formula, data = mydata(), family=binomial)
  return(ols)
})

output$olssummary = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
  summary(ols())
  }
  })

output$mscount = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    out()[10]
  }
})

output$correlation = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    round(cor(out()[[5]], use = "pairwise.complete.obs"),4)
  }
})

plot_data1 <- reactive({
  if (is.null(input$file)) {return(NULL)}
  else {
    my_data = out()[[5]]
    #my_data = mydata()
    #if (input$obs == "full dataset") { return(my_data) }
    #set.seed(1234)
    if (nrow(my_data)>1000){ testsample= sample(1:nrow(my_data), 1000 )
    Dataset1=my_data[testsample,]
    return(Winsorize(Dataset1,na.rm = TRUE))}
    else {return((my_data))}
  }
})

output$corplot = renderPlot({
  if (is.null(input$file)) {return(NULL)}
  else {
    #pairs(mydata())
    pairs(plot_data1(),pch=20, col="grey")
  }
})

output$corplot1 = renderPlot({
  if (is.null(input$file)) {return(NULL)}
  else {
    my_data = out()[[5]]
    cor.mat <- round(cor(my_data),2)
    corrplot(cor.mat, 
             type = "upper",    # upper triangular form
             order = "hclust",  # ordered by hclust groups
             tl.col = "black",  # text label color
             tl.srt = 45)  
  }
})

output$datatable = renderTable({
  if (is.null(input$file)) {return(NULL)}
  else {
  Y.log.odds = ols()$fitted.values
  data.frame(Y.log.odds,mydata())
  }
}, options = list(lengthMenu = c(10, 20, 50), pageLength = 10)  )

output$validation = renderPrint({
  if (is.null(input$file)) {return(NULL)}
    y_actual = mydata()[,1]
    y_predicted = as.integer(ols()$fitted.values>input$cutoff)
    confusion_matrix = table(y_predicted,y_actual)
    accuracy = (sum(diag(confusion_matrix))/sum(confusion_matrix))
    out = list(Confusion_matrix = confusion_matrix, Accuracy_Hit_Rate = accuracy)
    out
})

output$confusionmatrix = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
  data.fit = as.integer(ols()$fitted.values<input$cutoff)
  val = predict(ols(),newdata=mydata(), type='response')
  data.fit = as.integer(val>input$cutoff)
  #data.fit = predict(ols(),newdata=mydata(), type='response')
  yfu=mydata()[,input$yAttr]
  funname = function(x) {gsub(" ", ".",x)}; yfu = sapply(yfu, funname)
  funname = function(x) {gsub("-", ".",x)}; yfu = sapply(yfu, funname)
  #funname = function(x) {gsub("_", ".",x)}; yfu = sapply(yfu, funname)
  
  data.act = as.integer(yfu==input$BaseAlternative)
  Confusion_Matrix = caret::confusionMatrix(as.factor(data.fit),as.factor(data.act), positive="1")
  #out=list(Sum_Specificity_Senstivity=Confusion_Matrix['Senstivity']+Confusion_Matrix['Specificity'], Confusion_Matrix=Confusion_Matrix)
  Confusion_Matrix  
  }
})

output$confusionmatrix1 = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  if ((input$yAttr %in%  colnames(Dataset.Predict()) ) ) {
    nuo=nrow(Dataset.Predict()[,input$yAttr]==input$BaseAlternative)
    val = predict(ols(),newdata=Dataset.Predict(), type='response')
    #ey=round(exp(val)/(1+exp(val)),4)
    data.fit = as.integer(val>input$cutoff)
    yfu=Dataset.Predict()[,input$yAttr]
    funname = function(x) {gsub(" ", ".",x)}; yfu = sapply(yfu, funname)
    funname = function(x) {gsub("-", ".",x)}; yfu = sapply(yfu, funname)
    #funname = function(x) {gsub("_", ".",x)}; yfu = sapply(yfu, funname)
    data.act = as.integer(yfu==input$BaseAlternative)
    Confusion_Matrix = caret::confusionMatrix(as.factor(data.fit),as.factor(data.act), positive="1")
    out=list(confusion_matrix_of_new_data=Confusion_Matrix)
    return(out)  
  }
})

output$roc = renderPlot({ 
  if (is.null(input$file)) {return(NULL)}
  else {
  pred.val = predict(ols(),mydata(),type="response")
  pred.lm = ROCR::prediction(pred.val, mydata()[,1])
  perf.lm = performance(pred.lm,"tpr", "fpr")
  #roc_graph<-cbind(perf.lm@x.values[[1]],perf.lm@y.values[[1]],perf.lm@alpha.values[[1]]);
  #write.csv(roc_graph, file="roc_graph1.csv")
  auc_ROCR = performance(pred.lm, measure = "auc")
  plot(perf.lm, main = c("AUC", auc_ROCR@y.values[[1]]), 
       xlab="False Positive Rate (1-Specificity)", ylab="True Positive Rate (Sensitivity)" )
  #lines(x = c(0,1), y = c(0,1))
  abline(a=0,b=1)
 #abline(a=1,b=-1)
  }
})


prediction = reactive({
  val = predict(ols(),newdata=Dataset.Predict(), type='response')
  #ey=round(exp(val)/(1+exp(val)),4)
  out = data.frame( obs.id=rownames(Dataset.Predict()), Y.log.odds = round(val,4), Dataset.Predict())
  names(out)[2] = paste0("Prob.",input$yAttr,".",input$BaseAlternative)
  return(out)
})
output$prediction =  renderPrint({
  if (is.null(input$filep)) {return(NULL)}
  head(prediction(),10)
})

output$prediction <- renderDataTable({
  if (is.null(input$filep)) {return(NULL)}
  else {
    prediction()
  }
}, options = list(lengthMenu = c(5, 30, 50,100), pageLength = 30))


inputprediction = reactive({
  val = predict(ols(),newdata=mydata(), type='response')
  #ey=round(exp(val)/(1+exp(val)),4)
  out = data.frame( obs.id=rownames(mydata()) ,Y.log.odds = round(val,4), mydata())
  names(out)[2] = paste0("Prob.",input$yAttr,".",input$BaseAlternative)
  return(out)
})

output$inputprediction =  renderPrint({
  if (is.null(input$file)) {return(NULL)}
  head(inputprediction(),10)
})

output$inputprediction <- renderDataTable({
  if (is.null(input$file)) {return(NULL)}
  else {
    inputprediction()
  }
}, options = list(lengthMenu = c(5, 30, 50,100), pageLength = 30))

output$resplot3 = renderPlot({
  if (is.null(input$file)) {return(NULL)}
  else {
    xl=paste0("Predicted Probability of ",input$yAttr," = ",input$BaseAlternative)
    yl=paste0(input$yAttr," = ",input$BaseAlternative)
    plot(ols()$fitted.values,mydata()[,1],main="Predicted (in red) vs. Actual", 
         #xlab="Predicted Probability of Y", ylab="Actual Y", 
         xlab=xl,ylab=yl,yaxt="n",
         col=round(ols()$fitted.values>input$cutoff,0)+1  ) 
    axis(2,at=c(1,2),labels=c(0,1))
    #abline(0,1)  
  }
})

dup = reactive({
  dup = which(duplicated(out()[[5]]))
  return(dup)
})

output$dup =  renderPrint({
  length(dup())
})

tsne_df = reactive({
  if (is.null(input$file)) {return(NULL)}
  else {
    if (length(dup())==0) {
      data = out()[[5]]
      tsne_object = Rtsne(as.matrix(data), perplexity = input$perp, num_threads=0, max_iter=input$iter)
    }
    else{
    dup = dup()
    data = out()[[5]]
    tsne_object = Rtsne(as.matrix(data[-dup,]), perplexity = input$perp, num_threads=0, max_iter=input$iter)
    }
    tsne_df1 = as.data.frame(tsne_object$Y) 
    tsne_df = setNames(tsne_df1,c("Dim.1", "Dim.2"))
    return(tsne_df)
  }
  
})

output$resplot4 = renderPlot({
  if (is.null(input$file)) {return(NULL)}
  else {
    
    dup = dup()
    tsne_df=tsne_df()
    if (length(dup())==0) {
    Y_var=mydata()[,input$yAttr]  
    ggplot(aes(x = Dim.1, y = Dim.2), data = tsne_df) +
      geom_point(aes(colour = Y_var), size = 3)
    }
    else{
      Y_var=mydata()[-dup,input$yAttr]
      ggplot(aes(x = Dim.1, y = Dim.2), data = tsne_df) +
        geom_point(aes(colour = Y_var), size = 3)
    }
  }
})

#------------------------------------------------#
output$downloadData1 <- downloadHandler(
  filename = function() { "Predicted Data.csv" },
  content = function(file) {
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    write.csv(predicted(), file, row.names=F, col.names=F)
  }
)
output$downloadData <- downloadHandler(
  filename = function() { "pregnancy.csv" },
  content = function(file) {
    write.csv(read.csv("data/pregnancy.csv"), file, row.names=F, col.names=F)
  }
)
output$downloadData2 <- downloadHandler(
  filename = function() { "Input Data With Prediction.csv" },
  content = function(file) {
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    write.csv(inputpredicted(), file, row.names=F, col.names=F)
  }
)

})

