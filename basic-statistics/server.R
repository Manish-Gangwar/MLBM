#################################################
#      Data Stats                      #
#################################################
if(!require("shiny")) {install.packages("shiny")}
if(!require("pastecs")){install.packages("pastecs")}
if(!require("RColorBrewer")){install.packages("RColorBrewer")}
if(!require("Hmisc")){install.packages("Hmisc")}
if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("reshape2")){install.packages("reshape2")}
if(!require("corrplot")) {install.packages("corrplot")}
#if(!require("PerformanceAnalytics")) {install.packages("PerformanceAnalytics")}
if(!require("EnvStats")) {install.packages("EnvStats")}
if(!require("fastDummies")) {install.packages("fastDummies")}
if(!require("magrittr")) {install.packages("magrittr")} 
if(!require("dplyr")) {install.packages("dplyr")}
if(!require("tidyr")) {install.packages("tidyr")}
if(!require("ggpubr")) {install.packages("ggpubr")}
if(!require("mice")) {install.packages("mice")}
if(!require("DescTools")) {install.packages("DescTools")}
if(!require("descriptr")) {install.packages("descriptr")}
if (!require("shinycssloaders")) {install.packages("shinycssloaders")}; library(shinycssloaders)
if(!require("shinyBS")) {install.packages("shinyBS")}


library(tidyr)
library(shiny)
library(pastecs)
library(RColorBrewer)
library(Hmisc)
library(ggplot2)
library(reshape2)
library(corrplot)
#library(PerformanceAnalytics)
library(EnvStats)
library(fastDummies)
library(magrittr)
library(dplyr)
library(ggpubr)
library(mice)
library(DescTools)#Winsorize
library(descriptr)

# library(gplot)

shinyServer(function(input, output,session) {
  
Datasetf <- reactive({
  if (is.null(input$file)) { return(NULL) }
  else{
    Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
    #for (i in 1:ncol(Dataset)){if (class(Dataset[,i])==c("character")) {Dataset[,i]=factor(Dataset[,i])}}
    return(Dataset)
  }
})
  
output$readdata <- renderDataTable({
  if (is.null(input$file)) {return(NULL)}
  else {
    Datasetf()
  }
}, options = list(lengthMenu = c(5, 30, 50,100), pageLength = 5))

output$samsel <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  else {
    selectInput("obs", "Select sub sample", c("quick run, 2,000 random obs", "10,000 random obs", "full dataset"), 
                selected = "quick run, 2,000 random obs")
  }
})

Dataset <- reactive({
  if (is.null(input$file)) {return(NULL)}
  else {
  if (is.null(input$obs)) {return(Datasetf())}  
  else {
    if (input$obs=="full dataset") { return(Datasetf()) }
    else if(input$obs=="10,000 random obs") 
    {
      if (nrow(Datasetf())>10000){
        set.seed(1234)
        testsample= sample(1:nrow(Datasetf()), 10000 )
        Dataset1=Datasetf()[testsample,]
        return(Dataset1)}
      else {return(Datasetf())}
    }
    else #if(input$obs=="quick run, 1,000 random obs")
    {
      if (nrow(Datasetf())>2000){
        set.seed(1234)
        testsample= sample(1:nrow(Datasetf()), 2000 )
        Dataset1=Datasetf()[testsample,]
        return(Dataset1)}
      else {return(Datasetf())}
    }  
    }}
})

output$xvarselect <- renderUI({
  #if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
  if (is.null(input$file)) {return(NULL)}
  else {
  checkboxGroupInput("xAttr", "Select variables",
                     colnames(Dataset()), colnames(Dataset()))
  }
})

Dataset.temp = reactive({
  if (is.null(input$file)) { return(NULL) }
  #if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
  mydata = Dataset()[,c(input$xAttr)]
})

nu.Dataset = reactive({
  if (is.null(input$file)) {return(NULL)}
  else {
  data = Dataset.temp()
  Class = NULL
  for (i in 1:ncol(data)){
    c1 = (class(data[,i]))
    Class = c(Class, c1)
  }
  nu = which(Class %in% c("numeric","integer"))
  nu.data = data[,nu] 
  return(nu.data)
  }
})

num.Dataset = reactive({
  if (is.null(input$file)) {return(NULL)}
  else {
    data = Dataset.temp()
    Class = NULL
    for (i in 1:ncol(data)){
      c1 = (class(data[,i]))
      Class = c(Class, c1)
    }
    num = which(Class %in% c("numeric","integer"))
    num.data = data[,num] 
    return(num.data)
  }
})

chr.Dataset = reactive({
  if (is.null(input$file)) {return(NULL)}
  else {
    data = Dataset.temp()
    Class = NULL
    for (i in 1:ncol(data)){
      c1 = (class(data[,i]))
      Class = c(Class, c1)
    }
    chr = which(Class %in% c("character","factor"))
    chr.data = data[,chr] 
    return(chr.data)
  }
})

output$fxvarselect <- renderUI({
  if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
  if (is.null(input$file)) {return(NULL)}
  else {
  checkboxGroupInput("fxAttr", "Convert to factor (categorical) variable",
                    #colnames(Dataset.temp()) )
                    colnames(Dataset.temp()), colnames(chr.Dataset()))#setdiff(colnames(Dataset.temp()),c(colnames(nu.Dataset()))) )
  }
})

output$dxvarselect1 <- renderUI({
  if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
  if (is.null(input$file)) {return(NULL)}
  else {
    checkboxGroupInput("dxAttr", "Select factor variable(s) to create dummies",
                       #colnames(Dataset.temp()) )
                       colnames(Dataset.temp())[,input$fxAttr],"") #setdiff(colnames(Dataset.temp()),c(colnames(nu.Dataset()))) )
  }
})

output$dxvarselect <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  else {
    data=nu.Dataset()
    data1 = data[, !names(data) %in% input$fxAttr]
    varSelectInput("dxAttr", "Select factor variable(s) to create dummies",
                   data = Dataset.temp()[,input$fxAttr],multiple = TRUE, selected = ""  )
  }
})

output$lxvarselect1 <- renderUI({
  if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
  if (is.null(input$file)) {return(NULL)}
  else {
    checkboxGroupInput("lxAttr", "Select X variable(s) for natural log transformation",
                       #colnames(Dataset.temp()) )
                       setdiff(colnames(nu.Dataset()),input$fxAttr), "" )
  }
})



output$imputemiss <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  else {
  if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    selectInput("imputemiss", "Impute missing values or drop missing value rows", 
                c("do not impute or drop rows", "impute missing values", "drop missing value rows"), 
                selected = "do not impute or drop rows")
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

output$imout1 <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  if (input$imputemiss == "do not impute or drop rows") {
    p("Note: to impute or drop missing values (if any) check options in the panel on the left.",style="color:black")}
  else if ((input$imputemiss == "impute missing values")) {
    p("Note: missing values imputed (if any) check options in the panel on the left.",style="color:black")
  }
  else { p("Note: missing value rows dropped (if any) check options in the panel on the left.",style="color:black") }
})

output$imout2 <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  if (input$imputemiss == "do not impute or drop rows") {
    p("Note: to impute or drop missing values (if any) check options in the panel on the left.",style="color:black")}
  else if ((input$imputemiss == "impute missing values")) {
    p("Note: missing values imputed (if any) check options in the panel on the left.",style="color:black")
  }
  else { p("Note: missing value rows dropped (if any) check options in the panel on the left.",style="color:black") }
})

mydata2 = reactive({
  mydata=Dataset()[,input$xAttr]
  if (input$imputemiss == "do not impute or drop rows") 
    {mydataimp=mydata}
  else if (input$imputemiss == "impute missing values") 
    { mice_mod = mice(mydata, printFlag=FALSE)
  mydataimp <- complete(mice_mod) }
  else # (input$imputemiss == "drop missing value rows") 
  { mydataimp = na.omit(mydata)  }
  return(mydataimp)
})


output$winsor <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  else {
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    selectInput("winsor", "Winsorize extreme values", 
                c( "no","bottom 0.5% and top 0.5%", "bottom 1%", "top 1%", "bottom 1% and top 1%"), selected = "no")
  }
})

mydata22 = reactive({
  x00 = mydata2()
  fxattr = c(input$dxAttr)
  if ( {length(fxattr) >=1} ) { 
    for (j in 1:length(fxattr)){
      x01 = as.data.frame(x00 %>% dplyr::select(!!!fxattr[j]))
      x02 = fastDummies::dummy_cols(x01, remove_first_dummy = FALSE,remove_selected_columns = TRUE)
      x00 =cbind(x00,x02)
    }}
  return(x00)
})


mydata3 = reactive({
  data = mydata22()#[,c(input$xAttr)]
  fxattr=(input$fxAttr)
  if (length(fxattr) >= 1){
    for (j in 1:length(fxattr)){
      data[,fxattr[j]] = factor(data[,fxattr[j]])
    }}
  fxattr=setdiff(input$fxAttr,setdiff(colnames(Dataset.temp()),c(colnames(nu.Dataset()))) )
  if (length(fxattr) >= 1){
    for (j in 1:length(fxattr)){
      data[,fxattr[j]] <- sub("^", "F.", data[,fxattr[j]])
      data[,fxattr[j]] = factor(data[,fxattr[j]])
    }}
  return(data)
})

output$winvarselect <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  else {
    #selcol=setdiff(colnames(nu.Dataset()),input$lxAttr)
    data=nu.Dataset()
    data1 = data[, !names(data) %in% c(input$fxAttr)]
    varSelectInput("winAttr", "Select X variable(s) to winsorize",
                   data = data1,multiple = TRUE, selected = ""  )
  }
})

wincol <-reactive({
data1=mydata3()
data2 = as.data.frame(data1 %>% dplyr::select(!!!input$winAttr))
return(data2)
})

output$winhead = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    head(wincol())
  }
})

mydata4 = reactive({
  data=mydata3()
  if (input$winsor == "no") {return(data)}
  else if (input$winsor == "bottom 1%") 
  {data[,c(colnames(wincol()))] <- lapply(data[,c(colnames(wincol()))], 
                                              function(x) Winsorize(x, minval = NULL, maxval = NULL, probs = c(0.01, 1), na.rm = TRUE)) } #probs = c(0.01, 0.99),
  else if (input$winsor == "top 1%") 
  {data[,c(colnames(wincol()))] <- lapply(data[,c(colnames(wincol()))], 
                                              function(x) Winsorize(x, minval = NULL, maxval = NULL, probs = c(0, 0.99), na.rm = TRUE)) } #probs = c(0.01, 0.99),
  else if (input$winsor == "bottom 0.5% and top 0.5%") 
  {data[,c(colnames(wincol()))] <- lapply(data[,c(colnames(wincol()))], 
                                              function(x) Winsorize(x, minval = NULL, maxval = NULL, probs = c(0.005, 0.995), na.rm = TRUE)) } #probs = c(0.01, 0.99),
  else (input$winsor == "bottom 1% and top 1%") 
  {data[,c(colnames(wincol()))] <- lapply(data[,c(colnames(wincol()))], 
                                              function(x) Winsorize(x, minval = NULL, maxval = NULL, probs = c(0.01, 0.99), na.rm = TRUE)) } #probs = c(0.01, 0.99),
  return(data)
})

output$lxvarselect <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  else {
    data=nu.Dataset()
    data1 = data[, !names(data) %in% input$fxAttr]
    varSelectInput("lxAttr", "Select X variable(s) for natural log transformation",
                   data = data1,multiple = TRUE, selected = ""  )
  }
})

mydata5 = reactive({
  data = mydata4()#[,c(input$xAttr)]
  if (length(input$lxAttr) >= 1){
    data1=NULL
    for (j in 1:length(input$lxAttr)){
      #data1 = as.data.frame(log(data[,input$lxAttr[j]]))
      data1 = as.data.frame(log(data %>% dplyr::select(!!!input$lxAttr[j])  ))
      names(data1) = paste0("Log(",input$lxAttr[j],")")
      #data = cbind(data[, !names(data) %in% c(input$lxAttr[j])],data1)
      data = cbind(data,data1)
    }
  }
  return(data)
})

output$sqvarselect <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  else {
    #selcol=setdiff(colnames(nu.Dataset()),input$lxAttr)
    data=nu.Dataset()
    data1 = data[, !names(data) %in% c(input$fxAttr)]
    varSelectInput("sqAttr", "Add X square term",
                   data = data1,multiple = TRUE, selected = ""  )
  }
})

mydata = reactive({
  data = mydata5()#[,c(input$xAttr)]
  if (length(input$sqAttr) >= 1){
    data1=NULL
    for (j in 1:length(input$sqAttr)){
      #data1 = as.data.frame(log(data[,input$lxAttr[j]]))
      data1 = as.data.frame((data %>% dplyr::select(!!!input$sqAttr[j]))^2)
      names(data1) = paste0("Sq(",input$sqAttr[j],")")
      #data = cbind(data[, !names(data) %in% c(input$lxAttr[j])],data1)
      data = cbind(data,data1)
    }
  }
  return(data)
})


output$summ <- renderPrint({
  if (is.null(input$file)) {return(NULL)}
  #else {  ds_screener(mydata())} 
  else {  str(mydata())} 
  })

if(!require("descriptr")) {install.packages("descriptr")}
library(descriptr)
output$screen_summary <- renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {  ds_screener(  mydata())} 
})

out = reactive({
data = mydata()
Missing=(data[!complete.cases(data),])
Missing2=head(Missing)
mscount=nrow(Missing)
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
nu.data = data[,nu] 
fa.data = data[,fa] 
Summary = list(Numeric.data = round(stat.desc(nu.data)[c(4,5,6,8,9,12,13),] ,4), factor.data = describe(fa.data))
# Summary = list(Numeric.data = round(stat.desc(nu.data)[c(4,5,6,8,9,12,13),] ,4), factor.data = describe(fa.data))

a = seq(from = 0, to=200,by = 4)
j = length(which(a < ncol(nu.data)))
out = list(Dimensions=Dimensions, Summary=Summary, Tail=Tail, fa.data,  nu.data,  a,  j, Head=Head,
           MissingDataRows=Missing,missing.data.rows.count=mscount,Missing.Observations=Missing2)
return(out)
})

testsample =  reactive({
  set.seed(input$numout)
  sample(1:nrow(mydata()), round(nrow(mydata())*((input$sample)/100)))
})

train_data = reactive({
  if (input$sample==0) {return(NULL)}
  else {  mydata()[-testsample(),]  }
})

test_data = reactive({
  if (input$sample==0) {return(NULL)}
  else {  mydata()[testsample(),] }
})

output$downloadclean <- downloadHandler(
  filename = function() { "full_data.csv" },
  content = function(file) {
    #write.csv(dummy_cols(mydata()), file, row.names=F, col.names=F)
    write.csv((mydata()), file, row.names=F, col.names=F)
  }
)

output$dummyclean = renderDataTable({
  if (is.null(input$file)) {return(NULL)}
  else {
    #dummy_cols(mydata())
    (mydata())
  }
}, options = list(lengthMenu = c(5, 30, 50,100), pageLength = 5))

output$downloadtrain <- downloadHandler(
  filename = function() { "train_data.csv" },
    content = function(file) {
    #write.csv(dummy_cols(mydata()), file, row.names=F, col.names=F)
    write.csv((train_data()), file, row.names=F, col.names=F)
  }
)

output$dummytrain = renderDataTable({
  if (input$sample==0) {return(NULL)}
  else {
    #dummy_cols(mydata())
    (train_data())
  }
}, options = list(lengthMenu = c(5, 30, 50,100), pageLength = 5))

output$downloadtest <- downloadHandler(
  filename = function() { "test_data.csv" },
    content = function(file) {
    #write.csv(dummy_cols(mydata()), file, row.names=F, col.names=F)
    write.csv((test_data()), file, row.names=F, col.names=F)
  }
)

output$dummytest = renderDataTable({
  if (input$sample==0) {return(NULL)}
  else {
    #dummy_cols(mydata())
    (test_data())
  }
}, options = list(lengthMenu = c(5, 30, 50,100), pageLength = 5))


output$cleanobs = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    dim( mydata())
  }
})

output$trainobs = renderPrint({
  if (input$sample==0) {return(NULL)}
  else {
    dim( train_data())
  }
})

output$testobs = renderPrint({
  if (input$sample==0) {return(NULL)}
  else {
    dim( test_data())
  }
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

output$missing2 = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    out()[11]
  }
})

output$missing1 = renderDataTable({
  if (is.null(input$file)) {return(NULL)}
  else {
    out()[[9]]
  }
}, options = list(lengthMenu = c(10, 25, 50, 100), pageLength = 10))


output$mscount = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    out()[10]
  }
})

output$summary = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    out()[1:2]
      }
})

# Select variables:
output$outselect <- renderUI({
  if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
  if (is.null(input$file)) {return(NULL)}
  else {
  selectInput("rAttr", "Select variable", colnames(mydata()), colnames(mydata())[1])
  }
})

output$outlier = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    if ( input$rAttr %in% colnames(out()[[5]]) ) {
    data = (out()[[5]])
    rosnerTest(na.omit(data[,input$rAttr]))
  }}
})

output$hist = renderPlot({
  if (is.null(input$file)) {return(NULL)}
  else {
    hist(mydata())#[,input$xAttr],main=paste("Histogram of" ,input$xAttr), xlab=input$xAttr)
  }
})

output$hist1 = renderPlot({
  if (is.null(input$rAttr)) {return(NULL)}
  else {
    data=na.omit(mydata())
    if ( input$rAttr %in% colnames(out()[[5]]) ) {
    densityp <- density(data[,input$rAttr])
    hist(data[,input$rAttr],breaks=20,probability=TRUE,
         main=paste("Histogram of" ,input$rAttr), xlab=input$rAttr)
    lines(densityp)   }
    else{  
      dt=as.data.frame(Freq(data[,input$rAttr]) ) 
      dotchart(dt[,2],dt[,1])
      }
  }
})

output$hist2 = renderPlot({
  if (is.null(input$rAttr)) {return(NULL)}
  else {
    if ( input$rAttr %in% colnames(out()[[5]]) ) {
    qqnorm(mydata()[,input$rAttr])
    qqline(mydata()[,input$rAttr])  }
  }
})

d_summary <- reactive({
  # validate(need(input$var_summary != '', 'Please select a variable.'))
  req(input$num_var)
  ds_summary_stats(data(), !! sym(input$num_var))
})

output$hist3 = renderPrint({
  if (is.null(input$rAttr)) {return(NULL)}
  else {
    if ( input$rAttr %in% colnames(out()[[5]]) ) {  
      ds_summary_stats(mydata(), !! sym(input$rAttr))
      }
      else{
      Selected_Variable=(mydata()[,input$rAttr])
      describe(Selected_Variable)
      }
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

# plotsample =  reactive({
#   sizedata= nrow(mydata())
#     if (sizedata>500) {smp=500} 
#     else {
#       smp=sizedata
#           }
#   sample( 1:sizedata, smp )
# })

plot_data1 <- reactive({
  if (is.null(input$file)) {return(NULL)}
  else {
    my_data =(out()[[5]])
    #my_data = mydata()
    #if (input$obs == "full dataset") { return(my_data) }
    #set.seed(1234)
      if (nrow(my_data)>1000){ testsample= sample(1:nrow(my_data), 1000 )
        Dataset1=my_data[testsample,]
        return((Winsorize(Dataset1,na.rm = TRUE)))}
      else {return((my_data))}
    }
})

# plot_data = reactive({
#   my_data = out()[[5]]
#   my_data[plotsample1(),]
# })


output$heatmap1 = renderPlot({ 
  if (is.null(input$file)) {return(NULL)}
  else {
   chart.Correlation(plot_data1(),hitogram=TRUE,pch=3)
  }
})

output$heatmap2 = renderPlot({ 
  if (is.null(input$file)) {return(NULL)}
  else {
    pairs(plot_data1())
  }
})


output$corplot3 = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    round(cor(plot_data1(), use = "pairwise.complete.obs"),4)
  }
})

output$corplot2 = renderPlot({
  if (is.null(input$file)) { return(NULL) }
  else{
    my_data = na.omit(plot_data1())
    cor.mat <- round(cor(my_data),2)
    corrplot(cor.mat, 
             type = "lower",    # upper triangular form
             order = "hclust",  # ordered by hclust groups
             tl.col = "black"  # text label color
             ,tl.srt = 45
             )  
    
  }
})

output$bplot = renderPlot({ 
  if (is.null(input$file)) {return(NULL)}
  else {
    data_wide= as.data.frame(na.omit(out()[[5]]))
    data_wide %>%
      gather(key="MesureType", value="Val") %>%
      ggplot( aes(x=factor(MesureType, levels = colnames(data_wide)), y=Val, fill=MesureType)) +
      #geom_violin(trim = FALSE) +
      geom_boxplot()
  }
})

output$sbplot = renderPlot({ 
  if (is.null(input$file)) {return(NULL)}
  else {
    data_wide= as.data.frame(scale(na.omit(out()[[5]]), center = T, scale = T))
    data_wide %>%
      gather(key="MesureType", value="Val") %>%
      ggplot( aes(x=factor(MesureType, levels = colnames(data_wide)), y=Val, fill=MesureType)) +
      geom_violin(trim = FALSE) +
      geom_boxplot(width=0.05)
      }
})

output$correlation = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
  round(cor(out()[[5]], use = "pairwise.complete.obs"),2)
  }
  })

output$corplot = renderPlot({
  if (is.null(input$file)) {return(NULL)}
  else {
    my_data = plot_data1()
    pairs(my_data,pch=20, col="grey")
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

output$corplot1 = renderPlot({
  if (is.null(input$file)) {return(NULL)}
  else {
    my_data = out()[[5]]
    cor.mat <- 1-cor(my_data)
    mds.cor <-  (cor.mat) %>%
      cmdscale() %>%
      as_tibble()
    colnames(mds.cor) <- c("Dim.1", "Dim.2")
    ggscatter(mds.cor, x = "Dim.1", y = "Dim.2", 
              size = 1,
              label = colnames(cor.mat),
              repel = TRUE)  
    }
})


output$downloadData <- downloadHandler(
  filename = function() { "califhouse.csv" },
  content = function(file) {
    write.csv(read.csv("data/califhouse.csv"), file, row.names=F, col.names=F)
  }
)

output$downloadData1 <- downloadHandler(
  filename = function() { "diabetes.csv" },
  content = function(file) {
    write.csv(read.csv("data/diabetes.csv"), file, row.names=F, col.names=F)
  }
)

})

