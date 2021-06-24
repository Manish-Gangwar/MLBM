#################################################
#               Topic  Analysis             #
#################################################
if(!require("shinyBS")) {install.packages("shinyBS")}; library(shinyBS)
if(!require("DT")) {install.packages("DT")}; library(DT)
if (!require("shinycssloaders")) {install.packages("shinycssloaders")}; library(shinycssloaders)

library(shiny)
library(text2vec)
library(tm)
library(tokenizers)
library(wordcloud)
library(slam)
library(maptpx)
library("shinyBS")

shinyUI(fluidPage(
  
  title = "Text Topic Analysis",
  titlePanel(title=div(img(src="logo.png",align='right'),"Text Topic Analysis")),
  # Input in sidepanel:
  sidebarPanel(
    tags$a(href="javascript:history.go(0)", 
           popify(tags$i(class="fa fa-refresh fa-1x"),
                  title = "", #Reload App", 
                  content = "click here to refresh the app",
                  placement = "right")),
    fileInput("file", "Upload input data (text/csv file)"),
    #uiOutput('id_var'),
    uiOutput("doc_var"),
    
    textInput("stopw", ("Enter stop words separated by comma(,)"), value = "will,can"),
    
    # selectInput("ws", "Weighing Scheme", 
    #             c("weightTf","weightTfIdf"), selected = "weightTf"), # weightTf, weightTfIdf, weightBin, and weightSMART.
    #htmlOutput("pre_proc1"),
    #htmlOutput("pre_proc2"),
    sliderInput("freq", "Minimum frequency of word:", min = 1,  max = 50, value = 2),
    numericInput("topic", "Number of topics to fit:", 2, min=2, max=20),
    actionButton(inputId = "apply",label = "Apply Changes", icon("refresh")),  
    hr(),
    #h4("Word Cloud Option"),
    sliderInput("max",  "Maximum number of words in wordcloud:", min = 1,  max = 200,  value = 100),
    br()),
  
  # Main Panel:
  mainPanel( 
    
    
    tabsetPanel(type = "tabs",
                #
                tabPanel("Overview & Example Dataset",h4(p("How to Use this App")),
                         
                         p("To use this app you need a document corpus in txt file format. Make sure each document is separated from another document with a new line character.
                           To do basic Text Analysis in your text corpus, click on Browse in left-sidebar panel and upload the txt file. Once the file is uploaded it will do the computations in 
                            back-end with default inputs and accordingly results will be displayed in various tabs.", align = "justify"),
                         p("If you wish to change the input, modify the input in left side-bar panel and click on Apply changes. Accordingly results in other tab will be refreshed
                           ", align = "Justify"),
                         h5("Note"),
                         p("You might observe no change in the outputs after clicking 'Apply Changes'. Wait for few seconds. As soon as all the computations
                           are over in back-end results will be refreshed",
                           align = "justify"),
                          #, height = 280, width = 400
                         verbatimTextOutput("start"),
                         h4(p("Download Sample Text File")), 
                         downloadButton('downloadData1', 'Download Nokia Lumia reviews txt file'),br(),br(),
                         #p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
                         #img(src = "example1.png")
                         br()),
                tabPanel("Input Data",
                         #h4("Uploaded data size"),
                        # verbatimTextOutput("up_size"),
                         h4("Review Input Data"),
                        shinycssloaders::withSpinner(DT::dataTableOutput("samp_data")),
                        br()),
                tabPanel("DTM & Word Cloud",
                         p("click on 'Apply Changes' in the panel on the left", style="color:red"),
                         h4("DTM Size"),
                         verbatimTextOutput("dtm_size"),
                         hr(),
                         h4("Document Term Matrix"),
                         shinycssloaders::withSpinner(DT::dataTableOutput("dtmsummary")),
                         hr(),
                         h4("Word Cloud"),
                         shinycssloaders::withSpinner(plotOutput("wordcloud",height = 700, width = 700)),
                         hr(),
                         h4("Weights Distribution of Wordcloud"),
                         DT::dataTableOutput("dtmsummary1"),br()),
                
                #tabPanel("Topic Model - Summary",verbatimTextOutput("summary")),
                tabPanel("Topics Wordcloud",
                         
                         shinycssloaders::withSpinner(uiOutput("plots2")),br()),
                tabPanel("Topics Co-occurrence",
                         numericInput("nodes", "Number of central nodes in a graph", 4),
                         numericInput("connection", "Number of max connection with central node", 5),
                         shinycssloaders::withSpinner(uiOutput("plots3")),br()),
                # tabPanel("Topics eta values",tableOutput("summary2")),
                
                #                         
                tabPanel("Token-Topic Loadings",h4("Top terms for each topic"), 
                         shinycssloaders::withSpinner(DT::dataTableOutput("score")),br()),
                
                tabPanel("Topic Scores as Doc Proportions",br(),br(),
                         downloadButton('downloadData2', 'Download Topic Proportions file'), br(),br(),
                         shinycssloaders::withSpinner(dataTableOutput("table")),br())
                
                         )
           )
       )
    )

