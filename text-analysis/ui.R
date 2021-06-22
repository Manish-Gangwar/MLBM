#################################################
#               Basic Text Analysis             #
#################################################

library(shiny)
library(text2vec)
library(tm)
library(tokenizers)
library(wordcloud)
library(slam)
library(stringi)
library(magrittr)
library(tidytext)
library(dplyr)
library(visNetwork)
library(tidyr)
library(DT)
library(stringr)
library(tools)
library("shinyBS")

shinyUI(fluidPage(
  title = "Basic Text Analysis",
  titlePanel(title=div(img(src="logo.png",align='right'),"Basic Text Analysis")),
  
  # Input in sidepanel:
  sidebarPanel(
    tags$a(href="javascript:history.go(0)", 
           popify(tags$i(class="fa fa-refresh fa-1x"),
                  title = "", #Reload App", 
                  content = "click here to refresh the app",
                  placement = "right")),
   # h4(p("Data Input")),
    #helpText("Note: first column of the input csv file must be an unique document id",style="color:darkblue"),
    fileInput("file", "Upload input data (text/csv file)"),
    #uiOutput('id_var'),
    uiOutput("doc_var"),
    #  hr(),
    textInput("stopw", ("Enter stop words separated by comma(,)"), value = "the,is,a"),
    
    # selectInput("ws", "Weighing Scheme", 
    #             c("weightTf","weightTfIdf"), selected = "weightTf"), # weightTf, weightTfIdf, weightBin, and weightSMART.
    #
    htmlOutput("pre_proc1"),
    htmlOutput("pre_proc2"),
    sliderInput("freq", "Minimum frequency of word:", min = 0,  max = 100, value = 2),    
    actionButton(inputId = "apply",label = "Apply Changes", icon("refresh")),
    hr(),
    #h4("Word Cloud Option"),
    sliderInput("max",  "Maximum number of words in wordcloud:", min = 10,  max = 200,  value = 50), 

#    actionButton(inputId = "apply",label = "Apply Changes", icon("refresh")),br(),br(),
    
    #actionButton(inputId = "apply",label = "Apply Changes", icon("refresh")),br(),br(),
br()),
  
  # Main Panel:
  mainPanel( 
    tabsetPanel(type = "tabs",
                #
                tabPanel("Overview & Example Dataset",h4(p("How to use this App")),
                         
                         h5("To use this app you need a document corpus in txt file format. Make sure each document is separated from another document with a new line character."),
                         p("To do basic 'text analysis' in your text corpus, click on Browse in left-sidebar panel and upload the txt/csv file. 
                         Once the file is uploaded it will do the computations in 
                            back-end with default inputs and accordingly results will be displayed in various tabs.
                            If you wish to change the input, modify the input in the left panel and click 'Apply Changes'. Accordingly results in other tab will be refreshed
                           ", align = "Justify"),
                        actionButton(inputId = "apply",label = "Apply Changes", icon("refresh")),
                         #h5("Note: You might observe no change in the outputs after clicking 'Apply Changes'. Wait for few seconds. As soon as all the computations are over in back-end results will be refreshed", align = "justify"),
                         #, height = 280, width = 400
                         br(),
                         h4(p("Download sample text file")),
                         downloadButton('downloadData1', 'download "Nokia Lumia" reviews'),br(),br(),
                       #  p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
                       #  img(src = "example1.png")
                )
                ,
                # tabPanel("Example dataset", h4(p("Download Sample text file")),
                #          downloadButton('downloadData1', 'Download Nokia Lumia reviews txt file'),br(),br(),
                #          p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
                #          img(src = "example1.png")),
                
                 tabPanel("Input Data",
                  h4("Review Input Data"), 
                 # shinycssloaders::withSpinner(dataTableOutput("readdata"),tags$head(tags$style("tfoot {display: table-header-group;}"))),br(),
                  shinycssloaders::withSpinner(DT::dataTableOutput("readdata")),br(), 
                  br()),
                
                
                tabPanel("DTM",
                          verbatimTextOutput("dtmsize"),
                         p("adjust minimum frequency of words in the DTM from the panel on the left"),
                         h4("Sample DTM (Document Term Matrix) "),
                         p("click on 'Apply Changes' in the panel on the left", style="color:red"),
                         shinycssloaders::withSpinner(DT::dataTableOutput("dtm_table")),br(), 
                         h4("Download DTM"),
                         #h3("-------------"),
                         verbatimTextOutput("dtm_text"),
                         downloadButton('download_dtm', 'download DTM'),br(),
                         h4("Word Cloud"),
                         p("adjust maximum number of words in the wordcloud in the panel on the left"),
                         shinycssloaders::withSpinner(plotOutput("wordcloud",height = 800, width = 800)),br(),
                         #textInput("in",label = "text"),
                         p("adjust minimum frequency and maximum number of words in the wordcloud in the panel on the left"),
                         h4("Word Cloud Weights"),
                         DT::dataTableOutput("dtmsummary1"),
                         br()),
                # tabPanel("TDM & Word Cloud",
                #          
                #          verbatimTextOutput("dtmsummary"),
                #          br(),
                #          br(),
                #          
                #         ),
                
                tabPanel("TF-IDF", 
                         verbatimTextOutput("idf_size"),
                         p("adjust minimum frequency of words in the TF-IDF from the panel on the left"),
                         h4("Sample TF-IDF (Term Frequency-Inverse Document Frequency) "),
                         p("click on 'Apply Changes' in the panel on the left", style="color:red"),
                         shinycssloaders::withSpinner(DT::dataTableOutput("idf_table")),br(), 
                         h4("Download TF-IDF"),
                         verbatimTextOutput("tfidf_text"),
                         downloadButton('download_tfidf', 'download TF-IDF'),br(),
                         h4("Word Cloud"),
                         p("adjust maximum number of words in the wordcloud in the panel on the left"),
                         shinycssloaders::withSpinner(plotOutput("idf_wordcloud",height = 800, width = 800)),br(),
                         p("adjust minimum frequency and maximum number of words in the wordcloud in the panel on the left"),
                         #textInput("in",label = "text"),
                         #sliderInput("freq1", "Minimum frequency in wordcloud:", min = 0,  max = 100, value = 2),
                         #sliderInput("max1",  "Maximum number of words in wordcloud:", min = 10,  max = 300,  value = 50),  
                         h4("Word Cloud Weights"),
                         DT::dataTableOutput("dtmsummary2"),br()),
                tabPanel("Concordance",
                         h4('Concordance'),
                         p('Concordance allows you to see the local context around a word of interest. 
                           It does so by building a moving window of words before and after the focal word\'s every 
                           instance in the corpus (inlcuding stop words). 
                           Below is the list of all instances of concordance in the corpus for your word of interest. 
                           You can change the concordance window or word of interest.',align = "Justify"),
                         textInput("concord.word",('word (sub-string) in lower case'),value = 'as'),
                         checkboxInput("regx","match sub-string in all words"),
                         sliderInput("window",'Concordance window size',min = 2,max = 20,5),
                         actionButton(inputId = "apply1",label = "Apply Changes", icon("refresh")),br(),br(),
                         #verbatimTextOutput("concordance"))
                          shinycssloaders::withSpinner(DT::dataTableOutput("concordance")),
                         
                         br()),
                
                tabPanel("Term Co-occurrence",
                         numericInput("nodes", "Number of central nodes in a graph", 4),
                         numericInput("connection", "Number of max connection with central node", 5),
                         h4("DTM Co-occurrence"),
                         shinycssloaders::withSpinner(visNetworkOutput("cog.dtm",height = 600, width = 600)),
                         h4("TF-IDF Co-occurrence"),
                         visNetworkOutput("cog.idf",height = 600, width = 600),
                        br()),
                tabPanel("Bigram",
                         h4('Collocations Bigrams'),
                         p('If a corpus has n word tokens, then it can have at most (n-1) bigrams. However, most of
                                    these bigram are uninteresting. The interesting ones - termed collocations bigrams - comprise
                                    those bigrams whose occurrence in the corpus is way more likely than would be true if the 
                                    constituent words in the bigram randomly came together. 
                                    Below is the list of top collocations 
                                    bigrams from the corpus you uploaded (including stop words).',align = "Justify"),
                         shinycssloaders::withSpinner(DT::dataTableOutput("bi.grams")),
                         h4("Bigram Word Cloud"),
                         p("adjust maximum number of words in the wordcloud in the panel on the left"),
                         shinycssloaders::withSpinner(plotOutput("bi_word_cloud",height=800,width=800)),
                         #p("adjust minimum frequency and maximum number of words in the wordcloud in the panel on the left"),
                         br() )

               # tabPanel("Downloads",

                         # h3("-----------------------------------------------------"),
                         # h4("Download Bigram Corpus"),
                         # verbatimTextOutput("bi_text"),
                         # downloadButton("download_bigram","download bigram corpus"),
                        # br())
                          
          
                
                
    )
  )
)
)
