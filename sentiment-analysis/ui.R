#################################################
#           tidy sentiment Analysis             #
#################################################

library("shiny")
library("tidytext")
library("tidyr")
library("dplyr")
library("ggplot2")
library("DT")
library("reshape2")
library("wordcloud")
library("plotly")

#--------------------------------------------#

shinyUI(fluidPage(
  
   # tags$head(includeScript("google_analytics.js")),
  title = "Sentiment Analysis",
  #titlePanel("Sentiment Analysis with tidytext"),
  titlePanel(title=div(img(src="logo.png",align='right'),"Sentiment Analysis App"),windowTitle	='Sentiment Analysis'),
  
  # Input in sidepanel:
  sidebarPanel(
    fileInput("file", "Upload input text/csv file"),  
    uiOutput("doc_var"),
    selectInput("lexicon", "Sentiment Dictionary",
    c("AFINN","bing","loughran","user defined"="userdefined"), selected = "AFINN"),
    
    uiOutput("dictionary"),
    textInput("stopw", ("Enter stop words separated by comma(,)"), value = "the,is,a"),
    # submitButton(text = "Apply Changes", icon("refresh"))
    
  br()),
  
  # Main Panel:
  mainPanel( 
    
    
    tabsetPanel(type = "tabs",
                #
                tabPanel("Overview",h4(p("How to use this App")),
                         
                         p("To use this app you need a document corpus in txt/csv file format. Make sure each document is separated from another document with a new line character.
                           To do basic sentiment analysis in your text corpus, click on Browse in left-sidebar panel and upload the txt/csv file. Once the file is uploaded it will do the computations in 
                            back-end with default inputs and accordingly results will be displayed in various tabs.", align = "justify"),
                         
                         p("You can change the sentiment dictionary in left-sidebar panel. This app supports four inbuilt sentiment dictionaries and one user defined dictionary. If a user selects User Defined dictionary, then a browse file input will appear below sentiment dictionary drop-down in left-side-bar panel and user can upload the user defined dictionary. This user defined dictionary should be in csv format and first column of the dictionary should be word and second column should be score. You can download the sample user defined dictionary below.", align = "justify"),
                         a(href="http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010","1- Afinn",target="_blank"),
                         p("AFINN is a list of English words rated for valence with an integer between minus five (negative) and plus five (positive). The words have been manually labeled by Finn Arup Nielsen in 2009-2011."),
                      
                         a(href="https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html","2- Bing",target="_blank"),
                         p("This sentiment dictionary is created by Bing Liu and collaborators. In this dictionary, words are classified as positive or negative."),
                      
                         a(href="http://www3.nd.edu/~mcdonald/Word_Lists.html","3- Loughran",target="_blank"),
                         p("This dictionary is created by Tim Loughran and Bill McDonald. In this dictionary each word is classified in financial context (uncertainty, litigious, constraining, superfluous, positive, negative)"),
                        
                         br(),
                         h4(p("Download sample text file")),
                         downloadButton('downloadData1', 'download "Nokia Lumia" reviews'),br(),
                         br()),
                
                tabPanel("Input Data",
                         h4("Review Input Data"), 
                         dataTableOutput("readdata"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),
                         
                         br()),
                
                # tabPanel("Sentiments - Plot",h4(),
                #          # verbatimTextOutput('chk'),
                #          # 
                #         # uiOutput("sent.plots"),
                #         # verbatimTextOutput("event")
                #          # h4("Weights Distribution of Wordcloud"),
                #          # verbatimTextOutput("dtmsummary1")
                #          ),
                tabPanel("Sentiments - Stats",br(),
                         downloadButton('downloadData2', 'Download Sentiment Scores'), br(),br(),
                         plotOutput("word.cloud", height="700", width="700"),
                         br(),
                         dataTableOutput("count"),
                         br(),br()
                         ),

                tabPanel("Document Level Analysis",br(),
                         uiOutput("sent.plots"),
                         numericInput("index", "Choose Document Index", 3),
                         h4("List of the most frequent sentiment words at each level"),
                         dataTableOutput("table"),
                         br(),
                         h4("Document parsed into sentences and sentiment score for each sentence"),
                         # downloadButton('downloadData4', 'Downlaod Sentiemnt Scores (Works only in browser)'), br(),br(),
                         dataTableOutput("table2"),br(),br())
                #                         
                         )
                )
  
)
)
