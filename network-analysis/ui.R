####################################################
#      Network App    #
####################################################

library("shiny")
library("igraph")
library('visNetwork')
library('dplyr')
library("tidyverse")
library('randomcoloR')
library("stringr")
library("shinyBS")
#library("foreign")

fluidPage(
  
   # Header:
  #titlePanel(title="Network"),
  title = "Network Analysis",
  titlePanel(title=div(img(src="logo.png",align='right'),"Network App")),
  
  # Input in sidepanel:
  sidebarPanel(
    tags$a(href="javascript:history.go(0)", 
           popify(tags$i(class="fa fa-refresh fa-1x"),
                  title = "", #Reload App", 
                  content = "click here to refresh the app",
                  placement = "right")),
   # h4(p("Data Input")),
    fileInput("file", "Upload 'Adjacency Matrix' (csv file with header)"),
    fileInput("file1", "Optional 'Node Properties Data' (csv file with header)"),
    # htmlOutput("yvarselect"),
    selectInput("mode","Mode of graph",c("directed", "undirected","max", "min", "upper",
                                         "lower", "plus"),"undirected"),
    # selectInput("comm","Find Communities",c("Yes", "No"),"No"),
    hr(),
    h4("Network Plot Options"),
    selectInput("cex2", "Vertex size based on", c("Degree","Betweeness","Closeness"),"Degree"),
    sliderInput("cex", "Increase vertex size by", min = 20,  max = 100, value = 50,round = FALSE),
    
    br(),
   # h5(p("Powered By:")),
    #img(src = "logo.png")
  ),
  # Main:
  mainPanel( 
    
    tabsetPanel(type = "tabs",
                
                tabPanel("Overview",
                            tags$br(),
                            p("Network analysis is a set of integrated techniques to depict relations among nodes and to analyze the structures that emerge from the recurrence of these relations. This app will help you in analyzing such relationships.",align="justify"),
                         h4(tags$a(href="https://en.wikipedia.org/wiki/Network_theory","Network Theory - Wikipedia",target="_blank")),
                         h4(tags$a(href="https://en.wikipedia.org/wiki/Graph_theory","Graph Theory - Wikipedia",target="_blank")),
                         
                            h4(p(tags$b("Data Input"))),
                            
                            p("This shiny application requires following two different types of data input from the users"),
                            tags$b("1. NxN Adjacency Matrix"),
                            p("It represents the relationship between the nodes"),
                            
                            img(src = "input_adj.png", height = 180, width = 600),
                            tags$br(),br(),
                            downloadButton('downloadData', 'Download example adjacecny matrix file'),
                            
                            tags$br(),
                            tags$br(),
                            tags$b("2. Node Properties Data"),
                            p("It represents propoerties of individual nodes"),
                            
                            img(src = "input_demo.png", height = 180, width = 400),
                            tags$br(),br(),
                            downloadButton('downloadData2', 'Download example demographic file'),
                            tags$br(),
                            tags$br(),
                         
                         
                         #p("Once csv file is uploaded successfully, application will display a overall network, community plot and network of communities in  different tabs.",align="justify"),
                         tags$br(),
                        
                         #tags$b(tags$i("Note:")),
                         #tags$br(),
                        # p(("1. Node names should not contain any special character.")),
                        # p("2. Download will not work with RStudio interface. It will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
                        # img(src = "example1.png") #, height = 280, width = 400
                         
                ),
                #
                tabPanel("Input Data",
                         h4("Review Adjacency Matrix"), 
                         # shinycssloaders::withSpinner(dataTableOutput("readdata"),tags$head(tags$style("tfoot {display: table-header-group;}"))),br(),
                         shinycssloaders::withSpinner(DT::dataTableOutput("readdata")),br(),hr(), 
                         shinycssloaders::withSpinner(DT::dataTableOutput("readdata2")),br(), 
                         br()),
                
                
                tabPanel("Network Centralities",br(),
                         shinycssloaders::withSpinner(downloadButton('downloadData1', 'download centralities file')), br(),
                         (tags$a(href="https://en.wikipedia.org/wiki/Centrality","Centrality - Wikipedia",target="_blank")),br(),
                         br(),dataTableOutput("centdata"),br()),
                
                
                tabPanel('Network Plot',
                         shinycssloaders::withSpinner(plotOutput("graph1", height = 700, width = 700)),
                         br(),htmlOutput("yvarselect"),
                         visNetworkOutput('int_net',width = '800px',height = '600px'),br()),
                #tabPanel("Network Plot",plotOutput("graph1", height = 800, width = 840)),
                
                
                tabPanel("Network Communities",
                         shinycssloaders::withSpinner(visNetworkOutput("com_net",height = 700, width = 700)),
                         dataTableOutput('com_cent'),
                         br()),
                tabPanel("Communities Plot",br(),
                         (tags$a(href="https://en.wikipedia.org/wiki/Community_structure","Community - Wikipedia",target="_blank")),br(),
                         
                         shinycssloaders::withSpinner(plotOutput("graph2", height = 700, width = 700)),
                         uiOutput("graph3"),helpText("Note: Seperate plot for community with size = 1 won't be shown"),
                         br())
                #visNetworkOutput('comm_plot')),
                #plotOutput("graph2", height = 800, width = 840),
                #uiOutput("graph3")), #, height = 800, width = 840
              )
            ) 
        ) 
    
