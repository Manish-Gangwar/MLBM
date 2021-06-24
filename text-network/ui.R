####################################################
#      Text Network Input for Recommendation    #
####################################################

library("shiny")
library("igraph")
library("tm")
library('visNetwork')
library('stringr')
library("shinyBS")
#library("foreign")

shinyUI(fluidPage(
  # Header:
  #headerPanel("Text Input for Recommendation App"),
  titlePanel(title=div(img(src="logo.png",align='right'),"Text Network App"), windowTitle	='Text Network App'),
  # Input in sidepanel:
  sidebarPanel(
    tags$a(href="javascript:history.go(0)", 
           popify(tags$i(class="fa fa-refresh fa-1x"),
                  title = "", #Reload App", 
                  content = "click here to refresh the app",
                  placement = "right")),
    
   # h5(p("Data Input")),
    fileInput("file", "Upload Data"),
    #fileInput("file1", "Upload Demographics data (csv file with header))"),
    # selectInput("mode","Mode of Graph",c("directed", "undirected","max", "min", "upper",
    #                                      "lower", "plus"),"undirected"),
    # htmlOutput("yvarselect"),
    # sliderInput("cex", "Data point labels font size", min = 0.1,  max = 3, value = 1,round = FALSE),
    # sliderInput("cex2", "Vertex Size", min = 0.1,  max = 20, value = 5,round = FALSE),
    

    #sliderInput("cutoff", " Minimum number of times brand is selected", min = 1,max = 50,value = 5,step = 1),
    hr(),
    #h4("Options for COG"),

    #sliderInput("cex2", "Vertex Size", min = 0.1,  max = 20, value = 5,round = FALSE),
    
    
    numericInput("nodes", "Number of central nodes in co-occurance graph", 5),
    numericInput("connection", "Number of max connection with central node in co-occurance graph", 10),
    sliderInput("cex", "Font size for labels in co-occurance graph", min = 0.1,  max = 3, value = 1,round = FALSE),
    
    br()
  ),
  # Main:
  mainPanel( 
    
    tabsetPanel(type = "tabs",
                
                
                
                #
                # tabPanel("Doc-Doc Network",plotOutput("graph1", height = 800, width = 840)),
                # tabPanel("Term-Term Network",plotOutput("graph2", height = 800, width = 840)),
                tabPanel("Overview & Example Dataset",
                         
                         h4(p("Overview")),
                         p("Network analysis refers to a family of methods that describe relationships between units of analysis. 
                         A network is comprised of nodes as well as the edges or connections between them.
                           ", align = "Justify"),
                         
                         p("one can represent a corpus of documents as a network where each node is a document, 
                           and the thickness or strength of the edges between them describes similarities between 
                           the words used in any two documents. Or, one can create a text network where individual words 
                           are the nodes, and the edges between them describe the regularity with which they co-occur in documents."
                           ,align="Justify"),
                         h4(tags$a(href="https://sicss.io/2018/materials/day3-text-analysis/text-networks/rmarkdown/SICSS_Text_Networks.html#:~:text=What%20is%20a%20Text%20Network%3F,-Network%20analysis%20refers&text=For%20example%2C%20one%20can%20represent,used%20in%20any%20two%20documents",
                                "Text Networks",target="_blank")),
                         h4(tags$a(href="https://en.wikipedia.org/wiki/Co-occurrence_network",
                                   "Co-occurance Graph - Wikipedia",target="_blank")),
                         hr(),
                         h4(p("How to use this App")),
                         p("", align = "justify"),
                         p("Upload data to sidebar panel it will generate varity of plots. One can adjust sliders for various plot attributes.
                           ", align = "Justify"),
                         h4(p("Input Data Format")),
                         p("Application takes input in following format
                           ", align = "Justify"),
                         img(src = "dataset.PNG"),
                         hr(),
                         h4(p("Download Sample input file")),
                         # 
                         downloadButton('dwnld', 'Download'),br(),br(),
                        # p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
                        # img(src = "example1.png"),
                         #, height = 280, width = 400
                         ),
                tabPanel("Input Data",
                         h4("Review Input Data"), 
                         # shinycssloaders::withSpinner(dataTableOutput("readdata"),tags$head(tags$style("tfoot {display: table-header-group;}"))),br(),
                         shinycssloaders::withSpinner(DT::dataTableOutput("readdata")),br(), 
                         br()),

                tabPanel("Doc-Doc COG",
                         p("Co-Occurance Graph for Docs"),
                         shinycssloaders::withSpinner(visNetworkOutput("graph3", height = 700, width = 700)),
                         h4("Download Doc-Doc Matrix"),
                         downloadButton('downloadData2', 'Download Doc-Doc Matrix'),h4("Sample Doc-Doc Matrix"),tableOutput('doc_doc'),
                         br()),
                tabPanel("Term-Term COG",
                         p("Co-Occurance Graph for Terms"),
                         shinycssloaders::withSpinner(visNetworkOutput("graph4", height = 700, width = 700)),
                         h4("Download Term-Term Matrix"),
                         downloadButton('downloadData3', 'Download Term-Term Matrix'),h4("Sample Term-Term Matrix"),tableOutput('term_term'),
                         br()),
                
                tabPanel("Bipartite Graph",br(),
                         numericInput("npoint", "Maximum number of terms in the graph", 20),
                         uiOutput("interactive_slider"),
                         tags$a(href="https://en.wikipedia.org/wiki/Bipartite_graph","Bipartite Graph - Wikipedia",target="_blank"),
                         shinycssloaders::withSpinner(visNetworkOutput("graph5", height = 700, width = 700)),
                         br()),
                
                tabPanel("Download Matrix", h4(p("Download DTM for Recommendation Analysis")), 
                downloadButton('downloadData1', 'Download DTM'),h4("Sample DTM"),tableOutput('dtm'),
                br())
                # tabPanel("Network Centralities",dataTableOutput("centdata"))
    )
  ) 
) 
)

