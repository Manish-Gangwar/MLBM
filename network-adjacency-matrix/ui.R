library(shiny)
library(tidyverse)
library(DT)
library(tibble)
library(descriptr)
library("shinyBS")

shinyUI(fluidPage(
  title = "Adjacency Matrix (Network) App",
  titlePanel(title=div(img(src="logo.png",align='right'),"Adjacency Matrix (Network) App")),
  
  # Input in sidepanel:
  sidebarPanel(
    tags$a(href="javascript:history.go(0)", 
           popify(tags$i(class="fa fa-refresh fa-1x"),
                  title = "", #Reload App", 
                  content = "click here to refresh the app",
                  placement = "right")),
    fileInput("file", "Upload CSV"),
   # checkboxInput('id_col',"Create ID variable",value = FALSE),
    uiOutput("sel_id_var"),
    uiOutput("colList"),
    #uiOutput("sel_fac_to_dumm"),
    htmlOutput("imputemiss"),
    br()),
  
  # Main Panel:
  mainPanel( 
    tabsetPanel(type = "tabs",
                #
                tabPanel("Overview & Example Dataset",
                         
                         h4(p("Overview")),
                         p("This app will help you in prepration of input data for network an app.", align = "Justify"),
                         hr(),
                         h4(p("How to use this App")),
                         p("", align = "justify"),
                         p("Upload a csv file from sidebar panel. After uploading data successfully, select identity variable and then adjust threshold from slider input to drop connections. After setting all the parameters, press apply changes. It will now calculate distance between each node and subsequently a downloadable adjacency matrix is made available as an input file to network an app.
                           ", align = "Justify"),
                         h4(p("Input Data Format")),
                         p("Application takes CSV (comma seperated) file as an input. Below is the example
                           ", align = "Justify"),
                         img(src = "dataset.png"),
                         hr(),
                         h4(p("Download Sample file")),
                         downloadButton('downloadData1', 'Download Sample Input file'),br(),br(),
                       #  p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
                       #  img(src = "example1.png")
                         #, height = 280, width = 400
                        br()),
                tabPanel("Input Data", 
                         h4("Review Input Data"),
                         shinycssloaders::withSpinner(dataTableOutput("sample_data")),
                         htmlOutput("imout"),
                         verbatimTextOutput("screen_summary"),
                         br()),
                tabPanel("Data for Adjacency",br(),
                        # h4("Dimensions"),
                         textOutput("df_size"),
                         h4("Numerical Data Columns for Adjacency Matrix"),
                        shinycssloaders::withSpinner(dataTableOutput("summ")),
                         #verbatimTextOutput("summ"),
                         #helpText("Note: In case of missing values, use data pre-proc app for imputation"),
                         br()),
                
              tabPanel("Download Adjaceny Matrix", br(),
                       #  p("to generate adjacency matrix, click on 'Apply Changes' in the panel on the left"),
                       helpText("selected top x percentile rows will form a link"),
                       sliderInput(inputId = "cut_off",label = "top x percentile",min = 0,max=1,step = 0.01,value = 0.25),
                       #actionButton(inputId = "apply",label = "Apply Changes", icon("refresh")),
                       hr(),
                         h4("Sample Adjaceny Matrix"),
                         shinycssloaders::withSpinner(dataTableOutput("sample_adj")),
                         hr(),
                         h4(p("Download Adjacency Matrix")),
                         downloadButton('download_adj_mat', 'Download Adjaceny Matrix'),br(),br(),
                         hr(),
                       uiOutput("node_attr"),
                       h4("Sample Node Attributes"),
                       shinycssloaders::withSpinner(dataTableOutput("sample_node")),
                         h4(p("Download Node Attributes")),
                         
                         downloadButton('download_node_attr', 'Download Node Attributes'),
                          br(),br(),hr() )
                
                
                
                
                
                
    )
  )
)
)