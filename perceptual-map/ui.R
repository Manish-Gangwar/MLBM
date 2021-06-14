#################################################
#               Joint Space Map                 #
#################################################


library("shiny")
library("shinyBS")
#library("foreign")
 
shinyUI(fluidPage(
  # Header:
 # titlePanel(title=div(img(src="logo.png",align='right'),"Joint Space Map")),
  headerPanel(title=div(img(src="logo.png",align = "right"), h2("Perceptual Map", style="bold")), windowTitle	='Joint Space Map'),
  #titlePanel("Joint Space Map"),
  # Input in sidepanel:
  sidebarPanel(
    tags$a(href="javascript:history.go(0)", 
           popify(tags$i(class="fa fa-refresh fa-1x"),
                  title = "", #Reload App", 
                  content = "click here to refresh the app",
                  placement = "right")),
    # Upload data:
    h4(p("Input Data")),
    fileInput("file", "Upload Perceptual data"),
    # upload secon data
    # Variable selection:
    numericInput("k0","Scaling Factor Entities",1.5),
    numericInput("k1","Scaling Factor User",1.5),
    #h5(p("Data Selection (Optional)")),
    h5(p("At least 3 attributes must be selected")),
    #h6(p("A -  Perceptual (Attributes)")),
    htmlOutput("varselect"),
    hr(),
    tags$a(href="javascript:history.go(0)", 
           popify(tags$i(class="fa fa-refresh fa-1x"),
                  title = "", #Reload App", 
                  content = "click here to refresh the app",
                  placement = "right")),
    h4(p("Optional - Preference Data Input")),
    fileInput("file1", "Upload Preference Data"),
    #h5(p("C -  Preference")),
    # upload secon data
    htmlOutput("varselect1"),
    br()
  ),
  # Main:
  mainPanel(
    
    tabsetPanel(type = "tabs",
                #
                
tabPanel("Overview",
         h4(p("Perceptual Map")),
         p("Perceptual Mapping is the use of graphs to identify the positioning of products/brands that consumers have, and find their preference. The graphs layout an X and Y axis with variables and ranges from the most desirable to least desirable. For instance, the far right may be listed as 'Upper class' while the left side will be 'Lower Class'. This allows for the placement of business names to help find the position that consumers place these businesses in relation to the variables listed."
           ,align="justify"), 
         a(href="https://en.wikipedia.org/wiki/Perceptual_mapping","- Wikipedia",target="_blank"),
         p("In the panel on the left, click on 'Browse' and upload 
the perceptual data. Note that this application can read only csv file(comma delimited file), so if you don't have csv input data file, 
first convert your data in csv format and then proceed. Make sure you have top row as variable names and first column as respondent id/name 
in csv file. As soon as upload is complete, this app will read the data and all the variables in perceptual file will reflect in the panel on
the left. Now you can navigate across different tab viz. 'PCA Variance Plot' tab, 'Perceptual Map' 
tab and 'Data' tab.",align="justify"),
         p('Perceptual Sample File'),
         img(src = "perceptual.png", height = 180, width = 400),
         br(),br(),
         downloadButton('downloadData3', 'download perceptual sample data'),br(),br(),
         hr(),
         h4("Optional - Add Preferences"),
         p("To make Joint Space map, click on second 'Browse' link and upload the preference data in csv format. As soon as upload is complete, respondents in preference
data file will reflect in the panel on the left. Sample perceptual file and preference file is shown 
below"),
         p('Preference Sample File'),
         img(src = "preference.png", height = 180, width = 400),
         br(),br(),
         #, height = 280, width = 400
         #  h4(p("Download sample perceptual & preference file")), 
         #downloadButton('downloadData1', 'Download Perceptual file example 1'),br(),br(),
         #          downloadButton('downloadData2', 'Download Preference file example 1'),br(),br(),
         #          
        

         downloadButton('downloadData4', 'download preference sample data'),br(),
  #        h4(p("Data Selection")),
  #        p("This app gives users additional functionality to modify the data. By default, all the variables in perceptual file are 
  #        selected for PCA but users can deselect/select variables in perceptual data in the panel on the left as per their requirement. 
  #        Similarly, users can also deselect/select respondents from preference data
  # in the panel on the left. As soon as users make a change in data selection, all the tabs will be updated.",
  #          align="justify"),
         
         br(),br()),
# tabPanel("Example dataset", 
#          p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
#          img(src = "example1.png")  ),

# tabPanel("Data Summary",#h4("Selected Variables"), verbatimTextOutput("head"),#verbatimTextOutput("tail"),
#          h4("Review Input Perceptual Data"), 
#          dataTableOutput("readdata"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),
#          #h4("Input Data Set"),
#          h4("Data Summary of Selected Attributes"),
#          htmlOutput("imout"),
#          verbatimTextOutput("summary"),
#          verbatimTextOutput("summ"),
#          verbatimTextOutput('screen_summary'),
#          br() ),
                tabPanel("Data",h5(p("Perceptual Data")),tableOutput("table"),tableOutput("table1")),
                tabPanel("PCA Variance Plot",plotOutput("plot1", width = "100%")),
                tabPanel("Perceptual Map",plotOutput("plot", height = 800, width = 840)),
                tabPanel("Spider Chart",br(),
                         #h6(p("B -  Perceptual (Firms - only for Spider Chart)")),
                         h4(p("At least 2 firms must be selected")),
                         #h4("Choose at least 2 firms to make spider chart"),
                         htmlOutput("varselect2"),
                         plotOutput("spiderplot", height = 800, width = 840)),
#     tableOutput("table"),  
#     tableOutput("table1"),
#     plotOutput("plot", width = "100%"),
#     plotOutput("plot1")
  br())
))
)
