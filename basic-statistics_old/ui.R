####################################################
#      Summary & Basic Stats                        #
####################################################

library("shiny")
library("shinyBS")
#library("foreign")

shinyUI(pageWithSidebar(
  # Header:
 # headerPanel("Regression App"),
  headerPanel(title=div(img(src="isb.png",align = "right"), h2("Basic Data Stats App", style="bold")), windowTitle	='Basic Stats'),
  
  # Input in sidepanel:
  sidebarPanel(
    tags$a(href="javascript:history.go(0)", 
           popify(tags$i(class="fa fa-refresh fa-1x"),
                  title = "", #Reload App", 
                  content = "click here to refresh the app",
                  placement = "right")),
    h4(p("Data Input")),
    fileInput("file", "Upload input data (csv file with header)"),

    h4(p("Data Selection")),
    htmlOutput("xvarselect"),
    htmlOutput("fxvarselect",style="color:grey"),
    htmlOutput("samsel"),
    htmlOutput("imputemiss"),
    htmlOutput("winsor"),
    htmlOutput("lxvarselect"),
    actionButton(inputId = "apply",label = "Apply Changes", icon("refresh")),
    #submitButton(text = "Apply Changes", icon("refresh")),br(),
    ),
  # Main:
  mainPanel( 
    
    tabsetPanel(type = "tabs",
                #
                
                tabPanel("Overview",
                         h4(p("How to use this shiny application")),
                         p("This shiny application require one data input from the user. To do so, click on the 'Browse' (in the panel on the left) and upload the csv data input file.
                           Note that this application can read only csv file (comma delimited file), so if you don't have csv input data file, first convert your data in csv format 
                           and then proceed. Make sure you have top row as variable names.",align="justify"),
                         p("Once csv file is uploaded successfully, all variables in the data file will reflect in the 'Data Selection' panel on the left. 
                            If you want to drop any variable from the analysis, just uncheck that variable and it will be dropped from the analysis."),
                          p("Next you can select factor (categorical) variables just by selecting that variable in the 'Select factor (categorical) variables' list.",align="justify"),
                        # h4(p("Download sample input file")),
                         downloadButton('downloadData', 'download housing sample data'),
                         br(),br(),
                        downloadButton('downloadData1', 'download diabetes sample data'),
                         br()
                        # p("*Please note that download will not work with RStudio interface. Download will work only in web-browsers."),
                         ),
                
                tabPanel("Data Summary",#h4("Selected Variables"), verbatimTextOutput("head"),#verbatimTextOutput("tail"),
                         h4("Review Input Data"), 
                         dataTableOutput("readdata"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),
                         h4("Data Summary of Selected Variables"),
                         shinycssloaders::withSpinner(verbatimTextOutput("summary")),
                         verbatimTextOutput('screen_summary'),
                         #h4("Missing Data"),verbatimTextOutput("missing2"),
                         #h4("Correlation Table"),verbatimTextOutput("correlation"),br(),
                         br()),
                tabPanel("Missing Data",#h4("Impute Missing Values"),
                         h4("Missing Data in Selected Variables"),
                         htmlOutput("imout"),
                         #(p('you can remove missing data variable(s) if any, or impute or drop rows',style="color:black")),
                         dataTableOutput("missing1"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),
                         br()),

                #          h4("Correlation Visulization"),
                #          (p('Remove missing data variable(s) if any - check  "Missing Data" tab',style="color:red")),
                #          p(plotOutput("corplot"),align="left"),
                #          h4("Visulizing Correlation Matrix"),plotOutput("corplot1",width = 500)),
                          
       #        tabPanel("Box Plot", h4("Box Plots"),plotOutput("bplot"),br(),br(),
        #                 h4("Box Plots of Scaled Data (mean=0, vairance=1)"),plotOutput("sbplot")),
       
       #source('ui/ui_dummy_en_EDA.R', local = TRUE)[[1]],
       tabPanel("Uni-variate Statistics",br(),
                #h4("Missing Data Rows"),
                verbatimTextOutput("mscount"),
                htmlOutput("imout1"),br(),
                htmlOutput("outselect"), 
                #h4("selcet a variable at the top for Rosner's outliers"),
                shinycssloaders::withSpinner(plotOutput("hist1")),br(),
                verbatimTextOutput("hist3"),br(),
                plotOutput("hist2"),br(),
               # verbatimTextOutput("outlier")
               br()),
       
              tabPanel("Bi-variate Statistics",
                       h4("Be patient generating bi-variate plots"),
                          h4("Bi-variate Plots"),
                         shinycssloaders::withSpinner(plotOutput("corplot")),
                          h4("Correlation Table"), 
                          verbatimTextOutput("correlation"),br(),
                          h4("Correlation Plot"),
                          htmlOutput("imout2"),
                          plotOutput("corplot2"),br(),
                          br()),
       
       tabPanel("Data Visualization",
                #h4("Select variable for er's outlier test"),
                h4("Be patient generating plots"),
              #  plotOutput("dens"),
              shinycssloaders::withSpinner(plotOutput("hist")),br(),br(),
                tags$a(href="https://en.wikipedia.org/wiki/Box_plot", "Boxplots descriptive statistics on wikipedia",target="_blank"),br(),br(),
                h4("Box Plots of Input Data"),plotOutput("bplot"),
                h4("Violin Plots of Standardize Input Data (mean=0, vairance=1)"),
                (p('Winsorize data to remove extreme values - 
                         check options in the panel on the left',style="color:black")),
                plotOutput("sbplot"),br(),
                #htmlOutput("outselect"),
                #verbatimTextOutput("outlier"),
                br()),
       
                tabPanel("Download Data",br(),
                         h4(p("Download input data with modifications as per options you selected in the panel on the left")),
                         downloadButton('downloadDatanew', 'download input data'), br(), br(),
                         #h5("First 10 rows of input data with dummy variable coding for factor/categorical variables"),
                         dataTableOutput("dummydata"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),br()
                         )
                )
      ) 
    ) 
  )
