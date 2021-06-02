####################################################
#      OLS App IIDS                          #
####################################################

library("shiny")
library("shinyBS")
#library("foreign")

shinyUI(pageWithSidebar(
  # Header:
  #headerPanel("OLS App"),
  headerPanel(title=div(img(src="logo.png",align = "right"), h2("Regression App", style="bold")), windowTitle	='Regression'),
  #titlePanel(title=div(img(src="logo.png",align='right'),"OLS App")),
  # Input in sidepanel:
  sidebarPanel(
    tags$a(href="javascript:history.go(0)", 
           popify(tags$i(class="fa fa-refresh fa-1x"),
                  title = "",
                  content = "click here to refresh the app",
                  placement = "right")),
    h4(p("Data Input")),
    fileInput("file", "Upload input data (csv file with header)"),
    
    h4(p("Data Selection")),
    htmlOutput("yvarselect"),
    htmlOutput("xvarselect"),
 #   submitButton(text = "Apply Changes", icon("refresh")),br(),
    htmlOutput("fxvarselect",style="color:gray" ),
    htmlOutput("samsel"),
    htmlOutput("imputemiss"),
    actionButton(inputId = "apply",label = "Apply Changes", icon("refresh")),
  #  sliderInput('sample','Set test sample percentage',10,40,20),
#   fileInput("filep", "Upload new data for prediction (csv file with header)"),
    br()
  ),
  # Main:
  mainPanel( 
    
    tabsetPanel(type = "tabs",
                #
                
                tabPanel("Overview",
                         h4(p("How to use this application")),
                         p("This application requires one data input from the user. To do so, click on the Browse (in left side-bar panel) and upload the csv data input file.
                           Note that this application can read only csv file (comma delimited file), so if you don't have csv input data file, first convert your data in csv format 
                           and then proceed. Make sure you have top row as variable names.",align="justify"),
                         p("Once csv file is uploaded successfully, variables in the data file will reflect in the 'Data Selection' panel on the left. Now you can select 
                            dependent variable (Y Variable) from drop-down menu. By default all other remaining variables will be selected as explanatory variables (X variables). 
                              If you want to drop any variable from explanatory variables, just uncheck that variable and it will be dropped from the model. 
                            If any of the variables selected in explanatory variables is a factor variable, you can define that variable as factor variable just
                            by selecting that variable in the last list of variables
                           ",align="justify"), 
                         h4(tags$a(href="https://en.wikipedia.org/wiki/Regression_analysis", "Note on Regression (Wikipedia)",target="_blank")),
                         #br(),
                         # h4(p("Download Sample Input File")),
                         downloadButton('downloadData', 'download sample data'),
                         br()
                          ),
                         
                tabPanel("Data Summary",#h4("Selected Variables"), verbatimTextOutput("head"),#verbatimTextOutput("tail"),
                         h4("Review Input Data"), 
                         dataTableOutput("readdata"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),
                         h4("Data Summary of Selected Y and X Varaibles"),
                         
                         shinycssloaders::withSpinner(verbatimTextOutput("summary")),
                         verbatimTextOutput('screen_summary'),
                         h4("Missing Data Rows (Sample)"),
                         htmlOutput("imout"),
                         verbatimTextOutput("missing"),
                         br()),
               # tabPanel("Correlation",h4("Correlation Table - Input data"), 
               tabPanel("Data Visualization",br(),
                        #h4("Select variable for er's outlier test"),
                        h4("Be patient generating plots"),
                        #plotOutput("dens"),
                        h4("Histograms"),
                        shinycssloaders::withSpinner(plotOutput("hist")),br(),
                        h4("Pair Plots"),
                        #(p('remove missing data variable(s) if any, or impute or drop rows - check  "Data Summary" tab and options in the panel on the left',style="color:black")),
                        shinycssloaders::withSpinner(plotOutput("corplot")),
                        br(),br(),
                        br()),
               
                tabPanel("Summary OLS",  
                        # h4('Number of Rows and Columns in Training Data'),
                        # verbatimTextOutput('trainobs'),
                       #  h4('Number of Rows and Columns in Test Data'),
                       #  verbatimTextOutput('testobs'),
                         h4("Summary Oridnary Least Square (OLS) Model"),htmlOutput("yout"), 
                       shinycssloaders::withSpinner(verbatimTextOutput("olssummary")),
                         p("Interpretation - one unit increase in X variable increases the outcome Y by coefficient's estimate (beta) units."),
                         verbatimTextOutput("ontr"),
                         h4("Correlation Table"),verbatimTextOutput("correlation"),
                         br() ),

                 tabPanel("Residuals Plot",
                          # h4('Missing Data Rows Count'),verbatimTextOutput("mscount"),
                          # (p('remove missing data variable(s) if any, or impute or drop rows - 
                          #    check  "Data Summary" tab and options in the left panel',style="color:black")),
                          h4('Mean Square Error of Input Data'),verbatimTextOutput("validationtr"),
                          htmlOutput("yout2"),
                          h4("Predicted Values vs. Actual Y (Input Data)"),shinycssloaders::withSpinner(plotOutput("resplot3",height = 800)),br(),
                          h4("Residual Errors vs. Predicted Values (Input Data)"), shinycssloaders::withSpinner(plotOutput("resplot2",height = 800)),br(),
                          h4("Residual Errors Plot (Input Data)"), shinycssloaders::withSpinner(plotOutput("resplot1",height = 800)), br(),
                          br()),
         
         
                tabPanel("Prediction Input Data", 
                        # h4('Mean Square Error of Test Data'),verbatimTextOutput("validationtr"),
                         h4("Download input data with predicted Y values"),
                         downloadButton('downloadData2', 'download predictions for input data'),
                         #h4("First 10 rows of predictions for input data"),
                         br(),br(),#h4('"Yhat" column is the predicted value.'),
                         htmlOutput("yout1"),
                         #verbatimTextOutput('inputprediction'),
                         dataTableOutput("inputprediction"),tags$head(tags$style("tfoot {display: table-header-group;}")),
                         br(),br(), #br(),br(),tableOutput("datatable")
                         ),
                
               # tabPanel("Corr-Hist",h4("Discriptive Analytics - Random 100 Input Data Rows"),plotOutput("heatmap1")),
               
                tabPanel("Prediction New Data",
                         h4("Upload new data for prediction, it should be have all selected X variables (csv file with header) "),
                         fileInput("filep", ""),
                         h4("New data"),
                         dataTableOutput("readdatap"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),br(),
                         h4('Number of rows and columns in selected input sub sample'),
                         verbatimTextOutput('inputobs'),
                         h4('Number of rows and columns in new data'),
                         verbatimTextOutput('newobs'),
                         verbatimTextOutput("validationnew"),
                         h4("Download new data with predictions"),
                         downloadButton('downloadData1', 'download predictions for new data'),
                         #h4("First 10 rows of predictions for new data (upload prediction data)"),
                         br(),br(),#h4('"Pred.___" column is the predicted Y values.'),
                         #verbatimTextOutput('prediction'),
                         htmlOutput("yout3"),
                         dataTableOutput("prediction"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),
                         br()),
                          
               
               tabPanel("Standardized OLS", 
                        h4("OLS on Standardized Data"), verbatimTextOutput("olssummarystd"),
                        h4("Summary Standardized Input Data (mean=0 variance=1)"),verbatimTextOutput("summarystd"),br())
               
               #,tabPanel("LASSO",h4("LASSO"), verbatimTextOutput("lassosummary"),br())
                        
                )
      ) 
    ) 
  )
