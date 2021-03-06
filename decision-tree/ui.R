###########################################################
#   Decision Tree App (ui)           #
###########################################################
library("visNetwork")
library("shinyBS")
shinyUI(
  fluidPage(
    
  #  titlePanel("Classification and Regression Tree"),
    headerPanel(title=div(img(src="isb.png",align = "right"), h2("Decision Tree App", style="bold")),windowTitle	='Decision Tree'),
    
    sidebarLayout(
      
      sidebarPanel(
        tags$a(href="javascript:history.go(0)", 
               popify(tags$i(class="fa fa-refresh fa-1x"),
                      title = "", #Reload App", 
                      content = "click here to refresh the app",
                      placement = "right")),
        h4(p("Data Input")),
        fileInput("file", "Upload data (csv file)"),
        # h4(p("Select Response Variable")),
        sliderInput('cp','Set complexity parameter',0,0.1,0),
        h4(p(" Data Selection")),
        htmlOutput("yvarselect"),
        htmlOutput("xvarselect"),
      #  submitButton(text = "Apply Changes", icon("refresh")),br(),
        htmlOutput("fyvarselect"),
        htmlOutput("fxvarselect"),
        htmlOutput("samsel"),
        htmlOutput("imputemiss"),
      #h4(p("Advance Options")),
      sliderInput('sample','Set test sample percentage',10,40,25),
       numericInput('numout','Input new number to draw new set of training and test data',5898),
      actionButton(inputId = "apply",label = "Apply Changes", icon("refresh")),
    #   fileInput("filep", "Upload new data for prediction (csv file)")
      ),   # end of sidebar panel
      
    mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Overview",
                             h4(p("How to use this application")),
                             p("This application requires a data input from the user. To do so, click on the 'Browse' (in the panel on the left) and upload the csv data input file.
                           Note that this application can read only csv file (comma delimited file), so if you don't have csv input data file, first convert your data in csv format 
                           and then proceed. Make sure you have top row as variable names.",align="justify"),
                             p("Once csv file is uploaded successfully, variables in the data file will reflect in the 'Data Selection' panel on the left. Now you can select 
                            dependent variable (Y Variable) from drop-down menu. By default all other remaining variables will be selected as explanatory variables (X variables). 
                              If you want to drop any variable from explanatory variables, just uncheck that variable and it will be dropped from the model.
                            ",align="justify"),
                             p('You can adjust the test sample proportion from the slider in the panel on the left. Test sample will be randomly selected from the input data set. 
                               If you have a similar data set on which you want to make the prediction based on decision tree, You can upload that data set in the "Prediction New Data" tab. 
                               Please note that prediction data should have all explanatory variables similar to model data.',align="justify"),
                             p('You can also adjust the complexity parameter in decision tree model to control size of the tree. A decision tree is a decision support tool that uses a tree-like model of decisions and their possible consequences, including chance event outcomes, resource costs, and utility. It is one way to display an algorithm that only contains conditional control statements.'),
  
                             h4(tags$a(href="https://en.wikipedia.org/wiki/Decision_tree", "Note on Decison Tree (Wikipedia)",target="_blank")),
                             
                             #h4(p("Download Sample Input File")),
                             downloadButton('downloadData', 'download sample data'),
                             br(), br(),
                             br(),
                             
                          #   p("*Please note that download will not work with RStudio interface. Download will work only in web-browsers.")
                            ), # close Overview tab
                            
                    #tabPanel("Data Summary",verbatimTextOutput('summarydata')),
                    tabPanel("Data Summary",
                             h4("Review Input Data"), 
                             dataTableOutput("readdatat"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),
                             #verbatimTextOutput("head"),#verbatimTextOutput("tail"),
                            h4("Data Summary of Selected Y and X Variables"),
                            htmlOutput("imout"),
                            shinycssloaders::withSpinner(verbatimTextOutput("summarydata")),
                            verbatimTextOutput('screen_summary'),
                            h4("Factor Variables in Training Data"),
                            verbatimTextOutput('factrain'),
                            h4("Factor Variables in Test Data"),
                            verbatimTextOutput('factest'),
                            h4("Missing Data Rows (Sample)"),
                            verbatimTextOutput("missing"),
                            br()),
                    # tabPanel("Data Visualization",
                    #          #br(),
                    #          #h4("Select variable for er's outlier test"),
                    #          h4("Be patient generating plots"),
                    #        #  plotOutput("dens"),
                    #          h4("Histograms"),
                    #          shinycssloaders::withSpinner(plotOutput("hist")),br(),
                    #           br(),
                    #          
                    #          h4("Correlation Table"),verbatimTextOutput("correlation"),
                    #          h4("Correlation Plots"),
                    #          htmlOutput("misswarn"),
                    #          #(p('remove missing data variable(s) if any, or impute or drop rows - check  "Data Summary" tab and options in the panel on the left',style="color:black")),
                    #          plotOutput("corplot1"),
                    #          br()),
                    tabPanel("Model Output",
                             br(),
                             htmlOutput("yout"),
                             h4('Variable importance'),
                             p("If you see an error message, some factor (categorical) variable in the test data 
                               has a new level that is not present in the training data; check 'Data Summary' tab"),
                             verbatimTextOutput('imp'),
                             h4('Number of Rows and Columns in Training Data'),
                             shinycssloaders::withSpinner(verbatimTextOutput('trainobs')),
                             htmlOutput("warning1"),
                             h4('Model Accuracy/Error of Training Data'),
                             verbatimTextOutput("validation"),
                             h4('Number of Rows and Columns in Test Data'),
                             verbatimTextOutput('testobs'),
                             h4('Model Accuracy/Error of Test Data'),
                             verbatimTextOutput("validation1"),
                             #h4("Download Input Data with Predictions"),
                             #downloadButton('downloadData0', 'Download predictions'),
                             h4('Model Result Summary'),
                             verbatimTextOutput("fcptable"),
                            # verbatimTextOutput("results"),
                            # h4("First 10 predictions of train data"),
                            # p('"Yhat" column is the predicted value.'),
                            # verbatimTextOutput('predictionorg'),
                            h4("Correlation Table"),verbatimTextOutput("correlation"),
                             br()
                             ),
                    tabPanel('Summary of Splits',#h5('Be patient model may take some time to finish estimation'),
                             shinycssloaders::withSpinner(verbatimTextOutput("mod_sum")),
                             #verbatimTextOutput("summary"),
                             br()),
                    
                    tabPanel("Decision Tree",#h5('Be patient model may take some time to finish estimation'),
                            #br(),
                            #h4("Missing Data Rows Count"),verbatimTextOutput("mscount"),
                            #(p('Remove missing data variable(s) if any - check  "Data Summary" tab',style="color:red")), 
                           
                            # helpText('Is outcome variable (Y) a factor (catregorical)? If yes, please, make sure that it is check-marked as a factor variable in the left panel.'),

                            h5("To optimally prune the tree, find the row with the lowest 'xerror' value 
                               under 'Model Results Summary' in the 'Model Output' tab."),
                            h5("Look at the corresponding CP value (corresponding to the lowest 'xerror' value) and
                              set the complexity parameter (in the left panel) close to that CP value."),
                            # (p('Is Y a factor (catregorical) variable? 
                            #       If yes, please, make sure it is check-marked as a factor variable 
                            #     in the left panel.',style="color:darkblue;text-decoration: underline")),
                            htmlOutput("warning"),
                            shinycssloaders::withSpinner(plotOutput("plot3",height = 1600)),
                            br(),br(), br()),
                  tabPanel("Decision Tree (Interactive)",
                           h4('Click on the node to see the decison path or place a cursor on the node 
                              to see details. Use scroll wheel to zoom in or out.'),
                           h5("To optimally prune the tree, find the row with the lowest 'xerror' value 
                               under 'Model Results Summary' in the 'Model Output' tab."),
                           h5("Look at the corresponding CP value (corresponding to the lowest 'xerror' value) and
                              set the complexity parameter (in the left panel) close to that CP value."),
                           shinycssloaders::withSpinner(visNetworkOutput("plot33", height=850)), 
                            br(),br()),
                  
                       #      h4('Visualize cross-validation results'),
                        #     plotOutput("plot1",height = 800)
          
               #     tabPanel("Node labels",plotOutput("plot2",height = 600, width = 850),
                #             #h4("First column is assigned node number for each obsrvn in model training data"),
                 #            #verbatimTextOutput("nodesout1"),
                  #           dataTableOutput("nodesout"),
                   #          br(),
                    #         h4("Download nodes data from model training data"),
                     #        br(),
                      #       downloadButton('downloadData3','Download nodes data (Works only in browser)')
                       #      ),
                    #tabPanel("Variable",verbatimTextOutput('imp')),
               
                tabPanel("Prediction Input Data",
                         h4('Average Model Accuracy/Error of Input Data'),
                         verbatimTextOutput("validation2"),
                        # verbatimTextOutput("validation2rf"),
                         #(p('Is your outcome (Y) variable factor (catregorical)? 
                         #         If yes, please, make sure that it is check-marked as a factor variable 
                        #        in the left panel.',style="color:red")),

                         h4("Download Input data with predictions"),
                         htmlOutput("yout1"),
                        downloadButton('downloadData0', 'download predictions for input data'),
                        #h4("First 10 rows of predictions for new data (upload prediction data)"),
                        br(),br(),#h4('"Yhat" column is the predicted value.'),
                        #verbatimTextOutput('prediction'),
                        dataTableOutput("predictionorg"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),br()
                ),
               
                    tabPanel("Prediction New Data",
                             h4("Upload new data for prediction, it should have all selected X variables (csv file with header)"),
                             fileInput("filep",""),
                             h4("New data"),
                             dataTableOutput("readdatap"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),br(),
                             h4('Number of rows and columns in selected input sub sample'),
                             verbatimTextOutput('inputobs'),
                             h4('Number of rows and columns in new data'),
                             verbatimTextOutput('newobs'),
                             verbatimTextOutput("validation3"),
                             h4("Download new data with predictions"),
                             downloadButton('downloadData1', 'download predictions for new data'),
                             #h4("First 10 rows of predictions for new data (upload prediction data)"),
                             br(),br(),#h4('"Yhat" column is the predicted value.'),
                             #verbatimTextOutput('prediction'),
                             htmlOutput("yout3"),
                             dataTableOutput("prediction"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),br()
                             
                             # #h4('Number of Rows and Columns in New Prediction Data'),
                             # #verbatimTextOutput('predictobs'),
                             # h4("First 10 rows of new data with predictions"),
                             # p('"Yhat" column is the predicted value.'),
                             # verbatimTextOutput('prediction'),
                             # h4("Download new data with predictions"),
                             # downloadButton('downloadData1', 'download predictions for new data')
                             )
                             
        ) # end of tabsetPanel
      )# end of main panel
    ) # end of sidebarLayout
  )  # end if fluidPage
) # end of UI



