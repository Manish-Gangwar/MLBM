library("shinyBS")
shinyUI(fluidPage(
  
  title = "Random Forest",
  titlePanel(title=div(img(src="isb.png",align='right'),"Random Forest")),
  sidebarPanel(
    tags$a(href="javascript:history.go(0)", 
           popify(tags$i(class="fa fa-refresh fa-1x"),
                  title = "", #Reload App", 
                  content = "click here to refresh the app",
                  placement = "right")),
    h4(p("Data Input")),
    fileInput("tr_data","Upload input data (csv file with header)",placeholder = "No File Selected"),
    h4(p("Data Selection")),
    uiOutput("y_ui"),
    uiOutput("x_ui"),
   # h4(p("Advance Options")),
    sliderInput("tr_per",
                label = "Set test sample percentage",
                min = 0,
                max = 40,
                value = 25,
                step=5),
    sliderInput("n_tree",
                label = "Number of trees",
                min = 50,
                max = 500,
                value = 50,
                step = 50),
    
  br()),
  mainPanel(
    # recommend review the syntax for tabsetPanel() & tabPanel() for better understanding
    # id argument is important in the tabsetPanel()
    # value argument is important in the tabPanle()
    tabsetPanel(
      tabPanel("Overview", value=1, 
               includeMarkdown("overview.md")
      ),
      tabPanel("Data Summary", br(),
               DT::dataTableOutput("samp"),
               hr(),
               h4("Data Summary of Selected X Variables"),
               verbatimTextOutput("data_str"),
               #shinycssloaders::withSpinner(plotOutput("corplot1")),br(),
               #h4("Missingness Map"),
               verbatimTextOutput("miss_plot")
              
              
      ),
      tabPanel("RF Results", br(),
               htmlOutput("yout"),
               htmlOutput("chry"),
               actionButton("apply","Train model"),
               h4("Model Summary"),
               helpText("be patient it may take a while to run multiple trees"),
               shinycssloaders::withSpinner(verbatimTextOutput("mod_sum")),
               
               #h4("Confusion Matrix (Train Set)"),
               #uiOutput("train_res"),
               
               #plotOutput('conf_train_plot'),
               #HTML('<button data-toggle="collapse" data-target="#demo">Detailed Result</button>'),
              # tags$div(id="demo",class="collapse",),
              h4("Training Data"), 
              verbatimTextOutput("conf_train"),
              htmlOutput("yout21"),
              h4("Test Data"), 
               #h4("Confuison Matrix (Test Set)"),
              # HTML('<button data-toggle="collapse" data-target="#demo1">Detailed Result</button>'),
              #uiOutput("test_res"),
              #plotOutput('conf_test_plot'),
              verbatimTextOutput("conf_test"),br(),
             h4("ROC Curve"),
             p("ROC curve is available for only binary classification problem"),
             plotOutput("roc"),

               #tags$div(id="demo1",class="collapse",)
              br() ),
      # tabPanel("RF Plots",value=3,
      #        #  h4('PCA plot'),
      #          plotOutput("pca_plot"),
      #         # h4("Error Rate Plot"),
      #          plotOutput("err_rate"),
      #         # h4("ROC-AUC Curve"),
      #          plotOutput("roc"),
      #         verbatimTextOutput("roc_val")
      # ),
      tabPanel("Variable Importance",value=3,
               #h4("No of nodes in trees"),
               #plotOutput("n_tree"),
               #h4("Variable Importance"),
               plotOutput("var_imp"),
               DT::dataTableOutput("var_imp_tb"),
               #verbatimTextOutput("roc_val"),
               br()),
      tabPanel("Prediction New Data",value=1,
               h4("Upload new data for prediction, it should have all selected X varaibles (csv file with header) "),
               fileInput("test_data","Upload prediction data (csv file with header)",placeholder = "No File Selected"),
               h4("New data"),
               DT::dataTableOutput("sampp"),br(),
               h4("Download new data with predictions"),
               downloadButton("download_pred", 'download predictions for new data'),br(),br(),
               DT::dataTableOutput("test_op"),
               
               
      ),
      id = "tabselected"
    )
  )
))

