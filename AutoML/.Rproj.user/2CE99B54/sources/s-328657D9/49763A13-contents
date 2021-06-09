library(dplyr)
shinyUI(fluidPage(
    
    title = "Auto ML for Supervised Learning",
    titlePanel(title=div(img(src="logo.png",align='right'),"Auto ML for Supervised Learning")),
    sidebarPanel(
        source("scripts//uiInput.R",local = TRUE)[[1]], 
        conditionalPanel(condition = "input.tabselected==3",
                         
        ),
        conditionalPanel(condition="input.tabselected==3",
                         uiOutput("y_ui"),
                         uiOutput("x_ui"),
                         # radioButtons("task","Select task",
                         #              choices = c("Classification" = 'clf',
                         #                           "Regression" = "reg")),
                         radioButtons("task","Select Model",
                                      choices = c("Classification" = "clf",
                                                  "Regression" = "reg")),
                         sliderInput("tr_per",
                                     label = "Percentage of Training Data",
                                     min = 0,
                                     max = 1,
                                     value = 0.7,
                                     step = 0.05),
                         sliderInput("max_tr_time",
                                     label = "Maximum Training Time (in Secs)",
                                     min = 10,
                                     max = 100,
                                     value = 30,
                                     step = 5),
                         actionButton("apply","Train model")
        ),
        
        
    ),
    mainPanel(
        # recommend review the syntax for tabsetPanel() & tabPanel() for better understanding
        # id argument is important in the tabsetPanel()
        # value argument is important in the tabPanle()
        tabsetPanel(
            tabPanel("Overview & Example Dataset", value=1, 
                     includeMarkdown("overview.md")
            ),
            tabPanel("Data Summary", value=3,
                     DT::dataTableOutput("samp"),
                     hr(),
                     h4("Data Structure"),
                     verbatimTextOutput("data_str"),
                     #h4("Data Summary table"),
                     #DT::dataTableOutput("data_summ_tb"),
                     h4("Missingness Map"),
                     plotOutput("miss_plot")
                     
            ),
            tabPanel("Results Leaderboard", value=3,
                     #h3("H2o Flow"),
                     uiOutput("h2o_flow"),
                     h4("Leaderboard"),
                     helpText("Training may take a while, upto a minute"),
                     hr(),
                     DT::dataTableOutput("lb_table"),
                     p("Leaderboard AUC scores"),
                     plotOutput("lb_auc_plot"),
                     
                     
            ),
            tabPanel("Best Model",value=3,
                     h4('Top Model recommended by Auto-ML'),
                     verbatimTextOutput("top_model"),
                     #h4("i^th model from leaderboard"),
                     uiOutput("ith_model"),
                     verbatimTextOutput("ith_model_op")
            ),
            
            tabPanel("Prediction Output",value=3,
                     helpText("Note: Please upload test data with same features in train dataset"),
                     DT::dataTableOutput("test_op"),
                     downloadButton("download_pred")
                     
            ),
            id = "tabselected"
        )
    )
))

