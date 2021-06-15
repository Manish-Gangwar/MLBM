if (!require('plotly')){install.packages("plotly")}; library("plotly")
if (!require('dplyr')){install.packages("dplyr")}; library("dplyr")
shinyUI(fluidPage(
    
    title = "PCA App",
    titlePanel(title=div(img(src="logo.png",align='right'),"PCA App")),
    sidebarPanel(
        
        conditionalPanel(condition = "input.tabselected==1",
                         p("First column must be observation id and data should be all numeric"),
                         fileInput("file", "Upload Input File"),
                         numericInput("k","Select number of components",min = 2,max=50,value=2)
        ),
        conditionalPanel(condition="input.tabselected==3",
                         
        ),
        
        
    ),
    mainPanel(

        tabsetPanel(
            tabPanel("Overview & Example Dataset", value=1, 
                     includeMarkdown("overview.md")
            ),
            tabPanel("Data Summary", value=1,
                    # h4("Data Dimensions"),
                    # verbatimTextOutput("dim"),
                     hr(),
                     h4("Review Input Dataset"),
                     DT::dataTableOutput("samp_data"),
                     hr(),
                    # h4("Missingness Map"),
                    # plotOutput("miss_plot")
                     
            ),
            tabPanel("Variance Explained", value=1,
                    plotlyOutput("var_exp"),
                    plotlyOutput("cum_var_exp") 
                     
            ),
            tabPanel("PCA Scores",value=1,
                     DT::dataTableOutput("scores_dt"),
                     plotlyOutput("bi_plot")
            ),
            
            tabPanel("PCA Loadings",value=1,
                    DT::dataTableOutput("loadings_dt"),
                    plotlyOutput("loading_hm")
            ),
            id = "tabselected"
        )
    )
))

