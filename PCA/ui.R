if (!require('plotly')){install.packages("plotly")}; library("plotly")
if (!require('dplyr')){install.packages("dplyr")}; library("dplyr")
if (!require('shinyBS')){install.packages("shinyBS")}; library("shinyBS")
shinyUI(fluidPage(
    
    title = "PCA App",
    titlePanel(title=div(img(src="logo.png",align='right'),"Principal Component Analysis App")),
    sidebarPanel(
        tags$a(href="javascript:history.go(0)", 
               popify(tags$i(class="fa fa-refresh fa-1x"),
                      title = "", #Reload App", 
                      content = "click here to refresh the app",
                      placement = "right")),
       # conditionalPanel(condition = "input.tabselected==1",
                         helpText("Note: first column of the input data must be an unique observation id",style="color:darkblue"),
                         fileInput("file", "Upload Input File"),
                         uiOutput("colList"),
                                br() ),
        #conditionalPanel(condition="input.tabselected==3",),
        # ),
    
    
    
    mainPanel(

        tabsetPanel(
            tabPanel("Overview", value=1, 
                     includeMarkdown("overview.md")
            ),
            tabPanel("Data Summary", value=1,
                    # h4("Data Dimensions"),
                    # verbatimTextOutput("dim"),
                    # hr(),
                     h4("Review Input Dataset"),
                     DT::dataTableOutput("samp_data"),
                     hr(),
                    h4("Selected Input Data Summary for PCA"),
                    htmlOutput("yout"),
                    verbatimTextOutput("summ"),
                    # h4("Missingness Map"),
                     plotOutput("miss_plot")
                     
            ),
            tabPanel("Variance Explained", value=1,
                    plotlyOutput("var_exp"),
                    plotlyOutput("cum_var_exp") 
                     
            ),
            
            tabPanel("PCA Loadings",value=1,br(),
                     uiOutput("dimk"),
                     DT::dataTableOutput("loadings_dt"),
                     plotlyOutput("loading_hm")
            ),
            
            tabPanel("PCA Scores",value=1,br(),
                     downloadButton('downloadDataX', 'download pca scores'), br(),br(),
                     DT::dataTableOutput("scores_dt"),
                    # plotlyOutput("bi_plot")
            ),
            

            id = "tabselected"
        )
    )
))

