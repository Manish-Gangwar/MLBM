if (!require(udpipe)) {install.packages("udpipe")};  library(udpipe)
if (!require(dplyr)) {install.packages("dplyr")};  library(dplyr)
if (!require(ggplot2)) {install.packages("ggplot2")};  library(ggplot2)
if (!require(stopwords)) {install.packages("stopwords")};  library(stopwords)
if (!require(shinyWidgets)) {install.packages("shinyWidgets")};  library(shinyWidgets)
if (!require(shinydashboard)) {install.packages("shinydashboard")};  library(shinydashboard)
if(!require(lattice)){install.packages('lattice')};library(lattice)
if (!require(shiny)) {install.packages("shiny")};  library(shiny)
if (!require(stringr)) {install.packages("stringr")};  library(stringr)
if (!require(tm)) {install.packages("tm")};  library(tm)
if (!require(markdown)) {install.packages("markdown")};  library(markdown)
if (!require(quanteda)) {install.packages("quanteda")};  library(quanteda)
if (!require(tools)) {install.packages("tools")};  library(tools)
if (!require(ggraph)) {install.packages("ggraph")};  library(ggraph)
if (!require(igraph)) {install.packages("igraph")};  library(igraph)
if (!require(DT)) {install.packages("DT")};  library(DT)
if (!require("shinycssloaders")) {install.packages("shinycssloaders")};  library(shinycssloaders)
if (!require("shinymaterial")) {install.packages("shinymaterial")};  library(shinymaterial)
#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tools)
library(shiny)
options(shiny.maxRequestSize=100*1024^2)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    dataset <- reactive({
        if (is.null(input$file)) {return(NULL)}
        else {
            if(file_ext(input$file$datapath)=="txt"){
                Document = readLines(input$file$datapath)
                Doc.id=seq(1:length(Document))
                calib=data.frame(Doc.id,Document)
                return(calib)
            }
            else{
                Document = read.csv(input$file$datapath ,header=TRUE, sep = ",", stringsAsFactors = F,encoding="UTF-8")
                #Document[,1] <- str_to_title(Document[,1])
               # Document[,1] <- make.names(Document[,1], unique=TRUE)
               # Document[,1] <- tolower(Document[,1])
               # Document[,1] <- str_replace_all(Document[,1],"\\.","_")
                #Document<-Document[complete.cases(Document), ]
                Document$Doc.id = seq(1:nrow(Document))
               # Document <- Document[!(duplicated(Document[,1])), ]
               # rownames(Document) <- Document[,1]
                return(Document)
                }
            }
    })
    
    
    text_summ <- reactive({summary(quanteda::corpus(dataset()[,input$y]))})
    quant_mod <- reactive({quanteda::corpus(dataset()[,input$y])})
    
    output$text <- renderUI({
        req(input$file$datapath)
        str1 <- paste("Total no of documets:", nrow(dataset()))
        str2 <- paste("range of sentences per document: ",min(text_summ()$Sentences),"-",max(text_summ()$Sentences))
        #str3 <- paste("Maximum number of sentence: ",)
        str4 <- paste("Average number of sentences per document: ",mean(text_summ()$Sentences))
        HTML(paste(str1, str2,str4, sep = '<br/>'))
    })

    output$text2 <- renderUI({
        req(input$file$datapath)
        str2 <- paste("range of words per document: ",min(text_summ()$Tokens),'-',max(text_summ()$Tokens))
        #str3 <- paste("range of words per document:: ",max(text_summ()$Tokens))
        str4 <- paste("Average number of word: ",mean(text_summ()$Tokens))
        HTML(paste(str2,str4, sep = '<br/>'))
    })
    
    
    
    
    
    cols <- reactive({colnames(dataset())})
    
    output$pre_proc1 <- renderUI({if(is.null(dataset())){
        return(NULL)
    }else{
        
        checkboxInput('html',"Remove HTML tags",value = TRUE)
        
    }
    })
    
    output$pre_proc2 <- renderUI({if(is.null(dataset())){
        return(NULL)
    }else{
        checkboxInput('num',"Remove Numbers",value = TRUE)
        
    }
    })
    
    
    output$pre_proc3 <- renderUI({if(is.null(dataset())){
        return(NULL)
    }else{
        checkboxInput('Id077',"Lemmatize",value = TRUE)
        
    }
    })
    
    y_col <- reactive({if(is.null(dataset())){
        return(NULL)
    }else{
        x <- match(input$x,cols())
        y_col <- cols()[-x]
        return(y_col)
    }
    })
    
    output$id_var <- renderUI({
        print(cols())
        selectInput("x","Select ID Column",choices = cols())
    })
    
    
    output$doc_var <- renderUI({
        selectInput("y","Select Text Column",choices = y_col())
    })
    
    dataset1 <- reactive({
        if (is.null(input$file)) { return(NULL) }
        else{
           cleaned_col <- text.clean(dataset()[,input$y],
                                              html_tags = input$html,
                                              numbers = input$num)
        }
        return(cleaned_col)
    })
    
    output$wc <- renderPlot({
        req(input$file$datapath)
        ds <- dataset()
        ds[,'cleaned_col'] <- dataset1()
        
        ds_words <- ds %>%
            unnest_tokens(word, cleaned_col) %>%
            anti_join(stop_words)
        
        count_word <- ds_words %>%
            count(word, sort = TRUE)
        
        # define a nice color palette
        pal <- brewer.pal(8,"Dark2")
        
        # plot the 50 most common words
        count_word%>% 
            with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors=pal))
    })
    
   dtm <- reactive({
        
       x <- anotated_data()
       #x[,doc_id] <- unique_identifier(x[,input$x])
       dtf <- subset(x, upos %in% input$pos_select)
       dtf <- document_term_frequencies(dtf,document = "doc_id",term = "lemma")
       dtm <- document_term_matrix(x = dtf)
       dtm1 <- as.matrix(dtm)
       dtm2 <- data.frame(Doc.Id = row.names(dtm1),dtm1)
      # print(dtm1[1:5,1:5])
       return(dtm2)
    })
    
    #
    output$dtm_text <- renderText({
        if (is.null(input$file) | is.null(input$model)) { return(NULL) }
        else{
            size = dim(dtm())
            dtm_size = paste("DTM has  ", size[1],"(rows)"," X ", size[2],"(columns)","")
        }
       
    })
    
    output$download_dtm <- downloadHandler(
        filename = function() {paste(str_split(input$file$name,"\\.")[[1]][1],"_dtm.csv",collapse = "") },
        content = function(file) {
            new_dtm <- dtm()
            write.csv(new_dtm, file, row.names=F)
        }
    )
    
    
    anotated_data <- reactive({
        if (is.null(input$model)) {return(NULL)}
        else{
            model <- udpipe_load_model(file = input$model$datapath)
            x <- udpipe_annotate(model, x = dataset1(),doc_id = dataset()[,input$x])
            x <- as.data.frame(x)
           # updateProgressBar(session = session, id = "pb4", value = input$slider)
            
        }
        return(x)
        
    })
    
    
    # Select variables:
    output$pos_select_ui <- renderUI({
        if (is.null(input$file)) { return(NULL) }
        else{
            
            # radioButtons("pos_select", "Display most frequent",
            #                    choiceNames = 
            #                        list('Noun', 'Verb','Adjective', 'Adverb',"Proper Noun"),
            #                    choiceValues =
            #                        list("NOUN", "VERB", "ADJ", "ADV","NNPS")
            # )
            
            checkboxGroupInput("pos_select",
                               "Choose most frequent",
                               choiceNames = list("Noun","Verb","Adjective","Adverb","Proper Noun"),
                               choiceValues = list("NOUN","VERB","ADJ","ADV","NNPS"),selected = "NOUN")
        }
    })
    
    stopw <- reactive({
        stpw <- unlist(strsplit(input$stopw,","))
        return(stpw)
    })

    output$pos_plot <- renderPlot({
        
        if (is.null(input$file) | is.null(input$model)) { return(NULL) }
        
        else{
            #if(input$pos_select=="NNPS"){
               # stats <- subset(anotated_data(), xpos %in% input$pos_select)
               
           # }else{
                choices <- paste(input$pos_select,collapse = ",")
                stats <- subset(anotated_data(), upos %in% input$pos_select)
                
           # }
            
            if(input$Id077){
                stats <- txt_freq(stats$lemma)
            }else{
                stats <- txt_freq(stats$token)   
            }
            
            
            if(length(stopw()!=0)){
            stats <- stats %>%
                    select(key, freq, freq_pct) %>% 
                    filter(!key %in% stopw())
            }
            
            stats$key <- factor(stats$key, levels = rev(stats$key))
            
            barchart(key ~ freq, data = head(stats, input$pos_slider), col = "cadetblue", 
                     main = paste0("Top 20 Most occurring"," ",choices), xlab = "Freq")
        }
        
    })
    

    #-----code for document level analysis---#
    
    output$doc_sel <- renderUI({
        choices <- as.numeric(unique(anotated_data()$doc_id))
        selectInput('d_sel',label = "Select Document",choices = choices,multiple = F)
    })
    
    output$sent_sel <- renderUI({
            
            req(input$d_sel)
            t <- anotated_data()%>%filter(doc_id==as.numeric(input$d_sel))
            max_sent <- max(t$sentence_id)
            selectInput('s_sel',label = "Select sentence",choices = 1:max_sent,multiple = F)
    })
    
    output$sel_sent1 <- renderText({
        req(input$d_sel)
        temp <- anotated_data()%>%filter(doc_id==input$d_sel & sentence_id==input$s_sel)
        unique(temp$sentence)
    })
    
    output$dep_tre <- renderPlot({
        req(input$d_sel)
        sent_annotation(anotated_data(),doc_num = input$d_sel,sent_num = input$s_sel,size=input$size_sel)
    })
    
    
    
    #--------#
    
    
    output$a_table <- renderDataTable({
        if (is.null(input$file) | is.null(input$model)) { return(NULL) }
        else{
            summ_table <- as.data.frame(table(anotated_data()$upos))
            colnames(summ_table) <- c("POS TAG", "Count")
            summ_table <- replace_abb(summ_table)
            return(summ_table)
        }
        
    },options = list(pageLength = 5))
    
    
    
    output$word_cloud <- renderPlot({
        
        if (is.null(input$file) | is.null(input$model)) { return(NULL) }
        else{
            if(input$pos_select=="NNPS"){
                stats <- subset(anotated_data(), xpos %in% input$pos_select)
            }else{
                stats <- subset(anotated_data(), upos %in% input$pos_select)
                
            }
            if(input$Id077){
                stats <- txt_freq(stats$lemma)
            }else{
                stats <- txt_freq(stats$token)   
            }
            
            
            if(length(stopw()!=0)){
                stats <- stats %>%
                    select(key, freq, freq_pct) %>% 
                    filter(!key %in% stopw())
            }
            
            stats$key <- factor(stats$key, levels = rev(stats$key))
            
            wordcloud(words = stats$key, 
                      freq = stats$freq, 
                      min.freq = input$min_freq, 
                      max.words = input$max_word,
                      random.order = FALSE, 
                      colors = brewer.pal(6, "Dark2"),
                      scale=c(5,0.5))
        }
        
    })
    
    #---Keyword tab code
    output$key_plot <- renderPlot({
        if (is.null(input$file) | is.null(input$model)) { return(NULL) }
        else{
            if(input$key_algo=="RAKE"){
                stats <- keywords_rake(x = anotated_data(), term = "lemma", group = "doc_id", 
                                       relevant = anotated_data()$upos %in% c("NOUN", "ADJ"))
                stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
                p1 <- barchart(key ~ rake, data = head(subset(stats, freq > 3), input$key_slider), col = "red", 
                               main = "Top Keywords identified by RAKE", 
                               xlab = "Rake")
                print(p1)
            }
            if(input$key_algo=="Noun-Verb Phrase"){
                phrase_tags <- as_phrasemachine(anotated_data()$upos, type = "upos")
                stats <- keywords_phrases(x = phrase_tags, term = tolower(anotated_data()$token),
                                          pattern = "(A|N)*N(P+D*(A|N)*N)*",
                                          is_regex = TRUE, detailed = FALSE)
                stats <- subset(stats, ngram > 1 & freq > 3)
                stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
                p2<- barchart(key ~ freq, data = head(stats, input$key_slider), col = "magenta",
                              main = "Keywords - simple noun phrases", xlab = "Frequency")
                print(p2)
                
            }
        }
        
    })
    
})
