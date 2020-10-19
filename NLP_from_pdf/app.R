# LIBRARIES ----
# Shiny
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(shinyalert)

# Sentiment
library(syuzhet)

# PDF reading and scrapping:
library(pdftools)

# Images loading (for preview):
library(magick)

# Visualization
library(DT)
library(plotly)
library(viridis)
library(hrbrthemes)
library(magrittr)
library(networkD3)

# Core
library(data.table)
library(tidyverse)
library(tidyquant)
library(stringr)

# NLP
library(udpipe)


# UD pipe load models ----
ud_model_eng <- udpipe_load_model("english-ewt-ud-2.4-190531.udpipe")
ud_model_esp <- udpipe_load_model("spanish-gsd-ud-2.4-190531.udpipe")

# Progress messages (placed here because they look ugly if placed within code, and easier to change them later)
message_1 <- "Step 1of 2: We're annotating and analyzing the sentiment of your texts. That means we're understanding how words are related, which are the most common words per type, and of course, what are your texts sentiments and emotions"
message_2 <- "Step 2 of 2: Identifying emotions can take a bit more. We're identifying which words belongs to which emotions. When this message closes, go to Sentiment Analysis tab and select the values you want to check."

##############
### HEADER ###
##############


header <- dashboardHeaderPlus(
  
  title = tagList(
    span(class = "logo-lg", "Oxygen ML")
    ,img(src = "https://image.flaticon.com/icons/svg/119/119593.svg")
  )
  ,enable_rightsidebar = F
  
)

###############
### SIDEBAR ###
###############

sidebar <- dashboardSidebar(
  
  sidebarMenu(
    menuItem(
      text= "NLP and sentiment analysis"
      ,tabName = "nlp"
      ,icon = icon("comment")
    )
    ,menuItem(
      text = "Other Oxygen ML tools"
      ,tabName = "tools"
      ,icon = icon("wrench")
    )
  )
)


############
### BODY ###
############

body <- dashboardBody(
  
  
  tabItems(
    
    
    ##############
    ### PAGE 1 ###
    ##############
    
    tabItem(
      tabName = "nlp"
      ,fluidRow(
        
        boxPlus(title = "Upload and select data",
                width = 12,
                status = "success",
                style= "background-color: #F6F6F6",
                closable = F,
                collapsible = F,
                HTML("Select a pdf file to analyze using NLP and sentiment analysis."),
                hr(),
                
                # ---- File input
                fileInput(inputId = "pdf_input",label = "Select file:",accept = c('.pdf')),
                
                # ---- Select language
                
                HTML('Please select language for analysis:'),
                selectInput(inputId = "language", choices = c("english", "spanish"),"", ""),
                hr(),
                
                # ---- Submit for analysis
                actionButton(inputId = "analyze", "Analyze", class = "btn-primary", style="color: #ffff; background-color: #41585D; border-color: #404040")
        ),
        
        boxPlus(
          title = "Pages selection",
          width = 12,
          status = "info",
          style= "background-color: #F6F6F6",
          closable = F,
          collapsible = T,
          HTML('Please select pages to show results'),
          uiOutput('picker')
          
        ),
        
        boxPlus(
          title = "Sentiment analysis",
          width = 12,
          status = "info",
          style= "background-color: #F6F6F6",
          closable = F,
          collapsible = T,
          
          
          # ---- Texts sentiment plot
          
          div(
            class = "col-sm-12 panel",
            div(class = "panel-body", plotlyOutput(outputId = "plotly_sentiment", height = 400) %>% withSpinner(color = "#41585D"))
          ),
          
          # ---- Texts sentiment bar plot
          
          div(
            class = "col-sm-12 panel",
            div(class = "panel-body", plotlyOutput(outputId = "plotly_bars", height = 400) %>% withSpinner(color = "#41585D"))
          ),
          
          # ---- Texts sentiment violin plot
          
          div(
            class = "col-sm-12 panel",
            div(class = "panel-body", plotlyOutput(outputId = "plotly_violin", height = 400) %>% withSpinner(color = "#41585D"))
          )
          
        ),
        
        boxPlus(
          title = "Emotions analysis",
          width = 12,
          status = "info",
          style= "background-color: #F6F6F6",
          closable = F,
          collapsible = T,
          
          # ---- Emotions plot
          
          div(
            class = "col-sm-12 panel",
            selectInput(inputId = "var_emotions", choices = c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust", "negative", "positive"),"", ""),
            div(class = "panel-body", plotlyOutput(outputId = "plotly_emotions", height = 400) %>% withSpinner(color = "#41585D"))
          )
        ),
        
        boxPlus(
          title = "Texts analysis",
          width = 12,
          status = "info",
          style= "background-color: #F6F6F6",
          closable = F,
          collapsible = T,
          
          # ---- Common words plots
          
          div(
            class = "col-sm-12 panel",
            sliderInput("num_words", 
                        h5("Common words in text by type (select number of words to plot)"), 
                        value = 20, min = 3, max = 50),   
            
            tabsetPanel(type = "tabs",
                        tabPanel("Nouns", plotlyOutput("plotly_nouns") %>% withSpinner(color = "#41585D")),
                        tabPanel("Adjectives", plotlyOutput("plotly_adjectives") %>% withSpinner(color = "#41585D")),
                        tabPanel("Verbs", plotlyOutput("plotly_verbs") %>% withSpinner(color = "#41585D"))
            )
          ),
          
          div(
            class = "col-sm-12 panel",
            sliderInput("words_connections",
                        h5("Connection between words (select a number of words to plot"),
                        min = 25, max = 300, value = 70),
            textInput("search_term", label = "Search by word:"),
            simpleNetworkOutput("graphos") %>% withSpinner(color = "#41585D")
          )
        )
      )
    )
    
    
    ##############
    ### PAGE 2 ###
    ##############
    
    ,tabItem(
      tabName = "tools",
      fluidRow(
        widgetUserBox(
          width = 12,
          type = 2,
          subtitle = "Automated machine learning tool",
          background = F,
          src = "https://www.flaticon.com/svg/static/icons/svg/305/305098.svg",
          color = "gray",
          a("Go to the app", href="https://manu-am.shinyapps.io/Oxygen-ML/")
        ),
        widgetUserBox(
          width = 12,
          type = 2,
          subtitle = "NLP & sentiment analysis from Twitter",
          background = F,
          src = "https://www.flaticon.com/svg/static/icons/svg/733/733579.svg",
          color = "gray",
          a("Go to the app", href="https://juan-izurieta.shinyapps.io/NLP_from_Twitter/")
        ),
        widgetUserBox(
          width = 12,
          type = 2,
          subtitle = "NLP & sentiment analysis from csv or xls files",
          background = F,
          src = "https://www.flaticon.com/svg/static/icons/svg/180/180855.svg",
          color = "gray",
          a("Go to the app", href="https://juan-izurieta.shinyapps.io/NLP_from_files")
        )
      )
    )
  )
)


##########
### UI ###
##########

ui <- dashboardPagePlus(
  header, 
  sidebar, 
  body, 
  skin = "black",
  useShinyalert()
)


##############
### SERVER ###
##############

server <- function(input, output, session) {
  
  # Start instructions:
  shinyalert(
    title = "Welcome",
    text = str_glue("<b>HOW TO USE THE TOOL:</b><br>", 
                    "<br>",
                    "<b>Select a pdf file.</b> Maximum size is 5Mb.</br>",
                    "<b>Select the analysis language.</b> English and Spanish are supported.<br>", 
                    "<b>Click 'Analize'</b> and wait for your results."),
    size = "m", 
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = TRUE,
    type = "",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "Start",
    confirmButtonCol = "#41585D",
    timer = 0,
    imageUrl = "https://drive.google.com/uc?export=view&id=1cCWQwUMmrn7UDG0TvNWjpyjiJFH37wnr", 
    imageWidth = "200",
    animation = TRUE
  )
  
  # Limit pdf size ----
  
  options(shiny.maxRequestSize = 5*1024^2)
  
  # Setup Reactive Values ----
  rv <- reactiveValues()
  
  observeEvent(input$analyze, {
    
    # Load data and variable names ----
    
    req(input$pdf_input)
    
    rv$pdf <- input$pdf_input
    
    # Read Text from PDF
    rv$text_data <- pdf_text(rv$pdf$datapath)
    
    rv$paragraph_text_tbl <- tibble(
      # Page Text
      page_text = rv$text_data
    ) %>%
      rowid_to_column(var = "page_num") %>%
      
      # Paragraph Text
      mutate(paragraph_text = str_split(page_text, pattern = "\\.\n")) %>%
      select(-page_text) %>%
      unnest(paragraph_text) %>%
      rowid_to_column(var = "paragraph_num") %>%
      select(page_num, paragraph_num, paragraph_text)
    
    
  }, ignoreNULL = FALSE)
  
  # Populate list of values from group variable ----
  
  observeEvent(input$analyze, {
    
    req(rv$paragraph_text_tbl)
    
    reactiveCategories <- reactive({return(as_tibble(rv$paragraph_text_tbl) %>% select(page_num) %>% unique())})
    reactiveCategories2 <- reactive({return(as_tibble(rv$paragraph_text_tbl) %>% select(page_num) %>% unique())})
    
    output$picker <- renderUI({
      choices <- reactiveCategories()
      pickerInput('var_categories', choices = choices, multiple = TRUE, options = list('actions-box' = TRUE))
    })
    
    
  })
  
  
  # Prepare data after selections ----
  
  observeEvent(input$analyze, {
    
    shinyalert(
      title = "Analyzing your texts",
      text = "Please wait, we'll let you know when results are ready.",
      size = "m", 
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = TRUE,
      type = "",
      showConfirmButton = FALSE,
      showCancelButton = FALSE,
      timer = 0,
      imageUrl = "https://drive.google.com/uc?export=view&id=1StE42A3JStLVe4Rn-5eI-1E-WOkFGppc", 
      imageWidth = "200",
      animation = F,
      immediate = T
    )
    
    req(rv$paragraph_text_tbl)
    
    rv$selected_data <- rv$paragraph_text_tbl
    
    # # text cleaning:
    rv$selected_data$text_clean <- gsub("&amp", "", rv$selected_data$paragraph_text)
    rv$selected_data$text_clean <- gsub("&amp", "", rv$selected_data$text_clean)
    rv$selected_data$text_clean <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", rv$selected_data$text_clean)
    rv$selected_data$text_clean <- gsub("@\\w+", "", rv$selected_data$text_clean)
    rv$selected_data$text_clean <- gsub("[[:punct:]]", "", rv$selected_data$text_clean)
    rv$selected_data$text_clean <- gsub("[[:digit:]]", "", rv$selected_data$text_clean)
    rv$selected_data$text_clean <- gsub("http\\w+", "", rv$selected_data$text_clean)
    rv$selected_data$text_clean <- gsub("[ \t]{2,}", "", rv$selected_data$text_clean)
    rv$selected_data$text_clean <- gsub("^\\s+|\\s+$", "", rv$selected_data$text_clean)
    
    rv$selected_data$text_clean <- iconv(rv$selected_data$text_clean, "UTF-8", "ASCII", sub="")
    rv$selected_data$text_clean <- tolower(rv$selected_data$text_clean)
    
    # Make annotations ----
    
    if(input$language == "english"){
      ud_model <- ud_model_eng
    } else {
      ud_model <- ud_model_esp
    }
    
    rv$annotations <- withProgress(message = message_1, value = 0, {incProgress(1/2)
      as.data.frame(udpipe_annotate
                    (ud_model,
                      x = rv$selected_data$text_clean,
                      doc_id = rv$selected_data$paragraph_num
                    )
      )
    }
    )
    
    # Join data to allow subsetting annotations by var_group ----
    
    # Annotations assign an alphanumeric id to each row, we need to extract the digit part:
    
    rv$annotations_with_group <- subset(rv$annotations, upos %in% c("NOUN", "ADJ", "VERB"))
    
    regexp <- "[[:digit:]]+"
    
    rv$annotations_with_group$paragraph_num <- as.integer(str_extract(rv$annotations_with_group$doc_id, regexp))
    
    rv$annotations_with_group <- left_join(x=rv$annotations_with_group, y=select(rv$selected_data, c(paragraph_num, page_num)), by = "paragraph_num")
    
    
    ## Get sentiments
    rv$sentiments <- data.frame(paragraph_num = rv$selected_data$paragraph_num, 
                                text = rv$selected_data$paragraph_text,
                                page = rv$selected_data$page_num,
                                ave_sentiment = syuzhet::get_sentiment(rv$selected_data$text_clean), 
                                stringsAsFactors = F)
    
    
    ## Add a polarity variable grouping average sentiment
    rv$sentiments <- rv$sentiments %>%
      mutate(polarity = ifelse(ave_sentiment < 0, "Negative",
                               ifelse(ave_sentiment > 0, "Positive","Neutral")))
    
    ## Group texts by polarity
    rv$polarity <- rv$sentiments %>%
      group_by(polarity, page) %>%
      mutate(count = n())
    
    ## Generate emotions
    ## Get emotions
    rv$emotions <- withProgress(message = message_2, value = 0, {incProgress(3/4)
      
      get_nrc_sentiment(rv$selected_data$text_clean, language = input$language)
      
      })
    
    rv$emotions$paragraph_num <- rv$selected_data$paragraph_num
    rv$emotions <- left_join(rv$emotions, select(rv$selected_data, c(paragraph_num, paragraph_text)), by='paragraph_num')
    
    # Ready message:
    shinyalert(
      title = "Ready!",
      text = "Click outside this box, <b>select the pages to analyze</b> and scroll down to check all the results.<br> All progress icons by Jovie Brett Bardoles.",
      size = "m", 
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = TRUE,
      type = "",
      showConfirmButton = FALSE,
      showCancelButton = FALSE,
      timer = 0,
      imageUrl = "https://drive.google.com/uc?export=view&id=12HSqK0UOjC8fly2y4QoXiT-nMJVkID0E", 
      animation = F,
      immediate = T
    )
    
    
  })
  
  
  # Plotly sentiments ----
  observeEvent(input$var_categories, {
    
    output$plotly_sentiment <- renderPlotly({
      
      req(rv$sentiments)
      
      label_wrap <- label_wrap_gen(width = 60)
      
      rv$filtered_sentiment <- filter(rv$sentiments, page %in% input$var_categories)
      
      g <- ggplot(rv$filtered_sentiment, aes(x=paragraph_num, y=ave_sentiment, size = abs(ave_sentiment), color = ave_sentiment)) +
        geom_point(alpha=0.9, aes(text = str_glue("<b>Paragraph number</b>: {paragraph_num}
                                                  <b>Average sentiment:</b> {ave_sentiment}
                                                  <b>Text:</b> {label_wrap(text)}"))) + 
        geom_hline(aes(yintercept = mean(ave_sentiment)), color = "black") +
        geom_hline(aes(yintercept = median(ave_sentiment) + 1.96*IQR(ave_sentiment)), color = "#ffd633") +
        geom_hline(aes(yintercept = median(ave_sentiment) - 1.96*IQR(ave_sentiment)), color = "#600080") +
        scale_size(range = c(.2, 8)) +
        scale_color_viridis(option="plasma") +
        theme_ipsum() +
        theme(legend.position = "bottom") +
        ylab("Average sentiment of text") +
        xlab("Texts")
      
      ggplotly(g, tooltip = "text")
      
    })
    
    # Polarity bar plot ----
    
    output$plotly_bars <- renderPlotly({
  
      req(rv$polarity)
  
      rv$filtered_polarity <- filter(rv$polarity, page %in% input$var_categories)
  
      g <- rv$filtered_polarity %>% ggplot( aes(x=polarity, y=count, fill=polarity)) +
        geom_bar(stat="identity") + coord_flip() +
        scale_fill_viridis(discrete=T, option="plasma") +
        theme_ipsum() +
        theme(legend.position = "none") +
        ylab("Number of texts") +
        xlab("Polarity category")
  
      ggplotly(g)
    })
    # 
    # # Plotly sentiments polarity violin -----
    # 
    output$plotly_violin <- renderPlotly({
  
      req(rv$filtered_sentiment)
  
      g <- rv$filtered_sentiment %>% filter(polarity != 'Neutral') %>%
        ggplot( aes(x=polarity, y=ave_sentiment, fill=polarity)) +
        geom_violin() +
        scale_fill_viridis(discrete=T, option="plasma") +
        theme_ipsum() +
        theme(legend.position = "none") +
        ylab("Average sentiment of texts") +
        xlab("Polarity category")
  
      ggplotly(g)
  
    })
     
    # # Plotly emotions ----
     
    output$plotly_emotions <- renderPlotly({
    
      req(rv$emotions)
    
      label_wrap <- label_wrap_gen(width = 60)
      g <- rv$emotions %>% ggplot( aes(x=paragraph_num, y=rv$emotions[,c(input$var_emotions)], col=rv$emotions[,c(input$var_emotions)])) +
        geom_point(aes(text = str_glue("<b>Paragraph</b>: {paragraph_num}
                                        <b>Text:</b> {label_wrap(paragraph_text)}"))) +
        geom_smooth(method = "loess", color = "cornflowerblue") +
        scale_color_viridis(option="plasma")
    
      ggplotly(g, tooltip = "text")
    
    })
     
    # # Plotly common nouns ----
     
    output$plotly_nouns <- renderPlotly({
    
      stats_nouns <- subset(rv$annotations_with_group, upos %in% c("NOUN"))
      stats_nouns <- filter(stats_nouns, page_num %in% input$var_categories)
      stats_nouns <- txt_freq(stats_nouns$lemma)
      stats_nouns$key <- factor(stats_nouns$key, levels = rev(stats_nouns$key))
    
      g <- stats_nouns %>% top_n(input$num_words) %>% ggplot( aes(x=key, y=freq)) +
        geom_bar(stat="identity", fill="#69b3a2", alpha=.6, width=.4) +
        coord_flip() +
        xlab("") +
        theme_bw()
    
      ggplotly(g, tooltip = c("key", "freq"))
    
    })
    
    # # # Plotly common adjectives ----
    
    output$plotly_adjectives <- renderPlotly({
    
      stats_adjectives <- subset(rv$annotations_with_group, upos %in% c("ADJ"))
      stats_adjectives <- filter(stats_adjectives, page_num %in% input$var_categories)
      stats_adjectives <- txt_freq(stats_adjectives$lemma)
      stats_adjectives$key <- factor(stats_adjectives$key, levels = rev(stats_adjectives$key))
    
      g <- stats_adjectives %>% top_n(input$num_words) %>% ggplot( aes(x=key, y=freq)) +
        geom_bar(stat="identity", fill="#69b3a2", alpha=.6, width=.4) +
        coord_flip() +
        xlab("") +
        theme_bw()
    
      ggplotly(g, tooltip = c("key", "freq"))
    })
    
    # # # Plotly common verbs ----
    
    output$plotly_verbs <- renderPlotly({
    
      stats_verbs <- subset(rv$annotations_with_group, upos %in% c("VERB"))
      stats_verbs <- filter(stats_verbs, page_num %in% input$var_categories)
      stats_verbs <- txt_freq(stats_verbs$lemma)
      stats_verbs$key <- factor(stats_verbs$key, levels = rev(stats_verbs$key))
  
      g <- stats_verbs %>% top_n(input$num_words) %>% ggplot( aes(x=key, y=freq)) +
        geom_bar(stat="identity", fill="#69b3a2", alpha=.6, width=.4) +
        coord_flip() +
        xlab("") +
        theme_bw()
    
      ggplotly(g, tooltip = c("key", "freq"))
    })
    
    # Plot interactive words connections ----
    
    
    rv$filtered_annotations <- filter(rv$annotations_with_group, page_num %in% input$var_categories)
    rv$cooc <- cooccurrence(x=subset(rv$filtered_annotations, upos %in% c('NOUN', 'ADJ')),
                            term = "lemma",
                            group = c('doc_id', 'paragraph_id', 'sentence_id'),
                            skipgram = 3,
                            relevant = T
    )
    
    output$graphos <- renderSimpleNetwork({
    
      connections <- data.frame(from=rv$cooc$term1, to=rv$cooc$term2, cooc=rv$cooc$cooc)
    
      if(input$search_term == ""){
        connections <- connections
      } else{
        connections <- connections %>% filter_all(any_vars(. %in% input$search_term))
      }
    
      grapho <- connections %>% arrange(desc(cooc)) %>% top_n(input$words_connections)
      p <- simpleNetwork(grapho,
                         height="500px",
                         width="700px",
                         linkDistance = 50,
                         charge = -350,
                         fontSize = 14,
                         fontFamily = "arial",
                         nodeColour = "#69b3a2",
                         zoom = T)
      p
    })
    
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)




