
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

# Visualization
library(DT)
library(plotly)
library(viridis)
library(hrbrthemes)
library(magrittr)
library(networkD3)

# Animation
library(gganimate)
library(gifski)

# Core
library(data.table)
library(tidyverse)
library(tidyquant)
library(stringr)

# NLP
library(udpipe)

# Using Twitter API
library(rtweet)

# Twitter authentication via web browser ----
token <- readRDS("token.rds")
bearer_token(token)

# UD pipe load models ----
ud_model_eng <- udpipe_load_model("english-ewt-ud-2.4-190531.udpipe")
ud_model_esp <- udpipe_load_model("spanish-gsd-ud-2.4-190531.udpipe")

# Progress messages ----
message_1 <- "Step 1 of 3: We're connecting to Twitter and retrieving your tweets"
message_2 <- "Step 2 of 3: We're mapping emotions. We're searching each word of your tweets in emotion-defined lexicons, so we can categorize tweets by predominant emotions in words."
message_3 <- "Last step: We're annotating and analyzing the sentiment of your tweets. That means we're understanding how words are related, which are the most common words per type, and of course, what are your tweets sentiments"

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
  width=280
  ,sidebarMenu(
    menuItem(
      text= "Twitter NLP & sentiment analysis"
      ,tabName = "data"
      ,icon = icon("heart")
    )
    ,menuItem(
      text= "Other Oxygen ML tools"
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
      tabName = "data"
      ,fluidRow(
        
        boxPlus(title = "Insert topic or hashtag",
                width = 12,
                status = "success",
                style= "background-color: #F6F6F6",
                closable = F,
                collapsible = F,
                HTML("Insert a hashtag or topic, select a number of tweets and a language for analysis"),
                hr(),
                
                # ---- Query input
                textInput(inputId = "query", label = "Topic or Hashtag"),
                sliderInput(
                  inputId = "n_tweets",
                  label   = "Number of tweets to retrieve:",
                  min     = 1,
                  max     = 1500,
                  value   = 500),
                
                # ---- Select language
                
                h5('Please select language for analysis:'),
                selectInput(inputId = "language", choices = c("english", "spanish"),"", ""),
                
                actionButton(inputId = "submit", "Search and analyze", class = "btn-primary", style="color: #ffff; background-color: #41585D; border-color: #404040"),
                hr()
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
            h5("Sentiment in tweets"),
            div(class = "panel-body", plotlyOutput(outputId = "plotly_sentiment", height = 400) %>% withSpinner(color = "#41585D"))
          ),
          
          # ---- Tweets table 
          div(
            class = "col-sm-12 panel",
            h5("Most retweeted tweets and their sentiment"),
            div(class = "panel-body", dataTableOutput(outputId = "DT_tweets", height = 400) %>% withSpinner(color = "#41585D"))
          ),
          
          # ---- Texts sentiment bar plot
          
          div(
            class = "col-sm-12 panel",
            h5("General sentiment of tweets"),
            div(class = "panel-body", plotlyOutput(outputId = "plotly_bars", height = 400) %>% withSpinner(color = "#41585D"))
          ),
          
          # ---- Texts sentiment violin plot
          
          div(
            class = "col-sm-12 panel",
            h5("General sentiment of tweets, a violin plot"),
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
        # widgetUserBox(
        #   width = 12,
        #   type = 2,
        #   subtitle = "NLP & sentiment analysis from Twitter",
        #   background = F,
        #   src = "https://www.flaticon.com/svg/static/icons/svg/733/733579.svg",
        #   color = "gray",
        #   a("Go to the app", href="https://manu-am.shinyapps.io/Oxygen-ML/")
        # ),
        widgetUserBox(
          width = 12,
          type = 2,
          subtitle = "NLP & sentiment analysis from PDFs",
          background = F,
          src = "https://www.flaticon.com/svg/static/icons/svg/337/337946.svg",
          color = "gray",
          a("Go to the app", href="https://juan-izurieta.shinyapps.io/NLP_from_pdf/")
        ),
        widgetUserBox(
          width = 12,
          type = 2,
          subtitle = "NLP & sentiment analysis from files",
          background = F,
          src = "https://www.flaticon.com/svg/static/icons/svg/180/180855.svg",
          color = "gray",
          a("Go to the app", href="https://juan-izurieta.shinyapps.io/NLP_from_files/")
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

server <- function(session, input, output) {
  
  # Start instructions:
  shinyalert(
    title = "Welcome",
    text = str_glue("<b>HOW TO USE THE TOOL:</b><br>", 
                    "<br>",
                    "<b>Insert a hashtag, keyword or topic,</b> and a number of tweets to download.<br>",
                    "<b>Select the analysis language.</b> English and Spanish are supported.<br>", 
                    "<b>Click 'Search and analize'</b> and wait for your results."),
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
  
  
  # Setup Reactive Values ----
  rv <- reactiveValues()
  
  observeEvent(input$submit, {
    
    
    # Process data ----
    
    req(input$submit)
    
    rv$data <-  withProgress(message = message_1, value = 0, {
      
      incProgress(1/4)
      
      # Progress message:
      
      shinyalert(
        title = "Downloading and analyzing tweets",
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
      
      
      search_tweets(
        q           = input$query, 
        n           = input$n_tweets, 
        include_rts = FALSE, 
        token       = token
      )
      
    })
    
    rv$selected_data <- rv$data %>% select(user_id, 
                                           status_id, 
                                           dateTime = created_at,
                                           user = screen_name, text, 
                                           rt = retweet_count)
    
    # Clean data
    rv$selected_data <- rv$data %>% select(user_id, 
                                           status_id, 
                                           dateTime = created_at,
                                           user = screen_name, text, 
                                           rt = retweet_count)
    
    rv$selected_data$text_clean <- gsub("&amp", "", rv$selected_data$text)
    rv$selected_data$text_clean <- gsub("&amp", "", rv$selected_data$text_clean)
    rv$selected_data$text_clean <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", rv$selected_data$text_clean)
    rv$selected_data$text_clean <- gsub("@\\w+", "", rv$selected_data$text_clean)
    rv$selected_data$text_clean <- gsub("[[:punct:]]", "", rv$selected_data$text_clean)
    rv$selected_data$text_clean <- gsub("[[:digit:]]", "", rv$selected_data$text_clean)
    rv$selected_data$text_clean <- gsub("http\\w+", "", rv$selected_data$text_clean)
    rv$selected_data$text_clean <- gsub("[ \t]{2,}", "", rv$selected_data$text_clean)
    rv$selected_data$text_clean <- gsub("^\\s+|\\s+$", "", rv$selected_data$text_clean)
    
    rv$selected_data$text_clean <- tolower(rv$selected_data$text_clean)
    rv$selected_data$text_clean <- iconv(rv$selected_data$text_clean, "UTF-8", "ASCII", sub="")
    
    ## Get time
    rv$time_now <- now(tzone = "GMT")
    
    ## Get sentiments
    rv$tweet_sentiments <- rv$selected_data %>% mutate(ave_sentiment = syuzhet::get_sentiment(rv$selected_data$text_clean, language = input$language))
    
    rv$tweet_sentiments <- rv$tweet_sentiments %>%
      mutate(minutes_from_now = round(difftime(rv$tweet_sentiments$dateTime,
                                               rv$time_now, units="mins"),0))
    
    ## Get polarity
    rv$tweet_sentiments <- rv$tweet_sentiments %>% 
      mutate(polarity = ifelse(ave_sentiment < 0, "Negative",
                               ifelse(ave_sentiment > 0, "Positive","Neutral")))
    
    rv$tweet_polarity <- rv$tweet_sentiments %>%
      group_by(minutes_from_now, polarity) %>%
      mutate(count = n())
    
    ## Get emotions
    
    rv$emotion_tweets <- withProgress(message = message_2, value = 0, {incProgress(2/4)
      
      get_nrc_sentiment(rv$selected_data$text_clean, language = input$language)
      
    })
    
    rv$emotion_tweets$status_id <- rv$selected_data$status_id
    rv$emotion_tweets <- left_join(rv$emotion_tweets, select(rv$selected_data, c(status_id, text, dateTime)), by='status_id')
    
    
    ## Annotate text
    
    
    if(input$language == "english"){
      ud_model <- ud_model_eng
    } else {
      ud_model <- ud_model_esp
    }
    
    
    rv$annotations <- withProgress(message = message_3, value = 0, 
                                   {incProgress(1/2)
                                     as.data.frame(udpipe_annotate(ud_model, 
                                                                   x = rv$selected_data$text_clean, 
                                                                   doc_id = rv$selected_data$status_id))
                                   }
    )
    
    ## Get co-ocurrences of words
    rv$cooc <- cooccurrence(x = subset(rv$annotations, upos %in% c('NOUN', 'ADJ')), 
                            term  = 'lemma', 
                            group = c('doc_id', 'paragraph_id', 'sentence_id')
    )
    
    # Ready message:
    shinyalert(
      title = "Ready!",
      text = "Click outside this box and scroll down to check all the results.<br> All progress icons by Jovie Brett Bardoles.",
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
    
    
  }, ignoreNULL = FALSE)

  
  
  # Plotly sentiments in time----
  output$plotly_sentiment <- renderPlotly({
    req(rv$tweet_sentiments)
    
    label_wrap <- label_wrap_gen(width = 60)
    g <- rv$tweet_sentiments %>% 
      ggplot(aes(x=dateTime, y=ave_sentiment, size = rt, color = ave_sentiment)) +
      geom_point(alpha=0.9,
                 aes(text = str_glue("<b>Date-time:,</b> {dateTime}
                                      <b>Sentiment score:</b> {round(rv$tweet_sentiments$ave_sentiment,2)}
                                      <b>Retweets:</b> {rt}
                                      <b>Tweet:</b> {label_wrap(text)}"))) +
      geom_hline(aes(yintercept = mean(ave_sentiment)), color = "black") +
      geom_hline(aes(yintercept = median(ave_sentiment) + 1.96*IQR(ave_sentiment)), color = "#ffd633") +
      geom_hline(aes(yintercept = median(ave_sentiment) - 1.96*IQR(ave_sentiment)), color = "#600080") +
      scale_size(range = c(.2, 18)) +
      scale_color_viridis(option="plasma") +
      theme_ipsum() +
      theme(legend.position = "bottom") +
      ylab("Average sentiment of tweet") +
      xlab("Date - time")
    
    ggplotly(g, tooltip = "text") 
    
  })
  
  
  # DT table of tweets ranked by RT ----
  
  output$DT_tweets <- DT::renderDataTable({
    
    req(rv$tweet_sentiments)
    
    rv$tweet_sentiments %>% 
      arrange(desc(rt)) %>% 
      mutate(sentiment_score = round(ave_sentiment,2)) %>% 
      select(rt, text, sentiment_score)
  })
  
  # Polarity bar plot ----
  
  output$plotly_bars <- renderPlotly({
    req(rv$tweet_polarity)
    
    g <- rv$tweet_polarity %>% ggplot( aes(x=polarity, y=count, fill=polarity)) +
      geom_bar(stat="identity") + coord_flip() +
      scale_fill_viridis(discrete=T, option="plasma") +
      theme_ipsum() + 
      theme(legend.position = "none") +
      ylab("Number of tweets") +
      xlab("Tweets polarity category")
    
    ggplotly(g)
  })
  
  # Plotly sentiments polarity violin -----
  output$plotly_violin <- renderPlotly({
    req(rv$tweet_polarity)
    
    g <- rv$tweet_polarity %>% filter(polarity != 'Neutral') %>%
      ggplot( aes(x=polarity, y=ave_sentiment, fill=polarity)) +
      geom_violin() +
      scale_fill_viridis(discrete=T, option="plasma") +
      theme_ipsum() +
      theme(legend.position = "none") +
      ylab("Average sentiment of tweets") +
      xlab("Tweets polarity category")
    
    ggplotly(g)
    
  })

  
  # Plotly tweets emotions ----
  
  output$plotly_emotions <- renderPlotly({
    
    req(rv$emotion_tweets)
    
    label_wrap <- label_wrap_gen(width = 60)
    
    g <- rv$emotion_tweets %>% ggplot( aes(x=dateTime, y=rv$emotion_tweets[,c(input$var_emotions)], col=rv$emotion_tweets[,c(input$var_emotions)])) + 
      geom_point(aes(text = str_glue("<b>id</b>: {status_id}
                                      <b>Text:</b> {label_wrap(text)}"))) + 
      geom_smooth(method = "loess", color = "cornflowerblue") + 
      scale_color_viridis(option="plasma") +
      ylab("Emotion valence") +
      xlab("Date or time") + labs(color = "Emotion valence") 
    
    ggplotly(g)
    
  })
  
  # Plotly common nouns ----
  output$plotly_nouns <- renderPlotly({
    
    req(rv$annotations)
    
    stats <- subset(rv$annotations, upos %in% c("NOUN")) 
    stats <- txt_freq(stats$lemma)
    stats$key <- factor(stats$key, levels = rev(stats$key))
    
    g <- stats %>% top_n(input$num_words) %>% ggplot( aes(x=key, y=freq)) +
      geom_bar(stat="identity", fill="#69b3a2", alpha=.6, width=.4) +
      coord_flip() +
      xlab("") +
      theme_bw()
    
    ggplotly(g, tooltip = c("key", "freq"))
    
  })
  
  # Plotly common adjectives ----
  output$plotly_adjectives <- renderPlotly({
    req(rv$annotations)
    stats <- subset(rv$annotations, upos %in% c("ADJ")) 
    stats <- txt_freq(stats$lemma)
    stats$key <- factor(stats$key, levels = rev(stats$key))
    
    g <- stats %>% top_n(input$num_words) %>% ggplot( aes(x=key, y=freq)) +
      geom_bar(stat="identity", fill="#69b3a2", alpha=.6, width=.4) +
      coord_flip() +
      xlab("") +
      theme_bw() 
    
    ggplotly(g, tooltip = c("key", "freq"))
  })
  
  # Plotly common verbs ----
  output$plotly_verbs <- renderPlotly({
    req(rv$annotations)
    stats <- subset(rv$annotations, upos %in% c("VERB")) 
    stats <- txt_freq(stats$lemma)
    stats$key <- factor(stats$key, levels = rev(stats$key))
    
    g <- stats %>% top_n(input$num_words) %>% ggplot( aes(x=key, y=freq)) +
      geom_bar(stat="identity", fill="#69b3a2", alpha=.6, width=.4) +
      coord_flip() +
      xlab("") +
      theme_bw() 
    
    ggplotly(g, tooltip = c("key", "freq"))
  })
  
  # Plot interactive words connections ----
  # Render word graph
  
  observeEvent(input$submit, ({
    
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
    
  }))
  
}


# Run the application 
shinyApp(ui = ui, server = server)




