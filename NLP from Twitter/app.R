
# LIBRARIES ----
# Shiny
library(shiny)
library(shinythemes)
library(shinyWidgets)

# Sentiment
library(sentimentr)

# Dates
library(lubridate)

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
library(tidyverse)
library(tidyquant)

# Using Twitter API
library(rtweet)

# NLP
library(udpipe)



# API keys ----

api_key <- readRDS("api_key.rds")
api_secret_key <- readRDS("api_secret_key.rds")

## Authenticate via web browser
token <- readRDS("token.rds")
bearer_token(token)

# UD pipe load model
ud_model <- udpipe_load_model("english-ewt-ud-2.4-190531.udpipe")
# ud_model <- readRDS("ud_model.rds")
# ud_model <- udpipe_load_model(ud_model$file_model)

# Progress messages (placed here because they look ugly if placed within code, and easier to change them later)
message_1 <- "Step 1 of 3: We're connecting to Twitter and retrieving your tweets"
message_2 <- "Step 2 of 3: We're annotating and analyzing the sentiment of your tweets. That means we're understanding how words are related, which are the most common words per type, and of course, what are your tweets sentiments and emotions"
message_3 <- "Last step! We're making an animated plot, may take up to 20 seconds. We'll show you how the trends of positive or negative tweets have changed in time, so if something trigger sentiments in your tweets, you'll know when that happened and how much affected the sentiment trends. Awesome!"

# ---- 1.0 UI ----
ui <- navbarPage(
    title = "Oxygen ML",
    collapsible = TRUE,
    inverse     = TRUE, 
    theme       = shinytheme("flatly"),
    
    shiny::tabPanel(
        title = "Twitter NLP and sentiment analysis",
        sidebarLayout(
            sidebarPanel(
                shiny::textInput(inputId = "query", label = "Topic or Hashtag"),
                setSliderColor(c("#4C787E", "#69b3a2", "#69b3a2"), c(1,2,3)),
                chooseSliderSkin("Modern"),
                sliderInput(
                    inputId = "n_tweets",
                    label   = "Number of tweets:",
                    min     = 1,
                    max     = 1500,
                    value   = 500),
                shiny::actionButton(inputId = "submit", "Submit", class = "btn-primary")
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
                div(
                    class = "row",
                    div(
                        class = "col-sm-12 panel",
                        div(class = "panel-heading", h5("Tweets sentiment in time")),
                        div(class = "panel-body", plotlyOutput(outputId = "plotly_sentiment", height = 400))
                    ),
                ),
                
                div(
                    class = "col-sm-12 panel",
                    div(class = "panel-heading", h5("Tweets Ranking by retweet")),
                    div(class = "panel-body", DT::dataTableOutput(outputId = "DT_tweets", height = 400))
                ),
                
                
                div(
                    class = "col-sm-6 panel",
                    div(class = "panel-heading", h5("Tweets sentiment category")),
                    div(class = "panel-body", plotlyOutput(outputId = "plotly_bars", height = 400))
                ),
                
                div(
                    class = "col-sm-6 panel",
                    div(class = "panel-heading", h5("Tweets sentiment category violin plot")),
                    div(class = "panel-body", plotlyOutput(outputId = "plotly_violin", height = 400))
                ),
                
                div(
                    class = "row",
                    div(
                        class = "col-sm-12 panel",
                        div(class = "panel-heading", h5("Tweets evolution in time")),
                        div(class = "panel-body", imageOutput("gganimate_plot", height = 400))
                    )
                ),
                
                div(
                    class = "row",
                    div(
                        class = "col-sm-12 panel",
                        div(class = "panel-heading", h5("Tweets emotions")),
                        div(class = "panel-body", plotlyOutput(outputId = "plotly_emotions", height = 400))
                    )
                ),
                
                div(
                    class = "row",
                    div(
                        class = "col-sm-12 panel",
                        div(class = "panel-heading", h5("Common words in tweets")),
                        
                        sliderInput("num_words", 
                                     h5("How many words should we plot?"), 
                                     value = 20, min = 3, max = 50),   
                        
                        tabsetPanel(type = "tabs",
                                    tabPanel("Nouns", plotlyOutput("plotly_nouns")),
                                    tabPanel("Adjectives", plotlyOutput("plotly_adjectives")),
                                    tabPanel("Verbs", plotlyOutput("plotly_verbs"))
                        )
                        
                    )
                ),
                
                div(
                    class = "col-sm-12 panel",
                    div(class= "panel-heading", h5("Words connections")),
                    sliderInput(
                        inputId = "words_connections",
                        label   = h5("How many connections should we plot?"),
                        min     = 25,
                        max     = 300,
                        value   = 70),
                    div(
                        class="panel-body", 
                        simpleNetworkOutput("graphos")
                    )
                ),
                
            )
        )
        
    )

)

# ---- 2.0 SERVER ----
server <- function(session, input, output) {
    
    # Setup Reactive Values ----
    rv <- reactiveValues()
    
    observeEvent(input$submit, {
        
        # Process data ----
        
        req(input$submit)
        
        rv$data <-  withProgress(message = message_1, value = 0, {
            
            incProgress(1/4)
            
            search_tweets(
                q           = input$query, 
                n           = input$n_tweets, 
                include_rts = FALSE, 
                lang        = "en",
                token       = token
            )
            
        })
        
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
        rv$tweet_sentiments <- rv$selected_data %>% mutate(text_split = get_sentences(text_clean)) %$% 
            sentiment_by(text_split, list(status_id, user, dateTime, text, text_clean, rt))
        
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
        rv$emotion_tweets <- rv$selected_data %>% get_sentences(rv$selected_data$text_clean) %>% 
            emotion(drop.unused.emotions = TRUE)
        
        rv$emotion_tweets_ordered <- arrange(rv$emotion_tweets, dateTime)
        
        ## Annotate text
        rv$annotations <- withProgress(message = message_2, 
                                       value = 0, {
            
            incProgress(1/2)
            
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
    
    # gganimation of tweets in time by sentiment ----
    
    output$gganimate_plot <- renderImage({

        withProgress(message = 'Rendering images',
                     value = 0, {incProgress(3/4, message = message_3)


        req(rv$tweet_polarity)

        outfile <- tempfile(fileext='.gif')

        ag <- rv$tweet_polarity %>% ggplot(aes(x=minutes_from_now, y=count, group=polarity, color=polarity)) +
            geom_line() +
            geom_point() +
            scale_color_viridis(discrete = T, option="plasma") +
            theme_ipsum() +
            ggtitle("Evolution of tweets by sentiment category") +
            ylab("Number of tweets") +
            transition_reveal(minutes_from_now)
        
        anim_save("outfile.gif", 
                  animate(ag, height = 400, width = 800, nframes = 40, fps = 10, 
                          renderer = gifski_renderer())
                  )

        list(src = "outfile.gif",
             contentType = 'image/gif'
        )
                     })

    }, deleteFile = TRUE)
    
    # Plotly tweets emotions ----
    
    output$plotly_emotions <- renderPlotly({
            req(rv$emotion_tweets_ordered)
            
            g <- plot(rv$emotion_tweets_ordered, facet = FALSE)
            
            ggplotly(g, tooltip = c("emotion"))
            
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
    output$graphos <- renderSimpleNetwork({
        req(rv$cooc)
        grapho <- data.frame(from=rv$cooc$term1, to=rv$cooc$term2)
        grapho <- grapho %>% top_n(input$words_connections)
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
}

# Run the application 
shinyApp(ui = ui, server = server)
