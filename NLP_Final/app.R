# NLPN SPANISH APP

# LIBRARIES ----

# Shiny
library(shiny)
library(shinythemes)

# PDF reading and scrapping:
library(pdftools)

# Images loading (for preview):
library(magick)

# Plotting
library(plotly)
library(tidyquant)
library(igraph)
library(networkD3)

# Operations with data:
library(tidyverse)
library(tidyquant)

# NLP
library(udpipe)
library(sentimentr)

# Download and load UDPIPE model set to English:
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

# UI ----
ui <- navbarPage(
    title = "Oxygen ML 0.0.1",
    collapsible = TRUE,
    position    = "static-top", 
    inverse     = FALSE, 
    theme       = shinytheme("flatly"),
    
    tabPanel(
        title = "NLP",
        includeCSS("css/styles.css"),
        sidebarLayout(
            # * SIDEBAR ----
            sidebarPanel = sidebarPanel(
                width = 3,
                h3("Tool for Natural Language Processing in pdfs"),
                HTML("Select a pdf file (up to 5Mb) to analyze using NLP"),
                hr(),
                shiny::fileInput(inputId = "pdf_input", label = "Select PDF", accept = ".pdf"),
                shiny::actionButton(inputId = "submit", "Analyze", class = "btn-primary"),
                hr()
                
            ),
            # * MAIN ----
            mainPanel = mainPanel(
                width = 9,
                div(
                    class = "col-sm-6 panel",
                    div(class= "panel-heading", h5("")),
                    div(
                        class="panel-body", style="height:700px",  
                        imageOutput("img_pdf", width = "80%", height = "600px"),
                        uiOutput("page_controls")
                    )
                ),
                div(
                    class = "col-sm-6 panel",
                    div(class= "panel-heading", h5("")),
                    div(
                        class="panel-body", style="height:700px",  
                        plotlyOutput("plotly_sentiment", height = "600px")
                    )
                ),
                div(
                    class = "col-sm-12 panel",
                    div(class= "panel-heading", h5("")),
                    div(
                        class="panel-body", style="height:700px",  
                        simpleNetworkOutput("graphos")
                    )
                ),
                div(
                    class = "col-sm-6 panel",
                    div(class= "panel-heading", h5("")),
                    div(class="panel-body", style="height:400px",
                        plotlyOutput("plotly_verbs", height = "350px")
                    )
                ),
                div(
                    class = "col-sm-6 panel",
                    div(class= "panel-heading", h5("")),
                    div(class="panel-body", style="height:400px",
                        plotlyOutput("plotly_nouns", height = "350px")
                    )
                ),
                div(
                    class = "col-sm-6 panel",
                    div(class= "panel-heading", h5("")),
                    div(class="panel-body", style="height:400px",
                        plotlyOutput("plotly_adjectives", height = "350px")
                    )
                ),
                div(
                    class = "col-sm-6 panel",
                    div(class= "panel-heading", h5("")),
                    div(class="panel-body", style="height:400px",
                        plotlyOutput("plotly_compounds", height = "350px")
                    )
                )
            )
        )
    )
)



server <- function(session, input, output) {
    
    # Limit PDF Files to 5MB
    options(shiny.maxRequestSize = 5*1024^2)
    
    rv <- reactiveValues()
    
    observeEvent(input$submit, {
        
        # Handle Inputs
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
        
        # Modeling
        withProgress(rv$sentiment_regression <- rv$paragraph_text_tbl %>% 
            pull(paragraph_text) %>%
            sentiment_by(), message="Analyzing sentiment")
        
        # Data Prep
        rv$data_prepared_tbl <- rv$paragraph_text_tbl %>%
            mutate(
                sentiment_regression = rv$sentiment_regression$ave_sentiment
            ) %>%
            mutate(
                label = str_glue("Page: {page_num}
                            Paragraph: {paragraph_num}
                            Sentiment: {round(sentiment_regression)}
                            ---
                            {str_wrap(paragraph_text, width = 80)}")
            )
        # Annotate texts
        withProgress(rv$annotations <- as.data.frame(udpipe_annotate(ud_model, 
                                          x = rv$paragraph_text_tbl$paragraph_text, 
                                          doc_id = rv$paragraph_text_tbl$paragraph_num)), 
                     message="Annotating text")
        
        # Concurrences
        withProgress(rv$cooc <- cooccurrence(x = subset(rv$annotations, upos %in% c('NOUN', 'ADJ')), 
                                          term  = 'lemma', 
                                          group = c('doc_id', 'paragraph_id', 'sentence_id'))
            , message = "Calculating concurrences")
        
            
    })
    
    # Debugging ----
    output$print <- renderPrint({
        list(
            pdf = rv$pdf,
            text_data = rv$text_data,
            paragraph_text_tbl = rv$paragraph_text_tbl,
            sentiment_regression = rv$sentiment_regression,
            annotations = rv$annotations
        )
    })
    
    # Render PDF Images ----
    output$img_pdf <- renderImage({
        
        req(rv$pdf)
        
        # Get page num
        page_num <- input$page_num
        
        # Read PDF Images
        rv$img_data <- image_read_pdf(rv$pdf$datapath, pages = page_num)
        
        tmpfile <- rv$img_data %>% 
            image_scale("600") %>%
            image_write(tempfile(fileext='jpg'), format = 'jpg')
        
        # Return a list
        list(src = tmpfile, contentType = "image/jpeg")
    })
    
    # Render PDF Viewer Controls -----
    output$page_controls <- renderUI({
        
        req(rv$pdf)
        
        n_max <- pdf_length(rv$pdf$datapath)
        
        div(
            class = "row",
            shiny::sliderInput(
                "page_num", 
                label = NULL, 
                value = 1, min = 1, max = n_max, step = 1, 
                width = "100%")
        )
        
    })
    
    # Render Plotly Sentiment ----
    output$plotly_sentiment <- renderPlotly({
        
        req(rv$data_prepared_tbl)
        
        g <- rv$data_prepared_tbl %>%
            ggplot(aes(page_num, sentiment_regression, color = sentiment_regression)) +
            geom_point(aes(text = label, 
                           size = abs(sentiment_regression))) +
            scale_color_viridis_c(option='plasma') +
            theme_tq() +
            labs(
                title = "Sentiment by page",
                x = "Page number", y = "Sentiment score",
                color = ""
            )
        
        ggplotly(g, tooltip = "text")
        
    })
    
    # Render word graph
    output$graphos <- renderSimpleNetwork({
        req(rv$cooc)
        limit_terms <- match(quantile(rv$cooc$cooc,  probs = c(0.90)),rv$cooc$cooc)
        grapho <- data.frame(from=rv$cooc$term1[1:limit_terms], to=rv$cooc$term2[1:limit_terms])
        p <- simpleNetwork(grapho, 
                           height="500px", 
                           width="700px",
                           linkDistance = 90,
                           charge = -500,
                           fontSize = 14,
                           fontFamily = "arial",
                           zoom = T)
        p
    })
    
    # Render Plotly Verbs ----
    output$plotly_verbs <- renderPlotly({
        req(rv$annotations)
        stats <- subset(rv$annotations, upos %in% c("VERB")) 
        stats <- txt_freq(stats$lemma)
        stats$key <- factor(stats$key, levels = rev(stats$key))
        
        g <- ggplot(stats[1:30,], aes(x=key, y=freq)) +
            geom_bar(stat="identity", fill="#69b3a2", alpha=.6, width=.4) +
            coord_flip() +
            xlab("") +
            theme_bw() + 
            theme(plot.title = element_text(size = 9)) +
            ggtitle("Top 30 most frequent verbs")
        
        ggplotly(g, tooltip = c("key", "freq"))
    })
    
    # Render Plotly nouns
    output$plotly_nouns <- renderPlotly({
        req(rv$annotations)
        stats <- subset(rv$annotations, upos %in% c("NOUN")) 
        stats <- txt_freq(stats$lemma)
        stats$key <- factor(stats$key, levels = rev(stats$key))
        
        g <- ggplot(stats[1:30,], aes(x=key, y=freq)) +
            geom_bar(stat="identity", fill="#69b3a2", alpha=.6, width=.4) +
            coord_flip() +
            xlab("") +
            theme_bw() + 
            theme(plot.title = element_text(size = 9)) +
            ggtitle("Top 30 most frequent nouns")
        
        ggplotly(g, tooltip = c("key", "freq"))
    })
    
    # Render Plotly adjectives
    output$plotly_adjectives <- renderPlotly({
        req(rv$annotations)
        stats <- subset(rv$annotations, upos %in% c("ADJ")) 
        stats <- txt_freq(stats$lemma)
        stats$key <- factor(stats$key, levels = rev(stats$key))
        
        g <- ggplot(stats[1:30,], aes(x=key, y=freq)) +
            geom_bar(stat="identity", fill="#69b3a2", alpha=.6, width=.4) +
            coord_flip() +
            xlab("") +
            theme_bw() +
            theme(plot.title = element_text(size = 9)) +
            ggtitle("Top 30 most frequent nouns")
        
        ggplotly(g, tooltip = c("key", "freq"))
    })
    
    # Render Plotly compound words
    output$plotly_compounds <- renderPlotly({
        req(rv$annotations)
        stats <- keywords_rake(x = rv$annotations, term = "lemma", group = "doc_id", 
                               relevant = rv$annotations$upos %in% c("NOUN", "ADJ"))
        stats$keyword <- factor(stats$keyword, levels = rev(stats$keyword))
        
        g <- ggplot(stats[1:10,], aes(x=keyword, y=rake)) +
            geom_bar(stat="identity", fill="#69b3a2", alpha=.7, width=.4) +
            coord_flip() +
            ylab("RAKE (Rapid Automatic Keyword Extraction)") +
            xlab("Compound word") +
            theme_bw() + 
            theme(plot.title = element_text(size = 9)) +
            ggtitle("Top 10 most frequent compound words")
        
        ggplotly(g, tooltip = c("keyword", "rake"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)