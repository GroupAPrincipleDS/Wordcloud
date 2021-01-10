library(shiny)
library(wordcloud)
library(tm)
library(colourpicker)
library(RWeka)
library(dplyr)

# Define server logic
shinyServer(function(input, output, session) {
    
    
    # Get list of brands from title search and update brand selection input.
    brands <- observeEvent(input$txtSearch, { 
        if (input$txtSearch != "") {
            sql <- paste("SELECT DISTINCT brand FROM data_with_details dwd ",
                         "WHERE UPPER(title) LIKE '%", toupper(input$txtSearch), "%' ",
                         "ORDER BY brand",
                         sep="")
            #cat(sql,"\n")
            res <- dbGetQuery(con, sql)
            
            # Update the selection for Brand. Include the 'All' option to select all brands.
            updateSelectInput(session, "selectBrand", choices=c('All',res), selected='All')
        }
    })
    
    # Update data table with selected brand.
    df <- eventReactive(input$btnSearch, { 
        if (input$txtSearch != "") {
            sql <- paste("SELECT AVG(overall) overall_avg, COUNT(overall) review_count, ",
                         "title, brand FROM data_with_details ",
                         "WHERE UPPER(title) LIKE '%", toupper(input$txtSearch), "%' ",
                         sep="")
            
            # If 'All' is not selected, construct the SQL to search for only the specific brand.
            if (input$selectBrand != 'All') {
                sql <- paste(sql,
                             "AND brand = ", dbQuoteString(con, input$selectBrand), " ",
                             sep="")
            }
            sql <- paste(sql,
                         "GROUP BY Title, Brand ORDER BY overall_avg DESC, review_count DESC ",
                         sep="")
            
            #cat(sql,"\n")
            res <- dbGetQuery(con, sql)
        }
    })
    
    
    # dataset for good review
    
    datasource <- eventReactive(input$btnSearch2,{
        good_t_w <- 
            t %>% 
            filter(title == input$ProductSearch) %>%
            filter(overall>=3) %>%
            select(summary)
        mycorpus <- Corpus(VectorSource(good_t_w $summary))
        mycorpus <-tm_map(mycorpus,content_transformer(tolower))
        mycorpus <- tm_map(mycorpus, removeNumbers)
        mycorpus <- tm_map(mycorpus, removeWords,stopwords("english"))
        mycorpus <- tm_map(mycorpus,removePunctuation)
        mycorpus <- tm_map(mycorpus,stripWhitespace)
        mycorpus <- tm_map(mycorpus, removeWords, c("one","two","three","four","five", "star","stars")) 
        
        
        
        token_delim <- " \\t\\r\\n.!?,;\"()"
        bitoken <- NGramTokenizer(mycorpus, Weka_control(min=2,max=2, delimiters = token_delim))
        bitoken <- bitoken[!bitoken %in% c("language =","list language","= en")]
        two_word <- data.frame(table(bitoken))
        sort_two <- two_word[order(two_word$Freq,decreasing=TRUE),]
        
        return(sort_two)
    })
    
    
    # dataset for bad review
    
    datasource_v2 <- eventReactive(input$btnSearch2,{
        
        bad_t_w <- 
            t %>% 
            filter(title == input$ProductSearch) %>%
            filter(overall<3) %>%
            select(summary)
        mycorpus_v2 <- Corpus(VectorSource(bad_t_w $summary))
        mycorpus_v2 <-tm_map(mycorpus_v2,content_transformer(tolower))
        mycorpus_v2 <- tm_map(mycorpus_v2, removeNumbers)
        mycorpus_v2 <- tm_map(mycorpus_v2, removeWords,stopwords("english"))
        mycorpus_v2 <- tm_map(mycorpus_v2,removePunctuation)
        mycorpus_v2 <- tm_map(mycorpus_v2,stripWhitespace)
        mycorpus_v2 <- tm_map(mycorpus_v2, removeWords, c("one","two","three","four","five", "star","stars")) 
        
        
        
        token_delim_v2 <- " \\t\\r\\n.!?,;\"()"
        bitoken_v2 <- NGramTokenizer(mycorpus_v2, Weka_control(min=2,max=2, delimiters = token_delim_v2))
        bitoken_v2 <- bitoken_v2[!bitoken_v2 %in% c("language =","list language","= en")]
        two_word_v2 <- data.frame(table(bitoken_v2))
        sort_two_v2 <- two_word_v2[order(two_word_v2$Freq,decreasing=TRUE),]
        
        return(sort_two_v2)
    })
    
    
    # table for product search
    output$tblProducts <- renderDataTable({df()})
    
    
    # wordcloud for good review
    
    output$good_review <- renderPlot({
        x <- datasource()$bitoken
        y <- datasource()$Freq
        minfreq_bigram <- 2
        
        wordcloud(x,y,random.order=FALSE,scale = c(3,0.5),min.freq = minfreq_bigram,colors = brewer.pal(8,"Dark2"),
                  max.words=50)
        
        
    })
    
    # wordcloud for bad review
    
    output$bad_review <- renderPlot({
        x <- datasource_v2()$bitoken
        y <- datasource_v2()$Freq
        minfreq_bigram <- 2
        
        wordcloud(x,y,random.order=FALSE,scale = c(3,0.5),min.freq = minfreq_bigram,colors = brewer.pal(8,"Dark2"),
                  max.words=50)
        
        
    })
    
    
})
