library(shiny)
library(quanteda)
library(data.table)

shinyServer(function(input, output, session) {
        
        df <- isolate({
                #setwd("C:/Github/Projects/Word Suggestion/wordsuggestion")
                bigrams <- fread("data/bigrams.csv", sep=",", header=TRUE, nrows = 237663)
                trigrams <- fread("data/trigrams.csv", sep=",", header=TRUE, nrows = 477825)
                quadgrams <- fread("data/quadgrams.csv", sep=",", header=TRUE, nrows = 562053)
                pentagrams <- fread("data/pentagrams.csv", sep=",", header=TRUE, nrows = 562053)
                
        })
        
        predictor <- function(text.input){
                text.input <- iconv(text.input, from = "UTF-8", to = "ASCII", sub = "")
                text.token <- tokens(char_tolower(text.input), 
                                     remove_punct = TRUE, 
                                     remove_symbols = T, 
                                     remove_separators = T, 
                                     remove_twitter = T, 
                                     remove_hyphens = T, 
                                     remove_numbers = T)
                
                text.token <- as.vector(unlist(text.token))
                
                n <- length(text.token)
                        
                # If the user input has length 0 (the user hasn't input anything), then return nothing
                if (n == 0) {
                        return() 
                }
                
                # If the user input has length 4 or more, try to find a corresponding pentagram
                if (n >= 4) {
                        n <- 4
                        
                        ## Create a search criteria of the last 4 words input by the user
                        search.criteria <- paste(text.token[length(text.token)-3], 
                                                 text.token[length(text.token)-2],
                                                 text.token[length(text.token)-1],
                                                 text.token[length(text.token)],
                                                 "", sep = " ")
                        ## Find all corresponding ngrams that contain the search criteria
                        search.result <- grep(search.criteria, pentagrams[, x], value = T)
                        
                        if (identical(character(0),search.result)) { n <- n - 1 } ## if no gram is found than repeat the same process with a lower ngram.
                }
                
                if (n == 3) {
                        search.criteria <- paste(text.token[length(text.token)-2],
                                                 text.token[length(text.token)-1],
                                                 text.token[length(text.token)],
                                                 "", sep = " ")
                        search.result <- grep(search.criteria, quadgrams[, x], value = T)
                        if (identical(character(0),search.result)) { n <- n - 1 }
                }
                
                if (n == 2) {
                        search.criteria <- paste(text.token[length(text.token)-1],
                                                 text.token[length(text.token)],
                                                 "", sep = " ")
                        search.result <- grep(search.criteria, trigrams[, x], value = T)
                        if (identical(character(0),search.result)) { n <- n - 1 }
                        
                }
                
                if (n == 1) {
                        search.criteria <- paste(text.token[length(text.token)]
                                                 ,"", sep = " ")
                        search.result <- grep(search.criteria, bigrams[, x], value = T)
                }
                
                if (identical(character(0),search.result)) {
                        sug.table <- data.table("We haven't found any",
                                                "suggestions, check for",
                                                "spelling errors") ## If all levels are searched and no ngram is found, this message is returned
                } else {
                        search.result <- strsplit(search.result, split = " ") ## Splitting all words of all ngrams found
                        
                        ## Picking the last word of the first, second and third most frequent results
                        fst <- search.result[[1]][length(search.result[[1]])] 
                        if (length(search.result) > 1) {snd <- search.result[[2]][length(search.result[[2]])]} else {snd <- " "} ## If there is a second result, return the last word of it
                        if (length(search.result) > 2) {trd <- search.result[[3]][length(search.result[[3]])]} else {trd <- " "} ## If there is a third result, return the last word of it
                        sug.table <- data.table(fst, snd, trd)
                }
        }
        
        # Updating Button 1 outcome
        observeEvent(input$sug1, {
                suggestions <- predictor(input$text.input)
                sug1 <- suggestions[[1]]
                lastChar <- nchar(input$text.input)
                if(substr(input$text.input, lastChar, lastChar) == " "){
                        updateTextAreaInput(session, "text.input", value = paste0(input$text.input, sug1," "))
                } else {
                        updateTextAreaInput(session, "text.input", value = paste(input$text.input, sug1, sep = " "))
                }
        })
        
        # Updating Button 2 outcome
        observeEvent(input$sug2, {
                suggestions <- predictor(input$text.input)
                sug2 <- suggestions[[2]]
                lastChar <- nchar(input$text.input)
                if(substr(input$text.input, lastChar, lastChar) == " "){
                        updateTextAreaInput(session, "text.input", value = paste0(input$text.input, sug2," "))
                } else {
                        updateTextAreaInput(session, "text.input", value = paste(input$text.input, sug2, sep = " "))
                }
        })
        
        # Updating Button 3 outcome
        observeEvent(input$sug3, {
                suggestions <- predictor(input$text.input)
                sug3 <- suggestions[[3]]
                lastChar <- nchar(input$text.input)
                if(substr(input$text.input, lastChar, lastChar) == " "){
                        updateTextAreaInput(session, "text.input", value = paste0(input$text.input, sug3," "))
                } else {
                        updateTextAreaInput(session, "text.input", value = paste(input$text.input, sug3, sep = " "))
                }
        })
        
        #Updating buttons layout
        observeEvent(input$text.input, {
                text.input <- input$text.input
                suggestions <- predictor(text.input)
                
                #Updating buttons
                updateActionButton(session, inputId = "sug1", label = paste(suggestions[[1]]))
                updateActionButton(session, inputId = "sug2", label = paste(suggestions[[2]]))
                updateActionButton(session, inputId = "sug3", label = paste(suggestions[[3]]))
        })
        
})