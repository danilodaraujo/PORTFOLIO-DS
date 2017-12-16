predictor <- function(text.input) {
        
        # The same preparation that was done to generate the ngrams is done with the user input
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
                search.result <- grep(search.criteria, pentagrams, value = T)
                
                ## if no gram is found than repeat the same process with a lower ngram.
                if (identical(character(0),search.result)) { n <- n - 1 } 
        }
        
        if (n == 3) {
                search.criteria <- paste(text.token[length(text.token)-2],
                                         text.token[length(text.token)-1],
                                         text.token[length(text.token)],
                                         "", sep = " ")
                search.result <- grep(search.criteria, quadgrams, value = T)
                if (identical(character(0),search.result)) { n <- n - 1 }
        }
        
        if (n == 2) {
                search.criteria <- paste(text.token[length(text.token)-1],
                                         text.token[length(text.token)],
                                         "", sep = " ")
                search.result <- grep(search.criteria, trigrams, value = T)
                if (identical(character(0),search.result)) { n <- n - 1 }
                
        }
        
        if (n == 1) {
                search.criteria <- paste(text.token[length(text.token)]
                                         ,"", sep = " ")
                search.result <- grep(search.criteria, bigrams, value = T)
        }
        
        if (identical(character(0),search.result)) {
                print("We haven't found any suggestion, please check for spelling errors.") ## If all levels are searched and no ngram is found, this message is returned
        } else {
                search.result <- strsplit(search.result, split = " ") ## Splitting all words of all ngrams found
                
                ## Picking the last word of the first, second and third most frequent results
                fst <- search.result[[1]][length(search.result[[1]])] 
                if (length(search.result) > 1) {snd <- search.result[[2]][length(search.result[[2]])]} else {snd <- ""} ## If there is a second result, return the last word of it
                if (length(search.result) > 2) {trd <- search.result[[3]][length(search.result[[3]])]} else {trd <- ""} ## If there is a third result, return the last word of it
                sug.table <- data.table(fst, snd, trd)
                return(sug.table)
        }
}