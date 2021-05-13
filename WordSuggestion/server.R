library(shiny)
library(stringr)
library(stringi)
library(dplyr)
library(tm)
library(NLP)
library(RWeka)


load(file = "ngrams.Rda")

predict_fun <- function(inputString, unigram.cp, bigram.cp, trigram.cp) {
    if (length(inputString) > 0) {
        
        inputString <- tolower(inputString)
        unigram.tokenizer <- function(x) NGramTokenizer(x,Weka_control(min=1,max=1))
        inputString.token <- unigram.tokenizer(inputString)

        if (length(inputString.token) > 2) {
            inputString.token <- tail(inputString.token,2)
            prediction.df <- trigram.cp[(trigram.cp$word1==inputString.token[1] 
                                         & trigram.cp$word2==inputString.token[2]),]
            row.names(prediction.df) <- NULL
            prediction1 <- prediction.df$predicted[1]
            prediction2 <- prediction.df$predicted[2]
            prediction3 <- prediction.df$predicted[3]
            
            if (is.na(prediction1)) {
                prediction.df <- bigram.cp[(bigram.cp$word1==inputString.token[2]),]
                row.names(prediction.df) <- NULL
                prediction1 <- prediction.df$predicted[1]
                prediction2 <- prediction.df$predicted[2]
                prediction3 <- prediction.df$predicted[3]
            }
            
        } else if (length(inputString.token) == 2) {
            prediction.df <- trigram.cp[(trigram.cp$word1==inputString.token[1] 
                                         & trigram.cp$word2==inputString.token[2]),]
            row.names(prediction.df) <- NULL
            prediction1 <- prediction.df$predicted[1]
            prediction2 <- prediction.df$predicted[2]
            prediction3 <- prediction.df$predicted[3]
            
            if (is.na(prediction1)) {
                prediction.df <- bigram.cp[(bigram.cp$word1==inputString.token[2]),]
                row.names(prediction.df) <- NULL
                prediction1 <- prediction.df$predicted[1]
                prediction2 <- prediction.df$predicted[2]
                prediction3 <- prediction.df$predicted[3]
            }
            
        } else if (length(inputString.token) == 1) {
            prediction.df <- bigram.cp[(bigram.cp$word1==inputString.token[1]),]
            row.names(prediction.df) <- NULL
            prediction1 <- prediction.df$predicted[1]
            prediction2 <- prediction.df$predicted[2]
            prediction3 <- prediction.df$predicted[3]
        } else if (length(inputString.token) < 1) {
            prediction1 <- "Please enter one or more words"
            prediction2 <- ""
            prediction3 <- ""
        }
        
    }
    return(c(prediction1, prediction2, prediction3))
}

shinyServer(
    function(input, output) {
        observe({
            if (input$goButton > 0) {
                param <- input$text
                prediction <- predict_fun(input$text, unigram.cp, bigram.cp, 
                                          trigram.cp)
                output$text1 <- renderText({ 
                    paste("First word:    ", prediction[1])
                })
                output$text2 <- renderText({ 
                    paste("Second word: ", prediction[2])
                })
                output$text3 <- renderText({ 
                    paste("Third word:  ", prediction[3])
                })
                
            }
        })    
    }
)