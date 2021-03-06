---
title: "WordSuggestion Guide"
author: "Sovann Prey"
date: "5/13/2021"
output: html_document
---



# Summary
This application aims to predict the most likely next word based on a string of text that the users enter into the input box.
                            
# Datasets
To make word suggestion, the application uses a mixed sample of news, blogs and twitter text, which has been manipulated in order to facilitate the suggestion process.

# App Usage
Users type any text in the input box. The only limitation is that the input needs to consist of only non-numeric, non-special characters and spaces. If the text length is 2 words or more, the algorithm will isolate the last two words and make a suggestion based on these. If you only input one word, the algorithm will make a suggestion based on that. Since the maximum input has been restricted to two words to make the application faster, some of the predictions might not make sense when inputting large sentences.

# App Algorithms
The algorithm calculates the probabilities of the next word occurring and presents three words that have the highest probability to be the next word based on the input provided. If there is no prediction based on the input, the output will be NA.
                          
# Source Code
The source code can be found in this [GutHub link](https://github.com/preysovann/wrdsgst)
        
