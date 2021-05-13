library(shiny)
library(knitr)

shinyUI(navbarPage("Menu",
                   tabPanel("Application",
                            pageWithSidebar(
                              headerPanel("Word Suggestion"),
                              sidebarPanel(
                                textInput("text", label = h5("Please enter text below 
                                                             (characters only)")),
                                actionButton("goButton", "Predict")
                              ),
                              mainPanel(
                                h3('Closest matches'),
                                textOutput("text1"),
                                textOutput("text2"),
                                textOutput("text3")
                              )
                            )
                   ),
                   tabPanel("User Guide", 
                            HTML(markdown::markdownToHTML(knit("user_guide.Rmd"))))
                   
  )
)