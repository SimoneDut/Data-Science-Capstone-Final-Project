library(shiny)

shinyUI(navbarPage("Predict the next word",
                   tabPanel("App",
                            sidebarLayout(
                              sidebarPanel(
                                textInput(inputId = "sentence", label = "Input sentence"),
                                sliderInput(inputId = "max_results", label = "Number of results", min = 1, max = 5, value = 3)
                                ),
                              mainPanel(
                                h5(tags$b("Predicted next words")),
                                conditionalPanel(condition="input.max_results>=1",
                                                 splitLayout(
                                                   verbatimTextOutput(outputId = "next_word1"),
                                                   actionButton("update1", "Update")
                                                )
                                ),
                                conditionalPanel(condition="input.max_results>=2",
                                                 splitLayout(
                                                   verbatimTextOutput(outputId = "next_word2"),
                                                   actionButton("update2", "Update")
                                                 )
                                ),
                                conditionalPanel(condition="input.max_results>=3",
                                                 splitLayout(
                                                   verbatimTextOutput(outputId = "next_word3"),
                                                   actionButton("update3", "Update")
                                                 )
                                ),
                                conditionalPanel(condition="input.max_results>=4",
                                                 splitLayout(
                                                   verbatimTextOutput(outputId = "next_word4"),
                                                   actionButton("update4", "Update")
                                                 )
                                ),
                                conditionalPanel(condition="input.max_results>=5",
                                                 splitLayout(
                                                   verbatimTextOutput(outputId = "next_word5"),
                                                   actionButton("update5", "Update")
                                                 )
                                ),
                                h5(tags$b("Probability plot")),
                                plotOutput(outputId = "plot")
                                )
                              )
                            ),
                   tabPanel("Instructions",
                            mainPanel(
                              h4(tags$b("Purpose of this App"), align = "center"),
                              br(),
                              h5("This App allows users to predict the next word based on a sentence provided and a desired number of results", align = "justify"),
                              h5("Additionally, the user can select a result to have it automatically added to the sentence.", align = "justify"),
                              h5("Furthermore, it is possible to read the probability of each result from a plot.", align = "justify"),
                              h5("Note: the model is based on a 3-grams model.", align = "justify"),
                              br(),
                              h4(tags$b("How to set the parameters"), align = "center"),
                              br(),
                              h5("In the top left corner of the page, it is possible to type the sentence in the 'Input sentence' input text box.", align = "justify"),
                              h5("The desired number of results can then by selected through the 'Number of results' input slider.", align = "justify"),
                              br(),
                              h4(tags$b("Results"), align = "center"),
                              br(),
                              h5("Based on the parameters, the results are automatically shown in the top right corner of the page.", align = "justify"),
                              h5("By clicking 'Update' next to each result, it will be automatically added at the end of the sentence.", align = "justify"),
                              h5("Finally, the plot in the bottom right corner of the page will display the probability of each result.", align = "justify"),
                              width = 4)
                            )
                   )
        )
