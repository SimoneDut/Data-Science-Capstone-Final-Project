library(shiny)
source("utils.R")

shinyServer(function(input, output, session) {
  
  observe({
    max_results <- input$max_results
    results <- nextwords(input$sentence, input$max_results)
    if (max_results >= 1) {
      output$next_word1 <- renderText(results$next_word[1])
    } else {
      output$next_word1 <- NULL
    }
    if (max_results >= 2) {
      output$next_word2 <- renderText(results$next_word[2])
    } else {
      output$next_word2 <- NULL
    }
    if (max_results >= 3) {
      output$next_word3 <- renderText(results$next_word[3])
    } else {
      output$next_word3 <- NULL
    }
    if (max_results >= 4) {
      output$next_word4 <- renderText(results$next_word[4])
    } else {
      output$next_word4 <- NULL
    }
    if (max_results >= 5) {
      output$next_word5 <- renderText(results$next_word[5])
    } else {
      output$next_word5 <- NULL
    }
    output$plot <- renderPlot(barplot(results$p * 100, names.arg = results$next_word, col = "darkblue", xlab = "Next word", ylab = "Probability (%)", ylim = c(0, 100)))

  })
    
  observeEvent(input$update1, {
    isolate({ updateTextInput(session, "sentence", value = paste(input$sentence, nextwords(input$sentence, input$max_results)$next_word[1])) })
  })
  observeEvent(input$update2, {
    isolate({ updateTextInput(session, "sentence", value = paste(input$sentence, nextwords(input$sentence, input$max_results)$next_word[2])) })
  })
  observeEvent(input$update3, {
    isolate({ updateTextInput(session, "sentence", value = paste(input$sentence, nextwords(input$sentence, input$max_results)$next_word[3])) })
  })
  observeEvent(input$update4, {
    isolate({ updateTextInput(session, "sentence", value = paste(input$sentence, nextwords(input$sentence, input$max_results)$next_word[4])) })
  })
  observeEvent(input$update5, {
    isolate({ updateTextInput(session, "sentence", value = paste(input$sentence, nextwords(input$sentence, input$max_results)$next_word[5])) })
  })
  
})
