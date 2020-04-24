## Dependencies
require(quanteda)
require(data.table)

## Constants
badwordsFileName <- "badwords.txt"

badwords <- readLines(badwordsFileName)[-369]

nextwordstable <- function(n_search, vect_search, results_temp, p_left) {
  
  root_search <- paste0(vect_search, collapse = "_") 
  if (root_search %in% model[[n_search]]$root) {
    results_new <- model[[n_search]][model[[n_search]]$root == root_search, c("last_word", "p")]
    if (is.null(results_temp)) {
      results_temp <- data.table(next_word = results_new$last_word, p = results_new$p)
    } else {
      alpha <- p_left / (1- sum(results_new[results_new$last_word %in% results_temp$next_word, ]$p))
      results_new <- results_new[!(results_new$last_word %in% results_temp$next_word), ]
      results_temp <- rbind(results_temp, data.table(next_word = results_new$last_word, p = alpha*results_new$p))
    }
    rm(results_new)
    p_left = 1 - sum(results_temp$p)
  }
  
  if (n_search == 1) {
    return(results_temp)
  } else  {
    if (length(vect_search) == 1) {
      vect_search <- "."
    } else  {
      vect_search <- vect_search[-1]
    }
    return(nextwordstable(n_search-1, vect_search, results_temp, p_left))
  }
  
}

nextwords <- function(sentence, max_results) {
  
  sentence <- paste0("<bbbooosss>", gsub("[\n|!|?|\\.]", "<eeeooosss><bbbooosss>", sentence))     #paste0("<bbbooosss>", sentence)
  names(sentence) <- "sentence"
  corp_sentence <- corpus(sentence)
  toks_sentence <- tokens(corp_sentence, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, split_hyphens = TRUE, remove_url = TRUE)
  toks_sentence <- tokens_tolower(toks_sentence)
  toks_sentence <- tokens_replace(toks_sentence, c("bbbooosss", "eeeooosss"), c("<bos>", "<eos>"), valuetype = "fixed")
  toks_sentence <- tokens_replace(toks_sentence, badwords, rep("<badword>", length(badwords)), case_insensitive = TRUE)
  vect_sentence <- as.character(toks_sentence)
  
  n_search <- min(length(vect_sentence) + 1, length(model))
  vect_search <- vect_sentence[(length(vect_sentence)-n_search+2):length(vect_sentence)]
  
  # calculate table
  results <- nextwordstable(n_search, vect_search, NULL, 1)

  # remove <bos>, <eos> and <badword> from the results  
  results <- results[!(next_word %in% c("<bos>", "<eos>", "<badword>")),]
  
  # order
  setorder(results, -p)
  
  # limit rows
  if (nrow(results) > max_results) {
    results <- results[1:max_results, ]
  }
  
  return(results)
  
}


# Restore the model object
model <- readRDS(file = "model.rds")
