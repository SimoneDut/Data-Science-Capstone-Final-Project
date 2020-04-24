## Dependencies
require(quanteda)
require(data.table)
source("constants.R")

model <- list()
for (i in 1:n_grams) {

  # Do this only for highest order ngrams
  if (i >= min_n) {

      for (englishFileName in englishFileNames) {

      # Restore the nGramModel object
      ngram_temp <- readRDS(file = paste0(englishFileName, "_", i, "grams.rds"))
      # extract features with frequency of 1
      singlefeatures_temp <- ngram_temp[ngram_temp$frequency == 1,]$feature
      # extract features with frequency greater than 1
      nonsinglefeatures_temp <- ngram_temp[ngram_temp$frequency != 1,]$feature
      ## rm()
      rm(ngram_temp)
      ## gc()
      gc()
      saveRDS(singlefeatures_temp, file = paste0(englishFileName, "_", i, "singlefeatures.rds"))
      saveRDS(nonsinglefeatures_temp, file = paste0(englishFileName, "_", i, "nonsinglefeatures.rds"))
      ## rm()
      rm(singlefeatures_temp)
      rm(nonsinglefeatures_temp)
      ## gc()
      gc()
      
    }
    
    singlefeatures_combined <- NULL
    for (englishFileName in englishFileNames) {

      singlefeatures_temp <- readRDS(file = paste0(englishFileName, "_", i, "singlefeatures.rds"))
      if (is.null(singlefeatures_combined)) {
        singlefeatures_combined <- singlefeatures_temp
      } else {
        singlefeatures_combined <- c(singlefeatures_combined, singlefeatures_temp)
      }
      ## gc()
      gc()
    }
    ## rm()
    rm(singlefeatures_temp)
    ## gc()
    gc()
    singlefeatures_combined <- data.table(feature = singlefeatures_combined)
    singlefeatures_combined <- singlefeatures_combined[, list(nr = .N), by = feature]
    singlefeatures_combined <- singlefeatures_combined[nr > 1,]
    
    nonsinglefeatures_combined <- NULL
    for (englishFileName in englishFileNames) {

      nonsinglefeatures_temp <- readRDS(file = paste0(englishFileName, "_", i, "nonsinglefeatures.rds"))
      if (is.null(nonsinglefeatures_combined)) {
        nonsinglefeatures_combined <- nonsinglefeatures_temp
      } else {
        nonsinglefeatures_combined <- c(nonsinglefeatures_combined, nonsinglefeatures_temp)
      }
      ## gc()
      gc()
    }
    ## rm()
    rm(nonsinglefeatures_temp)
    ## gc()
    gc()
    nonsinglefeatures_combined <- data.table(feature = nonsinglefeatures_combined)
    nonsinglefeatures_combined <- nonsinglefeatures_combined[, list(nr = .N), by = feature]
    nonsinglefeatures_combined <- rbind(nonsinglefeatures_combined, singlefeatures_combined)[, "feature"]
    ## rm()
    rm(singlefeatures_combined)
    ## gc()
    gc()
    nonsinglefeatures_combined <- nonsinglefeatures_combined[, list(nr = .N), by = feature]
    nonsinglefeatures_combined <- nonsinglefeatures_combined$feature
    nonsinglefeatures_count <- length(nonsinglefeatures_combined)

  }

  complete_model_temp <- NULL
  complete_root_frequencies_temp <- NULL
  singlefeatures_count <- 0
  for (englishFileName in englishFileNames) {

    # Restore the nGramModel object
    ngram_temp <- readRDS(file = paste0(englishFileName, "_", i, "grams.rds"))
    
    # create the model
    feature_temp <- ngram_temp$feature
    frequency_temp <- ngram_temp$frequency
    ## rm()
    rm(ngram_temp)
    ## gc()
    gc()
    if (i == 1) {
      last_word_temp <- feature_temp
      root_temp <- rep(".", length(last_word_temp))
    } else {
      #feature_vect_temp <- strsplit(feature_temp, split = "_")
      root_temp <- sapply(feature_temp, function(x) { paste0(strsplit(x, split = "_")[[1]][-i], collapse = "_") })#sapply(feature_vect_temp, function(x) { paste0(x[1:(i-1)], collapse = "_") })
      last_word_temp <- sapply(feature_temp, function(x) { strsplit(x, split = "_")[[1]][i] })#sapply(feature_vect_temp, function(x) { x[i] })

      ## gc()
      gc()
    }
    model_temp <- data.table(feature = feature_temp, root = root_temp, last_word = last_word_temp, frequency = frequency_temp)
    
    ## rm()
    rm(feature_temp)
    rm(root_temp)
    rm(last_word_temp)
    rm(frequency_temp)
    ## gc()
    gc()
  
    # calculate root_frequency
    if (is.null(complete_root_frequencies_temp)) {
      complete_root_frequencies_temp <- model_temp[, list(root_frequency = sum(frequency)), by = root]
    } else {
      complete_root_frequencies_temp <- rbind(complete_root_frequencies_temp, model_temp[, list(root_frequency = sum(frequency)), by = root])
    }
    
    singlefeatures_count <- singlefeatures_count + sum(model_temp$frequency)
    
    if (i >= min_n) {
      model_temp <- model_temp[model_temp$feature %in% nonsinglefeatures_combined,]
      ## gc()
      gc()
    }

    singlefeatures_count <- singlefeatures_count - sum(model_temp$frequency)
    
    model_temp <- model_temp[, c("root", "last_word", "frequency")]
    
    if (is.null(complete_model_temp)) {
      complete_model_temp <- model_temp
    } else {
      complete_model_temp <- rbind(complete_model_temp, model_temp)
    }
    ## rm()
    rm(model_temp)
    ## gc()
    gc()
  }
  
  if (i >= min_n) {
    ## rm()
    rm(nonsinglefeatures_combined)
    ## gc()
    gc()
  }
  
  # group by root and last_word
  complete_model_temp <- complete_model_temp[, list(frequency = sum(frequency)), by = c("root", "last_word")]
  
  # add the root_frequency column
  complete_root_frequencies_temp <- complete_root_frequencies_temp[, list(root_frequency = sum(root_frequency)), by = root]

  complete_model_temp <- merge(complete_model_temp, complete_root_frequencies_temp, by = "root")
  
  ## rm()
  rm(complete_root_frequencies_temp)
  ## gc()
  gc()

  # calculate p
  complete_model_temp[, p := frequency / root_frequency]
  
  # calculate the discount with Good-Turing
  discount_GoodTuring_temp <- complete_model_temp[, list(nr = .N), by = frequency]
  
  if (i >= min_n) {
    discount_GoodTuring_temp <- rbind(discount_GoodTuring_temp, data.table(frequency = 1, nr = singlefeatures_count))
    ## rm()
    rm(nonsinglefeatures_count)
    rm(singlefeatures_count)
    ## gc()
    gc()
  }
  
  discount_GoodTuring_temp <- discount_GoodTuring_temp[order(frequency)]
  zr_temp <- numeric()
  # smooth
  if (nrow(discount_GoodTuring_temp) > 1) {
    for (j in 1:nrow(discount_GoodTuring_temp)) {
      if (j == 1) {
        zr_temp[j] <- discount_GoodTuring_temp$nr[j]/(0.5*discount_GoodTuring_temp$frequency[j+1])
      } else if (j == nrow(discount_GoodTuring_temp)) {
        zr_temp[j] <- discount_GoodTuring_temp$nr[j]/(discount_GoodTuring_temp$frequency[j] - discount_GoodTuring_temp$frequency[j-1])
      } else {
        zr_temp[j] <- discount_GoodTuring_temp$nr[j]/(0.5*(discount_GoodTuring_temp$frequency[j+1] - discount_GoodTuring_temp$frequency[j-1]))
      }
    }
  }
  discount_GoodTuring_temp[, zr := zr_temp]
  # regression
  regression_coeff_temp <- lm(log(discount_GoodTuring_temp$zr) ~ log(discount_GoodTuring_temp$frequency))$coeff
  discount_GoodTuring_temp[, zr_regression := exp(regression_coeff_temp[1])*(frequency^regression_coeff_temp[2])]
  # calculate dr
  dr_temp <- numeric()
  z_1 <- exp(regression_coeff_temp[1])*(1^regression_coeff_temp[2])
  z_k_plus_1 <- exp(regression_coeff_temp[1])*((k+1)^regression_coeff_temp[2])
  for (j in 1:nrow(discount_GoodTuring_temp)) {
    if (discount_GoodTuring_temp$frequency[j] <= k) {
      z_r <- exp(regression_coeff_temp[1])*(discount_GoodTuring_temp$frequency[j]^regression_coeff_temp[2])
      z_r_plus_1 <- exp(regression_coeff_temp[1])*((discount_GoodTuring_temp$frequency[j]+1)^regression_coeff_temp[2])
      dr_temp[j] <- (((discount_GoodTuring_temp$frequency[j]+1)/discount_GoodTuring_temp$frequency[j])*(z_r_plus_1/z_r) - (k+1)*z_k_plus_1/z_1)/(1 - (k+1)*z_k_plus_1/z_1)
    } else {
      dr_temp[j] <- 1
    }
  }
  discount_GoodTuring_temp[, dr := dr_temp]
  # update dr
  complete_model_temp <- merge(complete_model_temp, discount_GoodTuring_temp, by = "frequency")
  
  ## rm()
  rm(discount_GoodTuring_temp)
  ## gc()
  gc()
  
  # calculate p_star
  complete_model_temp[, p_star := dr * p]
  # select the important columns
  complete_model_temp <- complete_model_temp[, c("root", "last_word", "p_star", "dr", "p")]
  # order the complete_model_temp
  setorder(complete_model_temp, root, -p_star, last_word)
  model[[i]] <- complete_model_temp
}

# Save the model object to a file
saveRDS(model, file = "model.rds")
