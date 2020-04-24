## Dependencies
require(quanteda)
require(readtext)
source("constants.R")

# Downloading the bad words file, if necessary
if (!file.exists(badwordsFileName)) {
  download.file(badwordsFileUrl, destfile = badwordsFileName, method = "curl")
}
badwords <- readLines(badwordsFileName)[-369]

toks_raw <- NULL
toks_combined <- NULL

for (englishFileName in englishFileNames) {

  for (i in 1:n_splitfiles) {
    
    data_raw_temp <- readtext(paste0(englishFilePath, englishFileName, "_", i, ".txt"),
                              docvarsfrom = "filenames", 
                              docvarnames = c("language", "source"),
                              dvsep = "_US.",
                              encoding = "UTF-8")
    
    # Add Beginning Of Sentence (<bbbooosss>) and End Of Sentence (<eeeooosss>)
    data_raw_temp$text <- paste0("<bbbooosss>", gsub("[\n|!|?|\\.]", "<eeeooosss><bbbooosss>", data_raw_temp$text), "<eeeooosss>")
    data_raw_temp$text <- gsub("<bbbooosss><eeeooosss>", "", data_raw_temp$text)
    
    corp_raw_temp <- corpus(data_raw_temp)
    toks_raw_temp <- tokens(corp_raw_temp, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, split_hyphens = TRUE, remove_url = TRUE)
    toks_raw_temp <- tokens_tolower(toks_raw_temp)
    
    ## Change bbbooosss -> <bos>, eeeooosss -> <eos>
    toks_raw_temp <- tokens_replace(toks_raw_temp, c("bbbooosss", "eeeooosss", "bbbaaadddwwwooorrrddd"), c("<bos>", "<eos>", "<badword>"), valuetype = "fixed")
    
    ## Replace bad words -> <badword>
    toks_raw_temp <- tokens_replace(toks_raw_temp, badwords, rep("<badword>", length(badwords)), case_insensitive = TRUE)

    if (is.null(toks_raw)) {
      toks_raw <- toks_raw_temp
    } else {
      toks_raw <- toks_raw + toks_raw_temp
    }
  }
  
  saveRDS(toks_raw, file = paste0(englishFileName, "_toks.rds"))
  
  if (is.null(toks_combined)) {
    toks_combined <- toks_raw
  } else {
    toks_combined <- toks_combined + toks_raw
  }
  
  ## rm()
  rm(data_raw_temp)
  rm(corp_raw_temp)
  rm(toks_raw_temp)
  rm(toks_raw)
  
  ## gc()
  gc()
  
  toks_raw <- NULL

}

# lemmatization to remove the least frequent words
statistic <- textstat_frequency(dfm(toks_combined))
frequencies <- statistic$frequency
replaceWords <- statistic$feature
cumulativeDistribution <- cumsum(frequencies/sum(frequencies))
replaceWords <- tail(replaceWords, length(replaceWords) - sum(cumulativeDistribution <= coverage))
replaceLemma <- rep("<unk>", length(replaceWords))

## rm()
rm(toks_raw)
rm(toks_combined)
rm(statistic)
rm(frequencies)
rm(cumulativeDistribution)

## gc()
gc()


for (englishFileName in englishFileNames) {
  
  toks_filtered <- readRDS(file = paste0(englishFileName, "_toks.rds"))
  toks_filtered <- tokens_replace(toks_filtered, replaceWords, replaceLemma, valuetype = "fixed")
  
  ## gc()
  gc()
  
  # create the n-grams models
  for (i in 1:n_grams) {

    toks_temp <- tokens_ngrams(toks_filtered, n = i)
    
    ## gc()
    gc()
    
    df_temp <- dfm(toks_temp)
    
    ## rm()
    rm(toks_temp)
    
    ## gc()
    gc()
    
    ngram_temp <- textstat_frequency(df_temp)
    
    ## rm()
    rm(df_temp)
    
    ## gc()
    gc()
    
    # solve the problem between sentences
    indicesBetweenSentences <- grep("((.*)<eos>(.+))|((.+)<bos>(.*))", ngram_temp$feature)
    
    if (length(indicesBetweenSentences) != 0) {
      ngram_temp <- ngram_temp[-indicesBetweenSentences]
    }
    
    ## rm()
    rm(indicesBetweenSentences)
    
    ## gc()
    gc()
    
    # remove features with <unk>
    ngram_temp <- ngram_temp[-grep("(.*)<unk>(.*)", ngram_temp$feature)]
    
    ## gc()
    gc()
    
    # Save the nGramModel object to a file
    saveRDS(ngram_temp, file = paste0(englishFileName, "_", i, "grams.rds"))
    
    ## rm()
    rm(ngram_temp)
    
    ## gc()
    gc()
    
  }
  
  ## rm()
  rm(toks_filtered)

  ## gc()
  gc()

}
