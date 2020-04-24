require(quanteda)
require(readtext)
source("constants.R")

# Downloading the zip file, if necessary
if (!file.exists(dataFileName)) {
  download.file(dataFileUrl, destfile = dataFileName, method = "curl")
  unzip(dataFileName)
}

for (englishFileName in englishFileNames) {

  lines <- readLines(paste0(englishFilePath, englishFileName, ".txt"))
  
  total_lines <- length(lines)
  indices <- sample(total_lines)
  
  stop_indices <- sapply(0:n_splitfiles, function(x) { floor(total_lines*x/n_splitfiles) })
  
  for (i in 1:n_splitfiles) {
    outputfile <- file(paste0(englishFilePath, englishFileName, "_", i, ".txt"))
    writeLines(lines[indices[(stop_indices[i]+1):stop_indices[i+1]]], outputfile)
    close(outputfile)
  }

}