# Set source files 

buildModel <- function(modelDir="Model", modelType="KN", stopwordsflag=FALSE) {
 
  source("library_setup.r", local = TRUE)
  source("model_helpers.r", local = TRUE)
  options(java.parameters = "-Xmx4g" ) #increase Java memory 
  options(mc.cores=1)  ## Need to set this because tokenizer seems to hang when running on OSX Java with parallel processing.
  
  # Setup environment by loading the necessary libraries
  setupEnvironment()
  
  #Establish the Corpus and Perform Clean Up
  
  # I will use the tm package on the unified sample data file to create a corpus. The clean up will involve the following:
  #   
  # - convert to lowercase
  # - remove characters /, @ |
  # - common punctuation
  # - remove numbers
  # - remove english stop words
  # - strip whitespace
  # 
  # End the pre-processing by establishing that the repository contains text documents. 
  # 
  # profanity filtering
  badwords <- readLines("badwords.txt")
  stop_words <- readLines("stop_words.txt")
  
  # Use the tm package to perform basic filtering
  removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
  removeWWW <- function(x) gsub("www[[:alnum:]]*", "", x)
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
  
  cname <- file.path(modelDir)  #point to the document repository
  docs <- Corpus(DirSource(cname)) # create the corpus

  if (stopwordsflag==TRUE) docs <- tm_map(docs, content_transformer(removeWords), stop_words) # remove english stop words
  docs <- tm_map(docs, content_transformer(removeWords), badwords)
  docs <- tm_map(docs, content_transformer(tolower))  # convert to lowercase
  docs <- tm_map(docs, toSpace, "/|@|\\|")  # remove more transforms
  docs <- tm_map(docs, content_transformer(removePunctuation)) # remove punctuation
  docs <- tm_map(docs, content_transformer(removeNumbers)) # remove numbers
  docs <- tm_map(docs, content_transformer(stripWhitespace)) # strip whitespace
  docs <- tm_map(docs, content_transformer(removeURL))
  docs <- tm_map(docs, content_transformer(removeWWW))
  if (stopwordsflag==TRUE) docs <- tm_map(docs, content_transformer(removeWords), stop_words) # remove english stop words
  docs <- tm_map(docs, content_transformer(removeWords), badwords)
  
  # Finalize the pre-processing by setting the documents as text documents
  docs <- tm_map(docs, PlainTextDocument)   
  
  # Now I will create 1, 2, and 3 gram tokenizers and then the document term
  # matrix for each of these. I will also remove sparse terms from each of
  # the document term matrices. This will get rid of less-frequent terms.
  
  token <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1))}
  onetm <- DocumentTermMatrix(docs, control = list(tokenize = token, wordLengths = c(3, Inf)))
  onetms <- removeSparseTerms(onetm, 0.999)
  rm(onetm)
  
  token <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))}
  twotm <- DocumentTermMatrix(docs, control = list(tokenize = token, wordLengths = c(3, Inf)))
  twotms <- removeSparseTerms(twotm, 0.999)
  rm(twotm)
  
  token <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))}
  threetm <- DocumentTermMatrix(docs, control = list(tokenize = token, wordLengths = c(3, Inf)))
  threetms <- removeSparseTerms(threetm, 0.999)
  rm(threetm)

  token <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 4, max = 4))}
  fourtm <- DocumentTermMatrix(docs, control = list(tokenize = token, wordLengths = c(3, Inf)))
  fourtms <- removeSparseTerms(fourtm, 0.999)
  rm(fourtm)

  token <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 5, max = 5))}
  fivetm <- DocumentTermMatrix(docs, control = list(tokenize = token, wordLengths = c(3, Inf)))
  fivetms <- removeSparseTerms(fivetm, 0.999)
  rm(fivetm)
  
  #   Build Model
  
  #  The basic model is: 
      
  #   P(wn | w1, w2, w3, wn-1) - conditional probability of the last word 
  #   
  #   Model Building - create the model table. We start
  #   by counting the number of times each word has been used.

  if (modelType=="KN") # build for KN model
  {
    # Create the probability matrices for each of the n-grams
    onetm.pMatrix <- probabilityMatrix(onetms, modelType)
    setkey(onetm.pMatrix, term)
    twotm.pMatrix <- probabilityMatrix(twotms, modelType)
    twotm.pMatrix.starting_uni <- copy(twotm.pMatrix)
    twotm.pMatrix.ending_uni <- copy(twotm.pMatrix)
    setkey(twotm.pMatrix, term)
    setkey(twotm.pMatrix.starting_uni, starting_uni)
    setkey(twotm.pMatrix.ending_uni, ending_uni)
    threetm.pMatrix <- probabilityMatrix(threetms, modelType)
    threetm.pMatrix.starting_bi <- copy(threetm.pMatrix)
    threetm.pMatrix.ending_bi <- copy(threetm.pMatrix)
    setkey(threetm.pMatrix, term)
    setkey(threetm.pMatrix.starting_bi, starting_bi)
    setkey(threetm.pMatrix.ending_bi, ending_bi)
    
    # Gather counts of data to be used in runtime
    threetm.rowcount <- nrow(threetm.pMatrix)
    twotm.rowcount <- nrow(twotm.pMatrix)
    onetm.rowcount <- nrow(onetm.pMatrix)
    
    # Save the objects for the prediction 
    save(onetm.pMatrix, twotm.pMatrix, twotm.pMatrix.starting_uni, twotm.pMatrix.ending_uni,
         threetm.pMatrix, threetm.pMatrix.starting_bi, threetm.pMatrix.ending_bi,
         badwords, stop_words, threetm.rowcount, twotm.rowcount, onetm.rowcount,
         file= paste0(modelDir, "-KN-", "modelData.RData"))
    
    rm(onetm.pMatrix, twotm.pMatrix, threetm.pMatrix, docs, 
       threetm.rowcount, twotm.rowcount, onetm.rowcount, 
       twotm.pMatrix.starting_uni, twotm.pMatrix.ending_uni,
       threetm.pMatrix.starting_bi, threetm.pMatrix.ending_bi, onetms, twotms, threetms, 
       fourtms, fivetms, onetm, twotm, threetm, 
       fourtm, fivetm)
  }
  if (modelType=="BF") # Build for back off model
  {
    # Create the probability matrices for each of the n-grams
    onetm.pMatrix <- probabilityMatrix(onetms, modelType)
    setkey(onetm.pMatrix, starting)
    twotm.pMatrix <- probabilityMatrix(twotms, modelType)
    setkey(twotm.pMatrix, starting)
    threetm.pMatrix <- probabilityMatrix(threetms, modelType)
    setkey(threetm.pMatrix, starting)
    fourtm.pMatrix <- probabilityMatrix(fourtms, modelType)
    setkey(fourtm.pMatrix, starting)
    fivetm.pMatrix <- probabilityMatrix(fivetms, modelType)
    setkey(fivetm.pMatrix, starting)
    
    # Gather counts of data to be used in runtime
    fivetm.rowcount <- nrow(fivetm.pMatrix)
    fourtm.rowcount <- nrow(fourtm.pMatrix)
    threetm.rowcount <- nrow(threetm.pMatrix)
    twotm.rowcount <- nrow(twotm.pMatrix)
    onetm.rowcount <- nrow(onetm.pMatrix)
    
    # Save the objects for the prediction 
    save(onetm.pMatrix, twotm.pMatrix,
         threetm.pMatrix,fourtm.pMatrix, fivetm.pMatrix,
         fivetm.rowcount, fourtm.rowcount,
         badwords, stop_words, threetm.rowcount, twotm.rowcount, onetm.rowcount,
         file= paste0(modelDir, "-BF-", "modelData.RData"))
    
    rm(onetm.pMatrix, twotm.pMatrix, threetm.pMatrix, fourtm.pMatrix, fivetm.pMatrix, docs, 
       threetm.rowcount, twotm.rowcount, onetm.rowcount, fourtm.rowcount, fivetm.rowcount, token,
       onetms, twotms, threetms, 
       fourtms, fivetms, onetm, twotm, threetm, 
       fourtm, fivetm)
  }
  gc(verbose=FALSE)
}

