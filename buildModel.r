# Set source files 

buildModel <- function(modelDir) {
 
  # Set the working directory
  #setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Coursera/Data Science Capstone/Report")
  
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

  docs <- tm_map(docs, removeWords, stopwords("english")) # remove english stop words
  docs <- tm_map(docs, removeWords, badwords)
  docs <- tm_map(docs, content_transformer(tolower))  # convert to lowercase
  docs <- tm_map(docs, toSpace, "/|@|\\|")  # remove more transforms
  docs <- tm_map(docs, content_transformer(removePunctuation)) # remove punctuation
  docs <- tm_map(docs, content_transformer(removeNumbers)) # remove numbers
  docs <- tm_map(docs, content_transformer(stripWhitespace)) # strip whitespace
  docs <- tm_map(docs, content_transformer(removeURL))
  docs <- tm_map(docs, content_transformer(removeWWW))
  
  # Finalize the pre-processing by setting the documents as text documents
  docs <- tm_map(docs, PlainTextDocument)   
  
  # Now I will create 1, 2, and 3 gram tokenizers and then the document term
  # matrix for each of these. I will also remove sparse terms from each of
  # the document term matrices. This will get rid of less-frequent terms.
  
  OneGramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1))}
  #onetm <- DocumentTermMatrix(docs, control = list(tokenize = OneGramTokenizer, wordLengths = c( 1, Inf)))
  onetm <- DocumentTermMatrix(docs, control = list(tokenize = OneGramTokenizer))
  onetms <- removeSparseTerms(onetm, 0.999)
  
  TwogramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))}
  twotm <- DocumentTermMatrix(docs, control = list(tokenize = TwogramTokenizer))
  twotms <- removeSparseTerms(twotm, 0.999)
  
  ThreegramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))}
  threetm <- DocumentTermMatrix(docs, control = list(tokenize = ThreegramTokenizer))
  threetms <- removeSparseTerms(threetm, 0.999)
  
  #   Build Model
  
  #  The basic model is: 
      
  #   P(wn | w1, w2, w3, wn-1) - conditional probability of the last word give the
  #   previous words Store language models in logs
  #   
  #   Model Building We want to create the probabilities table. We start
  #   by counting the number of times each word has been used.

  #Create the probability matrices for each of the n-grams
  onetm.pMatrix <- probabilityMatrix(onetm)
  twotm.pMatrix <- probabilityMatrix(twotm)
  threetm.pMatrix <- probabilityMatrix(threetm)
  
  # Save the objects for the prediction 
  save(onetm.pMatrix, twotm.pMatrix, threetm.pMatrix, badwords, stop_words, file= paste0(modelDir, "-", "modelData.RData"))
  rm(onetm.pMatrix, twotm.pMatrix, threetm.pMatrix, docs)
  gc(verbose=FALSE)
}

