predictbf<- function(input, modelID, maxResults=3, logging=FALSE, stopwordsflag=FALSE)
{
  # load the necessary libraries
  source("cleantext.r", local = TRUE)
  
  # Initialize local variables
  searchQuadgramFlag <- FALSE
  searchTrigramFlag <- FALSE
  searchBigramFlag <- FALSE
  searchUnigramFlag <- FALSE
  numberOfX <- 0
  gramsExecuted <- matrix(0:0, nrow = 4, ncol = 1) # will keep track of what was executed
  
  
  # Clean up the input data
  inputItems <- cleantext(input, stopwordsflag = FALSE, modelID)
  input <- inputItems[1]  # Cleaned input string
  input1 <- inputItems[2] # First word of input
  input2 <- inputItems[3] # Second word of input
  input3 <- inputItems[4] # Third word of input
  input4 <- inputItems[5] # Third word of input
  nwords <- inputItems[6] # Total number of input characters provided 
  if (nwords==1) input2 <- input1
  
  # Check if the number of characters and words in input is sufficient
  # We need at least 1 words that at least 2 characters for the prediction to execute
  if (nchar(input) < 2 || nwords < 1 ) {
    results <- as.vector("Invalid length of input")
    return(results)
  }

  if (nwords==4) # we have three words provided as input...search in pentagram
  {
    # Get all possible ending unigrams from trigram data
    # This will give a listing possible predictions for the two words that were provided 
    x_words <- fivetm.pMatrix[J(input), nomatch=0]
    x_words$count <- as.numeric(x_words$count)
    x_words <- x_words[order(count, decreasing=TRUE), ]
    numberOfX <- nrow(x_words)
    if (numberOfX>maxResults) { 
      x_words<-x_words[1:maxResults, ]
      numberOfX <-maxResults
    }
    gramsExecuted[1] <- gramsExecuted[1]+1
    if (numberOfX==0) searchQuadgramFlag = TRUE # not found in trigram so set flag to back off to bigram
  }
  
  if (nwords==3 || searchQuadgramFlag==TRUE) # we have three words provided as input...search in quadgram
  {
    # Get all possible ending unigrams from trigram data
    # This will give a listing possible predictions for the two words that were provided 
    x_words <- fourtm.pMatrix[J(input), nomatch=0]
    x_words$count <- as.numeric(x_words$count)
    x_words <- x_words[order(count, decreasing=TRUE), ]
    numberOfX <- nrow(x_words)
    if (numberOfX>maxResults) { 
      x_words<-x_words[1:maxResults, ]
      numberOfX <-maxResults
    }
    if (numberOfX==0) searchTrigramFlag = TRUE # not found in trigram so set flag to back off to bigram
  }
  
  if (nwords==2 || searchTrigramFlag==TRUE) # we have two words provided as input...search in trigram
  {
    # Get all possible ending unigrams from trigram data
    # This will give a listing possible predictions for the two words that were provided 
    x_words <- threetm.pMatrix[J(input), nomatch=0]
    x_words$count <- as.numeric(x_words$count)
    x_words <- x_words[order(count, decreasing=TRUE), ]
    numberOfX <- nrow(x_words)
    if (numberOfX>maxResults) { 
      x_words<-x_words[1:maxResults, ]
      numberOfX <-maxResults
    }
    if (numberOfX==0) searchBigramFlag = TRUE # not found in trigram so set flag to back off to bigram
  }
    
  if (nwords==1 || searchBigramFlag==TRUE) # Search bigram
  {
    # Input was not found in trigram table so go to bigram
    x_words <- twotm.pMatrix[input2, nomatch=0]
    x_words$count <- as.numeric(x_words$count)
    x_words <- x_words[order(count, decreasing=TRUE), ]
    numberOfX <- nrow(x_words)
    if (numberOfX>maxResults) { 
      x_words<-x_words[1:maxResults, ]
      numberOfX <-maxResults
    }
    if (numberOfX==0) searchUnigramFlag = TRUE #bigram now found so back off to unigram
  }
  
  if (searchUnigramFlag==TRUE) # search unigram; this will only run if tri and bi fail
  {
    # Neither tri or bigram was found.
    # So take the top ten unigrams and send that information
    x_words <- onetm.pMatrix
    x_words$count <- as.numeric(onetm.pMatrix$count)
    if (nrow(x_words) > maxResults) { 
      x_words<-x_words[order(x_words$count, decreasing=TRUE), ][1:maxResults,]
      numberOfX <-maxResults
    }
    #x_words$ending_uni <- x_words$term
  }
  
  return(x_words$prediction)
}

