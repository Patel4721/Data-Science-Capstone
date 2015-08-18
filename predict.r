predictWord <- function(input, modelDir, maxResults=3, logging=FALSE, discount=0.75, stopwordsflag=TRUE)
{
  # Clean up the input data
  input <- tolower(input)
  input <- rev(unlist(strsplit(input," ")))
  if (stopwordsflag) input <- setdiff(input, stop_words)
  input <- removePunctuation(input)
  input <- removeNumbers(input)
  nwords <- length(input)
  input <- setdiff(input, badwords)
  input <- input[grepl('[[:alpha:]]',input)]
  input2 <- input[1]
  input1 <- input[2]
  if (is.na(input1)) input1 <- ""
  if (is.na(input2)) input2 <- ""
  
  if (length(input1)>0 && length(input2>0)) {input <- paste(input1, input2, sep = ' ') 
  } else {
    if (nchar(input1)>0) input <- input1
    else if (nchar(input2)>0) input <- input2
  }
  
  # Check if the number of characters and words in input is sufficient
  # We need at least 1 words that at least 2 characters for the prediction to execute
  if (nchar(input) < 3 || nwords < 1 ) {
    results <- as.vector("Invalid length of input")
    return(results)
  }
  
  # Find out how many words there are in the input - at this point it will be one or two words
  nInputsWords <- sapply(stri_split_fixed(input, " "), length)
  
  # Get all possible ending unigrams from trigram data
  # This will give a listing of all possible predicted words 
  x_words <- threetm.pMatrix.starting_bi[J(input), nomatch=0]
  x_words$count <- as.numeric(x_words$count)
  x_words <- x_words[order(count, decreasing=T), ]
  numberOfX <- nrow(x_words)
  if (numberOfX>3) { 
    x_words<-x_words[1:3, ]
    numberOfX <-3
  }

  # Now we want to loop through all possible predicted words
  if (numberOfX>0)
  {  
    # The input was found in the trigram table
    input_bi_count <- as.numeric(twotm.pMatrix[input, nomatch=0]$count)
    tri_lamda <- discount / input_bi_count * numberOfX
    tri_prob <- 0
    prob <- 0
    
    for (i in 1:numberOfX)
      {
        cw2w1 <- as.numeric(threetm.pMatrix[paste0(input, " ", x_words$ending_uni[i]), nomatch=0]$count)
        # Calculate the trigram probability
        tri_prob <- max((cw2w1-discount),0) / input_bi_count
        
        # Continuity count for the bigram portion of KN algorithm
        cw2 <- nrow(twotm.pMatrix.ending_uni[input2, nomatch=0])
        
        # Calculate the probability of the bigram
        bi_prob <- max((cw2x-discount),0) / cw2 
        
        wtypes <- nrow(twotm.pMatrix.starting_uni[input2, nomatch=0])
        bi_lamda <- discount / cw2 * wtypes
        
        cx <- nrow(twotm.pMatrix.ending_uni[x_words$ending_uni[i], nomatch=0])
        uni_prob <- cx / twotm.rowcount
        prob[i] <- tri_prob + tri_lamda*(bi_prob + bi_lamda*uni_prob)
      }
  } else
  {
        # Input was not found in trigram table so go to bigram
        x_words <- twotm.pMatrix.starting_uni[input2, nomatch=0]
        x_words$count <- as.numeric(x_words$count)
        x_words <- x_words[order(count, decreasing=T), ]
        numberOfX <- nrow(x_words)
        if (numberOfX>3) { 
          x_words<-x_words[1:3, ]
          numberOfX <-3
        }
        bi_prob <- 0
        prob <- 0
  
        if (numberOfX>0)
        {
          for (i in 1:numberOfX)
          {
            cw2x <- as.numeric(twotm.pMatrix[paste0(input2, " ", x_words$ending_uni[i]), nomatch=0]$count)
            cw2 <- nrow(twotm.pMatrix.ending_uni[input2, nomatch=0])
            
            if (length(cw2)>0) bi_prob <- max((cw2x-discount),0) / cw2 
            bi_lamda <-0 
            wtypes <- nrow(twotm.pMatrix.starting_uni[input2, nomatch=0])
            if (length(wtypes)>0 && (length(cw2)>0)) bi_lamda <- discount / cw2 * wtypes
            cx <- nrow(twotm.pMatrix.ending_uni[x_words$ending_uni[i], nomatch=0])
            uni_prob <- cx / twotm.rowcount
            prob[i] <- bi_prob + bi_lamda*uni_prob
            
          }
        } else
        {
          # Neither tri or bigram was found.
          # So take the top ten unigrams and send that information
          x_words <- onetm.pMatrix
          x_words$count <- as.numeric(onetm.pMatrix$count)
          x_words <- x_words[order(x_words$count, decreasing=TRUE), ][1:10,]
          x_words$ending_uni <- x_words$term
          prob <- as.numeric(x_words$term_prob)
        }
  }
  
  nextwords <- x_words$ending_uni
  results <- data.frame(cbind(predicted_word=nextwords, prob))
  results <- results[order(prob, decreasing = T),]
  rowcount <- nrow(results)
  
  if (is.null(rowcount)) rowcount <-1
  if (rowcount<maxResults) maxResults <- rowcount
  results <- results[1:maxResults, ]
  
  resultsVector <- as.vector(results[, 1])
  return(resultsVector)
}

