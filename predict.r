predictWord <- function(modelDir, input, maxResults=3, logging=FALSE, discount=0.75, stopwordsflag=TRUE)
{
  if (logging) sink("log.txt", append=FALSE, split=FALSE)
  
  #setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Coursera/Data Science Capstone/Report")
  source("library_setup.r", local = TRUE)
  setupEnvironment()
    
  # Load the prediction data
  load(paste0(modelDir, "-", "modelData.RData"))
  
  # Clean up the input data
  input <- tolower(input)
  if (stopwordsflag)  stop_words <- readLines("stop_words.txt")
  input <- rev(unlist(strsplit(input," ")))
  if (stopwordsflag) input <- setdiff(input, stop_words)
  input <- removePunctuation(input)
  input <- removeNumbers(input)
  nwords <- length(input)
  input <- setdiff(input, badwords)
  input <- input[grepl('[[:alpha:]]',input)]
  input2 <- input[1]
  input1 <- input[2]
  input <- paste(input1,input2,sep = ' ')
  
  # Check if the number of characters and words in input is sufficient
  # We need at least 1 words for the prediction to execute
  if (nchar(input) == 0 || nwords < 1 || is.na(input2)) {
    results <- data.frame("Invalid length of input", 0)
    colnames(results) <- c("predicted_word", "prob")
    return(results)
  }
  
  # Find out how many words there are in the input - at this point it will be one or two words
  nInputsWords <- sapply(stri_split_fixed(input, " "), length)
  
  # Get all possible ending unigrams from trigram data
  # This will give a listing of all possible predicted words 
  x_words <- threetm.pMatrix[grep(paste0("^", input, " "), threetm.pMatrix$term), ]
  numberOfX <- nrow(x_words)
  if (logging) print(paste("Looking for:", input))
  if (logging) print(paste("Possible words:", x_words$ending_uni))
  
  # Now we want to loop through all possible predicted words
  if (numberOfX>0)
  {  
    # The input was found in the trigram table
    input_bi_count <- as.numeric(twotm.pMatrix[grep(paste0("^", input, "$"), twotm.pMatrix$term), ]$count)
    tri_lamda <- discount / input_bi_count * numberOfX
    
    if (logging) print(paste("Calculating trigram probability..."))
    if (logging) print(paste("tri_lamda=", tri_lamda))
    
    tri_prob <- 0
    prob <- 0
    
    for (i in 1:numberOfX)
      {
        cw2w1 <- as.numeric(threetm.pMatrix[grep(paste0("^", input, " ", x_words$ending_uni[i], "$"), threetm.pMatrix$term), ]$count)
        tri_prob <- max((cw2w1-discount),0) / input_bi_count
        
        if (logging) print(paste("i=", i, "predicted word=", x_words$ending_uni[i], "cw2w1=", cw2w1, "tri_prob=", tri_prob))
        
        #cw2x <- as.numeric(threetm.pMatrix[grep(paste0(" ", input2, " ", x_words$ending_uni[i], "$"), threetm.pMatrix$term), ]$ending_count)
        #cw2x <- as.numeric(twotm.pMatrix[grep(paste0("^", input2, " ", x_words$ending_uni[i], "$"), twotm.pMatrix$term), ]$ending_count)
        cw2xcount <- grepl(paste0("^", input2, " ", x_words$ending_uni[i], "$"), twotm.pMatrix$term)
        cw2x <- sum(cw2xcount)
        cw2count <- grepl(paste0("^", input2, "$"), twotm.pMatrix$ending_uni)  # Get bigrams ending with input2
        cw2 <-  sum(cw2count)
        
        bi_prob <- max((cw2x-discount),0) / cw2 
        
        if (logging) print(paste("i=", i,"cw2x=", cw2x, "cw2=", cw2, "bi_prob=", bi_prob))
        
        wtypescount <- grepl(paste0("^", input2, " "), twotm.pMatrix$term) # Get bigrams starting with input2
        wtypes <-  sum(wtypescount)
        bi_lamda <- discount / cw2 * wtypes
        
        if (logging) print(paste("i=", i,"wtypes=", wtypes, "bi_lamda=", bi_lamda))
        
        cxcount <- grepl(paste0(" ", x_words$ending_uni[i], "$"), twotm.pMatrix$term)
        cx <- sum(cxcount)
        
        uni_prob <- cx / nrow(twotm.pMatrix)
        
        if (logging) print(paste("i=", i,"cx=", cx, "uni_prob=", uni_prob))
        
        prob[i] <- tri_prob + tri_lamda*(bi_prob + bi_lamda*uni_prob)
        
        if (logging) print(paste("i=", i,"Predicted word=", x_words$ending_uni[i], "prob=", prob[i]))

      }
  } else
  {
        # Input was not found in trigram table so go to bigram
        x_words <- twotm.pMatrix[grep(paste0("^", input2, " "), twotm.pMatrix$term), ]
        numberOfX <- nrow(x_words)
        bi_prob <- 0
        prob <- 0
        
        if (logging) print(paste("Possible words at bigram level:", x_words$ending_uni))

        if (numberOfX>0)
        {
          if (logging) print(paste("Calculating bigram probability..."))
          
          for (i in 1:numberOfX)
          {
            cw2x <- as.numeric(twotm.pMatrix[grep(paste0("^", input2, " ", x_words$ending_uni[i], "$"), twotm.pMatrix$term), ]$count)
              #grepl(paste0("^", input2, " ", x_words$ending_uni[i], "$"), twotm.pMatrix$term)
            #cw2x <- sum(cw2xcount)
            cw2 <- as.numeric(onetm.pMatrix[grep(paste0("^", input2, "$"), onetm.pMatrix$term), ]$count)
            
            bi_prob <- max((cw2x-discount),0) / cw2 
            if (logging) print(paste("i=", i,"cw2x=", cw2x, "cw2=", cw2, "bi_prob=", bi_prob))
            
            wtypes <- as.numeric(onetm.pMatrix[grep(paste0("^", input2, "$"), onetm.pMatrix$term), ]$count)
            bi_lamda <- discount / cw2 * wtypes
            
            cxcount <- grepl(paste0(" ", x_words$ending_uni[i], "$"), twotm.pMatrix$term)
            cx <- sum(cxcount)
            if (logging) print(paste("i=", i,"wtypes=", wtypes, "bi_lamda=", bi_lamda))
            
            uni_prob <- cx / nrow(twotm.pMatrix)
            
            if (logging) print(paste("i=", i,"cx=", cx, "uni_prob=", uni_prob))
            
            prob[i] <- bi_prob + bi_lamda*uni_prob
            
            if (logging) print(paste("i=", i,"Predicted word=", x_words$ending_uni[i], "prob=", prob[i]))
          }
        } else
        {
          if (logging) print(paste("Calculating unigram probability..."))
          
          x_words <- onetm.pMatrix
          x_words$ending_uni <- x_words$term
          prob <- as.numeric(onetm.pMatrix$term_prob)
    
        }
    
  }
  
  nextwords <- x_words$ending_uni
  # nextwords <- setdiff(nextwords, badwords)
  results <- cbind(predicted_word=nextwords, prob)
  results <- results[order(prob, decreasing = T),]
  rowcount <- nrow(results)
  
  if (is.null(rowcount)) rowcount <-1
  if (rowcount<maxResults) maxResults <- rowcount
  if (rowcount>3) results <- results[1:maxResults, ]
  if (logging) sink()

  return(results)
}

