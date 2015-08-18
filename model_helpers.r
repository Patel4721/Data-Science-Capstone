getStartingCount <-function(mytable, searchTerm)
{
  findterms <-grepl(paste0("^",searchTerm, " "), mytable)
  sum(findterms)
}

getEndingCount <-function(mytable, searchTerm)
{
  findterms <-grepl(paste0("^", searchTerm, "$"), mytable)
  sum(findterms)
}

probabilityMatrix <-function(docMatrix, modelType)
{
  # Sum up the term frequencies
  termSums<-cbind(colnames(as.matrix(docMatrix)), as.numeric(colSums(as.matrix(docMatrix))))
  
  # Calculate the probabilties
  # termSums<-cbind(termSums,(as.numeric(termSums[ , 2])/sum(as.numeric(termSums[ , 2]))))
  
  # Split the gram into individual words so we can extract the starting word and ending word
  # From the n-gram
  tmp <- stri_split_fixed(as.character(termSums[, 1]), " ")
  tmp <- do.call(rbind, tmp)
  numberOfElements <- ncol(tmp)
  
  # We need to know the number of columns in the n-gram split so we can extract only the 
  # first word (starting uni) and the ending word (ending uni).
  # Will also establish the starting bigram for trigram tables. 
  if (modelType=="KN")
  {
    # This is so we can use Keynes-Neyes smoothing later and use Pcombination to 
    # account for words like "Franciso" being more frequntly followed by "San"
    if (numberOfElements==1) termSums <- cbind(termSums, tmp[, 1], "", "", "")
    if (numberOfElements==2) termSums <- cbind(termSums, tmp[, 1], tmp[, 2], "", "")
    if (numberOfElements==3) termSums <- cbind(termSums, tmp[, 1], tmp[, 3], 
                                               paste(tmp[, 1], tmp[, 2]), 
                                               paste(tmp[, 2], tmp[, 3]))
    # Add pretty names to the columns
    colnames(termSums)<-c("term","count", "starting_uni", 
                          "ending_uni", "starting_bi", "ending_bi")
    
  }
  if (modelType=="BF")
  {
    # This is so we can use back off 
    # tmp[, 1] - is first term
    # tmp[, 2] - is second term
    # tmp[, 3] - is third term
    if (numberOfElements==1) termSums <- cbind(termSums, tmp[, 1], tmp[, 1])
    if (numberOfElements==2) termSums <- cbind(termSums, tmp[, 1], tmp[, 2])
    if (numberOfElements==3) termSums <- cbind(termSums, paste(tmp[, 1], tmp[, 2]), tmp[, 3])
    if (numberOfElements==4) termSums <- cbind(termSums, paste(tmp[, 1], tmp[, 2], tmp[, 3]), tmp[, 4])
    if (numberOfElements==5) termSums <- cbind(termSums, paste(tmp[, 1], tmp[, 2], tmp[, 3], tmp[, 4]), tmp[, 5])
    # Add pretty names to the columns

    colnames(termSums)<-c("term","count", "starting", "prediction")

  } 
  # Clean up by deleting  temporary variables
  rm(tmp)
  rm(numberOfElements)
  
  termSums <- data.table(termSums)
  termSums
}

