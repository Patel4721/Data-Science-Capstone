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

probabilityMatrix <-function(docMatrix)
{
  # Sum up the term frequencies
  termSums<-cbind(colnames(as.matrix(docMatrix)),as.numeric(colSums(as.matrix(docMatrix))))
  
  # Calculate the probabilties
  termSums<-cbind(termSums,(as.numeric(termSums[ , 2])/sum(as.numeric(termSums[ , 2]))))
  
  # Split the gram into individual words so we can extract the starting word and ending word
  # From the n-gram
  tmp <- stri_split_fixed(as.character(termSums[, 1]), " ")
  tmp <- do.call(rbind, tmp)
  numberOfElements <- ncol(tmp)
  
  # We need to know the number of columns in the n-gram split so we can extract only the 
  # first word (starting uni) and the ending word (ending uni).
  
  # This is so we can use Keynes-Neyes smoothing later and use Pcombination to 
  # account for words like "Franciso" being more frequntly followed by "San"
  if (numberOfElements==1) termSums <- cbind(termSums, tmp[, 1], "")
  if (numberOfElements==2) termSums <- cbind(termSums, tmp[, 1], tmp[, 2])
  if (numberOfElements==3) termSums <- cbind(termSums, tmp[, 1], tmp[, 3])
  
  # Clean up by deleting  temporary variables
  rm(tmp)
  rm(numberOfElements)

  # Add pretty names to the columns
  colnames(termSums)<-c("term","count", "term_prob", "starting_uni", "ending_uni")
  
  termSums <- data.table(termSums, key=c("term", "starting_uni", "ending_uni"))
  #Vector <- 0
  #eVector <- 0
  #rowsTermSums <- nrow(termSums)
  #for (i in 1:rowsTermSums)
  #{
  #  sVector[i] <- getStartingCount(termSums$starting_uni, termSums$starting_uni[i])
  #  eVector[i] <- getEndingCount(termSums$ending_uni, termSums$ending_uni[i])
  #}
  
  #termSums$starting_count <- sVector # number of times starting uni is in data
  #termSums$ending_count <- eVector   # number of times ending uni is in data
  #termSums$s_cont_prob <- sVector/rowsTermSums # Continuation probability
  #termSums$e_cont_prob <- eVector/rowsTermSums # Continuation probability
  
  #rm(sVector)
  #rm(eVector)
  
  termSums
}

