cleanText <- function(input, stopwordsflag=FALSE, modelID="BF3") {
 
# Clean the input text
#
# Inputs:  input - character string with sentence to be searched
#          stopwordsflag - indicates whether stop words should be removed from input string
#          modelID - which model is being run.  This is needed to set up the number
#                    of input words and to point 
  
# initialize local variables
inputItems <- c("", "", "", "", "", "")
  
if (nchar(input)>0)
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
  
  if (nwords>4) nwords<-4
  
  if (nwords==4){
    input4 <- input[1]
    input3 <- input[2]
    input2 <- input[3]
    input1 <- input[4]
    input <- paste(input1, input2, input3, input4, sep = ' ') 
  }
  if (nwords==3){
    input4 <- ""
    input3 <- input[1]
    input2 <- input[2]
    input1 <- input[3]
    input <- paste(input1, input2, input3, sep = ' ') 
  }
  if (nwords==2){
    input4 <- ""
    input3 <- ""
    input2 <- input[1]
    input1 <- input[2]
    input <- paste(input1, input2, sep = ' ') 
  }
  if (nwords==1){
    input4 <- ""
    input3 <- ""
    input2 <- ""
    input1 <- input[1]
    input <- input1
  }

  if (is.na(input1)) input1 <- ""
  if (is.na(input2)) input2 <- ""
  if (is.na(input3)) input3 <- ""
  if (is.na(input4)) input4 <- ""
  
  inputItems[1] <- input
  inputItems[2] <- input1
  inputItems[3] <- input2
  inputItems[4] <- input3
  inputItems[5] <- input4
  inputItems[6] <- nwords
} else
{
  inputItems[1] <- ""
  inputItems[2] <- ""
  inputItems[3] <- ""
  inputItems[4] <- ""
  inputItems[5] <- ""
  inputItems[6] <- ""
}
return(inputItems)
}

