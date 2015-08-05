# select CRAN mirror globally

setupEnvironment <-function()
{
  r <- getOption("repos")
  r["CRAN"] <- "http://cran.us.r-project.org"
  options(repos = r)
  rm(r)
  
  # Install and load the libraries we will need - only if needed
  
  list.of.packages <- c("ggplot2", "R.utils", "tm", "SnowballC", "RWeka", "caret", "caTools", "dplyr", "stringi", "gridExtra", "knitr", "data.table")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  suppressPackageStartupMessages(library("tm"))
  suppressPackageStartupMessages(library("RWeka"))
  suppressPackageStartupMessages(library("knitr"))
  suppressPackageStartupMessages(library("data.table"))
  suppressPackageStartupMessages(library("ggplot2"))
  suppressPackageStartupMessages(library("gridExtra"))
  suppressPackageStartupMessages(library("R.utils"))
  suppressPackageStartupMessages(library("SnowballC"))
  suppressPackageStartupMessages(library("caret"))
  suppressPackageStartupMessages(library("caTools"))
  suppressPackageStartupMessages(library("dplyr"))
  suppressPackageStartupMessages(library("stringi"))

}
