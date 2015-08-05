# I will load the three files and then create a sample.  Since the file size is
# relatively large and the processing power I have available is not so great, I
# decided to sample the data.  I will create a sample that has 30% of the data
# from each of the data sources. I will save the complement (70%) for future
# testing purpose. I use sample.split() from the caTools library to create a
# random sample from the data.
# 
# A file will be written to the directory specified by modelDir with the 5% sample and the
# testing data will be saved to directory specified by testDir
# 

createSamples <- function(sampleSize = 1/3, modelDir="model", testDir="test")
{
  
  #setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Coursera/Data Science Capstone/Report")
  source("library_setup.r", local = TRUE)
  setupEnvironment()
  
  twitter_file <- "../final/en_US/en_US.twitter.txt"
  news_file <- "../final/en_US/en_US.news.txt"
  blog_file <- "../final/en_US/en_US.blogs.txt"
  
  twitter <- readLines(twitter_file, encoding = 'UTF-8')
  news <- readLines(news_file, encoding = 'UTF-8')
  blogs <- readLines(blog_file, encoding = 'UTF-8')
  
  inSample <- sample.split(twitter, SplitRatio=sampleSize)
  twitter_sample <- twitter[inSample]
  twitter_test_sample <- twitter[!inSample]
  
  inSample <- sample.split(news, SplitRatio=sampleSize)
  news_sample <- news[inSample]
  news_test_sample <- news[!inSample]
  
  inSample <- sample.split(blogs, SplitRatio=sampleSize)
  blogs_sample <- blogs[inSample]
  blogs_test_sample <- blogs[!inSample]
  
  all_sample <- c(twitter_sample, news_sample, blogs_sample)
  #all_sample <- cleanupFiles(all_sample)
  
  all_test_sample <- c(twitter_test_sample, news_test_sample, blogs_test_sample)
  #all_test_sample <- cleanupFiles(all_test_sample)
  
  writeLines(all_sample, paste0(modelDir, "/modelSample.txt"))
  writeLines(all_test_sample, paste0(testDir, "/testSample.txt"))

}

cleanupFiles <- function(allSampleData) {
  
  #allSampleData <- iconv(allSampleData, from = "UTF-8", to = "latin1") 
  #allSampleData <- iconv(allSampleData, "latin1", "ASCII")
  
  allSampleData <- gsub("(.*?)($|'|[^[:punct:]]+?)(.*?)", "\\2", allSampleData)
  
  allSampleData <- gsub("^( )+", "", allSampleData)
  allSampleData <- gsub("( )+", " ", allSampleData)
  
  allSampleData <- gsub("( ){2,}", " ", allSampleData)
  
  allSampleData <- gsub(" (['-])+", "", allSampleData)
  allSampleData <- gsub("['-]+ ", "", allSampleData)
  allSampleData <- gsub("^(['-])+", "", allSampleData)
  allSampleData <- gsub("['-]+$", "", allSampleData)
  
  allSampleData <- allSampleData[ allSampleData != "( )+" ]
  allSampleData <- allSampleData[ allSampleData != "" ]
  
}

