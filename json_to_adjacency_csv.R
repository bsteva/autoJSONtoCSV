library(tidyverse)
library(data.table)
library(igraph)
library(rgexf)
library(dplyr)
library(jsonlite)
library(stringr)
#-------------------------------------------------- begin function def -----------------------------------

# addToQueue: function that adds user input (in the form of file directory) to a queue

addToQueue <- function() {
  curRead <- readline(prompt = "Enter file: (Press enter when done) ")
  # if input isn't blank or NULL
  if(curRead != "" & !is.null(curRead)) {
    readQueue <- c(curRead)
  }
  
  while(curRead != "") {
    curRead <- readline(prompt = "Enter file: (Press enter when done) ")
    if(curRead != "" & !is.null(curRead)) {
      # appends more to the queue vector
      readQueue <- append(readQueue, curRead)
    }
  }
  return(readQueue)
}


# JSONtoCSV: function that cleans and converts JSON to CSV

JSONtoCSV <- function(fileName) {
  rawJSONs <- stream_in(file(fileName))
  df <- data.frame(rawJSONs$user$screen_name, rawJSONs$retweeted_status$user$screen_name)
  data <- df
  #------------------------------------------------------ make adjaceny list ------------------------------------------

  #remove all rows with space in the retweets
  data <- data[data$rawJSONs.retweeted_status.user.screen_name != "", ]
  
  #get the frequencies
  edgelist2 <- dplyr::summarise(dplyr::group_by(data, rawJSONs.user.screen_name, rawJSONs.retweeted_status.user.screen_name), count =n())
  
  #Sort descending and remove duplicates pair just in case
  decreasing_df <- edgelist2[order(edgelist2$count, decreasing = TRUE), ]
  decreasing_df <- decreasing_df[!duplicated(decreasing_df[c(1,2)]), ]
  
  # cut off range by numbers of rows (uncomment when asked)
  # decreasing_df <- decreasing_df[1:50000, ]
  
  Type <- rep("Directed", nrow(decreasing_df))
  
  #assign tags, in this case edges are directred, meaning username is connected to the retweeted user name
  decreasing_df$Type <- Type
  
  #change column names
  colnames(decreasing_df) <- c("Source", "Target", "Weight","Type")
  
  decreasing_df <- decreasing_df[decreasing_df$Weight >= 6, ] # new cutoff range, filter everything below 6 weight
  
  # organize the columns so gephi recogize them
  decreasing_df <- decreasing_df[,c("Source", "Target", "Type", "Weight")]
  
  # gets rid of NA rows
  decreasing_df <- decreasing_df[complete.cases(decreasing_df),]
  
  #give the directory where you want to save the file
  setwd('SET_DIRECTORY_HERE')
  
  # substring so the file name can be created automatically
  csvName <- str_sub(fileName, -18, -6)
  csvName <- paste(csvName, ".csv", sep = "")
  
  readr::write_csv(decreasing_df, csvName)
}

#--------------------------------------------------- function def end --------------------------------------------

# MAKE SURE INPUT IS IN `YYYY-MM-DD-HH` FORMAT SO NAME IS EXPORTED PROPERLY
# queue vector
ATQreturn <- addToQueue()

setwd('SET_DIRECTORY_HERE')

# loops through queue vector and runs JSON-CSV function
for(i in 1:length(ATQreturn)) {
  fileName <- ATQreturn[i] 
  JSONtoCSV(fileName)
}

#EOF