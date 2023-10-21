rm(list=ls())
gc()

library(tm)
library(openNLP)
library(readr)
library(lexicon)
library(textstem)
library(Dict)
library(ips)
library(dplyr)
library(reshape2)
library(hash)
library(purrr)
library(purrrlyr)
library(data.table)
library(stringr)
library(words)
library(RWeka)

set.seed(123)

wd          <- '/Users/danielmackie/Capstone'
outpath     <- paste(wd, '/dat', sep='')
csvoutpath  <- paste(outpath, '/csv', sep='')
training <- TRUE
if(training) {
  sub <- 'train' 
} else {
  sub <- 'test'
}

splitCombine <- function(x, n) {
  list <- unlist(strsplit(x, " +"))
  i <- length(list)-n
  gram1 <- paste(list[i:n], collapse='_')
  gram2 <- list[n+1]
  newString <- paste(gram1, '|', gram2, sep='')
  list <- NULL
  gram1 <- NULL
  gram2 <- NULL
  return(newString)
}

extractGramAsVector <- function(x, n, tokenizer) {
  if(n==0) { return(tokenizer(x)) }
  return(lapply(tokenizer(x), splitCombine, n))
}

processData <- FALSE
aggData <- FALSE
truncateData <- TRUE
pTyp <- 0
debug <- FALSE
batch <- 100000
cutFile <- FALSE
masterCorpus <- "c_all.RData"
batchFilePrefix <- "corpus_part_"
batchFileSuffix <- ".RData"
tmp <- "/tmp"

processCorpus <- function(c, sub, b) {
  length <- length(c)
  steps <- ceiling(as.numeric(length / b))
  
  for(s in 1:steps) {
    start <- (b*s) - b + 1
    end <- (b*s)
    remainder <- length - end
    sc <- c[start:min(length,end)]
    save(sc, file=paste(wd, tmp, '/', sub, '/', batchFilePrefix, s, '.RData', sep=''))
    rm(sc)
  }
}

if(cutFile) {
  load(paste(wd, '/', masterCorpus, sep=''))
  
  smpl <- sample(seq_along(c_all), length(c_all) * 0.7)
  nsmpl <- !(seq_along(c_all) %in% smpl)
  train <- c_all[smpl]
  test <- c_all[nsmpl]
  
  rm(c_all)
  gc()
  
  processCorpus(train, "train", batch)
  processCorpus(test, "test", batch)
}

if(pTyp == 0) {
  csvoutpath <- paste(csvoutpath, "/uniGram", sep="")
  n <- 0
  outCsvFile <- "uniGram"
  outDataFile <- "uniGram.RData"
}

if(pTyp == 1) {
  csvoutpath <- paste(csvoutpath, "/biGram", sep="")
  n <- 1
  outCsvFile <- "biGram"
  outDataFile <- "biGram.RData"
}

if(pTyp == 2) {
  csvoutpath <- paste(csvoutpath, "/triGram", sep="")
  n <- 2
  outCsvFile <- "triGram"
  outDataFile <- "triGram.RData"
}

if(pTyp == 3) {
  csvoutpath <- paste(csvoutpath, "/quadGram", sep="")
  n <- 3
  outCsvFile <- "quadGram"
  outDataFile <- "quadGram.RData"
}

if(processData) {
  NgramTokenizer <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = n+1, max=n+1)) }
  
  firstStep <- 1
  steps <- 30
  
  for(s in firstStep:steps) {
    result <- hash()
    file <- paste(wd, tmp, '/', sub, '/', batchFilePrefix, s, batchFileSuffix, sep='')
    load(file)
    
    start <- 1
    end <- length(sc)
    print(paste("processing file:", file, "with:", end, "rows."))
    pb <- txtProgressBar(min = start, max = end, initial = 1, char = "=", width = NA, "", "", style = 3)
    for(i in start:end) {
      setTxtProgressBar(pb, i)
      l <- extractGramAsVector(sc[[i]]$content, n=n, tokenizer=NgramTokenizer)
      for(g in l) {
        if(g == "") next
        result[[g]] <- if(has.key(g, result)) as.numeric(result[[g]])+1 else 1
      }
    }
    
    gram <- keys(result)
    freq <- values(result)
    df <- cbind(gram, freq)
    df <- as.data.frame(df)
    df <- data.frame(lapply(df, as.character))
    for(col in colnames(df)) {
      Encoding(df[[col]]) <- "UTF-8"
    }
    
    write.csv(df, paste(csvoutpath, '/', outCsvFile, '_part_', s, '.csv', sep=""), row.names = FALSE)
    close(pb)
    
    sc <- NULL
    df <- NULL
    gram <- NULL
    freq <- NULL
    pb <- NULL
    clear(result)
    result <- NULL
    gc()
  }
}


if(aggData) {
  df <- list.files(path=csvoutpath, pattern=".*part.*.csv", full.names = TRUE) %>%
    map_dfr(read_csv, col_types = cols(gram = "c", freq = "c"))
  
  df$freq <- as.numeric(df$freq)
  
  agg <- df %>% 
    group_by(gram) %>%
    summarise(freq=sum(freq))
  
  write.csv(agg, paste(csvoutpath, '/', outCsvFile, '_agg.csv', sep=''), row.names = FALSE)
  
  gc()
}


if(truncateData) {
  sourceFilePath <- paste(csvoutpath, '/', outCsvFile, '_agg.csv', sep='')
  dt <- fread(sourceFilePath)
  agg <- as.data.frame(dt)
  
  data(profanity_alvarez)
  profanity <- gsub('[^A-Za-z0-9]', '', profanity_alvarez)
  
  regx <- "\\b^[a-z|_]+\\b"
  if(n>=1) {
    agg$gramSplit <- str_split_fixed(agg$gram, "\\|", 2)
    agg$gramA <- agg$gramSplit[,1]
    agg$gramB <- agg$gramSplit[,2]
    
    agg <- subset(agg, grepl(regx, agg$gramA))
    agg <- subset(agg, grepl(regx, agg$gramB))
    
    agg$gramASplit <- str_split_fixed(agg$gramA, "\\_", n)
    agg <- subset(agg, !agg$gramB %in% unique(tolower(profanity)))
    
    agg$gramSplit <- NULL
    agg$gramASplit <- NULL
    agg$gramA <- NULL
    agg$gramB <- NULL
    
  } else {
    agg <- subset(agg, grepl(regx, agg$gram))
    agg <- subset(agg, !(agg$gram %in% unique(tolower(profanity))))
  }
  
  write.csv(agg, paste(csvoutpath, '/', outCsvFile, '_agg_truncated.csv', sep=''), row.names = FALSE)
}

