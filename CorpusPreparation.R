library(tm)
library(openNLP)
library(readr)
library(textstem)
library(dplyr)

fetchCorpus <- function(sourcepath, targetpath, filename, dbname, dbtype, nsize, test, inmem) {
  print(paste('started fetchCorpus for file: ', filename, sep=''))
  if(! test) {
    setwd(targetpath)
    if(file.exists(dbname)) {
      file.remove(dbname)
    }
    txt <- character()
    setwd(sourcepath)
    txt <- c(txt, read_lines(filename, skip=0, n_max = nsize))
    setwd(targetpath)
    if(! inmem) {
      print(paste("databse found: ", dbname, sep=''))
      c <- PCorpus(VectorSource(txt), 
                   dbControl = list(dbName = dbname, dbType = dbtype))
    } else {
      c <- VCorpus(VectorSource(txt))
    }
  } else {
    data("crude")
    c <- crude
  }
  print(paste("Number of documents in the corpus is: ", length(c), sep=""))
  print(paste('ended fetchCorpus for file: ', filename, sep=''))
  return(c)
}

wd          <- '/Users/danielmackie/Capstone'
inpath      <- paste(wd, '/dat/final/en_US', sep='')
outpath     <- paste(wd, '/dat', sep='')
dboutpath   <- paste(outpath, '/db', sep='')
csvoutpath  <- paste(outpath, '/csv', sep='')

blogs  <- 'en_US.blogs.txt'
tweets <- 'en_US.twitter.txt'
news   <- 'en_US.news.txt'

twitterDb <- 'twitterdb'
newsDb    <- 'newsdb'
blogsDb   <- 'blogsdb'
dbType    <- 'DB1'

testing <- FALSE
sampleSize <- 5000000

transformCorpus <- function(c) {
  c <- tm_map(c, stripWhitespace)
  c <- tm_map(c, removePunctuation)
  c <- tm_map(c, content_transformer(tolower))
  c <- tm_map(c, content_transformer(lemmatize_words))
  return(c)
}

c_tweets <- fetchCorpus(inpath, dboutpath, tweets, twitterDb, dbType, nsize=sampleSize, test=testing, inmem=TRUE)
c_tweets <- transformCorpus(c_tweets)
save(c_tweets, file=paste(wd, '/c_tweets.RData', sep=''))
rm(c_tweets)
gc()

c_news <- fetchCorpus(inpath, dboutpath, news, newsDb, dbType, nsize=sampleSize, test=testing, inmem=TRUE)
c_news <- transformCorpus(c_news)
save(c_news, file=paste(wd, '/c_news.RData', sep=''))
rm(c_news)
gc()

c_blogs <- fetchCorpus(inpath, dboutpath, blogs, blogsDb, dbType, nsize=sampleSize, test=testing, inmem=TRUE)
c_blogs <- transformCorpus(c_blogs)
save(c_blogs, file=paste(wd, '/c_blogs.RData', sep=''))
rm(c_blogs)


load(paste(wd, '/', 'c_tweets.RData', sep=''))
load(paste(wd, '/', 'c_news.RData', sep=''))
load(paste(wd, '/', 'c_blogs.RData', sep=''))

c_all <- c(c_tweets, c_news, c_blogs)
save(c_all, file=paste(wd, '/c_all.RData',sep=''))
