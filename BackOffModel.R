library(data.table)
library(dplyr)
library(stringr)
library(tm)
library(lexicon)
library(words)
library(hash)

wd          <- '/Users/danielmackie/Capstone'
outpath     <- paste(wd, '/dat', sep='')
csvpath  <- paste(outpath, '/csv', sep='')

gramFile <- "uniGram_agg_truncated.csv"
sourceFilePath <- paste(csvpath, '/uniGram/', gramFile, sep='')
dt <- fread(sourceFilePath)
uniGram <- as.data.frame(dt)

gramFile <- "biGram_agg_truncated.csv"
sourceFilePath <- paste(csvpath, '/biGram/', gramFile, sep='')
dt <- fread(sourceFilePath)
biGram <- as.data.frame(dt)

gramFile <- "triGram_agg_truncated.csv"
sourceFilePath <- paste(csvpath, '/triGram/', gramFile, sep='')
dt <- fread(sourceFilePath)
triGram <- as.data.frame(dt)

gramFile <- "quadGram_agg_truncated.csv"
sourceFilePath <- paste(csvpath, '/quadGram/', gramFile, sep='')
dt <- fread(sourceFilePath)
quadGram <- as.data.frame(dt)

tDf <- function(dat) {
  dat <- dat[order(dat$freq, decreasing=TRUE), ]
  dat$gram <- str_split_fixed(dat$gram, '\\|', 2)
  dat$c <- dat$freq
  dat$gramA <- dat$gram[,1]
  dat$gramB <- dat$gram[,2]
  dat$gram <- NULL
  return(dat)
}

prepDf <- function(dat) {
  bool <- "gramB" %in% colnames(dat)
  if(bool) {
    agg <- dat %>%
      group_by(gramA) %>%
      summarise(C=sum(c))
    dat <- dat %>% left_join(agg, by=c('gramA'='gramA'))
  } else {
    agg <- dat %>%
      summarise(C=sum(c))
    dat$C < agg$C
  }
  
  dat$P <- (dat$c / dat$C)
  return(dat)
}

uniGram <- tDf(uniGram)
biGram <- tDf(biGram)
triGram <- tDf(triGram)
quadGram <- tDf(quadGram)

uniCut <- 1000
biCut <- 5
triCut <- 5
quadCut <- 2

uniGram <- uniGram[uniGram$c > uniCut, ]
biGram <- biGram[biGram$c > biCut, ]
triGram <- triGram[triGram$c > triCut, ]
quadGram <- quadGram[quadGram$c > quadCut, ]

uniGram <- prepDf(uniGram)
biGram <- prepDf(biGram)
triGram <- prepDf(triGram)
quadGram <- prepDf(quadGram)

aggQuad <- quadGram %>% group_by(gramA) %>% reframe(dv = list(unlist(as.list(setNames(P, gramB)))))
aggTri <- triGram %>% group_by(gramA) %>% reframe(dv = list(unlist(as.list(setNames(P, gramB)))))
aggBi <- biGram %>% group_by(gramA) %>% reframe(dv = list(unlist(as.list(setNames(P, gramB)))))
aggUni <- uniGram %>% reframe(dv = list(unlist(as.list(setNames(P, gramA)))))
lUni <- aggUni$dv[[1]]

hkeys <- c(aggQuad$gramA, aggTri$gramA, aggBi$gramA)
hvals <- c(aggQuad$dv, aggTri$dv, aggBi$dv)
model <- hash(keys=hkeys, values=hvals)

pb <- txtProgressBar(min = 1, max = length(unique(aggBi$gramA)), initial = 1, char = "=", width = NA, "", "", style = 3)

salt <- '#UNIGRAM#'
counter <- 0
for(g in unique(aggBi$gramA)) {
  setTxtProgressBar(pb, counter)
  dv <- aggBi[aggBi$gramA == g, 'dv']$dv[[1]]
  keys <- setdiff(names(lUni), names(dv))
  l <- lUni[keys]
  l <- l[order(unlist(l), decreasing=TRUE)][1:10]
  l <- unlist(lapply(l, function(x) x/sum(l)))
  k <- paste(salt, g, sep='')
  model[k] <- l
  counter <- counter + 1
}

save(model, file=paste(wd, '/', 'model', '/v1/', 'm.RData', sep=''))


