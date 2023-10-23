library(hash)
library(openNLP)
library(textstem)
library(tm)

wd          <- '/Users/danielmackie/Capstone'
sub         <- 'val'
tmp         <- 'tmp'

file <- paste(wd, '/', tmp, '/', sub, '/', 'val.RData', sep='')
load(file)

file <- paste(wd, '/', 'model', '/', 'v1', '/', 'm.RData', sep='')
load(file)

c <- 0
r <- 0
j <- 1

docs <- list()
end <- length(val)
pb <- txtProgressBar(min = 1, max = end, initial = 1, char = "=", width = NA, "", "", style = 3)

for(i in 1:end) {
  setTxtProgressBar(pb, i)
  x <- val[[i]]$content
  l <- unlist(strsplit(x, ' +'))
  if(length(l) > 0) {
    docs[[j]] <- l
    j <- j + 1
  }
}

gN <- function(x, n) {
  x <- unlist(x)
  return(x[1:n])
}

gTN <- function(x, n) {
  x <- unlist(x)
  return(sort(x, decreasing = TRUE)[1:n])
}

aD <- double()
aD[4] <- 0.3
aD[3] <- 0.9
aD[2] <- 0.9
aD[1] <- 1

predict <- function(m, s, d, n, itr, hasParent, result = list()) {
  if(is.null(s)) { return() }
  # s <- tolower(s)
  # s <- stripWhitespace(s)
  # s <- removePunctuation(s)
  # s <- lemmatize_words(s)
  ls <- strsplit(s, " +")
  t <- ifelse(itr > 1, lapply(ls, tail, itr-1), sapply(ls, tail, itr))
  salt <- '#UNIGRAM#'
  g <- ifelse(itr == 1, paste(salt, unlist(t), sep=''), paste(unlist(t), collapse='_'))
  if(has.key(key=g, hash=m)) {
    nxt <- NULL
    if(hasParent) {
      a <- (1-d[itr+1]) / sum(d[itr]*m[[g]])
      nxt <- gN(d[itr] * m[[g]] * a, n)
    } else {
      nxt <- gN(d[itr] * m[[g]], n) 
      hasParent <- TRUE
    }
    result <- c(result, nxt)
  }
  if(itr == 1) {
    result <- gTN(result, n * 2) 
    wrd <- gN(unique(names(result)), n)
    return(wrd)
  }
  predict(m, s, d, n, itr-1, hasParent, result)
}

run <- function(spct = 0.001, tN = 3, d) {
  pcnt <- spct
  set.seed(1234)
  smpl <- sample(seq_along(docs), length(docs) * pcnt)
  nsmpl <- !(seq_along(docs) %in% smpl)
  valSmpl <- docs[smpl]
  
  end <- floor(length(valSmpl))
  print(paste('document count:', end))
  pb <- txtProgressBar(min = 1, max = end, initial = 1, char = "=", width = NA, "", "", style = 3)
  for(i in 1:end) {
    setTxtProgressBar(pb, i)
    l <- valSmpl[[i]]
    for(e in 1:length(l)) {
      g <- NA
      if(e == 4) {
        g <- paste(l[e-2], " ", l[e-1], " ", l[e], sep='')
      }
      if(e == 3) {
        g <- paste(l[e-1], " ", l[e], sep='')
      }
      if(e == 2) {
        g <- l[e]
      }
      aNw <- l[e+1]
      if(!is.na(aNw) & !is.na(g)) {
        p <- predict(model, g, d, tN, 4, FALSE, list())
        match <- ifelse(aNw %in% p, 1, 0)
        c <- c + 1
        r <- r + match
      }
    }
  }
  vals <- character()
  accuracy <- round(r/c, 3)
  for(i in 1:length(d)) {
    vals <- c(vals, paste(i, '-gram|', d[i], sep=''))
  }
  print(paste("Top-", tN, " accuracy: ", accuracy, " d: ", paste(vals, collapsee=", "), sep=""))
}

findOpt <- function(s, type=2, d, tN=3) {
  max <- 0.0
  clone <- c(d)
  for(i in 1:9) {
    t <- i/10
    clone[type] <- t
    a <- run(spct=s, d=clone, tN)
    cond <- a > max
    max <- ifelse(cond, a, max)
    d[type] <- ifelse(cond, t, d[type])
  }
  print(paste("Optimal ", type, "-gram discount: ", d[type], sep=""))
  return(d)
}

tD <- double()
tD[4] <- 0.90
tD[3] <- 0.90
tD[2] <- 0.98
tD[1] <- 1

#testing
sample <-0.01
run(sample, d=tD, 3)

#validation
sample <-0.01
tD <- findOpt(sample, type=4, d=tD, 3)
tD <- findOpt(sample, type=3, d=tD, 3)
tD <- findOpt(sample, type=2, d=tD, 3)
print(paste(tD, collapse="|"))