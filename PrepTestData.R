wd          <- '/Users/danielmackie/Capstone'
sub         <- 'test'
tmp         <- 'tmp'
batchFilePrefix <- "corpus_part_"
batchFileSuffix <- ".RData"

firstStep <- 1
steps <- 13

for(s in firstStep:steps) {
  file <- paste(wd, '/', tmp, '/', sub, '/', batchFilePrefix, s, batchFileSuffix, sep='')
  load(file)
  c <- c(c, unlist(sc, recursive = FALSE))
  rm(sc)
}

smpl <- sample(seq_along(c), length(c) * 0.3)
nsmpl <- !(seq_along(c) %in% smpl)
val <- c[smpl]
test <- c[nsmpl]

rm(c)

sub <- 'val'
f <- paste(wd, '/', tmp, '/', sub, '/val.RData', sep='')
save(val, file=f)


sub <- 'test'
f <- paste(wd, '/', tmp, '/', sub, '/test.RData', sep='')
save(val, file=f)