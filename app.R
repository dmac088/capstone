library(shiny)
library(hash)
library(openNLP)
library(textstem)
library(tm)
library(igraph)


githubURL <- "https://github.com/dmac088/capstone/raw/main/m.RData"
load(url(githubURL))

gN <- function(x, n) {
  x <- unlist(x)
  return(x[1:n])
}

gTN <- function(x, n) {
  x <- unlist(x)
  return(sort(x, decreasing = TRUE)[1:n])
}

aD <- double()
aD[4] <- 0.9
aD[3] <- 0.9
aD[2] <- 0.98
aD[1] <- 1

predict <- function(m, s, d, n, itr, hasParent=FALSE, result = list()) {
  if(is.null(s)) { return() }
  s <- tolower(s)
  s <- stripWhitespace(s)
  s <- removePunctuation(s)
  s <- lemmatize_words(s)
  ls <- strsplit(s, " +")
  t <- ifelse(itr > 1, lapply(ls, tail, itr-1), sapply(ls, tail, itr))
  salt <- '#UNIGRAM#'
  g <- ifelse(itr == 1, paste(salt, unlist(t), sep=''), paste(unlist(t), collapse='_'))
  if(itr == 1) {
    result <- gTN(result, n * 2) 
    wrd <- unlist(lapply(gN(unique(names(result)), n), function(x) { ifelse(is.na(x), 'UNKNOWN', x) }))
    return(wrd)
  }
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
  predict(m, s, d, n, itr-1, hasParent, result)
}

constructEdges <- function(from, to, depth=2, n) {
  if(depth==1) {
    df <- data.frame(
     from = from,
     to = to
    )
    df$weight <- 1
    df <- df %>% 
      unique
    return(df)
  }
  p <- lapply(to, function(x) predict(m=model, s=x, d=aD, n=n, itr=4, FALSE))
  p <- unlist(p)
  vGrm <- c(from, to)
  vWrd <- c(to, p)
  constructEdges(vGrm, vWrd, depth-1, n)
}

shinyApp (
  ui = basicPage(
    h3("NLP - Word Prediction App"),
    sidebarPanel(
      textInput("inText", label="Enter text", value = NULL),
      helpText(
        "You should see the next word predictions on the right side 1-2 seconds after pressing the Submit button."
      ),
      submitButton(text = "Submit", icon = NULL, width = NULL),
      helpText("\n"),
      helpText("You can try the following quiz test cases."),
      helpText("Quiz 2:"),
      helpText("-\"and a case of\""),
      helpText("-\"it would mean the\""),
      helpText("-\"and make me the\""),
      helpText("Quiz 3: "),
      helpText("-\"from the bottom to the\""),
      helpText("-\"hadn't time to take a\""),
      helpText("Note: the accuracy is not perfect since only 70% of the corpora was reserved for training, also infrequent grams were removed to optimize model size for RPubs"),
      
    ),
      mainPanel(
        verbatimTextOutput("value"),
        plotOutput("plot")
      )
    ),
  server = function(input, output) {
    
    n <- 3
    
    output$value <- renderPrint({
      req(input$inText)
      predict(m=model, s=input$inText, d=aD, n=3, itr=4, FALSE)
    })
    
    output$plot <- renderPlot({
      req(input$inText)
      gram <- paste(tail(unlist(strsplit(input$inText, " +")), n), collapse=' ')
      key3 <- paste(tail(unlist(strsplit(input$inText, " +")), n), collapse='_')
      key2 <- paste(tail(unlist(strsplit(input$inText, " +")), n-1), collapse='_')
      key1 <- paste(tail(unlist(strsplit(input$inText, " +")), n-2), collapse='_')

      if(!has.key(hash=model, key=key1) &
         !has.key(hash=model, key=key2) &
         !has.key(hash=model, key=key3)) {
        print(paste("key not found for gram:", gram))
        return()
      }
      
      p <- predict(m=model, s=gram, d=aD, n=n, itr=4, FALSE)
      vGrm <- rep(gram, n)
      vWrd <- p
      
      df <- constructEdges(vGrm, vWrd, n=n)
      
      g <- graph_from_data_frame(df, directed = TRUE)
      plot(g, edge.width=df$weight)
    })
  }
)
