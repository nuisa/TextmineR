library(shiny)
library(wordcloud)
library(qdap)
library(tm)
library(RWeka)
shinyServer(function(input,output){

  ##textminingfuncties 
  tokenizer <- function(x) 
    NGramTokenizer(x, Weka_control(min = input$aantal, max = input$aantal))
  
  clean_corpus <- function(corpus){
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, tolower)
    return(corpus)
  }
  
  
  ##reactive values
  data <- reactive({
    file <- input$file
    if(is.null(file)){return()}
    read.table(file = file$datapath, sep = input$sep, header = TRUE, 
               stringsAsFactors = FALSE, fill = TRUE)
  })
  
  data2 <- reactive({
    bestand <- input$bestand
    if(is.null(bestand)){return()}
    read.table(file = bestand$datapath, sep = input$sep, header = TRUE, 
               stringsAsFactors = FALSE, fill = TRUE)
  })
  
  kw1 <- reactive({
    if(is.null(data())){return()}
    a <- paste(data()$bericht.tekst, collapse = " ")
    woorden_vec1 <- VectorSource(a)
    woorden_cor1 <- VCorpus(woorden_vec1)
    clean_corpus(woorden_cor1)
    
    woorden_tdm1 <- TermDocumentMatrix(
      woorden_cor1, 
      control = list(tokenize = tokenizer))
    
    woorden_m1 <- as.matrix(woorden_tdm1)
    woorden_freq1 <- rowSums(woorden_m1)
    woorden_freq_s1 <- sort(woorden_freq1, decreasing = TRUE)
    as.table(woorden_freq_s1)
  })
 
  kw2 <- reactive({
    if(is.null(data2())){return()}
    b <- paste(data2()$bericht.tekst, collapse = " ")
    woorden_vec2 <- VectorSource(b)
    woorden_cor2 <- VCorpus(woorden_vec2)
    clean_corpus(woorden_cor2)
    
    woorden_tdm2 <- TermDocumentMatrix(
      woorden_cor2, 
      control = list(tokenize = tokenizer))
    
    woorden_m2 <- as.matrix(woorden_tdm2)
    woorden_freq2 <- rowSums(woorden_m2)
    woorden_freq_s2 <- sort(woorden_freq2, decreasing = TRUE)
    as.table(woorden_freq_s2)
    
  })
  
  verschillen <- reactive({
    if(is.null(data())){return()}
    if(is.null(data2())){return()}
    
    oovj <- paste(data()$bericht.tekst, collapse = " ")
    oovn <- paste(data2()$bericht.tekst, collapse = " ")
    all_blog<- c(oovj, oovn)
    all_oov <- VectorSource(all_blog)
    all_corpus <- VCorpus(all_oov)
    clean_corpus(all_corpus)
    
    
    oov_tdm <- TermDocumentMatrix(
      all_corpus, 
      control = list(tokenize = tokenizer))
    
    colnames(oov_tdm) <-c("oovj", "oovn")
    all_m <- as.matrix(oov_tdm)
    common_words <- subset(all_m, all_m[, 1] > 0  & all_m[, 2] <= input$max)
    difference <- abs(common_words[, 1] - common_words[, 2])
    common_words <- cbind(common_words, difference)
    common_words <- common_words[order(common_words[, 3], decreasing = TRUE), ]
    oov_df <- data.frame(bestand1 = common_words[, 1], 
                         bestand2 = common_words[, 2], 
                         woorden = rownames(common_words[,]))
    
  })
  
  ##download functies
  output$downloadKw1 <- downloadHandler(
    
    filename = function(){
      paste("download_", Sys.Date(), ".csv")
    },
    content = function(file){
      write.csv(kw1(), file, row.names = FALSE)
    }
  )
  
  output$downloadKw2 <- downloadHandler(
    
    filename = function(){
      paste("download_", Sys.Date(), ".csv")
    },
    content = function(file){
      write.csv(kw2(), file, row.names = FALSE)
    }
  )
  
  output$downloadVer <- downloadHandler(
    
    filename = function(){
      paste("download_", Sys.Date(), ".csv")
    },
    content = function(file){
      write.csv(verschillen(), file, row.names = FALSE)
    }
  )
  
  ##table outputs
  output$kwb1 <- renderTable({
    if(is.null(kw1())){return()}
    kw1()
  })
  
  output$kwb2 <- renderTable({
    if(is.null(kw2())){return()}
    kw2()
  })

  output$verschil <- renderDataTable({
    if(is.null(verschillen())){return()}
    verschillen()
    
    
  })
  
  ##plot output
  output$plot <- renderPlot({
    oovj <- paste(data()$bericht.tekst, collapse = " ")
    oovn <- paste(data2()$bericht.tekst, collapse = " ")
    all_blog<- c(oovj, oovn)
    all_oov <- VectorSource(all_blog)
    all_corpus <- VCorpus(all_oov)
    clean_corpus(all_corpus)
    
    
    oov_tdm <- TermDocumentMatrix(
      all_corpus, 
      control = list(tokenize = tokenizer))
    
    colnames(oov_tdm) <-c("bestand 1", "bestand 2")
    all_m <- as.matrix(oov_tdm)
    comparison.cloud(all_m, colors = c("orange", "blue"), max.words = 50)
    
  })

  

 
  ##tabbladen
  output$tb <- renderUI({
    if(is.null(data()))
    h5("Gemaakt met:", tags$img(src = "RStudio-Ball.png", heigth = 200, width = 200))
    else
      tabsetPanel(tabPanel("Bestand 1", tableOutput("kwb1")), 
                  tabPanel("Bestand 2", tableOutput("kwb2")),
                  tabPanel("Verschillen", dataTableOutput("verschil")),
                  tabPanel("Comp cloud", plotOutput("plot")))
  })
})






