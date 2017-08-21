library(shiny)

shinyUI(fluidPage(
  titlePanel("Upload je bestanden"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "upload bestand 1"),
      fileInput("bestand", "upload bestand 2"),
      
      radioButtons(inputId = "sep", label = "Separator", choices = c("puntKomma" = ";", 
                  "Komma" = ",")),
      radioButtons(inputId = "aantal", label = "Woorden", choices = c("Keyword" = 1,
                                                                      "Bigram" = 2,
                                                                      "Trigram"= 3)),
      numericInput(inputId = "max", label = "Maximale frequentie", min = 0, max = 50,
                   value = 0),
      
      downloadButton("downloadKw1", "Download KW1"),
      downloadButton("downloadKw2", "Download KW2"),
      downloadButton("downloadVer", "Download verschillen")
     
      
      
      
    ),
    
    mainPanel(
      uiOutput("tb")
    )
  )
))
