library(shiny)
load("Dane.RData")


shinyUI(fluidPage(

    # Tytyul aplikacji
    titlePanel("Analiza rozwoju epidemii HIV/AIDS"),
    
    #TabsetPanel - wybor zakladki
    tabsetPanel(
        tabPanel("KONTYNENT",
                 sidebarPanel(
                     selectInput("kontynent", "Region:", 
                                 choices = kontynenty),
                     hr(),
                     selectInput("cecha", "Cecha:", 
                                 choices = c("nowe","przypadek","smierc")),
                     hr(),
                     selectInput("wiek", "Przedzial wiekowy:", 
                                 choices = opis),
                     hr(),
                     verbatimTextOutput('analiza')
                 ),
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Obecnie",plotOutput("wykAnaliza"),
                                  downloadButton('pobierz', 'Pobierz'),
                                  hr(),
                                  textOutput("uwagadane")),
                         tabPanel("Przewidywania",
                                  plotOutput("wykPredykcja"),
                                  downloadButton('pobierzPred', 'Pobierz'))
                                 
                     ),
                 )),
        tabPanel("SWIAT",
                 sidebarPanel(
                     sliderInput("rok", "Rok:", min = 1990, max = 2019, 
                                 value = 2005, sep = ""),
                     hr(),
                     textOutput("uwagawyswietlanie")
                 ),
                mainPanel(
                    plotOutput("udzial")
                    
                ))
    )
))
