library(shiny)
library(ggplot2)
library(forecast)
library(dplyr)
load("Dane.RData")


# Zdefiniowanie opcji do wyboru oraz do wyswietlania wykresow
shinyServer(function(session, input, output) {

    observe({
        updateSelectInput( session, "kontynent",
                           choices = dana$region
        )
    })
    
    observe({
        updateSelectInput(session, "cecha",
                          choices = dana %>% 
                             filter(region == input$kontynent)%>%
                             select(cecha))
    })
    
    observe({
        updateSelectInput(session, "wiek",
                             choices = dana %>%
                                filter(cecha == input$cecha)%>%
                                select(wiek))
    })
    
    output$pobierz <-downloadHandler(
        filename = function(){
            print(paste0(input$wiek, '.pdf'))
        },
        content = function(file){
            ggsave(file, plotInput())
        
    })
    
    output$analiza <- renderPrint({
        tableInput()
    })
    
    tableInput <- reactive({
        region <- input$kontynent
        cecha <- input$cecha
        nazwa <- paste0(cecha,region)
        
        if(nazwa == 'noweAzja')
            summary(noweAzja[input$wiek])
        else if (nazwa == 'smiercAzja')
            summary(smiercAzja[input$wiek])
        else if (nazwa == 'przypadekAzja')
            summary(przypadekAzja[input$wiek])
        else if (nazwa == 'noweKaraiby')
            summary(noweKaraiby[input$wiek])
        else if (nazwa == 'smiercKaraiby')
          summary(smiercKaraiby[input$wiek])
        else if (nazwa == 'przypadekKaraiby')
          summary(przypadekKaraiby[input$wiek])
        else if (region == 'Wschodnia i Poludniowa Afryka' && cecha == "smierc")
          summary(smiercWPAfryka[input$wiek])
        else if (region == 'Wschodnia i Poludniowa Afryka' && cecha == "nowe")
          summary(noweWPAfryka[input$wiek])
        else if (region == 'Wschodnia i Poludniowa Afryka' && cecha == "przypadek")
          summary(przypadekWPAfryka[input$wiek])
        else if (region == 'Wschodnia Europa i Centralna Azja' && cecha == "smierc")
          summary(smiercWEurAzja[input$wiek])
        else if (region == 'Wschodnia Europa i Centralna Azja' && cecha == "nowe")
          summary(noweWEurAzja[input$wiek])
        else if (region == 'Wschodnia Europa i Centralna Azja' && cecha == "przypadek")
          summary(przypadekWEurAzja[input$wiek])
        else if (region == 'Srodkowa i Poludniowa Ameryka' && cecha == "smierc")
          summary(smiercSPAmeryka[input$wiek])
        else if (region == 'Srodkowa i Poludniowa Ameryka' && cecha == "nowe")
          summary(noweSPAmeryka[input$wiek])
        else if (region == 'Srodkowa i Poludniowa Ameryka' && cecha == "przypadek")
          summary(przypadekSPAmeryka[input$wiek])
        else if (region == 'Srodkowozachodnia i Polnocna Afryka' && cecha == "smierc")
          summary(smiercSWPAfryka[input$wiek])
        else if (region == 'Srodkowozachodnia i Polnocna Afryka' && cecha == "nowe")
          summary(noweSWPAfryka[input$wiek])
        else if (region == 'Srodkowozachodnia i Polnocna Afryka' && cecha == "przypadek")
          summary(przypadekSWPAfryka[input$wiek])
        else if (region == 'Zachodnia i Centralna Afryka' && cecha == "smierc")
          summary(smiercZCAfryka[input$wiek])
        else if (region == 'Zachodnia i Centralna Afryka' && cecha == "nowe")
          summary(noweAzja[input$wiek])
        else if (region == 'Zachodnia i Centralna Afryka' && cecha == "przypadek")
          summary(przypadekZCAfryka[input$wiek])
        else if (region == 'Zachodnia i Centralna Europa oraz Ameryka Polnocna' && cecha == "smierc")
          summary(smiercEurAm[input$wiek])
        else if (region == 'Zachodnia i Centralna Europa oraz Ameryka Polnocna' && cecha == "nowe")
          summary(noweEurAm[input$wiek])
        else if (region == 'Zachodnia i Centralna Europa oraz Ameryka Polnocna' && cecha == "przypadek")
          summary(przypadekEurAm[input$wiek])
    })
    
    output$wykAnaliza <- renderPlot({
        plotInput()
  
    })

    plotInput <- reactive({
        region <- input$kontynent
        cecha <- input$cecha
        nazwa <- paste0(cecha,region)
        if (nazwa == 'noweAzja'){
                ggplot(noweAzja, aes(rok, noweAzja[,input$wiek])) +
                    geom_point(mapping = aes(rok, noweAzja[,input$wiek]), color = 'blue') +
                    geom_line(mapping = aes(rok, noweAzja[,input$wiek]), color = 'blue')+
                    xlab("rok") + ylab("Liczba nowych przypadkow") +
                    ggtitle(sprintf("Nowe przypadki zakazen dla przedzialu: %s (%s)", input$wiek, region))
          
            }else if (nazwa == 'smiercAzja'){
                ggplot(smiercAzja, aes(rok, smiercAzja[,input$wiek])) +
                    geom_point(mapping = aes(rok, smiercAzja[,input$wiek]), color = 'blue') +
                    geom_line(mapping = aes(rok, smiercAzja[,input$wiek]), color = 'blue')+
                    xlab("rok") + ylab("Liczba zgonow") +
                    ggtitle(sprintf("Smierc spowodowana wirusem dla przedzialu: %s (%s)", input$wiek, region))
              
            }else if (nazwa == 'przypadekAzja'){
                ggplot(przypadekAzja, aes(rok, przypadekAzja[,input$wiek])) +
                    geom_point(mapping = aes(rok, przypadekAzja[,input$wiek]), color = 'blue') +
                    geom_line(mapping = aes(rok, przypadekAzja[,input$wiek]), color = 'blue')+
                    xlab("rok") + ylab("Liczba istniejacych przypadkow") +
                    ggtitle(sprintf("Istniejce przypadki dla przedzialu: %s (%s)", input$wiek, region))
              
            }else if (nazwa == 'noweKaraiby'){
                ggplot(noweKaraiby, aes(rok, noweKaraiby[,input$wiek])) +
                    geom_point(mapping = aes(rok, noweKaraiby[,input$wiek]), color = 'blue') +
                    geom_line(mapping = aes(rok, noweKaraiby[,input$wiek]), color = 'blue')+
                    xlab("rok") + ylab("Liczba nowych przypadkow") +
                    ggtitle(sprintf("Nowe przypadki zakazen dla przedzialu: %s (%s)", input$wiek, region))
              
            }else if (nazwa == 'smiercKaraiby'){
                ggplot(smiercKaraiby, aes(rok, smiercKaraiby[,input$wiek])) +
                    geom_point(mapping = aes(rok, smiercKaraiby[,input$wiek]), color = 'blue') +
                    geom_line(mapping = aes(rok, smiercKaraiby[,input$wiek]), color = 'blue')+
                    xlab("rok") + ylab("Liczba zgonow") +
                    ggtitle(sprintf("Smierc spowodowana wirusem dla przedzialu: %s (%s)", input$wiek, region))
              
            }else if (nazwa == 'przypadekKaraiby'){
                ggplot(przypadekKaraiby, aes(rok, przypadekKaraiby[,input$wiek])) +
                    geom_point(mapping = aes(rok, przypadekKaraiby[,input$wiek]), color = 'blue') +
                    geom_line(mapping = aes(rok, przypadekKaraiby[,input$wiek]), color = 'blue')+
                    xlab("rok") + ylab("Liczba isteniejacych przypadkow") +
                    ggtitle(sprintf("Istniejce przypadki dla przedzialu: %s (%s)", input$wiek, region))
              
            }else if (region == 'Wschodnia i Poludniowa Afryka' && cecha == "smierc"){
              ggplot(smiercSWPAfryka, aes(rok, smiercWPAfryka[,input$wiek])) +
                geom_point(mapping = aes(rok, smiercWPAfryka[,input$wiek]), color = 'blue') +
                geom_line(mapping = aes(rok, smiercWPAfryka[,input$wiek]), color = 'blue')+
                xlab("rok") + ylab("Liczba zgonow") +
                ggtitle(sprintf("Smierc spowodowana wirusem dla przedzialu: %s (%s)", input$wiek, region))
              
            }else if (region == 'Wschodnia i Poludniowa Afryka' && cecha == "nowe"){
              ggplot(noweSWPAfryka, aes(rok, noweWPAfryka[,input$wiek])) +
                geom_point(mapping = aes(rok, noweWPAfryka[,input$wiek]), color = 'blue') +
                geom_line(mapping = aes(rok, noweWPAfryka[,input$wiek]), color = 'blue')+
                xlab("rok") + ylab("Liczba nowych przypadkow") +
                ggtitle(sprintf("Nowe przypadki zakazen dla przedzialu: %s (%s)", input$wiek, region))
              
            }else if (region == 'Wschodnia i Poludniowa Afryka' && cecha == "przypadek"){
              ggplot(przypadekSWPAfryka, aes(rok, przypadekWPAfryka[,input$wiek])) +
                geom_point(mapping = aes(rok, przypadekWPAfryka[,input$wiek]), color = 'blue') +
                geom_line(mapping = aes(rok, przypadekWPAfryka[,input$wiek]), color = 'blue')+
                xlab("rok") + ylab("Liczba istniejcych przypadkow") +
                ggtitle(sprintf("Istniejce przypadki dla przedzialu: %s (%s)", input$wiek, region))
              
            }else if (region == 'Wschodnia Europa i Centralna Azja' && cecha == "smierc"){
              ggplot(smiercWEurAzja, aes(rok, smiercWEurAzja[,input$wiek])) +
                geom_point(mapping = aes(rok, smiercWEurAzja[,input$wiek]), color = 'blue') +
                geom_line(mapping = aes(rok, smiercWEurAzja[,input$wiek]), color = 'blue')+
                xlab("rok") + ylab("Liczba zgonow") +
                ggtitle(sprintf("Smierc spowodowana wirusem dla przedzialu: %s (%s)", input$wiek, region))
              
            }else if (region == 'Wschodnia Europa i Centralna Azja' && cecha == "nowe"){
              ggplot(noweWEurAzja, aes(rok, noweWEurAzja[,input$wiek])) +
                geom_point(mapping = aes(rok, noweWEurAzja[,input$wiek]), color = 'blue') +
                geom_line(mapping = aes(rok, noweWEurAzja[,input$wiek]), color = 'blue')+
                xlab("rok") + ylab("Liczba nowych przypadkow") +
                ggtitle(sprintf("Nowe przypadki zakazen dla przedzialu: %s (%s)", input$wiek, region))
              
            }else if (region == 'Wschodnia Europa i Centralna Azja' && cecha == "przypadek"){
              ggplot(przypadekWEurAzja, aes(rok, przypadekWEurAzja[,input$wiek])) +
                geom_point(mapping = aes(rok, przypadekWEurAzja[,input$wiek]), color = 'blue') +
                geom_line(mapping = aes(rok, przypadekWEurAzja[,input$wiek]), color = 'blue')+
                xlab("rok") + ylab("Liczba istniejcych przypadkow") +
                ggtitle(sprintf("Istniejce przypadki dla przedzialu: %s (%s)", input$wiek, region))
            
            }else if (region == 'Srodkowa i Poludniowa Ameryka' && cecha == "smierc"){
              ggplot(smiercSPAmeryka, aes(rok, smiercSPAmeryka[,input$wiek])) +
                geom_point(mapping = aes(rok, smiercSPAmeryka[,input$wiek]), color = 'blue') +
                geom_line(mapping = aes(rok, smiercSPAmeryka[,input$wiek]), color = 'blue')+
                xlab("rok") + ylab("Liczba zgonow") +
                ggtitle(sprintf("Smierc spowodowana wirusem dla przedzialu: %s (%s)", input$wiek, region))
              
            }else if (region == 'Srodkowa i Poludniowa Ameryka' && cecha == "nowe"){
              ggplot(noweSPAmeryka, aes(rok, noweSPAmeryka[,input$wiek])) +
                geom_point(mapping = aes(rok, noweSPAmeryka[,input$wiek]), color = 'blue') +
                geom_line(mapping = aes(rok, noweSPAmeryka[,input$wiek]), color = 'blue')+
                xlab("rok") + ylab("Liczba nowych przypadkow") +
                ggtitle(sprintf("Nowe przypadki zakazen dla przedzialu: %s (%s)", input$wiek, region))
            
            }else if (region == 'Srodkowa i Poludniowa Ameryka' && cecha == "przypadek"){
              ggplot(przypadekSPAmeryka, aes(rok, przypadekSPAmeryka[,input$wiek])) +
                geom_point(mapping = aes(rok, przypadekSPAmeryka[,input$wiek]), color = 'blue') +
                geom_line(mapping = aes(rok, przypadekSPAmeryka[,input$wiek]), color = 'blue')+
                xlab("rok") + ylab("Liczba istniejcych przypadkow") +
                ggtitle(sprintf("Istniejce przypadki dla przedzialu: %s (%s)", input$wiek, region))
              
            }else if (region == 'Srodkowozachodnia i Polnocna Afryka' && cecha == "smierc"){
              ggplot(smiercSWPAfryka, aes(rok, smiercSWPAfryka[,input$wiek])) +
                geom_point(mapping = aes(rok, smiercSWPAfryka[,input$wiek]), color = 'blue') +
                geom_line(mapping = aes(rok, smiercSWPAfryka[,input$wiek]), color = 'blue')+
                xlab("rok") + ylab("Liczba zgonow") +
                ggtitle(sprintf("Smierc spowodowana wirusem dla przedzialu: %s (%s)", input$wiek, region))
              
            }else if (region == 'Srodkowozachodnia i Polnocna Afryka' && cecha == "nowe"){
              ggplot(noweSWPAfryka, aes(rok, noweSWPAfryka[,input$wiek])) +
                geom_point(mapping = aes(rok, noweSWPAfryka[,input$wiek]), color = 'blue') +
                geom_line(mapping = aes(rok, noweSWPAfryka[,input$wiek]), color = 'blue')+
                xlab("rok") + ylab("Liczba nowych przypadkow") +
                ggtitle(sprintf("Nowe przypadki zakazen dla przedzialu: %s (%s)", input$wiek, region))
              
            }else if (region == 'Srodkowozachodnia i Polnocna Afryka' && cecha == "przypadek"){
              ggplot(przypadekSWPAfryka, aes(rok, przypadekSWPAfryka[,input$wiek])) +
                geom_point(mapping = aes(rok, przypadekSWPAfryka[,input$wiek]), color = 'blue') +
                geom_line(mapping = aes(rok, przypadekSPAmeryka[,input$wiek]), color = 'blue')+
                xlab("rok") + ylab("Liczba istniejcych przypadkow") +
                ggtitle(sprintf("Istniejce przypadki dla przedzialu: %s (%s)", input$wiek, region))
              
            } else if (region == 'Zachodnia i Centralna Afryka' && cecha == "smierc"){
              ggplot(smiercZCAfryka, aes(rok, smiercZCAfryka[,input$wiek])) +
                geom_point(mapping = aes(rok, smiercZCAfryka[,input$wiek]), color = 'blue') +
                geom_line(mapping = aes(rok, smiercZCAfryka[,input$wiek]), color = 'blue')+
                xlab("rok") + ylab("Liczba zgonow") +
                ggtitle(sprintf("Smierc spowodowana wirusem dla przedzialu: %s (%s)", input$wiek, region))
              
            }else if (region == 'Zachodnia i Centralna Afryka' && cecha == "nowe"){
              ggplot(noweZCAfryka, aes(rok, noweZCAfryka[,input$wiek])) +
                geom_point(mapping = aes(rok, noweZCAfryka[,input$wiek]), color = 'blue') +
                geom_line(mapping = aes(rok, noweZCAfryka[,input$wiek]), color = 'blue')+
                xlab("rok") + ylab("Liczba nowych przypadkow") +
                ggtitle(sprintf("Nowe przypadki zakazen dla przedzialu: %s (%s)", input$wiek, region))
              
            }else if (region == 'Zachodnia i Centralna Afryka' && cecha == "przypadek"){
              ggplot(przypadekZCAfryka, aes(rok, przypadekZCAfryka[,input$wiek])) +
                geom_point(mapping = aes(rok, przypadekZCAfryka[,input$wiek]), color = 'blue') +
                geom_line(mapping = aes(rok, przypadekZCAfryka[,input$wiek]), color = 'blue')+
                xlab("rok") + ylab("Liczba istniejcych przypadkow") +
                ggtitle(sprintf("Istniejce przypadki dla przedzialu: %s (%s)", input$wiek, region))
              
            }else if (region == 'Zachodnia i Centralna Europa oraz Ameryka Polnocna' && cecha == "smierc"){
              ggplot(smiercEurAm, aes(rok, smiercEurAm[,input$wiek])) +
                geom_point(mapping = aes(rok, smiercEurAm[,input$wiek]), color = 'blue') +
                geom_line(mapping = aes(rok, smiercEurAm[,input$wiek]), color = 'blue')+
                xlab("rok") + ylab("Liczba zgonow") +
                ggtitle(sprintf("Smierc spowodowana wirusem dla przedzialu: %s (%s)", input$wiek, region))
              
            }else if (region == 'Zachodnia i Centralna Europa oraz Ameryka Polnocna' && cecha == "nowe"){
              ggplot(noweEurAm, aes(rok, noweEurAm[,input$wiek])) +
                geom_point(mapping = aes(rok, noweEurAm[,input$wiek]), color = 'blue') +
                geom_line(mapping = aes(rok, noweEurAm[,input$wiek]), color = 'blue')+
                xlab("rok") + ylab("Liczba nowych przypadkow") +
                ggtitle(sprintf("Nowe przypadki zakazen dla przedzialu: %s (%s)", input$wiek, region))
              
            }else if (region == 'Zachodnia i Centralna Europa oraz Ameryka Polnocna' && cecha == "przypadek"){
              ggplot(przypadekEurAm, aes(rok, przypadekEurAm[,input$wiek])) +
                geom_point(mapping = aes(rok, przypadekEurAm[,input$wiek]), color = 'blue') +
                geom_line(mapping = aes(rok, przypadekEurAm[,input$wiek]), color = 'blue')+
                xlab("rok") + ylab("Liczba istniejcych przypadkow") +
                ggtitle(sprintf("Istniejce przypadki dla przedzialu: %s (%s)", input$wiek, region))
            }
 
    })    
    
    uwagaDane <- reactive({
      "Jesli WYKRES nie zostal narysowany -> BRAK DOSTEPNYCH DANYCH;
        Zakladka PRZEWIDYWANIA - nie dziala"
    })
    
    output$uwagadane <-renderText({
      uwagaDane()
    })
    
    
    output$wykPredykcja <- renderPlot({
      plotInput2()
    })
    
    plotInput2 <- reactive({
        region <- input$kontynent
        cecha <- input$cecha
        nazwa <- paste0(cecha,region)
        
        if(nazwa == 'noweAzja'){
            mymodel <- auto.arima(noweAzja[input$wiek])
            myforecast <- forecast(mymodel,level=c(95),h = 10)
            autoplot(myforecast, showgap = FALSE, xlab = "Szereg czasowy", 
                     ylab = "Ludnosc")
        }
        else if(nazwa == 'smiercAzja'){
            mymodel <- auto.arima(smiercAzja[input$wiek])
            myforecast <- forecast(mymodel,level=c(95),h = 10)
            autoplot(myforecast, showgap = FALSE, xlab = "Szereg czasowy", 
                   ylab = "Ludnosc")
        }
        else if(nazwa == 'przypadekAzja'){
          mymodel <- auto.arima(przypadekAzja[input$wiek])
          myforecast <- forecast(mymodel,level=c(95),h = 10)
          autoplot(myforecast, showgap = FALSE, xlab = "Szereg czasowy", 
                   ylab = "Ludnosc")
        }
        else if(nazwa == 'smiercKaraiby'){
          mymodel <- auto.arima(smiercKaraiby[input$wiek])
          myforecast <- forecast(mymodel,level=c(95),h = 10)
          autoplot(myforecast, showgap = FALSE, xlab = "Szereg czasowy", 
                   ylab = "Ludnosc")
        }
        else if(nazwa == 'noweKaraiby'){
          mymodel <- auto.arima(noweKaraiby[input$wiek])
          myforecast <- forecast(mymodel,level=c(95),h = 10)
          autoplot(myforecast, showgap = FALSE, xlab = "Szereg czasowy", 
                   ylab = "Ludnosc")
        }
        else if(nazwa == 'przypadekKaraiby'){
          mymodel <- auto.arima(przypadekKaraiby[input$wiek])
          myforecast <- forecast(mymodel,level=c(95),h = 10)
          autoplot(myforecast, showgap = FALSE, xlab = "Szereg czasowy", 
                   ylab = "Ludnosc")
        }
        else if (region == 'Wschodnia i Poludniowa Afryka' && cecha == "smierc"){
          mymodel <- auto.arima(smiercWPAfryka[input$wiek])
          myforecast <- forecast(mymodel,level=c(95),h = 10)
          autoplot(myforecast, showgap = FALSE, xlab = "Szereg czasowy", 
                   ylab = "Ludnosc")
        }
        else if (region == 'Wschodnia i Poludniowa Afryka' && cecha == "nowe"){
          mymodel <- auto.arima(noweWPAfryka[input$wiek])
          myforecast <- forecast(mymodel,level=c(95),h = 10)
          autoplot(myforecast, showgap = FALSE, xlab = "Szereg czasowy", 
                   ylab = "Ludnosc")
        }
        else if (region == 'Wschodnia i Poludniowa Afryka' && cecha == "przypadek"){
          mymodel <- auto.arima(przypadekWPAfryka[input$wiek])
          myforecast <- forecast(mymodel,level=c(95),h = 10)
          autoplot(myforecast, showgap = FALSE, xlab = "Szereg czasowy", 
                   ylab = "Ludnosc")
        }
        else if (region == 'Wschodnia Europa i Centralna Azja' && cecha == "smierc"){
          mymodel <- auto.arima(smiercWEurAzja[input$wiek])
          myforecast <- forecast(mymodel,level=c(95),h = 10)
          autoplot(myforecast, showgap = FALSE, xlab = "Szereg czasowy", 
                   ylab = "Ludnosc")
        }
        else if (region == 'Wschodnia Europa i Centralna Azja' && cecha == "nowe"){
          mymodel <- auto.arima(noweWEurAzja[input$wiek])
          myforecast <- forecast(mymodel,level=c(95),h = 10)
          autoplot(myforecast, showgap = FALSE, xlab = "Szereg czasowy", 
                   ylab = "Ludnosc")
        }
        else if (region == 'Wschodnia Europa i Centralna Azja' && cecha == "przypadek"){
          mymodel <- auto.arima(przypadekWEurAzja[input$wiek])
          myforecast <- forecast(mymodel,level=c(95),h = 10)
          autoplot(myforecast, showgap = FALSE, xlab = "Szereg czasowy", 
                   ylab = "Ludnosc")
        }
        else if (region == 'Srodkowa i Poludniowa Ameryka' && cecha == "smierc"){
          mymodel <- auto.arima(smiercSPAmeryka[input$wiek])
          myforecast <- forecast(mymodel,level=c(95),h = 10)
          autoplot(myforecast, showgap = FALSE, xlab = "Szereg czasowy", 
                   ylab = "Ludnosc")
        }
        else if (region == 'Srodkowa i Poludniowa Ameryka' && cecha == "nowe"){
          mymodel <- auto.arima(noweSPAmeryka[input$wiek])
          myforecast <- forecast(mymodel,level=c(95),h = 10)
          autoplot(myforecast, showgap = FALSE, xlab = "Szereg czasowy", 
                   ylab = "Ludnosc")
        }
        else if (region == 'Srodkowa i Poludniowa Ameryka' && cecha == "przypadek"){
          mymodel <- auto.arima(przypadekSPAmeryka[input$wiek])
          myforecast <- forecast(mymodel,level=c(95),h = 10)
          autoplot(myforecast, showgap = FALSE, xlab = "Szereg czasowy", 
                   ylab = "Ludnosc")
        }
        else if (region == 'Srodkowozachodnia i Polnocna Afryka' && cecha == "smierc"){
          mymodel <- auto.arima(smiercSWPAfryka[input$wiek])
          myforecast <- forecast(mymodel,level=c(95),h = 10)
          autoplot(myforecast, showgap = FALSE, xlab = "Szereg czasowy", 
                   ylab = "Ludnosc")
        }
        else if (region == 'Srodkowozachodnia i Polnocna Afryka' && cecha == "nowe"){
          mymodel <- auto.arima(noweSWPAfryka[input$wiek])
          myforecast <- forecast(mymodel,level=c(95),h = 10)
          autoplot(myforecast, showgap = FALSE, xlab = "Szereg czasowy", 
                   ylab = "Ludnosc")
        }
        else if (region == 'Srodkowozachodnia i Polnocna Afryka' && cecha == "przypadek"){
          mymodel <- auto.arima(przypadekSWPAfryka[input$wiek])
          myforecast <- forecast(mymodel,level=c(95),h = 10)
          autoplot(myforecast, showgap = FALSE, xlab = "Szereg czasowy", 
                   ylab = "Ludnosc")
        }
        else if (region == 'Zachodnia i Centralna Afryka' && cecha == "smierc"){
          mymodel <- auto.arima(smiercZCAfryka[input$wiek])
          myforecast <- forecast(mymodel,level=c(95),h = 10)
          autoplot(myforecast, showgap = FALSE, xlab = "Szereg czasowy", 
                   ylab = "Ludnosc")
        }
        else if (region == 'Zachodnia i Centralna Afryka' && cecha == "nowe"){
          mymodel <- auto.arima(noweZCAfryka[input$wiek])
          myforecast <- forecast(mymodel,level=c(95),h = 10)
          autoplot(myforecast, showgap = FALSE, xlab = "Szereg czasowy", 
                   ylab = "Ludnosc")
        }
        else if (region == 'Zachodnia i Centralna Afryka' && cecha == "przypadek"){
          mymodel <- auto.arima(przypadekZCAfryka[input$wiek])
          myforecast <- forecast(mymodel,level=c(95),h = 10)
          autoplot(myforecast, showgap = FALSE, xlab = "Szereg czasowy", 
                   ylab = "Ludnosc")
        }
        else if (region == 'Zachodnia i Centralna Europa oraz Ameryka Polnocna' && cecha == "smierc"){
          mymodel <- auto.arima(smiercEurAm[input$wiek])
          myforecast <- forecast(mymodel,level=c(95),h = 10)
          autoplot(myforecast, showgap = FALSE, xlab = "Szereg czasowy", 
                   ylab = "Ludnosc")

        }
        else if (region == 'Zachodnia i Centralna Europa oraz Ameryka Polnocna' && cecha == "nowe"){
          mymodel <- auto.arima(noweEurAm[input$wiek])
          myforecast <- forecast(mymodel,level=c(95),h = 10)
          autoplot(myforecast, showgap = FALSE, xlab = "Szereg czasowy", 
                   ylab = "Ludnosc")
        }
        else if (region == 'Zachodnia i Centralna Europa oraz Ameryka Polnocna' && cecha == "przypadek"){
          mymodel <- auto.arima(przypadekEurAm[input$wiek])
          myforecast <- forecast(mymodel,level=c(95),h = 10)
          autoplot(myforecast, showgap = FALSE, xlab = "Szereg czasowy", 
                   ylab = "Ludnosc")
        }

    })
    
    output$Pred <-downloadHandler(
      filename = "predykcja.pdf",
      content = function(file){
        ggsave(file, plotInput2())})

    cecha <- reactive({
      input$cecha
    })
    
    plotInput3 <- reactive({
      rok <- input$rok
      regiony <- paste0(cecha(),"Total")
      
      if(regiony == 'smiercTotal'){
        if( rok == '1990'){
          opisprocent <- round(100*smiercTotal$`1990`/sum(smiercTotal$`1990`),1)
          pie(smiercTotal$`1990`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: SMIERC w 1990",
              col = rainbow(length(smiercTotal$`1990`)))
          par(xpd = TRUE)
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(smiercTotal$`1990`)))
        }

        else if( rok == '1991'){
          opisprocent <- round(100*smiercTotal$`1991`/sum(smiercTotal$`1991`),1)
          pie(smiercTotal$`1991`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: SMIERC w 1991",
              col = rainbow(length(smiercTotal$`1990`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(smiercTotal$`1991`)))
        }
          
        else if( rok == '1992'){
            opisprocent <- round(100*smiercTotal$`1992`/sum(smiercTotal$`1992`),1)
            pie(smiercTotal$`1992`, labels = paste(opisprocent, sep = " ", "%"),
                main = "Udzial poszczegolnych kontynentow cecha: SMIERC w 1992",
                col = rainbow(length(smiercTotal$`1992`)))
            legend("topright", kontynentyWykres, cex = 0.7, 
                   fill = rainbow(length(smiercTotal$`1992`)))
          
        }

        else if( rok == '1993'){
          opisprocent <- round(100*smiercTotal$`1993`/sum(smiercTotal$`1993`),1)
          pie(smiercTotal$`1993`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: SMIERC w 1993",
              col = rainbow(length(smiercTotal$`1993`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(smiercTotal$`1993`)))
          
        }
        else if( rok == '1994'){
          opisprocent <- round(100*smiercTotal$`1994`/sum(smiercTotal$`1994`),1)
          pie(smiercTotal$`1994`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: SMIERC w 1994",
              col = rainbow(length(smiercTotal$`1994`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(smiercTotal$`1994`)))
        }
 
        else if( rok == '1995'){
          opisprocent <- round(100*smiercTotal$`1995`/sum(smiercTotal$`1995`),1)
          pie(smiercTotal$`1995`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: SMIERC w 1995",
              col = rainbow(length(smiercTotal$`1995`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(smiercTotal$`1995`)))
        }

        else if( rok == '1996'){
          opisprocent <- round(100*smiercTotal$`1996`/sum(smiercTotal$`1996`),1)
          pie(smiercTotal$`1996`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: SMIERC w 1996",
              col = rainbow(length(smiercTotal$`1996`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(smiercTotal$`1996`)))
        }

        else if( rok == '1997'){
          opisprocent <- round(100*smiercTotal$`1997`/sum(smiercTotal$`1997`),1)
          pie(smiercTotal$`1997`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: SMIERC w 1997",
              col = rainbow(length(smiercTotal$`1997`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(smiercTotal$`1997`)))
        }

        else if( rok == '1998'){
          opisprocent <- round(100*smiercTotal$`1998`/sum(smiercTotal$`1998`),1)
          pie(smiercTotal$`1998`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: SMIERC w 1998",
              col = rainbow(length(smiercTotal$`1998`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(smiercTotal$`1998`)))
        }

        else if( rok == '1999'){
          opisprocent <- round(100*smiercTotal$`1999`/sum(smiercTotal$`1999`),1)
          pie(smiercTotal$`1999`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: SMIERC w 1999",
              col = rainbow(length(smiercTotal$`1999`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(smiercTotal$`1999`)))
        }

        else if( rok == '2000'){
          opisprocent <- round(100*smiercTotal$`2000`/sum(smiercTotal$`2000`),1)
          pie(smiercTotal$`2000`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: SMIERC w 2000",
              col = rainbow(length(smiercTotal$`2000`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(smiercTotal$`2000`)))
        }

        else if( rok == '2001'){
          opisprocent <- round(100*smiercTotal$`2001`/sum(smiercTotal$`2001`),1)
          pie(smiercTotal$`2001`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: SMIERC w 2001",
              col = rainbow(length(smiercTotal$`2001`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(smiercTotal$`2001`)))
        }

        else if( rok == '2002'){
          opisprocent <- round(100*smiercTotal$`2002`/sum(smiercTotal$`2002`),1)
          pie(smiercTotal$`2002`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: SMIERC w 2002",
              col = rainbow(length(smiercTotal$`2002`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(smiercTotal$`2002`)))
        }

        else if( rok == '2003'){
          opisprocent <- round(100*smiercTotal$`2003`/sum(smiercTotal$`2003`),1)
          pie(smiercTotal$`2003`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: SMIERC w 2003",
              col = rainbow(length(smiercTotal$`2003`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(smiercTotal$`2003`)))
        }

        else if( rok == '2004'){
          opisprocent <- round(100*smiercTotal$`2004`/sum(smiercTotal$`2004`),1)
          pie(smiercTotal$`2004`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: SMIERC w 2004",
              col = rainbow(length(smiercTotal$`2004`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(smiercTotal$`2004`)))
        }

        else if( rok == '2005'){
          opisprocent <- round(100*smiercTotal$`2005`/sum(smiercTotal$`2005`),1)
          pie(smiercTotal$`2005`, labels = paste(opisprocent,sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: SMIERC w 2005",
              col = rainbow(length(smiercTotal$`2005`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(smiercTotal$`2005`)))
        }

        else if( rok == '2006'){
          opisprocent <- round(100*smiercTotal$`2006`/sum(smiercTotal$`2006`),1)
          pie(smiercTotal$`2006`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: SMIERC w 2006",
              col = rainbow(length(smiercTotal$`2006`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(smiercTotal$`2006`)))
        }

        else if( rok == '2007'){
          opisprocent <- round(100*smiercTotal$`2007`/sum(smiercTotal$`2007`),1)
          pie(smiercTotal$`2007`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: SMIERC w 2007",
              col = rainbow(length(smiercTotal$`2007`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(smiercTotal$`2007`)))
        }

        else if( rok == '2008'){
          opisprocent <- round(100*smiercTotal$`2008`/sum(smiercTotal$`2008`),1)
          pie(smiercTotal$`2008`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: SMIERC w 2008",
              col = rainbow(length(smiercTotal$`2008`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(smiercTotal$`2008`)))
        }

        else if( rok == '2009'){
          opisprocent <- round(100*smiercTotal$`2009`/sum(smiercTotal$`2009`),1)
          pie(smiercTotal$`2009`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: SMIERC w 2009",
              col = rainbow(length(smiercTotal$`2009`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(smiercTotal$`2009`)))
        }

        else if( rok == '2010'){
          opisprocent <- round(100*smiercTotal$`2010`/sum(smiercTotal$`2010`),1)
          pie(smiercTotal$`2010`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: SMIERC w 2010",
              col = rainbow(length(smiercTotal$`2010`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(smiercTotal$`2010`)))
        }

        else if( rok == '2011'){
          opisprocent <- round(100*smiercTotal$`2011`/sum(smiercTotal$`2011`),1)
          pie(smiercTotal$`2011`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: SMIERC w 2011",
              col = rainbow(length(smiercTotal$`2011`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(smiercTotal$`2011`)))
        }

        else if( rok == '2012'){
          opisprocent <- round(100*smiercTotal$`2012`/sum(smiercTotal$`2012`),1)
          pie(smiercTotal$`2012`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: SMIERC w 2012",
              col = rainbow(length(smiercTotal$`2012`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(smiercTotal$`2012`)))
        }

        else if( rok == '2013'){
          opisprocent <- round(100*smiercTotal$`2013`/sum(smiercTotal$`2013`),1)
          pie(smiercTotal$`2013`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: SMIERC w 2013",
              col = rainbow(length(smiercTotal$`2013`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(smiercTotal$`2013`)))
        }

        else if( rok == '2014'){
          opisprocent <- round(100*smiercTotal$`2014`/sum(smiercTotal$`2014`),1)
          pie(smiercTotal$`2014`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: SMIERC w 2014",
              col = rainbow(length(smiercTotal$`2014`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(smiercTotal$`2014`)))
        }

        else if( rok == '2015'){
          opisprocent <- round(100*smiercTotal$`2015`/sum(smiercTotal$`2015`),1)
          pie(smiercTotal$`2015`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: SMIERC w 2015",
              col = rainbow(length(smiercTotal$`2015`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(smiercTotal$`2015`)))
        }

        else if( rok == '2016'){
          opisprocent <- round(100*smiercTotal$`2016`/sum(smiercTotal$`2016`),1)
          pie(smiercTotal$`2016`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: SMIERC w 2016",
              col = rainbow(length(smiercTotal$`2016`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(smiercTotal$`2016`)))
        }

        else if( rok == '2017'){
          opisprocent <- round(100*smiercTotal$`2017`/sum(smiercTotal$`2017`),1)
          pie(smiercTotal$`2017`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: SMIERC w 2017",
              col = rainbow(length(smiercTotal$`2017`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(smiercTotal$`2017`)))
        }

        else if( rok == '2018'){
          opisprocent <- round(100*smiercTotal$`2018`/sum(smiercTotal$`2018`),1)
          pie(smiercTotal$`2018`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: SMIERC w 2018",
              col = rainbow(length(smiercTotal$`2018`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(smiercTotal$`2018`)))
        }

        else if( rok == '2019'){
          opisprocent <- round(100*smiercTotal$`2019`/sum(smiercTotal$`2019`),1)
          pie(smiercTotal$`2019`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: SMIERC w 2019",
              col = rainbow(length(smiercTotal$`2019`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(smiercTotal$`2019`)))
        }
        
      }else if(regiony == 'noweTotal'){
        if( rok == '1990'){
          opisprocent <- round(100*noweTotal$`1990`/sum(noweTotal$`1990`),1)
          pie(noweTotal$`1990`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: NOWE PRZYPADKI w 1990",
              col = rainbow(length(noweTotal$`1990`)))
          par(xpd = TRUE)
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(noweTotal$`1990`)))
        }
        
        else if( rok == '1991'){
          opisprocent <- round(100*noweTotal$`1991`/sum(noweTotal$`1991`),1)
          pie(noweTotal$`1991`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: NOWE PRZYPADKI w 1991",
              col = rainbow(length(noweTotal$`1990`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(noweTotal$`1991`)))
        }
        
        else if( rok == '1992'){
          opisprocent <- round(100*noweTotal$`1992`/sum(noweTotal$`1992`),1)
          pie(noweTotal$`1992`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: NOWE PRZYPADKI w 1992",
              col = rainbow(length(noweTotal$`1992`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(noweTotal$`1992`)))
        }
        
        else if( rok == '1993'){
          opisprocent <- round(100*noweTotal$`1993`/sum(noweTotal$`1993`),1)
          pie(noweTotal$`1993`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: NOWE PRZYPADKI w 1993",
              col = rainbow(length(noweTotal$`1993`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(noweTotal$`1993`)))
          
        }
        else if( rok == '1994'){
          opisprocent <- round(100*noweTotal$`1994`/sum(noweTotal$`1994`),1)
          pie(noweTotal$`1994`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: NOWE PRZYPADKI w 1994",
              col = rainbow(length(noweTotal$`1994`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(noweTotal$`1994`)))
        }
        
        else if( rok == '1995'){
          opisprocent <- round(100*noweTotal$`1995`/sum(noweTotal$`1995`),1)
          pie(noweTotal$`1995`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: NOWE PRZYPADKI w 1995",
              col = rainbow(length(noweTotal$`1995`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(noweTotal$`1995`)))
        }
        
        else if( rok == '1996'){
          opisprocent <- round(100*noweTotal$`1996`/sum(noweTotal$`1996`),1)
          pie(noweTotal$`1996`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: NOWE PRZYPADKI w 1996",
              col = rainbow(length(noweTotal$`1996`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(noweTotal$`1996`)))
        }
        
        else if( rok == '1997'){
          opisprocent <- round(100*noweTotal$`1997`/sum(noweTotal$`1997`),1)
          pie(noweTotal$`1997`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: NOWE PRZYPADKI w 1997",
              col = rainbow(length(noweTotal$`1997`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(noweTotal$`1997`)))
        }
        
        else if( rok == '1998'){
          opisprocent <- round(100*noweTotal$`1998`/sum(noweTotal$`1998`),1)
          pie(noweTotal$`1998`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: NOWE PRZYPADKI w 1998",
              col = rainbow(length(noweTotal$`1998`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(noweTotal$`1998`)))
        }
        
        else if( rok == '1999'){
          opisprocent <- round(100*noweTotal$`1999`/sum(noweTotal$`1999`),1)
          pie(noweTotal$`1999`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: NOWE PRZYPADKI w 1999",
              col = rainbow(length(noweTotal$`1999`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(noweTotal$`1999`)))
        }
        
        else if( rok == '2000'){
          opisprocent <- round(100*noweTotal$`2000`/sum(noweTotal$`2000`),1)
          pie(noweTotal$`2000`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: NOWE PRZYPADKI w 2000",
              col = rainbow(length(noweTotal$`2000`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(noweTotal$`2000`)))
        }
        
        else if( rok == '2001'){
          opisprocent <- round(100*noweTotal$`2001`/sum(noweTotal$`2001`),1)
          pie(noweTotal$`2001`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: NOWE PRZTPADKI w 2001",
              col = rainbow(length(noweTotal$`2001`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(noweTotal$`2001`)))
        }
        
        else if( rok == '2002'){
          opisprocent <- round(100*noweTotal$`2002`/sum(noweTotal$`2002`),1)
          pie(noweTotal$`2002`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: NOWE PRZYPADKI w 2002",
              col = rainbow(length(noweTotal$`2002`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(noweTotal$`2002`)))
        }
        
        else if( rok == '2003'){
          opisprocent <- round(100*noweTotal$`2003`/sum(noweTotal$`2003`),1)
          pie(noweTotal$`2003`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: NOWE PRZYPADKI w 2003",
              col = rainbow(length(noweTotal$`2003`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(noweTotal$`2003`)))
        }
        
        else if( rok == '2004'){
          opisprocent <- round(100*noweTotal$`2004`/sum(noweTotal$`2004`),1)
          pie(noweTotal$`2004`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: NOWE PRZYPADKI w 2004",
              col = rainbow(length(noweTotal$`2004`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(noweTotal$`2004`)))
        }
        
        else if( rok == '2005'){
          opisprocent <- round(100*noweTotal$`2005`/sum(noweTotal$`2005`),1)
          pie(noweTotal$`2005`, labels = paste(opisprocent,sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: NOWE PRZYPADKI w 2005",
              col = rainbow(length(noweTotal$`2005`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(noweTotal$`2005`)))
        }
        
        else if( rok == '2006'){
          opisprocent <- round(100*noweTotal$`2006`/sum(noweTotal$`2006`),1)
          pie(noweTotal$`2006`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: NOWE PRZYPADKI w 2006",
              col = rainbow(length(noweTotal$`2006`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(noweTotal$`2006`)))
        }
        
        else if( rok == '2007'){
          opisprocent <- round(100*noweTotal$`2007`/sum(noweTotal$`2007`),1)
          pie(noweTotal$`2007`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: NOWE PRZYPADKI w 2007",
              col = rainbow(length(noweTotal$`2007`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(noweTotal$`2007`)))
        }
        
        else if( rok == '2008'){
          opisprocent <- round(100*noweTotal$`2008`/sum(noweTotal$`2008`),1)
          pie(noweTotal$`2008`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: NOWE PRZYPADKI w 2008",
              col = rainbow(length(noweTotal$`2008`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(noweTotal$`2008`)))
        }
        
        else if( rok == '2009'){
          opisprocent <- round(100*noweTotal$`2009`/sum(noweTotal$`2009`),1)
          pie(noweTotal$`2009`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: NOWE PRZYPADKI w 2009",
              col = rainbow(length(noweTotal$`2009`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(noweTotal$`2009`)))
        }
        
        else if( rok == '2010'){
          opisprocent <- round(100*noweTotal$`2010`/sum(noweTotal$`2010`),1)
          pie(noweTotal$`2010`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: NOWE PRZYPADKI w 2010",
              col = rainbow(length(noweTotal$`2010`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(noweTotal$`2010`)))
        }
        
        else if( rok == '2011'){
          opisprocent <- round(100*noweTotal$`2011`/sum(noweTotal$`2011`),1)
          pie(noweTotal$`2011`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: NOWE PRZYPADKI w 2011",
              col = rainbow(length(noweTotal$`2011`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(noweTotal$`2011`)))
        }
        
        else if( rok == '2012'){
          opisprocent <- round(100*noweTotal$`2012`/sum(noweTotal$`2012`),1)
          pie(noweTotal$`2012`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: NOWE PRZYPADKI w 2012",
              col = rainbow(length(noweTotal$`2012`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(noweTotal$`2012`)))
        }
        
        else if( rok == '2013'){
          opisprocent <- round(100*noweTotal$`2013`/sum(noweTotal$`2013`),1)
          pie(noweTotal$`2013`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: NOWE PRZYPADKI w 2013",
              col = rainbow(length(noweTotal$`2013`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(noweTotal$`2013`)))
        }
        
        else if( rok == '2014'){
          opisprocent <- round(100*noweTotal$`2014`/sum(noweTotal$`2014`),1)
          pie(noweTotal$`2014`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: NOWE PRZYPADKI w 2014",
              col = rainbow(length(noweTotal$`2014`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(noweTotal$`2014`)))
        }
        
        else if( rok == '2015'){
          opisprocent <- round(100*noweTotal$`2015`/sum(noweTotal$`2015`),1)
          pie(noweTotal$`2015`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: NOWE PRZYPADKI w 2015",
              col = rainbow(length(noweTotal$`2015`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(noweTotal$`2015`)))
        }
        
        else if( rok == '2016'){
          opisprocent <- round(100*noweTotal$`2016`/sum(noweTotal$`2016`),1)
          pie(noweTotal$`2016`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: NOWE PRZYPADKI w 2016",
              col = rainbow(length(noweTotal$`2016`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(noweTotal$`2016`)))
        }
        
        else if( rok == '2017'){
          opisprocent <- round(100*noweTotal$`2017`/sum(noweTotal$`2017`),1)
          pie(noweTotal$`2017`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: NOWE PRZYPADKI w 2017",
              col = rainbow(length(noweTotal$`2017`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(noweTotal$`2017`)))
        }
        
        else if( rok == '2018'){
          opisprocent <- round(100*noweTotal$`2018`/sum(noweTotal$`2018`),1)
          pie(noweTotal$`2018`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: NOWE PRZYPADKI w 2018",
              col = rainbow(length(noweTotal$`2018`)))
          legend("topright", kontynentyWykres, cex = 0.8, 
                 fill = rainbow(length(noweTotal$`2018`)))
        }
        
        else if( rok == '2019'){
          opisprocent <- round(100*noweTotal$`2019`/sum(noweTotal$`2019`),1)
          pie(noweTotal$`2019`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: NOWE PRZYPADKI w 2019",
              col = rainbow(length(noweTotal$`2019`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(noweTotal$`2019`)))
        }
        
        
      }else if(regiony == 'przypadekTotal'){
        if( rok == '1990'){
          opisprocent <- round(100*przypadekTotal$`1990`/sum(przypadekTotal$`1990`),1)
          pie(przypadekTotal$`1990`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: ISTNIEJACE PRZYPADKI w 1990",
              col = rainbow(length(przypadekTotal$`1990`)))
          par(xpd = TRUE)
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(noweTotal$`1990`)))
        }
        
        else if( rok == '1991'){
          opisprocent <- round(100*przypadekTotal$`1991`/sum(przypadekTotal$`1991`),1)
          pie(przypadekTotal$`1991`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: ISTNIEJACE PRZYPADKI w 1991",
              col = rainbow(length(przypadekTotal$`1990`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(przypadekTotal$`1991`)))
        }
        
        else if( rok == '1992'){
          opisprocent <- round(100*przypadekTotal$`1992`/sum(przypadekTotal$`1992`),1)
          pie(przypadekTotal$`1992`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: ISTNIEJACE PRZYPADKI w 1992",
              col = rainbow(length(przypadekTotal$`1992`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(przypadekTotal$`1992`)))
        }
        
        else if( rok == '1993'){
          opisprocent <- round(100*przypadekTotal$`1993`/sum(przypadekTotal$`1993`),1)
          pie(przypadekTotal$`1993`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: ISTNIEJACE PRZYPADKI w 1993",
              col = rainbow(length(przypadekTotal$`1993`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(przypadekTotal$`1993`)))
          
        }
        else if( rok == '1994'){
          opisprocent <- round(100*przypadekTotal$`1994`/sum(przypadekTotal$`1994`),1)
          pie(przypadekTotal$`1994`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: ISTNIEJACE PRZYPADKI w 1994",
              col = rainbow(length(przypadekTotal$`1994`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(przypadekTotal$`1994`)))
        }
        
        else if( rok == '1995'){
          opisprocent <- round(100*przypadekTotal$`1995`/sum(przypadekTotal$`1995`),1)
          pie(przypadekTotal$`1995`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: ISTNIEJACE PRZYPADKI w 1995",
              col = rainbow(length(przypadekTotal$`1995`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(przypadekTotal$`1995`)))
        }
        
        else if( rok == '1996'){
          opisprocent <- round(100*przypadekTotal$`1996`/sum(przypadekTotal$`1996`),1)
          pie(przypadekTotal$`1996`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: ISTNIEJACE PRZYPADKI w 1996",
              col = rainbow(length(przypadekTotal$`1996`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(przypadekTotal$`1996`)))
        }
        
        else if( rok == '1997'){
          opisprocent <- round(100*przypadekTotal$`1997`/sum(przypadekTotal$`1997`),1)
          pie(przypadekTotal$`1997`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: ISTNIEJACE PRZYPADKI w 1997",
              col = rainbow(length(przypadekTotal$`1997`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(przypadekTotal$`1997`)))
        }
        
        else if( rok == '1998'){
          opisprocent <- round(100*przypadekTotal$`1998`/sum(przypadekTotal$`1998`),1)
          pie(przypadekTotal$`1998`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: ISTNIEJACE PRZYPADKI w 1998",
              col = rainbow(length(przypadekTotal$`1998`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(przypadekTotal$`1998`)))
        }
        
        else if( rok == '1999'){
          opisprocent <- round(100*przypadekTotal$`1999`/sum(przypadekTotal$`1999`),1)
          pie(przypadekTotal$`1999`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: ISTNIEJACE PRZYPADKI w 1999",
              col = rainbow(length(przypadekTotal$`1999`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(przypadekTotal$`1999`)))
        }
        
        else if( rok == '2000'){
          opisprocent <- round(100*przypadekTotal$`2000`/sum(przypadekTotal$`2000`),1)
          pie(przypadekTotal$`2000`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: ISTNIEJACE PRZYPADKI w 2000",
              col = rainbow(length(przypadekTotal$`2000`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(przypadekTotal$`2000`)))
        }
        
        else if( rok == '2001'){
          opisprocent <- round(100*przypadekTotal$`2001`/sum(przypadekTotal$`2001`),1)
          pie(przypadekTotal$`2001`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: ISTNIEJACE PRZTPADKI w 2001",
              col = rainbow(length(przypadekTotal$`2001`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(przypadekTotal$`2001`)))
        }
        
        else if( rok == '2002'){
          opisprocent <- round(100*przypadekTotal$`2002`/sum(przypadekTotal$`2002`),1)
          pie(przypadekTotal$`2002`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: ISTNIEJACE PRZYPADKI w 2002",
              col = rainbow(length(przypadekTotal$`2002`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(przypadekTotal$`2002`)))
        }
        
        else if( rok == '2003'){
          opisprocent <- round(100*przypadekTotal$`2003`/sum(przypadekTotal$`2003`),1)
          pie(przypadekTotal$`2003`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: ISTNIEJACE PRZYPADKI w 2003",
              col = rainbow(length(przypadekTotal$`2003`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(przypadekTotal$`2003`)))
        }
        
        else if( rok == '2004'){
          opisprocent <- round(100*przypadekTotal$`2004`/sum(przypadekTotal$`2004`),1)
          pie(przypadekTotal$`2004`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: ISTNIEJACE PRZYPADKI w 2004",
              col = rainbow(length(przypadekTotal$`2004`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(przypadekTotal$`2004`)))
        }
        
        else if( rok == '2005'){
          opisprocent <- round(100*przypadekTotal$`2005`/sum(przypadekTotal$`2005`),1)
          pie(przypadekTotal$`2005`, labels = paste(opisprocent,sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: ISTNIEJACE PRZYPADKI w 2005",
              col = rainbow(length(przypadekTotal$`2005`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(przypadekTotal$`2005`)))
        }
        
        else if( rok == '2006'){
          opisprocent <- round(100*przypadekTotal$`2006`/sum(przypadekTotal$`2006`),1)
          pie(przypadekTotal$`2006`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: ISTNIEJACE PRZYPADKI w 2006",
              col = rainbow(length(przypadekTotal$`2006`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(przypadekTotal$`2006`)))
        }
        
        else if( rok == '2007'){
          opisprocent <- round(100*przypadekTotal$`2007`/sum(przypadekTotal$`2007`),1)
          pie(przypadekTotal$`2007`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: ISTNIEJACE PRZYPADKI w 2007",
              col = rainbow(length(przypadekTotal$`2007`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(przypadekTotal$`2007`)))
        }
        
        else if( rok == '2008'){
          opisprocent <- round(100*przypadekTotal$`2008`/sum(przypadekTotal$`2008`),1)
          pie(przypadekTotal$`2008`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: ISTNIEJACE PRZYPADKI w 2008",
              col = rainbow(length(przypadekTotal$`2008`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(przypadekTotal$`2008`)))
        }
        
        else if( rok == '2009'){
          opisprocent <- round(100*przypadekTotal$`2009`/sum(przypadekTotal$`2009`),1)
          pie(przypadekTotal$`2009`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: ISTNIEJACE PRZYPADKI w 2009",
              col = rainbow(length(przypadekTotal$`2009`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(przypadekTotal$`2009`)))
        }
        
        else if( rok == '2010'){
          opisprocent <- round(100*przypadekTotal$`2010`/sum(przypadekTotal$`2010`),1)
          pie(przypadekTotal$`2010`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: ISTNIEJACE PRZYPADKI w 2010",
              col = rainbow(length(przypadekTotal$`2010`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(przypadekTotal$`2010`)))
        }
        
        else if( rok == '2011'){
          opisprocent <- round(100*przypadekTotal$`2011`/sum(przypadekTotal$`2011`),1)
          pie(przypadekTotal$`2011`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: ISTNIEJACE PRZYPADKI w 2011",
              col = rainbow(length(przypadekTotal$`2011`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(przypadekTotal$`2011`)))
        }
        
        else if( rok == '2012'){
          opisprocent <- round(100*przypadekTotal$`2012`/sum(przypadekTotal$`2012`),1)
          pie(przypadekTotal$`2012`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: ISTNIEJACE PRZYPADKI w 2012",
              col = rainbow(length(przypadekTotal$`2012`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(przypadekTotal$`2012`)))
        }
        
        else if( rok == '2013'){
          opisprocent <- round(100*przypadekTotal$`2013`/sum(przypadekTotal$`2013`),1)
          pie(przypadekTotal$`2013`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: ISTNIEJACE PRZYPADKI w 2013",
              col = rainbow(length(przypadekTotal$`2013`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(przypadekTotal$`2013`)))
        }
        
        else if( rok == '2014'){
          opisprocent <- round(100*przypadekTotal$`2014`/sum(przypadekTotal$`2014`),1)
          pie(przypadekTotal$`2014`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: ISTNIEJACE PRZYPADKI w 2014",
              col = rainbow(length(przypadekTotal$`2014`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(przypadekTotal$`2014`)))
        }
        
        else if( rok == '2015'){
          opisprocent <- round(100*przypadekTotal$`2015`/sum(przypadekTotal$`2015`),1)
          pie(przypadekTotal$`2015`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: ISTNIEJACE PRZYPADKI w 2015",
              col = rainbow(length(przypadekTotal$`2015`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(przypadekTotal$`2015`)))
        }
        
        else if( rok == '2016'){
          opisprocent <- round(100*przypadekTotal$`2016`/sum(przypadekTotal$`2016`),1)
          pie(przypadekTotal$`2016`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: ISTNIEJACE PRZYPADKI w 2016",
              col = rainbow(length(przypadekTotal$`2016`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(przypadekTotal$`2016`)))
        }
        
        else if( rok == '2017'){
          opisprocent <- round(100*przypadekTotal$`2017`/sum(przypadekTotal$`2017`),1)
          pie(przypadekTotal$`2017`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: ISTNIEJACE PRZYPADKI w 2017",
              col = rainbow(length(przypadekTotal$`2017`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(przypadekTotal$`2017`)))
        }
        
        else if( rok == '2018'){
          opisprocent <- round(100*przypadekTotal$`2018`/sum(przypadekTotal$`2018`),1)
          pie(przypadekTotal$`2018`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: ISTNIEJACE PRZYPADKI w 2018",
              col = rainbow(length(przypadekTotal$`2018`)))
          legend("topright", kontynentyWykres, cex = 0.8, 
                 fill = rainbow(length(przypadekTotal$`2018`)))
        }
        
        else if( rok == '2019'){
          opisprocent <- round(100*przypadekTotal$`2019`/sum(przypadekTotal$`2019`),1)
          pie(przypadekTotal$`2019`, labels = paste(opisprocent, sep = " ", "%"),
              main = "Udzial poszczegolnych kontynentow cecha: ISTNIEJACE PRZYPADKI w 2019",
              col = rainbow(length(przypadekTotal$`2019`)))
          legend("topright", kontynentyWykres, cex = 0.7, 
                 fill = rainbow(length(przypadekTotal$`2019`)))
        }
      }
      
    })
    
    output$udzial <- renderPlot({
          plotInput3()
    })
    
    uwaga <- reactive({"UWAGA! Aby poprawnie odczytac wykres, 
                               nalezy ROZSZERZYC okno"})
    
    output$uwagawyswietlanie <- renderText({
      uwaga()
    })

    

})
