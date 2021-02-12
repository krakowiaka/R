#zaladowanie odpowiednich bibliotek
library(readxl)
library(dplyr)

kontynenty <- c('Azja_i_Pacyfik', 'Karaiby', 'Wschodnia_i_Poudniowa_Afryka', 
                'Wschodnia_Europa_i_Azja_Centralna', 
                'Ameryka_Srodkowa_i_Poludniowa','Srodkowozachodnia_i_Polnocna_Afryka', 
                'Zachodnia_i_Centralna_Afryka', 
                'Zachodnia_i_Centralna Europa_oraz_Ameryka_Polnocna','SWIAT')
opis <- c('wiek 0-14', 'wiek 15-24', 'wiek 15-49', 'wiek 50+')
rok <- rep(1990:2019)

#SMIERC SPOWODOWANA AIDS/HIV

#zaladowanie danych z odpowiednich plikow
dane1s <- read_excel('/Users/Studentka/Desktop/PADR_projekt/wiek_0_14/smierc014.xlsx' )
dane2s <- read_excel('/Users/Studentka/Desktop/PADR_projekt/wiek_15_24/smierc1524.xlsx')
dane3s <- read_excel('/Users/Studentka/Desktop/PADR_projekt/wiek_25_49/smierc2549.xlsx')
dane4s <- read_excel('/Users/Studentka/Desktop/PADR_projekt/wiek_50plus/smierc50plus.xlsx')

#transpozycja danych
daneTest1s = as.data.frame(t(dane1s))
daneTest2s = as.data.frame(t(dane2s))
daneTest3s = as.data.frame(t(dane3s))
daneTest4s = as.data.frame(t(dane4s))

#nazwanie kolumn w "nowych" danych
colnames(daneTest1s) <- kontynenty
colnames(daneTest2s) <- kontynenty
colnames(daneTest3s) <- kontynenty
colnames(daneTest4s) <- kontynenty


#stworzenie ramki danych z danymi o smierci na poszczegolnych kontynetach (regionach)
#na przedziale czasu 1990-2019 z uwgldnieniem przedzialow wiekowych ludnosci

#Azja i Pacyfik
smiercAzja <- data.frame(daneTest1s$Azja_i_Pacyfik, 
                         daneTest2s$Azja_i_Pacyfik,
                         daneTest3s$Azja_i_Pacyfik, 
                         daneTest4s$Azja_i_Pacyfik)
smiercAzja <-smiercAzja[-c(1),] 
smiercAzja <- data.frame(sapply(smiercAzja, function(x) {y<-gsub("\\[.*","",x); 
                                            z<- gsub("<","",y);gsub(" ","",z)}))
colnames(smiercAzja) <- opis
smiercAzja <- smiercAzja %>%
  mutate(`wiek 0-14`= as.numeric(`wiek 0-14`)) %>%
  mutate(`wiek 15-24` = as.numeric(`wiek 15-24`)) %>%
  mutate(`wiek 15-49` = as.numeric(`wiek 15-49`)) %>%
  mutate(`wiek 50+`= as.numeric(`wiek 50+`))
rownames(smiercAzja) <- rok

#Karaiby
smiercKaraiby <- data.frame(daneTest1s$Karaiby, 
                            daneTest2s$Karaiby,
                            daneTest3s$Karaiby, 
                            daneTest4s$Karaiby)
smiercKaraiby <- smiercKaraiby[-c(1),]
smiercKaraiby <- data.frame(sapply(smiercKaraiby, function(x){y<-gsub("\\[.*","",x); 
                                          z<- gsub("<","",y);gsub(" ","",z)}))
colnames(smiercKaraiby) <- opis
smiercKaraiby <- smiercKaraiby %>%
  mutate(`wiek 0-14`= as.numeric(`wiek 0-14`)) %>%
  mutate(`wiek 15-24` = as.numeric(`wiek 15-24`)) %>%
  mutate(`wiek 15-49` = as.numeric(`wiek 15-49`)) %>%
  mutate(`wiek 50+`= as.numeric(`wiek 50+`))
rownames(smiercKaraiby) <- rok

#Wschodnia i Poudniowa Afryka
smiercWPAfryka <- data.frame(daneTest1s$Wschodnia_i_Poudniowa_Afryka, 
                             daneTest2s$Wschodnia_i_Poudniowa_Afryka,
                             daneTest3s$Wschodnia_i_Poudniowa_Afryka, 
                             daneTest4s$Wschodnia_i_Poudniowa_Afryka)
smiercWPAfryka <- smiercWPAfryka[-c(1),]
smiercWPAfryka <- data.frame(sapply(smiercWPAfryka, function(x){y<-gsub("\\[.*","",x); 
                                            z<- gsub("<","",y);gsub(" ","",z)}))
colnames(smiercWPAfryka) <- opis
smiercWPAfryka <- smiercWPAfryka %>%
  mutate(`wiek 0-14`= as.numeric(`wiek 0-14`)) %>%
  mutate(`wiek 15-24` = as.numeric(`wiek 15-24`)) %>%
  mutate(`wiek 15-49` = as.numeric(`wiek 15-49`)) %>%
  mutate(`wiek 50+`= as.numeric(`wiek 50+`))
rownames(smiercWPAfryka) <- rok

#Wschodnia Europa i Azja Centralna
smiercWEurAzja <- data.frame(daneTest1s$Wschodnia_Europa_i_Azja_Centralna, 
                             daneTest2s$Wschodnia_Europa_i_Azja_Centralna,
                             daneTest3s$Wschodnia_Europa_i_Azja_Centralna, 
                             daneTest4s$Wschodnia_Europa_i_Azja_Centralna)
smiercWEurAzja <- smiercWEurAzja[-c(1),]
smiercWEurAzja <- data.frame(sapply(smiercWEurAzja, function(x){y<-gsub("\\[.*","",x); 
                                          z<- gsub("<","",y);gsub(" ","",z)}))
colnames(smiercWEurAzja) <- opis
smiercWEurAzja$`wiek 0-14`<-sapply(smiercWEurAzja$`wiek 0-14`,function(x){y<-gsub("...",0,x)})
smiercWEurAzja <- smiercWEurAzja %>%
  mutate(`wiek 0-14`= as.numeric(`wiek 0-14`)) %>%
  mutate(`wiek 15-24` = as.numeric(`wiek 15-24`)) %>%
  mutate(`wiek 15-49` = as.numeric(`wiek 15-49`)) %>%
  mutate(`wiek 50+`= as.numeric(`wiek 50+`))
rownames(smiercWEurAzja) <- rok

#Ameryka Srodkowa i Poludniowa
smiercSPAmeryka <- data.frame(daneTest1s$Ameryka_Srodkowa_i_Poludniowa, 
                              daneTest2s$Ameryka_Srodkowa_i_Poludniowa,
                              daneTest3s$Ameryka_Srodkowa_i_Poludniowa, 
                              daneTest4s$Ameryka_Srodkowa_i_Poludniowa)
smiercSPAmeryka <- smiercSPAmeryka[-c(1),]
smiercSPAmeryka <- data.frame(sapply(smiercSPAmeryka, function(x){y<-gsub("\\[.*","",x); 
                                        z<- gsub("<","",y);gsub(" ","",z)}))
colnames(smiercSPAmeryka) <- opis
smiercSPAmeryka <- smiercSPAmeryka %>%
  mutate(`wiek 0-14`= as.numeric(`wiek 0-14`)) %>%
  mutate(`wiek 15-24` = as.numeric(`wiek 15-24`)) %>%
  mutate(`wiek 15-49` = as.numeric(`wiek 15-49`)) %>%
  mutate(`wiek 50+`= as.numeric(`wiek 50+`))
rownames(smiercSPAmeryka) <- rok

#Srodkowozachodnia i Polnocna_Afryka
smiercSWPAfryka <- data.frame(daneTest1s$Srodkowozachodnia_i_Polnocna_Afryka, 
                              daneTest1s$Srodkowozachodnia_i_Polnocna_Afryka,
                              daneTest3s$Srodkowozachodnia_i_Polnocna_Afryka, 
                              daneTest4s$Srodkowozachodnia_i_Polnocna_Afryka)
smiercSWPAfryka <- smiercSWPAfryka[-c(1),]
smiercSWPAfryka <- data.frame(sapply(smiercSWPAfryka, function(x){y<-gsub("\\[.*","",x); 
                                        z<- gsub("<","",y);gsub(" ","",z)}))
colnames(smiercSWPAfryka) <- opis
smiercSWPAfryka <- smiercSWPAfryka %>%
  mutate(`wiek 0-14`= as.numeric(`wiek 0-14`)) %>%
  mutate(`wiek 15-24` = as.numeric(`wiek 15-24`)) %>%
  mutate(`wiek 15-49` = as.numeric(`wiek 15-49`)) %>%
  mutate(`wiek 50+`= as.numeric(`wiek 50+`))
rownames(smiercSWPAfryka) <- rok

#Zachodnia i Centralna_Afryka
smiercZCAfryka <- data.frame(daneTest1s$Zachodnia_i_Centralna_Afryka, 
                             daneTest2s$Zachodnia_i_Centralna_Afryka,
                             daneTest3s$Zachodnia_i_Centralna_Afryka, 
                             daneTest4s$Zachodnia_i_Centralna_Afryka)
smiercZCAfryka <- smiercZCAfryka[-c(1),]
smiercZCAfryka <- data.frame(sapply(smiercZCAfryka, function(x){y<-gsub("\\[.*","",x); 
                                          z<- gsub("<","",y);gsub(" ","",z)}))
colnames(smiercZCAfryka) <- opis
smiercZCAfryka <- smiercZCAfryka %>%
  mutate(`wiek 0-14`= as.numeric(`wiek 0-14`)) %>%
  mutate(`wiek 15-24` = as.numeric(`wiek 15-24`)) %>%
  mutate(`wiek 15-49` = as.numeric(`wiek 15-49`)) %>%
  mutate(`wiek 50+`= as.numeric(`wiek 50+`))
rownames(smiercZCAfryka) <- rok

#Zachodnia i Centralna Europa oraz Ameryka Polnocna
smiercEurAm <- data.frame(daneTest1s$`Zachodnia_i_Centralna Europa_oraz_Ameryka_Polnocna`, 
                          daneTest2s$`Zachodnia_i_Centralna Europa_oraz_Ameryka_Polnocna`,
                          daneTest3s$`Zachodnia_i_Centralna Europa_oraz_Ameryka_Polnocna`, 
                          daneTest4s$`Zachodnia_i_Centralna Europa_oraz_Ameryka_Polnocna`)
smiercEurAm <- smiercEurAm[-c(1),]
smiercEurAm <- data.frame(sapply(smiercEurAm, function(x){y<-gsub("\\[.*","",x); 
                                        z<- gsub("<","",y);gsub(" ","",z)}))
colnames(smiercEurAm) <- opis
smiercEurAm$`wiek 0-14`<-sapply(smiercEurAm$`wiek 0-14`,function(x){y<-gsub("...",0,x)})
smiercEurAm <- smiercEurAm %>%
  mutate(`wiek 0-14`= as.numeric(`wiek 0-14`)) %>%
  mutate(`wiek 15-24` = as.numeric(`wiek 15-24`)) %>%
  mutate(`wiek 15-49` = as.numeric(`wiek 15-49`)) %>%
  mutate(`wiek 50+`= as.numeric(`wiek 50+`))
rownames(smiercEurAm) <- rok

#NOWE PRZYPADKI

#zaldowanie danych z odpowiednich plikow
dane1np <- read_excel('/Users/Studentka/Desktop/PADR_projekt/wiek_0_14/nowe014.xlsx' )
dane2np <- read_excel('/Users/Studentka/Desktop/PADR_projekt/wiek_15_24/nowe1524.xlsx')
dane3np <- read_excel('/Users/Studentka/Desktop/PADR_projekt/wiek_25_49/nowe2549.xlsx')
dane4np <- read_excel('/Users/Studentka/Desktop/PADR_projekt/wiek_50plus/nowe50plus.xlsx')

#transpozycja danych
daneTest1np = as.data.frame(t(dane1np))
daneTest2np = as.data.frame(t(dane2np))
daneTest3np = as.data.frame(t(dane3np))
daneTest4np = as.data.frame(t(dane4np))

#nazwanie kolumn w "nowych" danych
colnames(daneTest1np) <- kontynenty
colnames(daneTest2np) <- kontynenty
colnames(daneTest3np) <- kontynenty
colnames(daneTest4np) <- kontynenty


#stworzenie ramki danych o nowych przypadkach na poszczegolnych kontynetach(regionach)
#na przedziale czasu 1990-2019 z uwgldnieniem przedzialow wiekowych ludnosci

#Azja i Pacyfik
noweAzja <- data.frame(daneTest1np$Azja_i_Pacyfik, 
                       daneTest2np$Azja_i_Pacyfik,
                       daneTest3np$Azja_i_Pacyfik, 
                       daneTest4np$Azja_i_Pacyfik)
noweAzja <- noweAzja[-c(1),] 
noweAzja <- data.frame(sapply(noweAzja, function(x) {y<-gsub("\\[.*","",x); 
                                            z<- gsub("<","",y);gsub(" ","",z)}))
colnames(noweAzja) <- opis
noweAzja <- noweAzja %>%
  mutate(`wiek 0-14`= as.numeric(`wiek 0-14`)) %>%
  mutate(`wiek 15-24` = as.numeric(`wiek 15-24`)) %>%
  mutate(`wiek 15-49` = as.numeric(`wiek 15-49`)) %>%
  mutate(`wiek 50+`= as.numeric(`wiek 50+`))
rownames(noweAzja) <- rok

#Karaiby
noweKaraiby <- data.frame(daneTest1np$Karaiby, 
                          daneTest2np$Karaiby,
                          daneTest3np$Karaiby, 
                          daneTest4np$Karaiby)
noweKaraiby <- noweKaraiby[-c(1),] 
noweKaraiby <- data.frame(sapply(noweKaraiby, function(x) {y<-gsub("\\[.*","",x); 
                                            z<- gsub("<","",y);gsub(" ","",z)}))
colnames(noweKaraiby) <- opis
noweKaraiby <- noweKaraiby %>%
  mutate(`wiek 0-14`= as.numeric(`wiek 0-14`)) %>%
  mutate(`wiek 15-24` = as.numeric(`wiek 15-24`)) %>%
  mutate(`wiek 15-49` = as.numeric(`wiek 15-49`)) %>%
  mutate(`wiek 50+`= as.numeric(`wiek 50+`))
rownames(noweKaraiby) <- rok

#Wschodnia i Poudniowa Afryka
noweWPAfryka <- data.frame(daneTest1np$Wschodnia_i_Poudniowa_Afryka, 
                             daneTest2np$Wschodnia_i_Poudniowa_Afryka,
                             daneTest3np$Wschodnia_i_Poudniowa_Afryka, 
                             daneTest4np$Wschodnia_i_Poudniowa_Afryka)
noweWPAfryka <- noweWPAfryka[-c(1),]
noweWPAfryka <- data.frame(sapply(noweWPAfryka, function(x){y<-gsub("\\[.*","",x); 
                                            z<- gsub("<","",y);gsub(" ","",z)}))
colnames(noweWPAfryka) <- opis
noweWPAfryka <- noweWPAfryka %>%
  mutate(`wiek 0-14`= as.numeric(`wiek 0-14`)) %>%
  mutate(`wiek 15-24` = as.numeric(`wiek 15-24`)) %>%
  mutate(`wiek 15-49` = as.numeric(`wiek 15-49`)) %>%
  mutate(`wiek 50+`= as.numeric(`wiek 50+`))
rownames(noweWPAfryka) <- rok

#Wschodnia Europa i Azja Centralna
noweWEurAzja <- data.frame(daneTest1np$Wschodnia_Europa_i_Azja_Centralna, 
                             daneTest2np$Wschodnia_Europa_i_Azja_Centralna,
                             daneTest3np$Wschodnia_Europa_i_Azja_Centralna, 
                             daneTest4np$Wschodnia_Europa_i_Azja_Centralna)
noweWEurAzja <- noweWEurAzja[-c(1),]
noweWEurAzja <- data.frame(sapply(noweWEurAzja, function(x){y<-gsub("\\[.*","",x); 
                                            z<- gsub("<","",y);gsub(" ","",z)}))
colnames(noweWEurAzja) <- opis
noweWEurAzja$`wiek 0-14`<-sapply(noweWEurAzja$`wiek 0-14`,function(x){y<-gsub("...",0,x)})
noweWEurAzja <- noweWEurAzja %>%
  mutate(`wiek 0-14`= as.numeric(`wiek 0-14`)) %>%
  mutate(`wiek 15-24` = as.numeric(`wiek 15-24`)) %>%
  mutate(`wiek 15-49` = as.numeric(`wiek 15-49`)) %>%
  mutate(`wiek 50+`= as.numeric(`wiek 50+`))
rownames(noweWEurAzja) <- rok

#Ameryka Srodkowa i Poludniowa
noweSPAmeryka <- data.frame(daneTest1np$Ameryka_Srodkowa_i_Poludniowa, 
                              daneTest2np$Ameryka_Srodkowa_i_Poludniowa,
                              daneTest3np$Ameryka_Srodkowa_i_Poludniowa, 
                              daneTest4np$Ameryka_Srodkowa_i_Poludniowa)
noweSPAmeryka <- noweSPAmeryka[-c(1),]
noweSPAmeryka <- data.frame(sapply(noweSPAmeryka, function(x){y<-gsub("\\[.*","",x); 
                                            z<- gsub("<","",y);gsub(" ","",z)}))
colnames(noweSPAmeryka) <- opis
noweSPAmeryka <- noweSPAmeryka %>%
  mutate(`wiek 0-14`= as.numeric(`wiek 0-14`)) %>%
  mutate(`wiek 15-24` = as.numeric(`wiek 15-24`)) %>%
  mutate(`wiek 15-49` = as.numeric(`wiek 15-49`)) %>%
  mutate(`wiek 50+`= as.numeric(`wiek 50+`))
rownames(noweSPAmeryka) <- rok

#Srodkowozachodnia i Polnocna_Afryka
noweSWPAfryka <- data.frame(daneTest1np$Srodkowozachodnia_i_Polnocna_Afryka, 
                              daneTest2np$Srodkowozachodnia_i_Polnocna_Afryka,
                              daneTest3np$Srodkowozachodnia_i_Polnocna_Afryka, 
                              daneTest4np$Srodkowozachodnia_i_Polnocna_Afryka)
noweSWPAfryka <- noweSWPAfryka[-c(1),]
noweSWPAfryka <- data.frame(sapply(noweSWPAfryka, function(x){y<-gsub("\\[.*","",x); 
                                          z<- gsub("<","",y);gsub(" ","",z)}))
colnames(noweSWPAfryka) <- opis
noweSWPAfryka <- noweSWPAfryka %>%
  mutate(`wiek 0-14`= as.numeric(`wiek 0-14`)) %>%
  mutate(`wiek 15-24` = as.numeric(`wiek 15-24`)) %>%
  mutate(`wiek 15-49` = as.numeric(`wiek 15-49`)) %>%
  mutate(`wiek 50+`= as.numeric(`wiek 50+`))
rownames(noweSWPAfryka) <- rok

#Zachodnia i Centralna_Afryka
noweZCAfryka <- data.frame(daneTest1np$Zachodnia_i_Centralna_Afryka, 
                             daneTest2np$Zachodnia_i_Centralna_Afryka,
                             daneTest3np$Zachodnia_i_Centralna_Afryka, 
                             daneTest4np$Zachodnia_i_Centralna_Afryka)
noweZCAfryka <- noweZCAfryka[-c(1),]
noweZCAfryka <- data.frame(sapply(noweZCAfryka, function(x){y<-gsub("\\[.*","",x); 
                                          z<- gsub("<","",y);gsub(" ","",z)}))
colnames(noweZCAfryka) <- opis
noweZCAfryka <- noweZCAfryka %>%
  mutate(`wiek 0-14`= as.numeric(`wiek 0-14`)) %>%
  mutate(`wiek 15-24` = as.numeric(`wiek 15-24`)) %>%
  mutate(`wiek 15-49` = as.numeric(`wiek 15-49`)) %>%
  mutate(`wiek 50+`= as.numeric(`wiek 50+`))
rownames(noweZCAfryka) <- rok

#Zachodnia i Centralna Europa oraz Ameryka Polnocna
noweEurAm <- data.frame(daneTest1np$`Zachodnia_i_Centralna Europa_oraz_Ameryka_Polnocna`, 
                          daneTest2np$`Zachodnia_i_Centralna Europa_oraz_Ameryka_Polnocna`,
                          daneTest3np$`Zachodnia_i_Centralna Europa_oraz_Ameryka_Polnocna`, 
                          daneTest4np$`Zachodnia_i_Centralna Europa_oraz_Ameryka_Polnocna`)
noweEurAm <- noweEurAm[-c(1),]
noweEurAm <- data.frame(sapply(noweEurAm, function(x){y<-gsub("\\[.*","",x); 
                                            z<- gsub("<","",y);gsub(" ","",z)}))
colnames(noweEurAm) <- opis
noweEurAm$`wiek 0-14`<-sapply(noweEurAm$`wiek 0-14`,function(x){y<-gsub("...",0,x)})
noweEurAm <- noweEurAm %>%
  mutate(`wiek 0-14`= as.numeric(`wiek 0-14`)) %>%
  mutate(`wiek 15-24` = as.numeric(`wiek 15-24`)) %>%
  mutate(`wiek 15-49` = as.numeric(`wiek 15-49`)) %>%
  mutate(`wiek 50+`= as.numeric(`wiek 50+`))
rownames(noweEurAm) <- rok

#PRZYPADKI

#zaldowanie danych z odpowiednich plikow
dane1p <- read_excel('/Users/Studentka/Desktop/PADR_projekt/wiek_0_14/przypadek014.xlsx' )
dane2p <- read_excel('/Users/Studentka/Desktop/PADR_projekt/wiek_15_24/przypadek1524.xlsx')
dane3p <- read_excel('/Users/Studentka/Desktop/PADR_projekt/wiek_25_49/przypadek2549.xlsx')
dane4p <- read_excel('/Users/Studentka/Desktop/PADR_projekt/wiek_50plus/przypadek50plus.xlsx')

#transpozycja danych
daneTest1p = as.data.frame(t(dane1p))
daneTest2p = as.data.frame(t(dane2p))
daneTest3p = as.data.frame(t(dane3p))
daneTest4p = as.data.frame(t(dane4p))

#nazwanie kolumn w "nowych" danych
colnames(daneTest1p) <- kontynenty
colnames(daneTest2p) <- kontynenty
colnames(daneTest3p) <- kontynenty
colnames(daneTest4p) <- kontynenty


#stworzenie ramki danych o przypadkach na poszczegolnych kontynetach(regionach)
#na przedziale czasu 1990-2019 z uwgldnieniem przedzialow wiekowych ludnosci

#Azja i Pacyfik
przypadekAzja <- data.frame(daneTest1p$Azja_i_Pacyfik, 
                            daneTest2p$Azja_i_Pacyfik,
                            daneTest3p$Azja_i_Pacyfik, 
                            daneTest4p$Azja_i_Pacyfik)
przypadekAzja <- przypadekAzja[-c(1),] 
przypadekAzja <- data.frame(sapply(przypadekAzja, function(x) {y<-gsub("\\[.*","",x); 
                                                  z<- gsub("<","",y);gsub(" ","",z)}))
colnames(przypadekAzja) <- opis
przypadekAzja <- przypadekAzja %>%
  mutate(`wiek 0-14`= as.numeric(`wiek 0-14`)) %>%
  mutate(`wiek 15-24` = as.numeric(`wiek 15-24`)) %>%
  mutate(`wiek 15-49` = as.numeric(`wiek 15-49`)) %>%
  mutate(`wiek 50+`= as.numeric(`wiek 50+`))
rownames(przypadekAzja) <- rok

#Karaiby
przypadekKaraiby <- data.frame(daneTest1p$Karaiby, 
                               daneTest2p$Karaiby,
                               daneTest3p$Karaiby, 
                               daneTest4p$Karaiby)
przypadekKaraiby <- przypadekKaraiby[-c(1),] 
przypadekKaraiby <- data.frame(sapply(przypadekKaraiby, function(x) {y<-gsub("\\[.*","",x); 
                                              z<- gsub("<","",y);gsub(" ","",z)}))
colnames(przypadekKaraiby) <- opis
przypadekKaraiby <- przypadekKaraiby %>%
  mutate(`wiek 0-14`= as.numeric(`wiek 0-14`)) %>%
  mutate(`wiek 15-24` = as.numeric(`wiek 15-24`)) %>%
  mutate(`wiek 15-49` = as.numeric(`wiek 15-49`)) %>%
  mutate(`wiek 50+`= as.numeric(`wiek 50+`))
rownames(przypadekKaraiby) <- rok

#Wschodnia i Poudniowa Afryka
przypadekWPAfryka <- data.frame(daneTest1p$Wschodnia_i_Poudniowa_Afryka, 
                           daneTest2p$Wschodnia_i_Poudniowa_Afryka,
                           daneTest3p$Wschodnia_i_Poudniowa_Afryka, 
                           daneTest4p$Wschodnia_i_Poudniowa_Afryka)
przypadekWPAfryka <- przypadekWPAfryka[-c(1),]
przypadekWPAfryka <- data.frame(sapply(przypadekWPAfryka, function(x){y<-gsub("\\[.*","",x); 
                                          z<- gsub("<","",y);gsub(" ","",z)}))
colnames(przypadekWPAfryka) <- opis
przypadekWPAfryka <- przypadekWPAfryka %>%
  mutate(`wiek 0-14`= as.numeric(`wiek 0-14`)) %>%
  mutate(`wiek 15-24` = as.numeric(`wiek 15-24`)) %>%
  mutate(`wiek 15-49` = as.numeric(`wiek 15-49`)) %>%
  mutate(`wiek 50+`= as.numeric(`wiek 50+`))
rownames(przypadekWPAfryka) <- rok

#Wschodnia Europa i Azja Centralna
przypadekWEurAzja <- data.frame(daneTest1p$Wschodnia_Europa_i_Azja_Centralna, 
                           daneTest2p$Wschodnia_Europa_i_Azja_Centralna,
                           daneTest3p$Wschodnia_Europa_i_Azja_Centralna, 
                           daneTest4p$Wschodnia_Europa_i_Azja_Centralna)
przypadekWEurAzja <- przypadekWEurAzja[-c(1),]
przypadekWEurAzja <- data.frame(sapply(przypadekWEurAzja, function(x){y<-gsub("\\[.*","",x); 
                                          z<- gsub("<","",y);gsub(" ","",z)}))
colnames(przypadekWEurAzja) <- opis
przypadekWEurAzja$`wiek 0-14`<-sapply(przypadekWEurAzja$`wiek 0-14`,function(x){y<-gsub("...",0,x)})
przypadekWEurAzja <- przypadekWEurAzja %>%
  mutate(`wiek 0-14`= as.numeric(`wiek 0-14`)) %>%
  mutate(`wiek 15-24` = as.numeric(`wiek 15-24`)) %>%
  mutate(`wiek 15-49` = as.numeric(`wiek 15-49`)) %>%
  mutate(`wiek 50+`= as.numeric(`wiek 50+`))
rownames(przypadekWEurAzja) <- rok

#Ameryka Srodkowa i Poludniowa
przypadekSPAmeryka <- data.frame(daneTest1p$Ameryka_Srodkowa_i_Poludniowa, 
                            daneTest2p$Ameryka_Srodkowa_i_Poludniowa,
                            daneTest3p$Ameryka_Srodkowa_i_Poludniowa, 
                            daneTest4p$Ameryka_Srodkowa_i_Poludniowa)
przypadekSPAmeryka <- przypadekSPAmeryka[-c(1),]
przypadekSPAmeryka <- data.frame(sapply(przypadekSPAmeryka, function(x){y<-gsub("\\[.*","",x); 
                                            z<- gsub("<","",y);gsub(" ","",z)}))
colnames(przypadekSPAmeryka) <- opis
przypadekSPAmeryka <- przypadekSPAmeryka %>%
  mutate(`wiek 0-14`= as.numeric(`wiek 0-14`)) %>%
  mutate(`wiek 15-24` = as.numeric(`wiek 15-24`)) %>%
  mutate(`wiek 15-49` = as.numeric(`wiek 15-49`)) %>%
  mutate(`wiek 50+`= as.numeric(`wiek 50+`))
rownames(przypadekSPAmeryka) <- rok

#Srodkowozachodnia i Polnocna_Afryka
przypadekSWPAfryka <- data.frame(daneTest1p$Srodkowozachodnia_i_Polnocna_Afryka, 
                            daneTest2p$Srodkowozachodnia_i_Polnocna_Afryka,
                            daneTest3p$Srodkowozachodnia_i_Polnocna_Afryka, 
                            daneTest4p$Srodkowozachodnia_i_Polnocna_Afryka)
przypadekSWPAfryka <- przypadekSWPAfryka[-c(1),]
przypadekSWPAfryka <- data.frame(sapply(przypadekWPAfryka, function(x){y<-gsub("\\[.*","",x); 
                                              z<- gsub("<","",y);gsub(" ","",z)}))
colnames(przypadekSWPAfryka) <- opis
przypadekSWPAfryka <- przypadekSWPAfryka %>%
  mutate(`wiek 0-14`= as.numeric(`wiek 0-14`)) %>%
  mutate(`wiek 15-24` = as.numeric(`wiek 15-24`)) %>%
  mutate(`wiek 15-49` = as.numeric(`wiek 15-49`)) %>%
  mutate(`wiek 50+`= as.numeric(`wiek 50+`))
rownames(przypadekSWPAfryka) <- rok

#Zachodnia i Centralna_Afryka
przypadekZCAfryka <- data.frame(daneTest1p$Zachodnia_i_Centralna_Afryka, 
                           daneTest2p$Zachodnia_i_Centralna_Afryka,
                           daneTest3p$Zachodnia_i_Centralna_Afryka, 
                           daneTest4p$Zachodnia_i_Centralna_Afryka)
przypadekZCAfryka <- przypadekZCAfryka[-c(1),]
przypadekZCAfryka <- data.frame(sapply(przypadekZCAfryka, function(x){y<-gsub("\\[.*","",x); 
                                            z<- gsub("<","",y);gsub(" ","",z)}))
colnames(przypadekZCAfryka) <- opis
przypadekZCAfryka <- przypadekZCAfryka %>%
  mutate(`wiek 0-14`= as.numeric(`wiek 0-14`)) %>%
  mutate(`wiek 15-24` = as.numeric(`wiek 15-24`)) %>%
  mutate(`wiek 15-49` = as.numeric(`wiek 15-49`)) %>%
  mutate(`wiek 50+`= as.numeric(`wiek 50+`))
rownames(przypadekZCAfryka) <- rok

#Zachodnia i Centralna Europa oraz Ameryka Polnocna
przypadekEurAm <- data.frame(daneTest1p$`Zachodnia_i_Centralna Europa_oraz_Ameryka_Polnocna`, 
                        daneTest2p$`Zachodnia_i_Centralna Europa_oraz_Ameryka_Polnocna`,
                        daneTest3p$`Zachodnia_i_Centralna Europa_oraz_Ameryka_Polnocna`, 
                        daneTest4p$`Zachodnia_i_Centralna Europa_oraz_Ameryka_Polnocna`)
przypadekEurAm <- przypadekEurAm[-c(1),]
przypadekEurAm <- data.frame(sapply(przypadekEurAm, function(x){y<-gsub("\\[.*","",x); 
                                            z<- gsub("<","",y);gsub(" ","",z)}))
colnames(przypadekEurAm) <- opis
przypadekEurAm$`wiek 0-14`<-sapply(przypadekEurAm$`wiek 0-14`,function(x){y<-gsub("...",0,x)})
przypadekEurAm <- przypadekEurAm %>%
  mutate(`wiek 0-14`= as.numeric(`wiek 0-14`)) %>%
  mutate(`wiek 15-24` = as.numeric(`wiek 15-24`)) %>%
  mutate(`wiek 15-49` = as.numeric(`wiek 15-49`)) %>%
  mutate(`wiek 50+`= as.numeric(`wiek 50+`))
przypadekEurAm$`wiek 0-14`[is.na(przypadekAzja$`wiek 0-14`)] <-0
rownames(przypadekEurAm) <- rok

#TOTAL

daneTs <- read_excel('/Users/Studentka/Desktop/PADR_projekt/wszyscy/smierc.xlsx')
daneTp <- read_excel('/Users/Studentka/Desktop/PADR_projekt/wszyscy/przypadek.xlsx')
daneTn <- read_excel('/Users/Studentka/Desktop/PADR_projekt/wszyscy/nowe.xlsx')

daneTestTs = as.data.frame((daneTs))
daneTestTp = as.data.frame((daneTp))
daneTestTn = as.data.frame((daneTn))

region <- c('regiony')
opis2 <- c(region,rok)

#SMIERC TOTAL

smiercTotal <- data.frame(sapply(daneTestTs, function(x) {y<-gsub("\\[.*","",x); 
                                        z<- gsub("<","",y);gsub(" ","",z)}))
smiercTotal <- smiercTotal[-c(9),]

smiercTotal <- smiercTotal %>%
  mutate( X1990 = as.numeric(X1990))%>%
  mutate( X1991 = as.numeric(X1991))%>%
  mutate( X1992 = as.numeric(X1992))%>%
  mutate( X1993 = as.numeric(X1993))%>%
  mutate( X1994 = as.numeric(X1994))%>%
  mutate( X1995 = as.numeric(X1995))%>%
  mutate( X1996 = as.numeric(X1996))%>%
  mutate( X1997 = as.numeric(X1997))%>%
  mutate( X1998 = as.numeric(X1998))%>%
  mutate( X1999 = as.numeric(X1999))%>%
  mutate( X2000 = as.numeric(X2000))%>%
  mutate( X2001 = as.numeric(X2001))%>%
  mutate( X2002 = as.numeric(X2002))%>%
  mutate( X2003 = as.numeric(X2003))%>%
  mutate( X2004 = as.numeric(X2004))%>%
  mutate( X2005 = as.numeric(X2005))%>%
  mutate( X2006 = as.numeric(X2006))%>%
  mutate( X2007 = as.numeric(X2007))%>%
  mutate( X2008 = as.numeric(X2008))%>%
  mutate( X2009 = as.numeric(X2009))%>%
  mutate( X2010 = as.numeric(X2010))%>%
  mutate( X2011 = as.numeric(X2011))%>%
  mutate( X2012 = as.numeric(X2012))%>%
  mutate( X2013 = as.numeric(X2013))%>%
  mutate( X2014 = as.numeric(X2014))%>%
  mutate( X2015 = as.numeric(X2015))%>%
  mutate( X2016 = as.numeric(X2016))%>%
  mutate( X2017 = as.numeric(X2017))%>%
  mutate( X2018 = as.numeric(X2018))%>%
  mutate( X2019 = as.numeric(X2019))

colnames(smiercTotal) <- opis2

#NOWE PRZYPADKI TOTAL

noweTotal <- data.frame(sapply(daneTestTn, function(x) {y<-gsub("\\[.*","",x); 
                                        z<- gsub("<","",y);gsub(" ","",z)}))
noweTotal <- noweTotal[-c(9),]

noweTotal <- noweTotal %>%
  mutate( X1990 = as.numeric(X1990))%>%
  mutate( X1991 = as.numeric(X1991))%>%
  mutate( X1992 = as.numeric(X1992))%>%
  mutate( X1993 = as.numeric(X1993))%>%
  mutate( X1994 = as.numeric(X1994))%>%
  mutate( X1995 = as.numeric(X1995))%>%
  mutate( X1996 = as.numeric(X1996))%>%
  mutate( X1997 = as.numeric(X1997))%>%
  mutate( X1998 = as.numeric(X1998))%>%
  mutate( X1999 = as.numeric(X1999))%>%
  mutate( X2000 = as.numeric(X2000))%>%
  mutate( X2001 = as.numeric(X2001))%>%
  mutate( X2002 = as.numeric(X2002))%>%
  mutate( X2003 = as.numeric(X2003))%>%
  mutate( X2004 = as.numeric(X2004))%>%
  mutate( X2005 = as.numeric(X2005))%>%
  mutate( X2006 = as.numeric(X2006))%>%
  mutate( X2007 = as.numeric(X2007))%>%
  mutate( X2008 = as.numeric(X2008))%>%
  mutate( X2009 = as.numeric(X2009))%>%
  mutate( X2010 = as.numeric(X2010))%>%
  mutate( X2011 = as.numeric(X2011))%>%
  mutate( X2012 = as.numeric(X2012))%>%
  mutate( X2013 = as.numeric(X2013))%>%
  mutate( X2014 = as.numeric(X2014))%>%
  mutate( X2015 = as.numeric(X2015))%>%
  mutate( X2016 = as.numeric(X2016))%>%
  mutate( X2017 = as.numeric(X2017))%>%
  mutate( X2018 = as.numeric(X2018))%>%
  mutate( X2019 = as.numeric(X2019))

colnames(noweTotal) <- opis2

#ISTNIEJACE PRZYPADKI TOTAL

przypadekTotal <- data.frame(sapply(daneTestTp, function(x) {y<-gsub("\\[.*","",x); 
                                        z<- gsub("<","",y);gsub(" ","",z)}))
przypadekTotal <- przypadekTotal[-c(9),]

przypadekTotal <- przypadekTotal %>%
  mutate( X1990 = as.numeric(X1990))%>%
  mutate( X1991 = as.numeric(X1991))%>%
  mutate( X1992 = as.numeric(X1992))%>%
  mutate( X1993 = as.numeric(X1993))%>%
  mutate( X1994 = as.numeric(X1994))%>%
  mutate( X1995 = as.numeric(X1995))%>%
  mutate( X1996 = as.numeric(X1996))%>%
  mutate( X1997 = as.numeric(X1997))%>%
  mutate( X1998 = as.numeric(X1998))%>%
  mutate( X1999 = as.numeric(X1999))%>%
  mutate( X2000 = as.numeric(X2000))%>%
  mutate( X2001 = as.numeric(X2001))%>%
  mutate( X2002 = as.numeric(X2002))%>%
  mutate( X2003 = as.numeric(X2003))%>%
  mutate( X2004 = as.numeric(X2004))%>%
  mutate( X2005 = as.numeric(X2005))%>%
  mutate( X2006 = as.numeric(X2006))%>%
  mutate( X2007 = as.numeric(X2007))%>%
  mutate( X2008 = as.numeric(X2008))%>%
  mutate( X2009 = as.numeric(X2009))%>%
  mutate( X2010 = as.numeric(X2010))%>%
  mutate( X2011 = as.numeric(X2011))%>%
  mutate( X2012 = as.numeric(X2012))%>%
  mutate( X2013 = as.numeric(X2013))%>%
  mutate( X2014 = as.numeric(X2014))%>%
  mutate( X2015 = as.numeric(X2015))%>%
  mutate( X2016 = as.numeric(X2016))%>%
  mutate( X2017 = as.numeric(X2017))%>%
  mutate( X2018 = as.numeric(X2018))%>%
  mutate( X2019 = as.numeric(X2019))

colnames(przypadekTotal) <- opis2


kontynentyWykres <- c('Azja i Pacyfik', 'Karaiby', 'Wschodnia i Poudniowa Afryka', 
                'Wschodnia Europa i Azja Centralna', 
                'Ameryka Srodkowa i Poludniowa','Srodkowozachodnia i Polnocna Afryka', 
                'Zachodnia i Centralna Afryka', 
                'Zachodnia i Centralna Europa oraz Ameryka Polnocna')


#DANE POMOCNE PRZY TWORZENIU APLIKACJI

#stworzenie ramki danych z opcjami do wyboru przez u¿ytkownika
region <- c(rep("Azja", 12),rep("Karaiby",12), rep("Wschodnia i Poludniowa Afryka",12),
            rep("Wschodnia Europa i Centralna Azja",12), rep("Srodkowa i Poludniowa Ameryka",12),
            rep("Srodkowozachodnia i Polnocna Afryka",12), rep("Zachodnia i Centralna Afryka",12),
            rep("Zachodnia i Centralna Europa oraz Ameryka Polnocna",12))

cecha <- c(rep("smierc",4), rep("nowe",4), rep("przypadek",4))
wiek <- c('wiek 0-14', 'wiek 15-24', 'wiek 15-49', 'wiek 50+')
dana <- data.frame(region, cecha=c(rep(cecha,8)), wiek=c(rep(wiek,8)))

cecha <- data.frame(cecha = c("nowe", "smierc", "przypadek"))

save(file = "Dane.RData", smiercAzja,smiercEurAm,smiercKaraiby,smiercSPAmeryka, 
     smiercSWPAfryka, smiercWEurAzja, smiercWPAfryka, smiercZCAfryka, smiercTotal, 
     noweAzja, noweEurAm, noweKaraiby, noweSPAmeryka, noweSWPAfryka,
     noweWEurAzja, noweWPAfryka, noweZCAfryka,
     przypadekAzja, przypadekEurAm, przypadekKaraiby, przypadekSPAmeryka, 
     przypadekSWPAfryka, przypadekWEurAzja, przypadekWPAfryka, przypadekZCAfryka, 
     dana, noweTotal, smiercTotal, przypadekTotal, rok, cecha, kontynentyWykres)

