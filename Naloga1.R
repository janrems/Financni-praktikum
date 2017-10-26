

#naloga 1

tabela_08 <- read_csv2("podatki/hist_EURIBOR_2008.csv") %>% t() %>% data.frame()

tabela_09 <- read_csv2("podatki/hist_EURIBOR_2009.csv") %>% t() %>% data.frame()

tabela_10 <- read_csv("podatki/hist_EURIBOR_2010.csv") %>% t() %>% data.frame()

tabela_08 <- tabela_08[,(1:15)]
tabela_09 <- tabela_09[,(1:15)]
tabela_10 <- tabela_10[,(1:15)]

colnames(tabela_08) <- as.character(unname(unlist(tabela_08[1,])))
colnames(tabela_09) <- as.character(unname(unlist(tabela_09[1,])))
colnames(tabela_10) <- as.character(unname(unlist(tabela_10[1,])))

tabela_08 <- tabela_08[-1,]
tabela_09 <- tabela_09[-1,]
tabela_10 <- tabela_10[-1,]

prvi_dnevi_08 <- c(rownames(tabela_08)[1])
mesec <- "01"
for (dan in rownames(tabela_08)){
  vzorec1 <- paste(".*/", mesec,"/.*", sep = "")
  vzorec2 <- ".*/(.*)/.*"
  if (grepl(vzorec1,dan) == FALSE){
    mesec <- sub(vzorec2, "\\1", dan,perl=TRUE)
    prvi_dnevi_08 <- c(prvi_dnevi_08, dan)
  }
}
tabela_08 <- tabela_08[prvi_dnevi_08,]

prvi_dnevi_09<- c(rownames(tabela_09)[1])
mesec <- "01"
for (dan in rownames(tabela_09)){
  vzorec1 <- paste(".*/", mesec,"/.*", sep = "")
  vzorec2 <- ".*/(.*)/.*"
  if (grepl(vzorec1,dan) == FALSE){
    mesec <- sub(vzorec2, "\\1", dan,perl=TRUE)
    prvi_dnevi_09 <- c(prvi_dnevi_09, dan)
  }
}
tabela_09 <- tabela_09[prvi_dnevi_09,]


prvi_dnevi_10 <- c(rownames(tabela_10)[1])
mesec <- "01"
for (dan in rownames(tabela_10)){
  vzorec1 <- paste(".*/", mesec,"/.*", sep = "")
  vzorec2 <- ".*/(.*)/.*"
  if (grepl(vzorec1,dan) == FALSE){
    mesec <- sub(vzorec2, "\\1", dan,perl=TRUE)
    prvi_dnevi_10 <- c(prvi_dnevi_10, dan)
  }
}
tabela_10 <- tabela_10[prvi_dnevi_10,]
tabela_10 <- tabela_10[-13,]

tabela_zdruzena <- rbind(tabela_08,tabela_09,tabela_10)

tabela_zdruzena[] <- lapply(tabela_zdruzena, as.character)

for(i in 1:15){
  tabela_zdruzena[17,i] <- gsub(",",".",tabela_zdruzena[17,i])
}


mesecna6 <- as.numeric(tabela_zdruzena$`6m`) %>% ts(start = c(2008,1), end = c(2010,12), frequency = 12)

mesecna12 <- as.numeric(tabela_zdruzena$`12m`) %>% ts(start = c(2008,1), end = c(2010,12), frequency = 12)

  
  
graf_1 <- ts.plot(mesecna6, mesecna12,
        gpars=list(xlab="Leto", ylab="Obrestna mera", col = c("blue", "red"), lwd = 3))

legenda_1 <- legend('topright', c("6 mesecna obrestna mera", "12 mesecna obrestna mera"), lty = 1, col = c("blue", "red"), lwd = 3)

#Naloga 2

zanimivi_datumi <- c("01/02/2008","03/03/2008","01/03/2010")

casovni_razmik <- c(0.25, 0.5, 0.75, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
obrestne_1 <- as.numeric(unname(unlist(tabela_zdruzena[2,])))
obrestne_2 <-as.numeric(unname(unlist(tabela_zdruzena[3,])))
obrestne_3 <- as.numeric(unname(unlist(tabela_zdruzena[27,])))

tabela_datumi <- data.frame(casovni_razmik,obrestne_1,obrestne_2, obrestne_3)
tabela_datumi1 <- melt(tabela_zdruzena)

graf_1.2.08 <- plot(casovni_razmik,obrestne_1, type = "l", main = "Krivulja obrestnih mer, dne 01.02.2008", xlab = "Čas dospelosti v mesecih", ylab = "Obrestna mera", col = "red", lwd = 3)
lines(casovni_razmik,obrestne_2,col="green")
lines(casovni_razmik,obrestne_3, col"blue")

graf_3.3.08 <- plot(casovni_razmik,obrestne_2, type = "l", main = "Krivulja obrestnih mer, dne 03.03.2008", xlab = "Čas dospelosti v mesecih", ylab = "Obrestna mera", col = "red", lwd = 3)

graf_1.3.10 <- plot(casovni_razmik,obrestne_3, type = "l", main = "Krivulja obrestnih mer, dne 03.03.2008", xlab = "Čas dospelosti v mesecih", ylab = "Obrestna mera", col = "red", lwd = 3)













