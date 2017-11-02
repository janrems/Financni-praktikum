

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
        legend('topright', c("6 mesecna obrestna mera", "12 mesecna obrestna mera"), lty = 1, col = c("blue", "red"), lwd = 3)

#Naloga 2

zanimivi_datumi <- c("01/02/2008","03/03/2008","01/03/2010")

casovni_razmik <- c(0.25, 0.5, 0.75, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
obrestne_1 <- as.numeric(unname(unlist(tabela_zdruzena[2,])))
obrestne_2 <-as.numeric(unname(unlist(tabela_zdruzena[3,])))
obrestne_3 <- as.numeric(unname(unlist(tabela_zdruzena[27,])))
obrestne_4 <- as.numeric(unname(unlist(tabela_zdruzena[7,])))

tabela_datumi <- data.frame(casovni_razmik,obrestne_1,obrestne_2, obrestne_3)
tabela_datumi1 <- melt(tabela_zdruzena)

graf_1.2.08 <- plot(x = casovni_razmik, y = obrestne_1, type = "o", 
                    ylim = c(min(4),max(5.5)), 
                    main = "Časovna stuktura obrestnih mer", xlab = "Čas dospelosti v mesecih", ylab = "Obrestna mera", col = "red",
                    lwd = 3)
lines(x = casovni_razmik,y = obrestne_2, type = "o", col = "blue", pch = 16, lwd = 3, text(10,4.45,"3.3.2008", col="blue"))
lines(x = casovni_razmik,y = obrestne_4, col = "green", type = "o", pch = 14, lwd = 3, text(10,5,"1.7.2008", col="green"))
lines(x = casovni_razmik, y = obrestne_1, type = "o", col = "red", lwd = 3, text(10,4.2,"1.2.2008", col="red"))



#3. naloga

#a

sest <- as.numeric(tabela_zdruzena$`6m`)
leto <- as.numeric(tabela_zdruzena$`12m`)


terminska_6x12 <- 2*((rep(1,36) + 1*leto)/(rep(1,36) + 1/2*sest)-rep(1,36))

tabela_zdruzena <- cbind(tabela_zdruzena,terminska_6x12)

#b

terminska_primerjava <- terminska_6x12[1:30]

tabela_primerjava <- tabela_zdruzena[7:36,9:10]


tabela_primerjava <- cbind(tabela_primerjava,terminska_primerjava)

tabela_primerjava <- tabela_primerjava[,-2]
colnames(tabela_primerjava) <- c("Euribor6m", "Napoved6m")

#c

prim8 <- tabela_primerjava[1:6,]

prim9 <- tabela_primerjava[7:18,]

prim10 <- tabela_primerjava[19:30,] 


graf_napoved_skupni <- plot(x = as.numeric(prim8$Napoved6m), y = as.numeric(prim8$Euribor6m), 
                            ylim = c(min(1),max(5.6)),xlim = c(min(1),max(5.6)), 
                            main = "Primerjava napovedane, z dejansko obrestno mero", xlab = "Napovedana" , ylab = "Dejanska obrestna mera")
points(x = as.numeric(prim8$Napoved6m), y = as.numeric(prim8$Euribor6m), type = "p", col = "red", pch = 16)
points(x = as.numeric(prim9$Napoved6m), y = as.numeric(prim9$Euribor6m), type = "p", col = "blue", pch = 16)
points(x = as.numeric(prim10$Napoved6m), y = as.numeric(prim10$Euribor6m), type = "p", col = "green", pch = 16)
abline(0,1, col = "grey",lwd = 2) 
abline(lm(as.numeric(tabela_primerjava$Euribor6m)~ as.numeric(tabela_primerjava$Napoved6m)),lwd = 2, col = "black")
legend('bottomright', c("2008", "2009", "2010"), pch = 16, col = c("red", "blue", "green"))


graf_napoved_08 <- plot(x = as.numeric(prim8$Napoved6m), 
                        y = as.numeric(prim8$Euribor6m), 
                        ylim = c(min(1.3),max(5.6)),
                        xlim = c(min(1.3),max(5.6)), 
                        type = "p", 
                        pch = 16, 
                        col = "red", 
                        main = "Primerjava napovedane, z dejansko obrestno mero v letu 2008", 
                        xlab = "Napovedana" , 
                        ylab = "Dejanska obrestna mera")
abline(0,1, col = "grey",lwd = 2) 
abline(lm(as.numeric(prim8$Euribor6m)~ as.numeric(prim8$Napoved6m)),lwd = 2, col = "black")


graf_napoved_09 <- plot(x = as.numeric(prim9$Napoved6m), 
                        y = as.numeric(prim9$Euribor6m),
                        ylim = c(min(1),max(3)),
                        xlim = c(min(1),max(3)),
                        type = "p", 
                        pch = 16, 
                        col = "blue", 
                        main = "Primerjava napovedane, z dejansko obrestno mero v letu 2009", 
                        xlab = "Napovedana" , 
                        ylab = "Dejanska obrestna mera")
abline(0,1, col = "grey",lwd = 2) 
abline(lm(as.numeric(prim9$Euribor6m)~ as.numeric(prim9$Napoved6m)),lwd = 2, col = "black")


graf_napoved_10 <- plot(x = as.numeric(prim10$Napoved6m), 
                        y = as.numeric(prim10$Euribor6m),
                        ylim = c(min(0.9),max(1.3)),
                        xlim = c(min(0.9),max(1.3)),
                        type = "p", 
                        pch = 16, 
                        col = "green", 
                        main = "Primerjava napovedane, z dejansko obrestno mero v letu 2010", 
                        xlab = "Napovedana" , 
                        ylab = "Dejanska obrestna mera")
abline(0,1, col = "grey",lwd = 2) 
abline(lm(as.numeric(prim10$Euribor6m)~ as.numeric(prim10$Napoved6m)),lwd = 2, col = "black")




