#PRVA DOMAČA NALOGA

# Opomba: Knižnjice, ki so potrebne za delovanje programa, so v mapi lib in datoteki libaries. Predlagam da skripto v celoti poženete.  #
# Tabele bi morale biti normalno dostopne, medtem ko morate, da se vam izrišejo grafi, kodo le-teh označiti in jo pognati, saj funkcija # 
# plot klicanja grafov kot spremenljivk ne podpira.                                                                                     #


# 1. Naloga

## Podnaloga a

###Tu pridobim podatke in jih zložim v tabele za vsako leto posebej

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

##Podnaloga b

### S for zankami in regularni izrazi izluščim le prve delovnike v mesecih

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

###Tabele različnih let združim v eno, pretvorim vsebino v vektorja tipa "character" in spremenim vrstico ki ima za decimalke vejice

tabela_zdruzena <- rbind(tabela_08,tabela_09,tabela_10)

tabela_zdruzena[] <- lapply(tabela_zdruzena, as.character)

for(i in 1:15){
  tabela_zdruzena[17,i] <- gsub(",",".",tabela_zdruzena[17,i])
}


## Podnaloga c
### Vrednosti polletne in letne obrestne mere zapišem kot časovne vrste


mesecna6 <- as.numeric(tabela_zdruzena$`6m`) %>% ts(start = c(2008,1), end = c(2010,12), frequency = 12)

mesecna12 <- as.numeric(tabela_zdruzena$`12m`) %>% ts(start = c(2008,1), end = c(2010,12), frequency = 12)

  
### Graf 6 mesečne in 12 mesečne obrestne mere tekom izbranega obdobja

graf_1 <- ts.plot(mesecna6, mesecna12,
        gpars=list(xlab="Leto", ylab="Obrestna mera", col = c("blue", "red"), lwd = 3))
        legend('topright', c("6 mesecna obrestna mera", "12 mesecna obrestna mera"), lty = 1, col = c("blue", "red"), lwd = 3)


# Naloga 2
        
## Podnaloga a

### Vektor zanimivih datumov              

zanimivi_datumi <- c("01/02/2008","03/03/2008","01/07/2010")

## Podnaloga b

### Naredim vektor kjer zaradi neenakih časovnih intervalov teden izrazim kot četrtino meseca

casovni_razmik <- c(0.25, 0.5, 0.75, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)

### Vrednosti obrestnih mer z različnimi dospetji, ob izbranih zanimivih datumih

obrestne_1 <- as.numeric(unname(unlist(tabela_zdruzena[2,])))
obrestne_2 <-as.numeric(unname(unlist(tabela_zdruzena[3,])))
obrestne_4 <- as.numeric(unname(unlist(tabela_zdruzena[7,])))

### Graf časovne strukture obrestnih mer

graf_1.2.08 <- plot(x = casovni_razmik, y = obrestne_1, type = "o", 
                    ylim = c(min(4),max(5.5)), 
                    main = "Časovna stuktura obrestnih mer", xlab = "Čas dospelosti v mesecih", ylab = "Obrestna mera", col = "red",
                    lwd = 3)
lines(x = casovni_razmik,y = obrestne_2, type = "o", col = "blue", pch = 16, lwd = 3, text(10,4.45,"3.3.2008", col="blue"))
lines(x = casovni_razmik,y = obrestne_4, col = "green", type = "o", pch = 14, lwd = 3, text(10,5,"1.7.2008", col="green"))
lines(x = casovni_razmik, y = obrestne_1, type = "o", col = "red", lwd = 3, text(10,4.2,"1.2.2008", col="red"))

### Opis obrestnih krivulj: Kronološko prva, rdeča krivulja, je tako imenovana obratna krivulja, saj od 3 mesečnega dospetja naprej, obrestna mera s      ### 
### povečevanjem dospetja pada. To kaže na negativna pričakovanja na trgih, kjer se bo z nizkimi obrestnimi merami poizkušalo vzpodbuditi potrošnjo.      ###
### To sovpada s stanjem duha na trgih leta 2008 pred začetkom velike finančne krize. Druga krivulja iz marca 2009 je po tromesečnem dospetju ravna,      ###
### kar je znak negotovosti na trgu. Tej negotovosti je sledila napoved izboljšanja, saj vidimo da je krivulja 1.7.2008 normalne oblike in napoveduje     ###
### stabilno obdobje z inflacijo, kar povečuje višino obrestne mere, glede na dospelost. Vendar pa so se te napovedi, kot je razvidno iz prejšnega grafa, ###
### izkazale za napačne, saj je obrestna mera tako s 6 , kot tudi 12 mesečno dospelostjo, proti koncu leta 2008 drastično padla.                         ###

#3. naloga

## Podnaloga a

### Terminsko obrestno mero sem izračunal vektorsko in vektor dodal, kot stolpec "terminska_6x12", tabeli tabela_zdruzena.

sest <- as.numeric(tabela_zdruzena$`6m`)
leto <- as.numeric(tabela_zdruzena$`12m`)


terminska_6x12 <- 2*((rep(1,36) + 1*leto)/(rep(1,36) + 1/2*sest)-rep(1,36))

tabela_zdruzena <- cbind(tabela_zdruzena,terminska_6x12)

# Podnaloga b

### Primerjavo med polletnimi obrestnimi merami in terminskimi obrestnimi merami tipa (6x12) sem prikazal v tabeli tabela_primerjava.                                                                                                        

terminska_primerjava <- terminska_6x12[1:30]

tabela_primerjava <- tabela_zdruzena[7:36,9:10]


tabela_primerjava <- cbind(tabela_primerjava,terminska_primerjava)

tabela_primerjava <- tabela_primerjava[,-2]
colnames(tabela_primerjava) <- c("Euribor6m", "Napoved6m")

## Podnaloga c


prim8 <- tabela_primerjava[1:6,]

prim9 <- tabela_primerjava[7:18,]

prim10 <- tabela_primerjava[19:30,] 

### V grafu "graf_napoved_skupni" je primerjava obrestnih mer s napovedanimi terminskimi obrestnimi merami za celotno proučevano obdobje.

graf_napoved_skupni <- plot(x = as.numeric(prim8$Napoved6m), y = as.numeric(prim8$Euribor6m), 
                            ylim = c(min(1),max(5.6)),xlim = c(min(1),max(5.6)), 
                            main = "Primerjava napovedane, z dejansko obrestno mero", xlab = "Napovedana" , ylab = "Dejanska obrestna mera")
points(x = as.numeric(prim8$Napoved6m), y = as.numeric(prim8$Euribor6m), type = "p", col = "red", pch = 16)
points(x = as.numeric(prim9$Napoved6m), y = as.numeric(prim9$Euribor6m), type = "p", col = "blue", pch = 16)
points(x = as.numeric(prim10$Napoved6m), y = as.numeric(prim10$Euribor6m), type = "p", col = "green", pch = 16)
abline(0,1, col = "grey",lwd = 2) 
abline(lm(as.numeric(tabela_primerjava$Euribor6m)~ as.numeric(tabela_primerjava$Napoved6m)),lwd = 2, col = "black")
legend('bottomright', c("2008", "2009", "2010"), pch = 16, col = c("red", "blue", "green"))

## Podnaloga d

### V grafu "graf_napoved_08" je primerjava obrestnih mer s napovedanimi terminskimi obrestnimi merami za leto 2008.

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


### V grafu "graf_napoved_09" je primerjava obrestnih mer s napovedanimi terminskimi obrestnimi merami za leto 2009.


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


### V grafu "graf_napoved_10" je primerjava obrestnih mer s napovedanimi terminskimi obrestnimi merami za leto 2010.


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


## Podnaloga e

### Da bi hipoteza pričakovanj trga držala, bi morale biti točke porazdeljene v bližini simetrale lihih kvadrantov oz. bi tam morala biti regresijska ###
### premica. Empirični podatki hipoteze z izjemo leta 2009, ne potrjujejo. Kar se tiče leta 2008, je drastično višje obrestne mere od napovedanih     ###
### mogoče povezati z padajočo krivuljo časovnih struktur obrestnih mer v začetku leta 2008 nato pa njeni normalizaciji v drugi polovici istega leta  ###
### (Glej nalogo 2). V letu 2009 je ujemanje z izjemo nekaj mesecev dokaj dobro, kar kaže na stabilizacijo trga. V letu 2010 pa empirični podatki     ###
### hipoteze ponovno ne potrjujejo, saj je napovedana terminska obrestna mera nižja od kasnejše dejanske. ###


