

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


