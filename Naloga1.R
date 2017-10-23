

#naloga 1

tabela_08 <- read_csv2("podatki/hist_EURIBOR_2008.csv") %>% t() %>% data.frame()

tabela_09 <- read_csv2("podatki/hist_EURIBOR_2009.csv") %>% t() %>% data.frame()

tabela_10 <- read_csv("podatki/hist_EURIBOR_2010.csv") %>% t() %>% data.frame()

tabela_08 <- tabela_08[,(1:15)]
tabela_09 <- tabela_09[,(1:15)]
tabela_10 <- tabela_10[,(1:15)]

colnames(tabela_08) <- as.numeric(tabela_08[2,])
