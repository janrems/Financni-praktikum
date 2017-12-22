#4. Domača naloga

library(dplyr)
library(Quandl)

## Podnaloga 1

### a


mydata = Quandl("LBMA/GOLD", start_date="2017-08-21", end_date="2017-12-21")

mydata <- mydata[c(88:1),c(1,2)] #Podatke obrnem in izbrišem tiste kjer je cena 0




### b

ts <- ts(data = mydata[2])


graf <- ts.plot(ts, xlab = "Čas", ylab = "Cena zlata v USD", main = "Zlato", col = "black", lwd = 2)


##Podnaloga 2

vzorec <- ts[c(1:10)]

### a


G <- function(vrsta,k){
  l <- length(vrsta)
  zglajena <- c()
  for (i in 1:(l -k)) {
    zglajena[i] <- sum(vrsta[i:(k+i-1)])/k
  }
  return(ts(zglajena))
}

### b

zglajena7 <- G(ts,7)

napoved <- function(vrsta,k){
  l <- length(vrsta)
  return(sum(vrsta[(l - k +1):l])/k)
}

napovedana7 <- napoved(ts,7)

### c

graf2<- ts.plot(ts, zglajena7, xlab='Cas', ylab ='Vrednost zlata v USD', main = 'Casovna vrsta zlata in njeno drseno povprečje ', col=c('black', 'orange'),lwd = 3)
legend('bottomright', legend = c('Prvotna', 'Drseno 7'),col = c('black', 'orange'),lwd = 1:1, bty = 'n')

### d

MSE <- function(vrsta,zglajena,k){
  l <- length(vrsta)
  vsota <- 0
  for(i in (k+1):l){
    vsota <- vsota + (vrsta[i]-zglajena[i-k])^2
  }
  return(vsota/(l-k))
}

MSE7 <- MSE(ts,zglajena7,7)

### e

zglajena14 <- G(ts,14)
zglajena30 <- G(ts,30)

napovedana14 <- napoved(ts,14)
napovedana30 <- napoved(ts,30)

MSE14 <- MSE(ts,napovedana14,14)
MSE30 <- MSE(ts,napovedana30,30)

graf2<- ts.plot(ts, zglajena14, xlab='Cas', ylab ='Vrednost zlata v USD', main = 'Casovna vrsta zlata in njeno drseno povprečje ', col=c('black', 'orange'),lwd = 3)
legend('bottomright', legend = c('Prvotna', 'Drseno 14'),col = c("black", "orange"),lwd = 1:1, bty = "n")


graf2<- ts.plot(ts, zglajena30, xlab='Cas', ylab ='Vrednost zlata v USD', main = 'Casovna vrsta zlata in njeno drseno povprečje ', col=c('black', 'orange'),lwd = 3)
legend('bottomright', legend = c('Prvotna', 'Drseno 30'),col = c("black", "orange"),lwd = 1:1, bty = "n")

## POdnaloga 2

### a

EG <- function(vrsta,a){
  dolzina <- length(vrsta)
  l <- c(vrsta[1])
  for (i in 2:dolzina){
    l[i] <- a*vrsta[i] +(1-a)*l[i-1]
  }
  return(ts(l))
}

### b

eks_zglajena <- EG(ts,0.5)

napoved <- eks_zglajena[88]


graf2<- ts.plot(ts, eks_zglajena, xlab='Cas', ylab ='Vrednost zlata v USD', main = 'Casovna vrsta zlata in njeno eksponentno drseno povprečje ', col=c('black', 'red'),lwd = 3)
legend('bottomright', legend = c('Prvotna', 'Eksponentno 0.5 '),col = c("black", "red"),lwd = 1:1, bty = "n")

### c

eks_MSE <- function(vrsta, a){
  l <- length(vrsta)
  napaka <- 0
  glajena <- EG(vrsta, a)
  for (i in 1:l){
    napaka <- napaka + (vrsta[i] - glajena[i])^2
  }
  return(napaka/(l-1))
}

optimalen_a <- optimize(eks_MSE,c(0,1),vrsta = ts)

### d

opt_zglajena <- EG(ts,optimalen_a$minimum)


opt_napoved <- opt_zglajena[88]

graf2<- ts.plot(ts, opt_zglajena, xlab='Cas', ylab ='Vrednost zlata v USD', main = 'Casovna vrsta zlata in njeno eksponentno drseno povprečje ', col=c('black', 'red'),lwd = 3)
legend('bottomright', legend = c('Prvotna', 'Eksponentno 0.99 '),col = c("black", "red"),lwd = 1:1, bty = "n")


