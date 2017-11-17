#2. DOMAČA NALOGA

##1. Naloga

###Podnaloga a


podatki <- scan("podatki/vzorec1.txt")

histogram <- hist(podatki, main = "Histogram višine škod", xlab = "Višina škode", ylab = "Število pojavitev")


###Podnaloga b
  
y <- mde(podatki, pweibull,start = list(shape = 1.5, scale = 1),measure = "CvM")
shape <- y$estimate[1]
scale <- y$estimate[2]

###Podnaloga c


histogram <- hist(podatki,probability = TRUE, main = "Primerava histograma in teoretične gostote ", xlab = "x", ylab = "f(x)")
curve(dweibull(x,shape, scale),add = TRUE,from = 0, to = 10)


porazdelitvi <- plot(ecdf(podatki), main = "Primerjava vzorćne in teoretične porazdelitvene funkcije")
curve(pweibull(x,shape, scale),add = TRUE,from = 0, to = 10,col = "red",lwd = 3)

###Podnaloga d


lambda <- 15
e_n <- lambda
e_y <- scale * gamma(1+1/shape)
var_y <- scale^2 * (gamma(1+2/shape) - (gamma(1+1/shape))^2)
var_n <- e_n

#Matematično upanje slučajne spremenljivke S
e_S <- e_y * e_n

#Varianca slučajne spremenljivke S
var_s <- var_y * e_n + e_y^2 * var_n




##Naloga 2

###Podnaloga a

h <- 0.5
n <- 18
diskretna <- discretize(1-exp(-(x/scale)^shape), step = h,from = 0,to =h*n,method = "rounding")

###Podnaloga b

graf <- plot(stepfun(seq(0.5,9,0.5),diffinv(diskretna)), main = "Porazdelitvena funkcija sluč. sprem. Y in njene diskretizacije")
curve(pweibull(x,shape, scale), add = TRUE, col = "red")

###Podnaloga c

diskretna_1 <- discretize(1-exp(-(x/scale)^shape), step = h,from = 0,to =1000 ,method = "rounding")
 
Fs <- aggregateDist("recursive", model.freq = "poisson",
                    model.sev = diskretna_1, lambda = 15, x.scale = h, tol = 0.002, convolve = 0)

###Podnaloga d

upanje_Fs <- as.numeric(knots(Fs) %*% diff(Fs))

varianca_Fs <- as.numeric(knots(Fs)^2 %*% diff(Fs)) - upanje_Fs^2

###Podnaloga e

tvegana_vrednost <- VaR(Fs,0.995)
pricakovani_izpad <- CTE(Fs,0.05)


##Naloga 3

###Podanaloga a

vektor_N <- rpois(10000,15)
vektor_S <- vector(mode= "numeric", length = 10000 )
k <- 1
for (i in vektor_N){
  vektor_Y <- rweibull(i,shape,scale)
  vektor_S[k] <- sum(vektor_Y)
  k <- k + 1
}

###Podnaloga b

upanje_S <- mean(vektor_S)

disperzija_S <- var(vektor_S)
  

###Podnaloga c

tvegana_vrednost2 <- VaR(ecdf(vektor_S),0.995)  
