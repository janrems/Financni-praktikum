#uvoz
podatki <- scan("podatki/vzorec1.txt")


#porazdelitev

y <- mde(podatki, pweibull,start = list(shape = 1.5, scale = 1),measure = "CvM")
shape <- y$estimate[1]
scale <- y$estimate[2]



histogram <- hist(podatki,probability = TRUE)
curve(dweibull(x,shape, scale),add = TRUE,from = 0, to = 10)

histogram2 <- plot(ecdf(podatki))
curve(pweibull(x,shape, scale),add = TRUE,from = 0, to = 10,col = "red",lwd = 3)


# upanje
lambda <- 15
e_n <- lambda
e_y <- scale * gamma(1+1/shape)
var_y <- scale^2 * (gamma(1+2/shape) - (gamma(1+1/shape))^2)
var_n <- e_n
e_S <- e_y * e_n

var_s <- var_y * e_n + e_y^2 * var_n


#2
h <- 0.5
n <- 18
diskretna <- discretize(1-exp(-(x/scale)^shape), step = h,from = 0,to =h*n,method = "rounding")

graf <- plot(stepfun(seq(0.5,9,0.5),diffinv(diskretna)))
curve(pweibull(x,shape, scale), add = TRUE, col = "red")




