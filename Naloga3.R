#3.Domača naloga

##Naloga 1

###Podnaloga a

S0 <- 50
u <- 1.05
d <- 0.95
U <- 5
R <- 0.03
t <- 3

izplacila_x <- c(0, 63.8-55.12, 0, 52.24-47.5, 57.74-52.5)
izplacila_y <- c(0, 57.88-50, 0, 47.38-45.12, 52.37-49.88)
###Podnaloga b

izplacilo <- function(vektor, T1, type){
  if(type == "call"){
    return(max(0,max(vektor[c((T1 +1):length(vektor))]) - max(vektor[c(1:T1)])))
  
  }
  else{
    return(max(0,min(vektor[c((T1+1):length(vektor))]) - min(vektor[c(1:T1)])  ))
  }
    
}


##Naloga 2

###Podnaloga a

binomski <- function(S0, u, d, U, R, T1, type){
  
  drevo <- hcube(rep(2,U), translation = -1)
  p = (1+R-d)/(u-d)
  
  k <- rowSums(drevo)
  
    
  p_bin = p^k* (1-p)^(U-k)
  drevo[drevo == 0] <- d
  drevo[drevo == 1] <- u
  
  drevo <- t(apply(drevo,1,cumprod))
  vrednosti <- cbind(S0,S0*drevo)
  
  izplacila <- apply(vrednosti,1,function(x) izplacilo(x,T1,type))
  
  
  
  
  E <- izplacila %*% p_bin
    
  
  
  return(E/(1+R)^U)  
    
}


###Podnaloga b

monte <- function(S0, u, d, U, R, T1, type, N){
  p <- (1+R-d)/(u-d)
  drevo <- matrix(rbinom(U*N,1,p),N,U)
  drevo[drevo == 1] <- u
  drevo[drevo == 0] <- d
  
  drevo <- t(apply(drevo,1, cumprod))
  
  vrednosti <- cbind(S0, S0*drevo)
  izplacila <- apply(vrednosti,1, function(x) izplacilo(x,T1,type))
  premija_opcije <- mean(izplacila)/(1+R)^U
  
  return(premija_opcije)
  
}

sim1 <- monte(60,1.05,0.95,15,0.01,8,"put",10) 
sim2 <- monte(60,1.05,0.95,15,0.01,8,"put",100) 
sim3 <- monte(60,1.05,0.95,15,0.01,8,"put",1000) 


##Naloga 3

N1 <- c()
N2 <- c()
N3 <- c()
M <- 100
for (k in c(1:M)){
    N1 <- c(N1,monte(60,1.05,0.95,15,0.01,8,"put",10))
    N2 <- c(N2,monte(60,1.05,0.95,15,0.01,8,"put",100))
    N3 <- c(N3,monte(60,1.05,0.95,15,0.01,8,"put",1000))
    }

premija_binom <- binomski(60,1.05,0.95,15,0.01,8,"put")

###N = 10
povp1 <- mean(N1)

odklon1 <- sqrt(var(N1))

h1 <- hist(N1,
           xlim = c(0,10),
           main = "Monte Carlo: N = 10",
           xlab = "Premija",
           ylab = "Frekvenca",
           col = "yellow"
           )
abline(v = povp1,col= "green",lwd = 2)
abline(v = premija_binom, col = "red", lty = 2)
arrows(x0 = povp1,y0 = 0, x1 = povp1 + odklon1, col = "green",length = 0.1,lwd = 2)
arrows(x0 = povp1,y0 = 0, x1 = povp1 - odklon1, col = "green",length = 0.1,lwd = 2)

legend("topright",
       legend = c("Monte Carlo", "Analiza modela"),
       col = c("green","red"),
       lty = c("solid","dashed"),
       cex = 0.8)


###N = 100
povp2 <- mean(N2)

odklon2 <- sqrt(var(N2))

h2 <- hist(N2,
           xlim = c(0,10),
           main = "Monte Carlo: N = 100",
           xlab = "Premija",
           ylab = "Frekvenca",
           col = "yellow"
)
abline(v = povp2,col= "green",lwd = 2)
abline(v = premija_binom, col = "red", lty = 2)
arrows(x0 = povp2,y0 = 0, x1 = povp2 + odklon2, col = "green",length = 0.1,lwd = 2)
arrows(x0 = povp2,y0 = 0, x1 = povp2 - odklon2, col = "green",length = 0.1,lwd = 2)

legend("topright",
       legend = c("Monte Carlo", "Analiza modela"),
       col = c("green","red"),
       lty = c("solid","dashed"),
       cex = 0.8)


###N = 1000
povp3 <- mean(N3)

odklon3 <- sqrt(var(N3))

h3 <- hist(N3,
           xlim = c(0,10),
           main = "Monte Carlo: N = 1000",
           xlab = "Premija",
           ylab = "Frekvenca",
           col = "yellow"
)
abline(v = povp3,col= "green",lwd = 2)
abline(v = premija_binom, col = "red", lty = 2)
arrows(x0 = povp3,y0 = 0, x1 = povp3 + odklon3, col = "green",length = 0.1,lwd = 2)
arrows(x0 = povp3,y0 = 0, x1 = povp3 - odklon3, col = "green",length = 0.1,lwd = 2)

legend("topright",
       legend = c("Monte Carlo", "Analiza modela"),
       col = c("green","red"),
       lty = c("solid","dashed"),
       cex = 0.8)



