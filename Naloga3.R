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

binomski <- function(S0, u, d, U, R, T1, type, N){
  E <- 0
  drevo <- hcube(rep(2,U), translation = -1)
  p = (1+R-d)/(u-d)
  for (i in c(1:2^U)){
    vrsta <- c(S0)
    P = p^(sum(drevo[i,]))* (1-p)^(U-sum(drevo[i,]))
    for(j in c(1:U)){
      vrsta[j+1] <- vrsta[j]* (u*drevo[i,j] + (1-drevo[i,j])*d)
      
    }
    
    S <- izplacilo(vrsta, T1, type)
    E <- E + P*S
    
  }
  
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



