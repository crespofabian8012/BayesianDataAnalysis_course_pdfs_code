chains <- 5
iter <- 20000
sims <- array (NA, c(iter, chains,2))
dimnames (sims) <- list (NULL, NULL,
                         c ("theta1", "theta2"))
sims[1,1,]=c(3,3)
sims[1,2,]=c(3,-3)
sims[1,3,]=c(-3,3)
sims[1,4,]=c(-3,-3)
sims[1,5,]=c(0,0)
theta1_update=function(){
  return(rnorm(1,theta1,0.2^2))
}
theta2_update=function(){
  return(rnorm(1,theta2,0.2^2))
}
par(pty="s")
plot(x=NULL,
     y=NULL,
     xlim=range(-4:4),
     ylim=range(-4:4),xlab="",ylab=""
)
points(x=c(3,3,-3,-3,0),y=c(3,-3,3,-3,0),pch=19)
r=0
u=0
for (m in 1:chains){
  for (t in 1:(iter-1)){
    theta1 <- sims[t,m,1]
    theta2 <- sims[t,m,2]
    theta1 <- theta1_update()
   
    #segments(x0=sims[t,m,1], y0=sims[t,m,2], x1 = theta1, y1 = sims[t,m,2])
    theta2 <- theta2_update()
    r=dnorm(theta1,0,1)*dnorm(theta2,0,1)/(dnorm(sims[t,m,1],0,1)*dnorm(sims[t,m,2],0,1))
    #print(r)
    r=min(r,1)
    u=runif(1,0,1)
    if (u<r){
      sims[t+1,m,] <- c (theta1,theta2)
      
    }
    else{
      sims[t+1,m,] <- c (sims[t,m,1],sims[t,m,2])
      
    }
    
    segments(x0=sims[t,m,1], y0=sims[t,m,2], x1 = sims[t+1,m,1], y1 = sims[t+1,m,2])
  }
}
