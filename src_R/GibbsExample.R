chains <- 4
iter <- 10
rho=0.8
sims <- array (NA, c(iter, chains,2))
dimnames (sims) <- list (NULL, NULL,
                         c ("theta1", "theta2"))
sims[1,1,]=c(2.5,2.5)
sims[1,2,]=c(2.5,-2.5)
sims[1,3,]=c(-2.5,2.5)
sims[1,4,]=c(-2.5,-2.5)
theta1_update=function(){
  return(rnorm(1,rho*theta2,sqrt(1-rho^2)))
}
theta2_update=function(){
  return(rnorm(1,rho*theta1,sqrt(1-rho^2)))
}
par(pty="s")
plot(x=NULL,
     y=NULL,
     xlim=range(-4:4),
     ylim=range(-4:4),xlab="",ylab=""
)
points(x=c(2.5,2.5,-2.5,-2.5),y=c(2.5,-2.5,2.5,-2.5),pch=19)
for (m in 1:chains){
  for (t in 1:(iter-1)){
    theta1 <- sims[t,m,1]
    theta2 <- sims[t,m,2]
    theta1 <- theta1_update()
    segments(x0=sims[t,m,1], y0=sims[t,m,2], x1 = theta1, y1 = sims[t,m,2])
    theta2 <- theta2_update()
    sims[t+1,m,] <- c (theta1,theta2)
    segments(x0=theta1, y0=sims[t,m,2], x1 = theta1, y1 = theta2)
  }
}
