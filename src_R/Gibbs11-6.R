theta_update=function(){
  theta_hat <- (mu/tau2 + promedios*length.j/sigma2)/(1/tau2 + length.j/sigma2)
  V_theta <- 1/(1/tau2 + length.j/sigma2)
  result=numeric(J)
  for(i in 1:J){
    result[i]=rnorm (1, theta_hat[i], sqrt(V_theta[i]))
    
  }
  return(result)
}
mu_update=function(){
  mu.hat=mean(theta)
  return(rnorm(1,mu.hat,sqrt(tau2/J)))
}
sigma2_update=function(){
  theta.aux=c(rep(theta,length.j))
  sigma2.hat=sum((datos$coagulacion -theta.aux)^2)/N
  sigma2=(1/rchisq(1,N))*N*sigma2.hat
  return(sigma2)
}
tau2_update=function(){
  tau2.hat=sum((theta-mu)^2)/(J-1)
  tau2=(1/rchisq(1,J-1))*(J-1)*tau2.hat
  return(tau2)
}
log.posteriori.conjunta.menos.theta_update=function(){
  theta.aux=c(rep(theta,length.j))
  sum=0.5*log(tau2)-10
  for (i in 1:N ){
    sum=sum + log(dnorm(datos$coagulacion[i],theta.aux[i],sqrt(sigma2)))
  }
  return(sum)
  
  
}
log.posteriori.conjunta.con.theta_update=function(){
  theta.aux=c(rep(theta,length.j))
  sum=0.5*log(tau2)
  for (i in 1:N ){
    sum=sum + log(dnorm(datos$coagulacion[i],theta.aux[i],sqrt(sigma2)))
  }
  for (j in 1:J){
    sum=sum+log(dnorm(theta[j],mu,sqrt(tau2)))
    
  }
  return(sum)
  
  
}
####################################################

dietas=as.factor(c(rep("A",4),rep("B",6),rep("C",6),rep("D",8)))
coagulacion=c(62, 60, 63, 59,63, 67, 71, 64, 65, 66,68, 66, 71, 67, 68, 68,56, 62, 60, 61, 63, 64, 63, 59)
datos=data.frame(dietas=dietas,coagulacion=coagulacion)
promedios=tapply(datos$coagulacion,datos$dietas,mean)
length.j=tapply(datos$coagulacion,datos$dietas,length)
J=length(levels(datos$dietas))
n.par=7
N=length(datos[,1])
chains <- 10
iter <- 100

sims <- array (NA, c(iter, chains,n.par+2))
dimnames (sims) <- list (NULL, NULL,
                         c ("theta1", "theta2","theta3","theta4","mu","sigma2","tau2","logpost1","logpost2"))


for (i in 1:chains){
  theta=tapply(datos$coagulacion,datos$dietas,sample,size=1)
  mu=mean(theta)
  sigma2=sigma2_update()
  tau2=tau2_update()
  
  log1=log.posteriori.conjunta.menos.theta_update()
  log2=log.posteriori.conjunta.con.theta_update()
  sims[1,i,]=c(theta,mu,sigma2,tau2,log1,log2)
  
}
#sims[1,1,]=c(theta.j,mu,sigma2,tau2)


# par(pty="s")
# plot(x=NULL,
#      y=NULL,
#      xlim=range(-4:4),
#      ylim=range(-4:4),xlab="",ylab=""
# )
#points(x=c(2.5,2.5,-2.5,-2.5),y=c(2.5,-2.5,2.5,-2.5),pch=19)
for (m in 1:chains){
  for (t in 1:(iter-1)){
    theta <- sims[t,m,1:J]    
    mu <- sims[t,m,5]
    sigma2 <- sims[t,m,6]
    tau2 <- sims[t,m,7]
    theta <- theta_update()
    
    mu <- mu_update()
    sigma2 <- sigma2_update()
    tau2 <- tau2_update()
    log1=log.posteriori.conjunta.menos.theta_update()
    log2=log.posteriori.conjunta.con.theta_update()
    sims[t+1,m,] <- c (theta,mu,sigma2,tau2,log1,log2)
    
  }
}
par(mar = rep(2, 4))
par(mfrow=c(2,1))
plot(1:iter,sims[1:iter,1,1], main="theta1 cadena 1",xlim=c(1,iter),ylim=c(55,65))
lines(1:iter,sims[1:iter,1,1])
plot(1:iter,sims[1:iter,2,1], main="theta1 cadena 2",xlim=c(1,iter),ylim=c(55,65))
lines(1:iter,sims[1:iter,2,1])

posterior.theta1=c(sims[floor(iter/2):iter,1:chains,1])
mean(posterior.theta1) 
hist(posterior.theta1,probability=TRUE)
densidad=density(posterior.theta1)
lines(densidad,col="red")
quantile(posterior.theta1,c(0.025,0.25,0.5,0.75,0.975))
posterior.theta2=c(sims[floor(iter/2):iter,1:chains,2])
mean(posterior.theta2) 
quantile(posterior.theta2,c(0.025,0.25,0.5,0.75,0.975))
posterior.theta3=c(sims[floor(iter/2):iter,1:chains,3])
mean(posterior.theta3) 
quantile(posterior.theta3,c(0.025,0.25,0.5,0.75,0.975))
posterior.theta4=c(sims[floor(iter/2):iter,1:chains,4])
mean(posterior.theta4) 
quantile(posterior.theta4,c(0.025,0.25,0.5,0.75,0.975))
posterior.mu=c(sims[floor(iter/2):iter,1:chains,5])
mean(posterior.mu) 
quantile(posterior.mu,c(0.025,0.25,0.5,0.75,0.975))
posterior.sigma=sqrt(c(sims[floor(iter/2):iter,1:chains,6]))
mean(posterior.sigma) 
quantile(posterior.sigma,c(0.025,0.25,0.5,0.75,0.975))
posterior.tau=sqrt(c(sims[floor(iter/2):iter,1:chains,7]))
mean(posterior.tau) 
quantile(posterior.tau,c(0.025,0.25,0.5,0.75,0.975))
log.posterior.sin.theta=(c(sims[floor(iter/2):iter,1:chains,8]))
mean(log.posterior.sin.theta) 
quantile(log.posterior.sin.theta,c(0.025,0.25,0.5,0.75,0.975))
log.posterior.con.theta=(c(sims[floor(iter/2):iter,1:chains,9]))
mean(log.posterior.con.theta) 
quantile(log.posterior.con.theta,c(0.025,0.25,0.5,0.75,0.975))
