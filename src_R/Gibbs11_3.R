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
  sigma2.hat=sum((datos$mediciones -theta.aux)^2)/N
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
    sum=sum + log(dnorm(datos$mediciones[i],theta.aux[i],sqrt(sigma2)))
  }
  return(sum)
  
  
}
log.posteriori.conjunta.con.theta_update=function(){
  theta.aux=c(rep(theta,length.j))
  sum=0.5*log(tau2)
  for (i in 1:N ){
    sum=sum + log(dnorm(datos$mediciones[i],theta.aux[i],sqrt(sigma2)))
  }
  for (j in 1:J){
    sum=sum+log(dnorm(theta[j],mu,sqrt(tau2)))
    
  }
  return(sum)
  
  
}
####################################################

maquinas=as.factor(c(rep("1",5),rep("2",5),rep("3",5),rep("4",5),rep("5",5),rep("6",5)))
mediciones=c(83, 92 ,92, 46, 67, 117, 109, 114, 104, 87,101, 93, 92, 86,
              67,105, 119, 116, 102, 116,79, 97, 103, 79, 92,57, 92, 104,
              77, 100)
datos=data.frame(maquinas=maquinas,mediciones=mediciones)
promedios=tapply(datos$mediciones,datos$maquinas,mean)
length.j=tapply(datos$mediciones,datos$maquinas,length)
J=length(levels(datos$maquinas))
n.par=9
N=length(datos[,1])
chains <- 10
iter <- 100

sims <- array (NA, c(iter, chains,n.par+2))
dimnames (sims) <- list (NULL, NULL,
                         c ("theta1", "theta2","theta3","theta4","theta5","theta6","mu","sigma2","tau2","logpost1","logpost2"))


for (i in 1:chains){
  theta=tapply(datos$mediciones,datos$maquinas,sample,size=1)
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
    mu <- sims[t,m,J+1]
    sigma2 <- sims[t,m,J+2]
    tau2 <- sims[t,m,J+3]
    theta <- theta_update()
    
    mu <- mu_update()
    sigma2 <- sigma2_update()
    tau2 <- tau2_update()
    log1=log.posteriori.conjunta.menos.theta_update()
    log2=log.posteriori.conjunta.con.theta_update()
    sims[t+1,m,] <- c (theta,mu,sigma2,tau2,log1,log2)
    
  }
}
# par(mar = rep(2, 4))
# par(mfrow=c(2,1))
# plot(1:iter,sims[1:iter,1,1], main="theta1 cadena 1",xlim=c(1,iter),ylim=c(55,65))
# lines(1:iter,sims[1:iter,1,1])
# plot(1:iter,sims[1:iter,2,1], main="theta1 cadena 2",xlim=c(1,iter),ylim=c(55,65))
# lines(1:iter,sims[1:iter,2,1])
library("rstan")
monitor(sims)
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
posterior.theta5=c(sims[floor(iter/2):iter,1:chains,5])
mean(posterior.theta5) 
quantile(posterior.theta5,c(0.025,0.25,0.5,0.75,0.975))
posterior.theta6=c(sims[floor(iter/2):iter,1:chains,6])
mean(posterior.theta6) 
plot(ts(posterior.theta6))
hist(posterior.theta6,probability=TRUE)
densidad=density(posterior.theta6)
lines(densidad,col="red")
quantile(posterior.theta6,c(0.025,0.25,0.5,0.75,0.975))
posterior.mu=c(sims[floor(iter/2):iter,1:chains,7])
mean(posterior.mu) 
quantile(posterior.mu,c(0.025,0.25,0.5,0.75,0.975))
posterior.sigma=sqrt(c(sims[floor(iter/2):iter,1:chains,8]))
posterior.sigma2=(c(sims[floor(iter/2):iter,1:chains,8]))
mean(posterior.sigma) 
quantile(posterior.sigma,c(0.025,0.25,0.5,0.75,0.975))
posterior.tau=sqrt(c(sims[floor(iter/2):iter,1:chains,9]))
posterior.tau2=(c(sims[floor(iter/2):iter,1:chains,9]))
mean(posterior.tau) 
quantile(posterior.tau,c(0.025,0.25,0.5,0.75,0.975))
log.posterior.sin.theta=(c(sims[floor(iter/2):iter,1:chains,10]))
mean(log.posterior.sin.theta) 
quantile(log.posterior.sin.theta,c(0.025,0.25,0.5,0.75,0.975))
log.posterior.con.theta=(c(sims[floor(iter/2):iter,1:chains,11]))
mean(log.posterior.con.theta) 
quantile(log.posterior.con.theta,c(0.025,0.25,0.5,0.75,0.975))
############################
#predictiva a posteriori para nueva observación máquina 6

nuevas.observaciones.maquina6=numeric(length(posterior.theta6))

for (i in 1:length(posterior.theta6)){
  theta_6=posterior.mu[i]
  sigma2=posterior.sigma2[i]
  nuevas.observaciones.maquina6[i]=rnorm(1,theta_6,sqrt(sigma2))
}
hist(nuevas.observaciones.maquina6,probability=TRUE)
densidad=density(nuevas.observaciones.maquina6)
lines(densidad,col="red")
quantile(nuevas.observaciones.maquina6,c(0.025,0.25,0.5,0.75,0.975))
#################################################
#predictiva a posteriori para nueva  máquina 7
theta7=numeric(length(posterior.mu))
for (i in 1:length(posterior.theta6)){
  mu=posterior.mu[i]
  tau2=posterior.tau2[i]
  theta7[i]=rnorm(1,mu,sqrt(tau2))
}
hist(theta7,probability=TRUE)
densidad=density(theta7)
lines(densidad,col="red")
quantile(theta7,c(0.025,0.25,0.5,0.75,0.975))
