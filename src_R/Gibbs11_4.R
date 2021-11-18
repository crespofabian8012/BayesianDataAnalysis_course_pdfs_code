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
  temp=(datos$mediciones -theta.aux)^2
  temp2=tapply(temp, rep(1:J,length.j), sum)
  v=temp2/length.j
  sigma2=numeric(J)
  for (i in 1:J){
  sigma2[i]=(1/rchisq(1,nu+length.j[i]))*(nu*sigma2_0+length.j[i]*v[i])
  
  }
  return(sigma2)
}
tau2_update=function(){
  tau2.hat=sum((theta-mu)^2)/(J-1)
  tau2=(1/rchisq(1,J-1))*(J-1)*tau2.hat
  return(tau2)
}
sigma2_0_update=function(){
  ll=100
  sigma2_0.array = seq(4,100,length=ll)
  PP = numeric(ll)
  for (i in 1:ll){
      
      PP[i] = log.posteriori.conjunta(nu,sigma2_0.array[i])
    }
  
  MM = max(PP)
PP = exp(PP-MM)
  ccc= sum(PP)
  PP = PP/ccc
  II = PP
NN=20

muestras.sigma2_0=sample(sigma2_0.array,size=20,prob=II)


return(mean(muestras.sigma2_0))
}


log.posteriori.conjunta=function(nu,sigma2_0.par){
  theta.aux=c(rep(theta,length.j))
  sigma2.aux=c(rep(sigma2,length.j))
  sum=0.5*log(tau2)+J*(nu/2 +1)*(log(sigma2_0.par))

    
    sum=sum + sum(log(dnorm(datos$mediciones,theta.aux,sqrt(sigma2.aux))))
 
    sum=sum-sum((nu/2 +1)*log(sigma2))-sum((nu*sigma2_0.par/(2*sigma2)))+sum(log(dnorm(theta,rep(mu,J),sqrt(rep(tau2,J)))))

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
n.par=14
N=length(datos[,1])
chains <- 10
iter <- 1000

sims <- array (NA, c(iter, chains,n.par+2))
dimnames (sims) <- list (NULL, NULL,
                         c ("theta1", "theta2","theta3","theta4","theta5","theta6","mu","sigma2_1","sigma2_2","sigma2_3","sigma2_4","sigma2_5","sigma2_6","tau2","sigma2_0","logpost2"))
################################################

nu.array=sample(5:10,size=chains,replace=TRUE)


for (i in 1:chains){
  theta=tapply(datos$mediciones,datos$maquinas,sample,size=1)
  nu=nu.array[i]
  mu=mean(theta)
  tau2=tau2_update()
  sigma2_0=runif(1,4,100)
  sigma2=sigma2_update()
  sigma2_0=sigma2_0_update()

  log3=log.posteriori.conjunta(nu,sigma2_0)
  sims[1,i,]=c(theta,mu,sigma2,tau2,sigma2_0,log3)
  
}

for (m in 1:chains){
  nu=nu.array[m]
  for (t in 1:(iter-1)){
    theta <- sims[t,m,1:J]    
    mu <- sims[t,m,J+1]
    sigma2 <- sims[t,m,(J+2):(2*J+1)]
    tau2 <- sims[t,m,2*J+2]
    sigma2_0 <- sims[t,m,2*J+3]
    theta <- theta_update()
    
    mu <- mu_update()
    tau2 <- tau2_update()
    sigma2 <- sigma2_update()
    sigma2_0 =sigma2_0_update()
    #log1=log.posteriori.conjunta.menos.theta_update()
    #log2=log.posteriori.conjunta.con.theta_update()
    log3=log.posteriori.conjunta(nu,sigma2_0)
    sims[t+1,m,] <- c (theta,mu,sigma2,tau2,sigma2_0,log3)
    
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

############################
#predictiva a posteriori para nueva observación máquina 6

# nuevas.observaciones.maquina6=numeric(length(posterior.theta6))
# 
# for (i in 1:length(posterior.theta6)){
#   theta_6=posterior.mu[i]
#   sigma2=posterior.sigma2[i]
#   nuevas.observaciones.maquina6[i]=rnorm(1,theta_6,sqrt(sigma2))
# }
# hist(nuevas.observaciones.maquina6,probability=TRUE)
# densidad=density(nuevas.observaciones.maquina6)
# lines(densidad,col="red")
# quantile(nuevas.observaciones.maquina6,c(0.025,0.25,0.5,0.75,0.975))
# #################################################
# #predictiva a posteriori para nueva  máquina 7
# theta7=numeric(length(posterior.mu))
# for (i in 1:length(posterior.theta6)){
#   mu=posterior.mu[i]
#   tau2=posterior.tau2[i]
#   theta7[i]=rnorm(1,mu,sqrt(tau2))
# }
# hist(theta7,probability=TRUE)
# densidad=density(theta7)
# lines(densidad,col="red")
# quantile(theta7,c(0.025,0.25,0.5,0.75,0.975))
