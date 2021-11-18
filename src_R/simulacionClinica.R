tiempo_total = (16-9) * 60
mu  = 1/10;              
tiempo_espera_min = 5
tiempo_espera_max = 20
thr = 0.01;
iteraciones = 100;
cant_doctores=3
numero_pacientes  = c();
pacientes_que_esperaron   = c();
espera_promedio = c();
tiempos_cierre = c();
tiempos_llegada=c()
for (i in 1:iteraciones){
  
  tiempos_llegada=c(rexp(1, mu))
  while (tiempos_llegada[length(tiempos_llegada)] <= tiempo_total )
    {
    tiempos_llegada=c(tiempos_llegada,tiempos_llegada[length(tiempos_llegada)]+rexp(1, mu))
    #print(tiempos_llegada)
    
  }
  tiempos_llegada=tiempos_llegada[1:length(tiempos_llegada)-1]
 print(tiempos_llegada)
  
  tiempos_espera = rep(0,length(tiempos_llegada))
  tiempos_servicio = rep(0,length(tiempos_llegada))
  tiempos_doctores= rep(0,cant_doctores);              
  tiempo_actual = tiempos_llegada[1]    
  proximo_paciente = 1;                   
  
  while ((proximo_paciente <= length(tiempos_llegada)) | (any(tiempos_doctores > tiempo_actual+thr)))
  { 
     if (length(which(tiempos_doctores <= tiempo_actual+thr))>0){
       indice= min(which(tiempos_doctores <= tiempo_actual+thr))
    
     }
     else{indice=-1}
     if (proximo_paciente<=length(tiempos_llegada)){
       if ((indice != -1) & (tiempos_llegada[proximo_paciente]<=tiempo_actual+thr)){
         
         tiempos_espera[proximo_paciente]=tiempo_actual-tiempos_llegada[proximo_paciente]
         tiempos_servicio[proximo_paciente]=tiempo_espera_min+(tiempo_espera_max-tiempo_espera_min)*runif(1,0,1)
         tiempos_doctores[indice]=tiempo_actual+tiempos_servicio[proximo_paciente]
         proximo_paciente=proximo_paciente+1
       }
       
       
       
     }
     if (any(tiempos_doctores <= tiempo_actual+thr)){
       
        if (proximo_paciente<=length(tiempos_llegada)){
          tiempo_actual=tiempos_llegada[proximo_paciente]
        }
        else{
          
          tiempo_actual=max(c(tiempos_doctores,tiempo_actual))
          tiempo_cierre=tiempo_actual
          
        }
       
     }
     else{
       tiempo_actual=min(tiempos_doctores)
       
       
     }
      
    }
  t=tiempo_cierre
  horas=floor(tiempo_cierre/60)
  t=t-60*horas
  minutos=floor(t)
  segundos=floor(60*t-60*minutos)
  numero_pacientes  = c(numero_pacientes,length(tiempos_llegada))
  pacientes_que_esperaron   = c(pacientes_que_esperaron,sum(tiempos_espera > thr));
  espera_promedio = c(espera_promedio,mean(tiempos_espera));
  tiempos_cierre = c(tiempos_cierre,tiempo_cierre);
}
print(mean(numero_pacientes))
print(quantile(numero_pacientes,c(0.25,0.75)))
print(pacientes_que_esperaron)
print(espera_promedio)
print(tiempos_cierre)
mean(tiempos_cierre)
print(quantile(tiempos_cierre,c(0.25,0.75)))
esperaron=sort(pacientes_que_esperaron)
print(mean(esperaron))
print(quantile(esperaron,c(0.25,0.75)))
ep=espera_promedio
print(mean(ep))
print(quantile(ep,c(0.25,0.75)))