datos<-read.csv("Muestra de datos sucursales.csv",header = T)

options(scipen=999)
sucsP<-c(1, 2, 4, 7, 8, 9, 12, 15, 17, 20, 23, 25, 26, 31, 32, 33, 34, 36, 37, 38, 39, 41, 44, 45, 46)
sucsS<-seq(1,49)[! seq(1,49) %in% sucsP]
sucsS320<-sucsS[9:24]
datosS<-datos[,sucsS]
library(ggplot2)
library(gridExtra)
library(fitdistrplus)

#valores y funciones que se utilizaran
pronosticosdtv2<-function(datosasignaciones){
  diashabiles<-length(datosasignaciones)-20
  resp<-vector("integer",diashabiles)
  for(i in 20:diashabiles){
    inf20<-i-19
    inf10<-i-9
    inf5<-i-4
    pronosticos10<-round(mean(datosasignaciones[inf10:i])*20)
    pronosticos5<-round(mean(datosasignaciones[inf5:i])*20)
    
    ajuste <- try(fitdistr(datosasignaciones[inf20:i],"negative binomial"))
    if(!inherits(ajuste,"try-error")){
      simulaciones <- vector("integer",2000)
      for(j in 1:length(simulaciones)){
        simulacion <- rnbinom(20,size = ajuste$estimate[1],mu=ajuste$estimate[2])
        simulaciones[j] <- sum(simulacion)
      }
      simulaciones  <- sort(simulaciones)
      pronosticos20<-simulaciones[round(0.80*length(simulaciones))]
    }else{
      pronosticos20<-sum(datosasignaciones[inf20:i])                                                                           #posible modificacion
      print(i)
    }
    
    resp[i+1]<-round(0.60*pronosticos20+0.30*pronosticos10+0.10*pronosticos5)
  }
  return(resp)
}

asignaciones<-matrix(0,350,24)
for(i in 1:24){
  if(sucsS[i] %in% sucsS320==T){
    asignaciones[1:320,i]<-datos[1:320,sucsS[i]]
  }else{
    asignaciones[,i]<-datos[,sucsS[i]]
  }
}

pronosticos<-matrix(0,350,24)
for(i in 1:24){
  if(sucsS[i] %in% sucsS320==T){
    pronosticos[1:300,i]<-pronosticosdtv2(datos[1:320,sucsS[i]])[2:301]
    pronosticos[1:19,i]<-770
  }else{
    pronosticos[1:330,i]<-pronosticosdtv2(datos[,sucsS[i]])[2:331]
    pronosticos[1:19,i]<-770
  }
}

valoresreales<-matrix(0,350,24)
for(i in 1:24){
  if(i<=8){
    for(j in 1:330){
      inf<-j+1
      sup<-j+20
      valoresreales[j,i]<-sum(datos[inf:sup,sucsS[i]])
    }
  }else{
    for(j in 1:300){
      inf<-j+1
      sup<-j+20
      valoresreales[j,i]<-sum(datos[inf:sup,sucsS[i]])
    }
  }
}




stocks<-matrix(0,350,24)
pedido<-matrix(0,350,24)
semaforos<-matrix(0,350,24)

leadtimeest<-11
leadtimemax<-20

checar.amarillo<-function(stock,pronos){
  dif<-stock-pronos
  if(dif<=150){
    resp<-1
  }else{
    resp<-0
  }
  return(resp)
}


#funcion de simulacion
for(s in 1:8){ 
  stocks[1,s]<-1500
for (i in 1:330){
  #--------------------------------Se va actualizando el Stock------------------------------------------------------------  
  
  if(i<2){
    stocks[i,s] <- stocks[i,s]-asignaciones[i,s]
  }
  else{
    if(stocks[i,s]>10000){
      stocks[i,s] <- stocks[i,s]-10000-asignaciones[i,s]
    }
    else{
      aux<-i-1
      stocks[i,s]  <- stocks[aux,s]-asignaciones[i,s]
    }
  }

  if(pedido[i,s]==0){
    amarillo<-checar.amarillo(stocks[i,s],pronosticos[i,s])
    if(amarillo==1){
      semaforos[i,s]<-'Amarillo'
      leadtimeobs<-round(rnorm(1,11,1.5))
      pedido[i,s]  <- 2
      for(k in 1:leadtimeobs){
        pedido[i+k,s]  <- 1
      }
      inicio<-i+1
      fin<-i+leadtimeobs
      stocks[i+leadtimeobs,s]  <- max((stocks[i,s]-sum(asignaciones[inicio:fin,s])),0)+(max(1500-stocks[i,s],0))+round(((leadtimeest/leadtimemax)*(pronosticos[i,s])))+10000
      semaforos[i+leadtimeobs,s]<-'Verde'
    }else{
      semaforos[i,s]<-'Verde'
    }
    
    
  }
  if(stocks[i,s]<(2/20)*pronosticos[i,s]){semaforos[i,s]<-'Morado'}  
  if(stocks[i,s]<0){stocks[i,s]<-0}
}
}



for(s in 9:24){ 
    stocks[1,s]<-1500
    for (i in 1:300){
      #--------------------------------Se va actualizando el Stock------------------------------------------------------------  
      
      if(i<2){
        stocks[i,s] <- stocks[i,s]-asignaciones[i,s]
      }
      else{
        if(stocks[i,s]>10000){
          stocks[i,s] <- stocks[i,s]-10000-asignaciones[i,s]
        }
        else{
          aux<-i-1
          stocks[i,s]  <- stocks[aux,s]-asignaciones[i,s]
        }
      }
      
      if(pedido[i,s]==0){
        amarillo<-checar.amarillo(stocks[i,s],pronosticos[i,s])
        if(amarillo==1){
          semaforos[i,s]<-'Amarillo'
          leadtimeobs<-round(rnorm(1,11,1.5))
          pedido[i,s]<-2
          for(k in 1:leadtimeobs){
            pedido[i+k,s]  <- 1
          }
          inicio<-i+1
          fin<-i+leadtimeobs
          stocks[i+leadtimeobs,s]  <- max((stocks[i,s]-sum(asignaciones[inicio:fin,s])),0)+(max(1500-stocks[i,s],0))+round(((leadtimeest/leadtimemax)*(pronosticos[i,s])))+10000
          semaforos[i+leadtimeobs,s]<-'Verde'
        }else{
          semaforos[i,s]<-'Verde'
        }
        
      }
      if(stocks[i,s]<(2/20)*pronosticos[i,s]){semaforos[i,s]<-'Morado'}
      if(stocks[i,s]<0){stocks[i,s]<-0}
      
    }
  }

#poner semaforos en gris
for(s in 1:24){
  semaforos[semaforos[,s]==0,s]<-'Apagado'
}


graficaproceso<-function(sucursal){
  if(sucursal<=8){
    estadisticas<-data.frame(stocks[1:330, sucursal], pronosticos[1:330, sucursal], pronosticos[1:330, sucursal]+150, valoresreales[1:330, sucursal], pedido[1:330, sucursal], semaforos[1:330,sucursal])
    colnames(estadisticas)<-c("Stock","Pronostico","PronosticoM","dt","Pedidos","Semaforos")
    ggrafica<-ggplot(estadisticas,aes(x=seq(1:330),y=Stock, color=Semaforos))+geom_point(size=2)+geom_line(aes(y=PronosticoM,linetype="Pronostico + 150"),color="green4")+geom_line(aes(y=Pronostico,linetype="Pronostico"),color="indianred1")+geom_line(aes(y=dt,linetype="d(t)"),color="gray46")+geom_line(aes(y=rep(0,330)),color="black")+geom_line(aes(y=rep(1500,330)),color="black",linetype="dotted")+geom_line(aes(y=rep(1600,330)),color="black")+scale_y_continuous(breaks = seq(0,2500,100))+scale_color_manual(values=c("gold","gray46","chartreuse4"))+ylab("Stock(t)")+xlab("t")+xlim(c(0,330))+ggtitle(paste0("Sucursal ",sucsS[sucursal],": Simulacion de proceso de resurtido automatizado"))+labs(linetype="Linea")+scale_linetype_manual(values=c("longdash","dotdash","solid"))+theme(plot.title = element_text(size=12,hjust = 0.5)) 
  }else{
    estadisticas<-data.frame(stocks[1:300, sucursal], pronosticos[1:300, sucursal], pronosticos[1:300, sucursal]+150, valoresreales[1:300, sucursal], pedido[1:300, sucursal],semaforos[1:300,sucursal]) 
    colnames(estadisticas)<-c("Stock","Pronostico","PronosticoM","dt","Pedidos","Semaforos")
    if(sucursal != 22 && sucursal!=21){
      if(('Morado' %in% semaforos[,sucursal])==T){
        ggrafica<-ggplot(estadisticas,aes(x=seq(1:300),y=Stock, color=Semaforos))+geom_point(size=2)+geom_line(aes(y=PronosticoM,linetype="Pronostico + 150"),color="green4")+geom_line(aes(y=Pronostico,linetype="Pronostico"),color="indianred1")+geom_line(aes(y=dt,linetype="d(t)"),color="gray66")+geom_line(aes(y=rep(0,300)),color="black")+geom_line(aes(y=rep(1500,300)),color="black",linetype="dotted")+geom_line(aes(y=rep(1600,300)),color="black")+scale_y_continuous(breaks = seq(0,2500,100))+scale_color_manual(values=c("gold","gray46","purple","chartreuse4"))+ylab("Stock(t)")+xlab("t")+xlim(c(0,330))+ggtitle(paste0("Sucursal ",sucsS[sucursal],": Simulacion de proceso de resurtido automatizado"))+labs(linetype="Linea")+scale_linetype_manual(values=c("longdash","dotdash","solid"))+theme(plot.title = element_text(size=12,hjust = 0.5)) 
      }else{
        ggrafica<-ggplot(estadisticas,aes(x=seq(1:300),y=Stock, color=Semaforos))+geom_point(size=2)+geom_line(aes(y=PronosticoM,linetype="Pronostico + 150"),color="green4")+geom_line(aes(y=Pronostico,linetype="Pronostico"),color="indianred1")+geom_line(aes(y=dt,linetype="d(t)"),color="gray66")+geom_line(aes(y=rep(0,300)),color="black")+geom_line(aes(y=rep(1500,300)),color="black",linetype="dotted")+geom_line(aes(y=rep(1600,300)),color="black")+scale_y_continuous(breaks = seq(0,2500,100))+scale_color_manual(values=c("gold","gray46","chartreuse4"))+ylab("Stock(t)")+xlab("t")+xlim(c(0,330))+ggtitle(paste0("Sucursal ",sucsS[sucursal],": Simulacion de proceso de resurtido automatizado"))+labs(linetype="Linea")+scale_linetype_manual(values=c("longdash","dotdash","solid"))+theme(plot.title = element_text(size=12,hjust = 0.5)) 
      }
         }else{
    ggrafica<-ggplot(estadisticas,aes(x=seq(1:300),y=Stock, color=Semaforos))+geom_point(size=2)+geom_line(aes(y=PronosticoM,linetype="Pronostico + 150"),color="green4")+geom_line(aes(y=Pronostico,linetype="Pronostico"),color="indianred1")+geom_line(aes(y=dt,linetype="d(t)"),color="gray66")+geom_line(aes(y=rep(0,300)),color="black")+geom_line(aes(y=rep(1500,300)),color="black",linetype="dotted")+geom_line(aes(y=rep(1600,300)),color="black")+scale_y_continuous(breaks = seq(0,2500,100))+scale_color_manual(values=c("chartreuse4","chartreuse4","chartreuse4"))+ylab("Stock(t)")+xlab("t")+xlim(c(0,330))+ggtitle(paste0("Sucursal ",sucsS[sucursal],": Simulacion de proceso de resurtido automatizado"))+labs(linetype="Linea")+scale_linetype_manual(values=c("longdash","dotdash","solid"))+theme(plot.title = element_text(size=12,hjust = 0.5)) 
    }
  }
  return(ggrafica)
}






graficaproceso(1)

