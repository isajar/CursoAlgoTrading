rm(list=ls())
library(RJSONIO)
suppressMessages(library(Rbitcoin))
suppressMessages(library(quantmod))

# variables generales
cDir <- "c:\\CTA\\"
url <- "https://www.bitstamp.net/api/transactions/"
tiempoSleep <- 15
periodoRSI <- 14
repeticiones <- 1500
capitalinicial <- 50000
# fin variables

cantidadDatos <- 0

horainicio = Sys.time()
totsegundos <- (tiempoSleep*repeticiones)
totmin <- totsegundos/60
tothoras <- totsegundos/3600
horafinal <- Sys.time()+totsegundos
print(paste("Hora de finalización (aprox.):",format(horafinal,"%d/%m/%Y %H:%M:%S")))


Sys.sleep(10)

## crea archivos que faltan
tmpdf <- data.frame(capitalinicial)
if(!file.exists(paste(cDir,"actual_capital.txt", sep=""))){
  write.table(tmpdf,file=paste(cDir,"actual_capital.txt", sep=""),sep=",", row.names=F, col.names=F, append=T)
}
## en este archivo se van a almacenar la cantidad de BTC en stock
tmpdf <- data.frame(0)
if(!file.exists(paste(cDir,"total_btc.txt", sep=""))){
  write.table(tmpdf,file=paste(cDir,"total_btc.txt", sep=""),sep=",", row.names=F, col.names=F, append=T)
}

bs_data <- fromJSON(url) # returns a list
bs_df <- do.call(rbind,lapply(bs_data,data.frame,stringsAsFactors=FALSE))
## se ordena lo que viene según la fecha y de manera ascendente
bs_df <- bs_df[order(bs_df$date),c(1:5)]
## se toman los últimos 15 datos históricos para no esperar valores para el RSI
btc_last <- as.numeric(head(bs_df,15)$price)

## se calcula y formatea la fecha del precio
fecha_last <- format(ISOdate(1970,1,1)+as.numeric(head(bs_df,15)$date), "%d/%m/%y")
hora_last <- format(ISOdate(1970,1,1)+as.numeric(head(bs_df,15)$date), "%H:%M:%S")


tmpdf <- data.frame(fecha = fecha_last, hora = hora_last, Last = btc_last)
## si no existe el archivo, se agregan los nombres de columna
colnames <- FALSE
if(!file.exists(paste(cDir,"btc_last.txt", sep=""))){
  colnames <- TRUE
}
## se crea el archivo donde se almacenan los precios con los encabezados
names(tmpdf) <- c("Fecha","Hora","Last")
write.table(tmpdf,file=paste(cDir,"btc_last.txt", sep=""),sep=",", row.names=F, col.names=colnames, append=T)

## variable para repetición

for(b in 1:repeticiones){
  
  ####
  
  # se modificó porque hay problemas con el protocolo
  #btc_last <- market.api.process(market = 'bitstamp', currency_pair = c('BTC', 'USD'), action='ticker')
  bs_data <- fromJSON(url) # returns a list
  #posible filtro por error
  #if(class(bs_data==list)){}
  bs_df <- do.call(rbind,lapply(bs_data,data.frame,stringsAsFactors=FALSE))
  btc_last <- as.numeric(head(bs_df,1)$price)
  fecha_last <- format(ISOdate(1970,1,1)+as.numeric(head(bs_df,1)$date), "%d/%m/%y")
  hora_last <- format(ISOdate(1970,1,1)+as.numeric(head(bs_df,1)$date), "%H:%M:%S")
  
  tmpdf <- data.frame(fecha = fecha_last, hora = hora_last, Last = btc_last)
  write.table(tmpdf,file=paste(cDir,"btc_last.txt", sep=""),sep=",", row.names=F, col.names=F, append=T)
  
    
    ####
    # trae el histórico del lasta para calcular el RSI
    btc_hist <- read.csv(paste(cDir,"btc_last.txt", sep=""),sep=",", header=T)
    print(btc_hist)
    #names(btc_hist) <- c("Market","Base","Quote","TimeStamp","Marke_Time","Last","VWAP","Vol","ASK","BID")
    #names(btc_hist) <- "Last"
    last_price_btc <- tail(btc_hist$Last,1)
    cantidadDatos <- nrow(btc_hist)
    if(cantidadDatos >= 15){
      rsi_14_btc <- round(RSI(btc_hist$Last,periodoRSI),3)
      last_rsi_14_btc <- tail(rsi_14_btc,1)
      ##### DEFINO CAPITAL INICIAL
      actual_capital <- tail(read.csv(paste(cDir,"actual_capital.txt",sep=""),sep=",",header=F),1)
      total_btc <- tail(read.csv(paste(cDir,"total_btc.txt",sep=""),sep=",",header=F),1)
      ###############################
      
      ##################################
      ########## ACA COMIENZA EL ALGORITMO PROPIAMENTE DICHO
      #######################################################
      
      ## 0.1*last price porque es lo mínimo con que puedo comprar bitcon
      if((last_rsi_14_btc < 20) & (actual_capital > (0.1*last_price_btc))) {
        op_btc <- paste(Sys.time(),"BUY 0.1 BTC at:", last_price_btc)	
        write.table(op_btc,file=paste(cDir,"btc_trades.txt", sep=""),sep=",", row.names=F, col.names=F, append=T)
        actual_capital <- actual_capital - (0.1 * last_price_btc)
        total_btc <- (total_btc) + 0.1
        write.table(actual_capital,file=paste(cDir,"actual_capital.txt", sep=""),sep=",", row.names=F, col.names=F, append=T)
        write.table(total_btc,file=paste(cDir,"total_btc.txt", sep=""),sep=",", row.names=F, col.names=F, append=T)
        
        print("BUYING 0.1 BTC")
        
      } else {
        
        if((last_rsi_14_btc > 80) & (total_btc >= 0.1)) {
          
          op_btc <- paste(Sys.time(),"SELL 0.1 BTC at:", last_price_btc)	
          
          #ENVIO AL MERCADO
          #RECIBO CONFIRMACION
          
          write.table(op_btc,file=paste(cDir,"btc_trades.txt", sep=""),sep=",", row.names=F, col.names=F, append=T)
          actual_capital <- actual_capital + (0.1 * last_price_btc)
          total_btc <- (as.numeric(total_btc) - 0.1)
          
          write.table(actual_capital,file=paste(cDir,"actual_capital.txt", sep=""),sep=",", row.names=F, col.names=F, append=T)
          write.table(total_btc,file=paste(cDir,"total_btc.txt", sep=""),sep=",", row.names=F, col.names=F, append=T)
          
          print("SELLING 0.1 BTC")
          
        } else {
          print("***************")
          print("DO NOTHING !!! ")
          print("")
        }
      }
      
      print(paste("Estamos Trabajando!!!",Sys.time()))
      print(paste("Actual Capital: ",actual_capital))
      print(paste("BTC Amount: ", total_btc))
      print(paste("Last RSI: ", last_rsi_14_btc))
      print(paste("INIT CAPITAL:  104390"))
      print(paste("TOTAL CAPITAL:",actual_capital+(total_btc*last_price_btc)))
      print("")
      print(paste("Hora de finalización (aprox.):",format(horafinal,"%d/%m/%Y %H:%M:%S")))
      print("***************")
    
    }else{
      print(paste("valores que faltan para calcular RSI:", 14 - cantidadDatos))
    }
  
  Sys.sleep(tiempoSleep)
  
}
