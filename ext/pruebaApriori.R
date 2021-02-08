rm(list=ls())
# Load packages
library(quantmod)
library(arules)

from <- Sys.Date() - 100
to <- Sys.Date()
companies <- c("GGAL.BA","YPFD.BA", "ALUA.BA", "SUPV.BA", 
               "BBAR.BA" ,"COME.BA", "PAMP.BA","BYMA.BA", 
               "BMA.BA","TECO2.BA", "CEPU.BA", "CRES.BA", 
               "CVH.BA","MIRG.BA", "EDN.BA", "TGSU2.BA",
               "TGNO4.BA","TRAN.BA","TXAR.BA","VALO.BA")
# PARA OPERAR CON VARIAS ACCIONES
#saco el close de todas omitiendo los nan's
acciones <- lapply(companies, function(sym) {
  Cl(getSymbols(sym,auto.assign = FALSE, from=from, to=to))
  })
df <- do.call(merge, acciones)
df <- na.omit(df)
############################
# Calculo CCL via GAL
getSymbols("GGAL")
df$ccl <- df$GGAL.BA.Close/GGAL$GGAL.Close * 10

## transformo el set en subas y bajas 

# SUBAS
# precio de hoy mayor que el de ayer? -> suba
df_suba <- lapply(df, function(x) {lag(x,-1) > x})
df_suba <- do.call(merge,df_suba)
# renombro columnas para mayor claridad en las reglas
df_suba <- lapply(df_suba, function(x) {
  nom <- substr(names(x),1,4)
  setNames(x,paste(nom,".SUBE",sep = "")) 
})
df_suba <- do.call(merge,df_suba)

#BAJAS
# el df de baja es el contrario de suba
df_baja <- !df_suba
# renombro columnas
df_baja <- lapply(df_baja, function(x) {
  nom <- substr(names(x),1,4)
  setNames(x,paste(nom,".BAJA",sep = "")) 
})
df_baja <- do.call(merge,df_baja)

df_final_xts <- merge(df_suba,df_baja)


df_final<-cbind.data.frame(df_final_xts)


rules <- apriori(df_final, parameter = list(supp = 0.6, conf = 0.8))
rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules)

