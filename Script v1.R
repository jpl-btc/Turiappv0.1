#################################
#                               #
# La union de las dos encuestas #
#                               #
#################################


# setwd("C:/JPL/Dropbox/TURIECO/Turiappv0.1")


testin <- function(package){
  if (!package %in% installed.packages()) {   
    message( "Instalando ", package)
    install.packages(package)
    print("paquete instalado")}
  else {
    message(" El paquete ", package, " ya se encontraba instalado.")}
}
testin("shiny")
testin("gsheet")
testin("ggplot2")
testin("log4r")
testin("tidyverse")
testin("scales")
testin("colorspace")
testin("shinydashboard")
testin("highcharter")
testin("PerformanceAnalytics")
testin("timetk")
testin("kableExtra")
testin("tidyquant")
testin("tibbletime")
testin("shinyWidgets")
testin("")

# que ondaaa

library(shiny) 
library(gsheet)
library(tidyverse)
library(log4r)
library(ggplot2)
library(highcharter)




######################################################################################################
###########ZONA DE TEST##################################################################
######################################################################################################
library(quantmod)
library(tidyverse)
library(timetk)
library(kableExtra)
library(highcharter)
library(PerformanceAnalytics)
library(tidyquant)
library(tibbletime)
library(tidyverse)

#testsheetfast
testsheetfast <-gsheet2tbl('https://docs.google.com/spreadsheets/d/1kgFsF0xDLmjMjIK4-uQxxTUAuKmpUTzrWFU2dqtTu_M/edit?usp=sharing') 



######################################################################################################
######################################################################################################
######################################################################################################
#CEMENTERIO
###############


################ GRAFICO 3 y 4 testeo ############################
#######################################################
########### ANTES VS DESPUES #########################
#######################################################

################ GRAFICO 3 ############################
# Promedios de PRE para cada individuo
media <- rowMeans(Tue1y2nona[,c(9:12)], na.rm=TRUE)    #CON
mediaPRECON <-as.data.frame(media)
mediaPRECON[["tipo"]] <- "1 CONOCIMIENTO"
mediaPRECON[["encuesta"]] <- "PREVIA"
mediaPRECON[["CategriaCA"]] <- "CONOCIMIENTO"
media <- rowMeans(Tue1y2nona[,c(13:22)], na.rm=TRUE)      #ACCIONES
mediaPREACC <-as.data.frame(media)
mediaPREACC[["tipo"]] <- "2 ACCIONES"
mediaPREACC[["encuesta"]] <- "PREVIA"
mediaPREACC[["CategriaCA"]] <- "ACCIONES"
# Promedios de ONL para cada individuo
media <- rowMeans(Tue1y2nona[,c(29:32)], na.rm=TRUE)      #CON
mediaONLCON <-as.data.frame(media)
mediaONLCON[["tipo"]] <- "3 CONOCIMIENTO"
mediaONLCON[["encuesta"]] <- "POSTERIOR"
mediaONLCON[["CategriaCA"]] <- "CON"
media <- rowMeans(Tue1y2nona[,c(33:42)], na.rm=TRUE)      #ACCIONES
mediaONLACC <-as.data.frame(media)
mediaONLACC[["tipo"]] <- "4 ACCIONES"
mediaONLACC[["encuesta"]] <- "POSTERIOR"
mediaONLACC[["CategriaCA"]] <- "ACCIONES"
mediaCA <- rbind(mediaPRECON,mediaPREACC,mediaONLCON,mediaONLACC)
BoxmediaCA <- ggplot(mediaCA, aes(x=tipo, y=media,fill=encuesta)) +
  geom_boxplot() +
  theme(legend.position="top")
################ GRAFICO 3 ############################

output$distPlot2 <- renderPlot({
  BoxmediaCA})

#  output$teststock <- renderHighchart({
#    getSymbols("GOOG", auto.assign = FALSE) %>% 
#       hchart})
################ GRAFICO 3 y 4 testeo ############################




################ GET DATA ############################
Tue1 <-gsheet2tbl('docs.google.com/spreadsheets/d/1kd84-Rn1HKYF9DJ2pdRPbZYmqC2MD_JjM95by7RFFik/edit?usp=sharing')
Tue2 <-gsheet2tbl('docs.google.com/spreadsheets/d/1luumPYYc6r0JJEMJ3xC4mNA1GUox0GTKoPgKgbrGyno/edit?usp=sharing')

Tue1y2 <- left_join(Tue1, Tue2, by = "Email Address")
Tue1y2nona <- na.omit(Tue1y2)
################ GET DATA ############################


################ FECHAS ############################
Hours <- format(as.POSIXct(strptime(Tue1y2nona$Timestamp.x,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%H:%M:%S")
Dates <- format(as.POSIXct(strptime(Tue1y2nona$Timestamp.x,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%Y-%m-%d")
Mes <- format(as.POSIXct(strptime(Tue1y2nona$Timestamp.x,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%Y-%m")
Tue1y2nona$Mes <- Mes
Tue1y2nona$Dates <- Dates
Tue1y2nona$Hours <- Hours
################ FECHAS ############################


################ preparacion GRAFICOS Hichart  ############################
Tue1y2nona$CONPRE <- rowMeans(Tue1y2nona[,c(9:12)], na.rm=TRUE)
Tue1y2nona$ACCPRE <- rowMeans(Tue1y2nona[,c(13:22)], na.rm=TRUE) 
Tue1y2nona$CONONL <- rowMeans(Tue1y2nona[,c(29:32)], na.rm=TRUE) 
Tue1y2nona$ACCONL <- rowMeans(Tue1y2nona[,c(33:42)], na.rm=TRUE) 
Tue1y2nona$CONDIF <- Tue1y2nona$CONONL - Tue1y2nona$CONPRE
Tue1y2nona$ACCDIF <- Tue1y2nona$ACCONL - Tue1y2nona$ACCPRE
################ preparacion GRAFICOS Hichart ############################




