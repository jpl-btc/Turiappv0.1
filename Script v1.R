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
testin("")
testin("shiny")



library(shiny) 
library(gsheet)
library(tidyverse)
library(log4r)
library(ggplot2)
library(highcharter)


Tue1 <-gsheet2tbl('docs.google.com/spreadsheets/d/1kd84-Rn1HKYF9DJ2pdRPbZYmqC2MD_JjM95by7RFFik/edit?usp=sharing')
Tue2 <-gsheet2tbl('docs.google.com/spreadsheets/d/1luumPYYc6r0JJEMJ3xC4mNA1GUox0GTKoPgKgbrGyno/edit?usp=sharing')

Tue1y2 <- left_join(Tue1, Tue2, by = "Email Address")
Tue1y2nona <- na.omit(Tue1y2)

################ FECHAS ############################
date <- format(as.Date.POSIXct(as.numeric(strptime(Tue1y2nona$Timestamp.x,"%m/%d/%Y %H:%M:%S",tz="GMT")) ,format = "%Y-%m-%d", origin="2019-01-01 00:00:00"))
Mes <- format(as.Date.POSIXct(as.numeric(strptime(Tue1y2nona$Timestamp.x,"%m/%d/%Y %H:%M:%S",tz="GMT")) ,format = "%Y-%m", origin="2019-01-01 00:00:00"))
Tue1y2nona$Mes <- Mes
Tue1y2nona$date <- date
################ FECHAS ############################

################ las DIF ############################
Tue1y2nona$CONPRE <- rowMeans(Tue1y2nona[,c(9:12)], na.rm=TRUE)
Tue1y2nona$ACCPRE <- rowMeans(Tue1y2nona[,c(13:22)], na.rm=TRUE) 
Tue1y2nona$CONONL <- rowMeans(Tue1y2nona[,c(29:32)], na.rm=TRUE) 
Tue1y2nona$ACCONL <- rowMeans(Tue1y2nona[,c(33:42)], na.rm=TRUE) 
Tue1y2nona$CONDIF <- Tue1y2nona$CONONL - Tue1y2nona$CONPRE
Tue1y2nona$ACCDIF <- Tue1y2nona$ACCONL - Tue1y2nona$ACCPRE
################ las DIF ############################



################ GRAFICO 1 ############################
#Highchart CONCIENCIA AMBIENTAL MENSUAL Preparo los datos
ACC <-subset(Tue1y2nona, select = c(Mes,ACCDIF))
MesACC <-aggregate(. ~Mes, data=ACC, mean, na.rm=TRUE)
MesACC$ACCDIF <- lapply(MesACC$ACCDIF, round, 1)
################ GRAFICO 1 ############################

#Highchart CONCIENCIA AMBIENTAL MENSUAL ACC
highchart() %>% 
  hc_chart(type = "column") %>% 
  hc_title(text = "Aumento de acciones a favor del medio ambiente") %>% 
  hc_subtitle(text = "De nuestros clientes un mes después de habernos visitado") %>% 
  hc_xAxis(categories = MesACC$Mes) %>% 
  hc_yAxis(title = list(text = "Puntaje en Conciencia ambiental")) %>% 
  hc_plotOptions(line = list(
    dataLabels = list(enabled = TRUE),
    enableMouseTracking = FALSE)
  ) %>% 
  hc_series(
    list(
      name = "Conciencia ambiental",
      data = MesACC$ACCDIF,
      color = "green")
  )

########################################################

################ GRAFICO 2 ############################
#Highchart CONOCIMIENTO AMBIENTAL MENSUAL Preparo los datos
CON <-subset(Tue1y2nona, select = c(Mes,CONDIF))
MesCON <-aggregate(. ~Mes, data=CON, mean, na.rm=TRUE)
MesCON$CONDIF <- lapply(MesCON$CONDIF, round, 1)
################ GRAFICO 2 ############################

#Highchart CONOCIMIENTO AMBIENTAL MENSUAL ACC
highchart() %>% 
  hc_chart(type = "column") %>% 
  hc_title(text = "Aumento del conocimiento ambiental") %>% 
  hc_subtitle(text = "De nuestros clientes un mes despues de habernos visitado") %>% 
  hc_xAxis(categories = MesCON$Mes) %>% 
  hc_yAxis(title = list(text = "Puntaje en Conocimiento")) %>% 
  hc_plotOptions(line = list(
    dataLabels = list(enabled = TRUE),
    enableMouseTracking = FALSE)
  ) %>% 
  hc_series(
    list(
      name = "Conocimiento de problematicas ambientales",
      data = MesCON$CONDIF,
      color = "BLUE")
  )
########################################################


###########
###########
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

library(ggplot2)
mediaCA <- rbind(mediaPRECON,mediaPREACC,mediaONLCON,mediaONLACC)
BoxmediaCA <- ggplot(mediaCA, aes(x=tipo, y=media,fill=encuesta)) +
  geom_boxplot() +
  theme(legend.position="top")
BoxmediaCA  # Graficos de cajas final PRE vs ONL





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



########## Testboxplot #############################################
hcboxplot(x = diamonds$x, var = diamonds$color, var2 = diamonds$cut,
          outliers = FALSE) %>% 
  hc_chart(type = "column") # to put box vertical

##hay que juntar y reordenar la datatable
hcboxplot(x = diamonds$x,#esta es la difcon#
          var = diamonds$AntesvsDespues, var2 = diamonds$conocimientooacciones,
          outliers = FALSE) %>% 
  hc_chart(type = "column") # to put box vertical
###################################################




########## hicharts stock##########################
x <- getSymbols("GOOG", auto.assign = FALSE)
hchart(x)
###############
###################################################
# testing el puto stock chart xts ohlc
########

library(xts)
xts1 <- xts(x=1:10, order.by=Sys.Date()-1:10)
data <- rnorm(5)
dates <- seq(as.Date("2017-05-01"), length=5, by="days")
xts2 <- xts(x=data, order.by=dates)
xts3 <- xts(x=rnorm(10), order.by=as.POSIXct(Sys.Date()+1:10), born=as.POSIXct("1899-05-08"),
            xts4 <- xts(x=1:10, order.by=Sys.Date()+1:10))
data(AirPassengers)
xts5 <- as.xts(AirPassengers)

core_data <- coredata(xts2)
index(xts1)
indexClass(xts2)
indexClass(convertIndex(xts,'POSIXct'))
indexTZ(xts5)
indexFormat(xts5) <- "%Y-%m-%d"
periodicity(xts5)
to.monthly(xts5)
hchart(xts3)
hchart(xts5, type= "stock")

x4 <-subset(Tue1y2nona, select = c(date,ACCDIF),na.rm=TRUE)
x5 <-as.Date("05-30-16", "%m-%d-%y")
xx <- to.daily(x4,drop.time=TRUE,name)
xx <- to.period(x4,period = "months", k = 1, indexAt, name=NULL, OHLC = TRUE)
Tue1y2nona2 <-Tue1y2nona %>% remove_rownames %>% column_to_rownames(var="Timestamp.x")
x4 <-subset(Tue1y2nona2, select = c(GOOG.Open,GOOG.High,GOOG.Low,GOOG.Close),na.rm=TRUE)
x4$Timestamp.x <-Tue1y2nona$Timestamp.x
hchart(x4, "scatter",x = "Timestamp.x", y = -1:8)
hchart(x)

testsheetfast$date <- as.Date(testsheetfast$date)
testsheetfast2 <-testsheetfast %>% remove_rownames %>% column_to_rownames(var="date")
highchart(type = "stock") %>% 
  hc_add_series(testsheetfast2$returns, type = "column",
                color = "green")
ACC <-subset(Tue1y2nona, select = c(Dates,ACCDIF))
DiaACC <-aggregate(. ~Dates, data=ACC, mean, na.rm=TRUE)

######################################################################################################
######################################################################################################
######################################################################################################

