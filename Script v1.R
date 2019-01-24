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
testin("shiny")
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

Hours <- format(as.POSIXct(strptime(Tue1y2nona$Timestamp.x,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%H:%M:%S")
#output
Hours

Dates <- format(as.POSIXct(strptime(Tue1y2nona$Timestamp.x,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%Y-%m-%d")
Dates

Mes <- format(as.POSIXct(strptime(Tue1y2nona$Timestamp.x,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%Y-%m")

Tue1y2nona$Mes <- Mes
Tue1y2nona$Dates <- Dates
Tue1y2nona$Hours <- Hours
Tue1y2nona$CONPRE <- rowMeans(Tue1y2nona[,c(9:12)], na.rm=TRUE)
Tue1y2nona$ACCPRE <- rowMeans(Tue1y2nona[,c(13:22)], na.rm=TRUE) 
Tue1y2nona$CONONL <- rowMeans(Tue1y2nona[,c(29:32)], na.rm=TRUE) 
Tue1y2nona$ACCONL <- rowMeans(Tue1y2nona[,c(33:42)], na.rm=TRUE) 
Tue1y2nona$CONDIF <- Tue1y2nona$CONONL - Tue1y2nona$CONPRE
Tue1y2nona$ACCDIF <- Tue1y2nona$ACCONL - Tue1y2nona$ACCPRE
Tue1y2nona$GOOG.Open  <- Tue1y2nona$ACCPRE
Tue1y2nona$GOOG.High  <- Tue1y2nona$ACCONL +1
Tue1y2nona$GOOG.Low   <- Tue1y2nona$ACCPRE -2
Tue1y2nona$GOOG.Close <- Tue1y2nona$ACCONL

#Tue1y2nona3<-Tue1y2nona[5:6,]

#Tue1y2nona4 <-Tue1y2nona3 %>% remove_rownames %>% column_to_rownames(var="Dates")

 Tue1y2nona2 <-Tue1y2nona %>% remove_rownames %>% column_to_rownames(var="Timestamp.x")
x4 <-subset(Tue1y2nona2, select = c(GOOG.Open,GOOG.High,GOOG.Low,GOOG.Close),na.rm=TRUE)

x4$Timestamp.x <-Tue1y2nona$Timestamp.x
hchart(x4, "scatter",x = "Timestamp.x", y = -1:8)
hchart(x)


#testsheetfast
testsheetfast <-gsheet2tbl('https://docs.google.com/spreadsheets/d/1kgFsF0xDLmjMjIK4-uQxxTUAuKmpUTzrWFU2dqtTu_M/edit?usp=sharing') 

library(tidyverse)
library(timetk)
library(kableExtra)
library(highcharter)
library(PerformanceAnalytics)
library(tidyquant)
library(tibbletime)
library(tidyverse)


testsheetfast$date <- as.Date(testsheetfast$date)


testsheetfast2 <-testsheetfast %>% remove_rownames %>% column_to_rownames(var="date")

highchart(type = "stock") %>% 
  hc_add_series(testsheetfast2$returns, type = "column",
                color = "green")


ACC <-subset(Tue1y2nona, select = c(Dates,ACCDIF))
DiaACC <-aggregate(. ~Dates, data=ACC, mean, na.rm=TRUE)


################ GRAFICO 1 ############################
#Highchart CONCIENCIA AMBIENTAL MENSUAL Preparo los datos
ACC <-subset(Tue1y2nona, select = c(Mes,ACCDIF))
MesACC <-aggregate(. ~Mes, data=ACC, mean, na.rm=TRUE)
MesACC$ACCDIF <- lapply(MesACC$ACCDIF, round, 1)

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

#Highchart CONOCIMIENTO AMBIENTAL MENSUAL ACC
highchart() %>% 
  hc_chart(type = "column") %>% 
  hc_title(text = "Aumento del conocimiento ambiental") %>% 
  hc_subtitle(text = "De nuestros clientes un mes después de habernos visitado") %>% 
  hc_xAxis(categories = MesCON$Mes) %>% 
  hc_yAxis(title = list(text = "Puntaje en Conocimiento")) %>% 
  hc_plotOptions(line = list(
    dataLabels = list(enabled = TRUE),
    enableMouseTracking = FALSE)
  ) %>% 
  hc_series(
    list(
      name = "Conocimiento de problemáticas ambientales",
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






###########ZONA DE TEST###############
x <- getSymbols("YHOO", auto.assign = TUE)
hchart(x, type= "stock")
library("quantmod")
x <- getSymbols(Tue1y2nona, auto.assign = TRUE)
hchart(x)
NUmediaONLCON <-subset(mediaPRECON, select = c(1))
NUmediaPRECON <-subset(mediaPRECON, select = c(1))
y <-  NUmediaONLCON - NUmediaPRECON
x <- getSymbols(y, auto.assign = TRUE)
library(mtcars)
x <- getSymbols("GOOG", auto.assign = FALSE)
hchart(x)

