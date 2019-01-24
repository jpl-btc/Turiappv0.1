#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(log4r)
loggerDebug <- create.logger()
logfile(loggerDebug) <- 'data/debugData.log'
level(loggerDebug) <- 'INFO'

loggerServer <- create.logger()
logfile(loggerServer) <- 'data/serverData.log'
level(loggerServer) <- 'INFO'

# examples of levels
# debug(logger, 'A Debugging Message') # Won't print anything
# info(logger, 'An Info Message')
# warn(logger, 'A Warning Message')
# error(logger, 'An Error Message')
# fatal(logger, 'A Fatal Error Message')

library(shiny) 
library(tidyverse)
library(gsheet)
library(ggplot2)


Tue1 <-gsheet2tbl('docs.google.com/spreadsheets/d/1kd84-Rn1HKYF9DJ2pdRPbZYmqC2MD_JjM95by7RFFik/edit?usp=sharing')
Tue2 <-gsheet2tbl('docs.google.com/spreadsheets/d/1luumPYYc6r0JJEMJ3xC4mNA1GUox0GTKoPgKgbrGyno/edit?usp=sharing')

Tue1y2 <- left_join(Tue1, Tue2, by = "Email Address")
Tue1y2nona <- na.omit(Tue1y2)




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


################ GRAFICO 1 ############################
#Highchart CONCIENCIA AMBIENTAL MENSUAL Preparo los datos
ACC <-subset(Tue1y2nona, select = c(Mes,ACCDIF))
MesACC <-aggregate(. ~Mes, data=ACC, mean, na.rm=TRUE)
MesACC$ACCDIF <- lapply(MesACC$ACCDIF, round, 1)
################ GRAFICO 1 ############################

################ GRAFICO 2 ############################
#Highchart CONOCIMIENTO AMBIENTAL MENSUAL Preparo los datos
CON <-subset(Tue1y2nona, select = c(Mes,CONDIF))
MesCON <-aggregate(. ~Mes, data=CON, mean, na.rm=TRUE)
MesCON$CONDIF <- lapply(MesCON$CONDIF, round, 1)
################ GRAFICO 2 ############################





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


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$distPlot2 <- renderPlot({
      BoxmediaCA})
  
  output$teststock <- renderHighchart({
      getSymbols("GOOG", auto.assign = FALSE) %>% 
      hchart})
  
  output$accionesmes <- renderHighchart({
    #Highchart CONCIENCIA AMBIENTAL MENSUAL ACC
    highchart() %>% 
      hc_chart(type = "column") %>% 
      hc_title(text = "Aumento de acciones a favor del medio ambiente") %>% 
      hc_subtitle(text = "De las personas guiadas un mes despues de habernos conocido") %>% 
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
    })
  
  output$conocimientomes <- renderHighchart({
    highchart() %>% 
      hc_chart(type = "column") %>% 
      hc_title(text = "Aumento del conocimiento ambiental") %>% 
      hc_subtitle(text = "De las personas guiadas un mes despues de habernos conocido") %>% 
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
  })
  
})
