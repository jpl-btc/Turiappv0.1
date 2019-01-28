#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

options(encoding = "UTF-8")


library(shiny)
library(shinydashboard)
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














#######################################################
################# INICIAR SERVIDORSHINY ###############
#######################################################

shinyServer(function(input, output) {
  
  #Filtra la tabla de datos en funcion de la seleccion de guia y ecoproducto


  dfInput <- reactive({
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
    
    if (input$guia=="Todos" & input$productoeco=="Todos") {
      Tue1y2nona }
    
    else if (input$guia =="Todos" & !input$productoeco=="Todos") {
      filter(Tue1y2nona,
             Tue1y2nona$`¿Cuál es el tipo de actividad que va a hacer?` == input$productoeco)}
    
    else if (!input$guia =="Todos" & input$productoeco=="Todos") {
      filter(Tue1y2nona,
             Tue1y2nona$`¿Cuál es el nombre de su guía?` == input$guia)}
    
    else if (!input$guia =="Todos" & !input$productoeco=="Todos") {
      filter(Tue1y2nona,
             Tue1y2nona$`¿Cuál es el nombre de su guía?` == input$guia &
               Tue1y2nona$`¿Cuál es el tipo de actividad que va a hacer?` == input$productoeco)}
    
  })

  

  
  #Highchart CONCIENCIA AMBIENTAL MENSUAL ACC
    output$accionesmes <- renderHighchart({
    ################ GRAFICO 1 ############################
    #Highchart CONCIENCIA AMBIENTAL MENSUAL Preparo los datos
    ACC <-subset(dfInput(), select = c(Mes,ACCDIF))
    MesACC <-aggregate(. ~Mes, data=ACC, mean, na.rm=TRUE)
    MesACC$ACCDIF <- lapply(MesACC$ACCDIF, round, 1)
    ################ GRAFICO 1 ############################
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
  
    #Highchart CONCIENCIA AMBIENTAL MENSUAL CON
      output$conocimientomes <- renderHighchart({
    ################ GRAFICO 2 ############################
    #Highchart CONOCIMIENTO AMBIENTAL MENSUAL Preparo los datos
    CON <-subset(dfInput(), select = c(Mes,CONDIF))
    MesCON <-aggregate(. ~Mes, data=CON, mean, na.rm=TRUE)
    MesCON$CONDIF <- lapply(MesCON$CONDIF, round, 1)
    ################ GRAFICO 2 ############################
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
  output$approvalBoxtodo <- renderInfoBox({
    infoBox(
      "Siempre", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "green", fill = TRUE
    )
  })
  output$approvalBoxmespasado <- renderInfoBox({
    infoBox(
      "Mes pasado", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "green", fill = TRUE
    )
  })
  

})