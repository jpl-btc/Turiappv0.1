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








#######################################################
################# INICIAR SERVIDORSHINY ###############
#######################################################

shinyServer(function(input, output) {
  
  
  
  #Filtra la tabla de datos en funcion de la seleccion de guia y ecoproducto
  dfInput <- reactive({

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
      
      output$infoBox1 <- renderInfoBox({
        x <- runif(1, 0, 10)
          #mean(subset(dfInput(), select = c(ACCDIF)))
        if(x < 6) {(color <- 'yellow')
          (title <-"Hay que esforzarse")
          (icon=icon("triangle-right", lib = "glyphicon")) 
        }
        else { (color <- 'green')
          (title <-"¡Muy bien!")
          (icon=icon("thumbs-up", lib = "glyphicon"))
        }
        infoBox(value = x,
                title = title,
                color = color,
                icon = icon,
                fill = TRUE)
      })
      output$infoBox2 <- renderInfoBox({
        x <- runif(1, 0, 10)
          #mean(subset(dfInput(), select = c(CONDIF)))
        
        if(x < 6) {(color <- 'yellow')
                  (title <-"Hay que esforzarse")
          (icon=icon("triangle-right", lib = "glyphicon")) 
        }
        else { (color <- 'green')
             (title <-"¡Muy bien!")
          (icon=icon("thumbs-up", lib = "glyphicon"))
        }
        infoBox(value = x,
                title = title,
                color = color,
                icon = icon,
                fill = TRUE)
      })

      
  ################ GRAFICO 3 ############################
  
  output$distPlot2 <- renderPlot({
    BoxmediaCA})
  

})