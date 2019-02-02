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
library(latexpdf) 


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
    x <- 50
    #mean(subset(dfInput(), select = c(ACCDIF)))
    if(x < 30) {(color <- 'yellow')
      (title <-"Hay que esforzarse")
      (icon=icon("triangle-right", lib = "glyphicon")) 
    }
    else { (color <- 'green')
      (title <-"En total")
      (icon=icon("thumbs-up", lib = "glyphicon"))
    }
    infoBox(value = paste0(x , "%"),
            title = title,
            color = color,
            icon = icon,
            fill = TRUE)
  })
  
  output$infoBox2 <- renderInfoBox({
    xmes <- lapply((runif(1, 0, 100)), round, 0)
    #mean(subset(dfInput(), select = c(CONDIF)))
    
    if(xmes < 50) {(color <- 'yellow') 
      (title <-"Último mes")
      (icon=icon("triangle-right", lib = "glyphicon")) 
    }
    else { (color <- 'green')
      (title <-"¡Muy bien!")
      (icon=icon("thumbs-up", lib = "glyphicon"))
    }
    infoBox(value = paste0(xmes , "%"),
            title = title,
            color = color,
            icon = icon,
            fill = TRUE)
  })
  
  output$infoBox3 <- renderInfoBox({
    infoBox(value = paste0( "Descargar"),
            title = "Certificado",
            color = "blue",
            icon =  icon("award"),
            fill = TRUE)
  })
  
  output$report <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      
      src <- normalizePath('report.Rmd')

      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(n = input$guia,input$productoeco)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  ################ GRAFICO 3 ############################
  
  output$distPlot2 <- renderPlot({
    BoxmediaCA})
  
  
})