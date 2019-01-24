#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinydashboard)
library(highcharter)
library(quantmod)

dashboardPage(
  dashboardHeader(title="TURI APP"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("General", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Antes vs Despues", tabName = "avd", icon = icon("th")),
      menuItem("Individuos", tabName = "individuos", icon = icon("th")),
      menuItem("Preguntas", tabName = "preguntas", icon = icon("th")),
      menuItem("Top Guias", tabName = "topguia", icon = icon("th")),
      menuItem("Global", tabName = "global", icon = icon("th")),
      menuItem("Acerca de", tabName = "acercade", icon = icon("info"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              h2("El aumento de conciencia ambiental de las personas que nos conocieron"),
              
              fluidRow(
                infoBox("New Orders", 10 * 2, icon = icon("credit-card"), fill = TRUE),
                infoBoxOutput("approvalBox2"),
                infoBox("LALA", 10 * 2, icon = icon("credit-card"), fill = TRUE)
              ),
              
              fluidRow(
                box(title="Acciones", status = "primary", solidHeader = TRUE,
                    highchartOutput("accionesmes")),
                
                box(title="Conocimiento", status = "primary", solidHeader = TRUE,
                    highchartOutput("conocimientomes")),
                
                box(title = "Controles", status = "primary", solidHeader = TRUE,
                    sliderInput("Excursion", "Tipo de producto ecoturistico:",
                                min = 1,
                                max = 50,
                                value = 30),
                    sliderInput("Guia", "Nombre de la persona que realizo la guiada, charla e interpretacion de fauna",
                                min = 1,
                                max = 50,
                                value = 30)
                    ),
                box(title = "Controles", status = "primary", solidHeader = TRUE,
                    sliderInput("Excursion", "Tipo de producto ecoturistico:",
                                min = 1,
                                max = 50,
                                value = 30),
                    sliderInput("Guia", "Nombre de la persona que realizo la guiada, charla e interpretacion de fauna",
                                min = 1,
                                max = 50,
                                value = 30)
                )
                
              )
      ),
      tabItem(tabName = "avd",
              h2("Detalle de las encuestas previas y online"),
              h4("Visualizacion de conciencia ambiental en la encuesta que hacen en su celular antes de la excursion y la posterior"),
              h3("Lanzamiento en Abril 2019"),
              h4("Vista previa aproximada:"),
              
              fluidRow(
                box(plotOutput("distPlot2")))
      ),
      tabItem(tabName = "individuos",
              h2("Lo que pasa exactamente en cada individuo particular"),
              h4("Detalle del aumento de conciencia ambiental en cada cliente particular"),
              h3("Lanzamiento en Junio 2019")
      ),
      tabItem(tabName = "preguntas",
              h2("Cada accion en concreto"),
              h4("Se va poder ver cual es el cambio en particular de cada pregunta"),
              h3("Lanzamiento en Agosto 2019")
      ),
      tabItem(tabName = "topguia",
              h2("Lista de los guias mas concientizadores"),
              h4("Ubicacion en una tabla de posiciones de los guias mas concientizadores de todos los que participan del proyecto"),
              h3("Lanzamiento en Octubre 2019")
      ),
      tabItem(tabName = "global",
              h2("Detalle en un mapa de la conciencia ambiental"),
              h4("Conciencia ambiental generada por todos los participantes de este proyecto"),
              h3("Lanzamiento en Diciembre 2019")
      ),
      tabItem(tabName = "acercade",
              h2("Acerca de esta aplicacion"),
              h4("Desde ya muchas gracias por apoyar el proyecto y estar aca. Esto va ir mejorando con los meses y an(i)os.
                 La paciencia apremia. Por ahora veran que tengo un problema con los acentos y en(i)es, mil disculpas, 
                 pero por ahora los acentos no son prioridad y es un error que requiere tiempo."),
              h4("Cualquier duda, necesidad, pregunta, o problema, mandenme un email a jplaclau@turi.eco les respondere a la brevedad.
                 O pueden usar el sistema de contacto de este mismo sitio web."),
              br(),
              h3("Por un ecoturismo generador de mas conciencia ambiental,"),
              h4("A su disposicion,"),
              h4("Jean-Paul Laclau"),
              h5("jplaclau@turi.eco")
              
              )
      )
  )
  )
    



