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

dashboardPage(
  dashboardHeader(title="TURI APP"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("General", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              
              fluidRow(
                box(plotOutput("distPlot2")),
                box(plotOutput("distPlot")),
                box(
                  title = "Controls",
                  sliderInput("bins", "Number of bins:",
                              min = 1,
                              max = 50,
                              value = 30)
                )
              )
      ),
      
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
    
    
  )
)

