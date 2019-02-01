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
testin("shinyBS")

# que ondaaa

library(shiny) 
library(gsheet)
library(tidyverse)
library(log4r)
library(ggplot2)
library(highcharter)
library(shinyBS) 




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



#  output$teststock <- renderHighchart({
#    getSymbols("GOOG", auto.assign = FALSE) %>% 
#       hchart})
################ GRAFICO 3 y 4 testeo ############################







