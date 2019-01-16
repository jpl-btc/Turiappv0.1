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
testin("dplyr")
testin("tidyr")
testin("gsheet")
testin("ggplot2")

library(shiny) 
library(dplyr) 
library(tidyr) 
library(gsheet)


Tue1 <-gsheet2tbl('docs.google.com/spreadsheets/d/1NPsqPnZGQDIwfscXwx36ZWeiF6CCEAgquN5juL3zB44/edit?usp=sharing')
Tue2 <-gsheet2tbl('docs.google.com/spreadsheets/d/1NswTR9EO-bxNcWOFsOF8eM-GiulCtf6MBh48VGzF614/edit?usp=sharing')

Tue1y2 <- left_join(Tue1, Tue2, by = "Email Address")


#Otra forma
url1 <- 'docs.google.com/spreadsheets/d/1NPsqPnZGQDIwfscXwx36ZWeiF6CCEAgquN5juL3zB44/edit?usp=sharing'
a1 <- gsheet2text(url, format='csv')
b1 <- read.csv(a, stringsAsFactors=FALSE)
url2 <- 'docs.google.com/spreadsheets/d/1NswTR9EO-bxNcWOFsOF8eM-GiulCtf6MBh48VGzF614/edit?usp=sharing'
a2 <- gsheet2text(url, format='csv')
b2 <- read.csv(a, stringsAsFactors=FALSE)

b1yb2 <- left_join(b1, b2, by = "Email Address")


###########
# Promedios de PRE para cada individuo
media <- rowMeans(Tue1y2[,c(3:6)], na.rm=TRUE)    #CON
mediaPRECON <-as.data.frame(media)
mediaPRECON[["tipo"]] <- "1 CONOCIMIENTO"
mediaPRECON[["encuesta"]] <- "PREVIA"
mediaPRECON[["CategriaCA"]] <- "CONOCIMIENTO"
media <- rowMeans(Tue1y2[,c(7:15)], na.rm=TRUE)      #ACCIONES
mediaPREACC <-as.data.frame(media)
mediaPREACC[["tipo"]] <- "2 ACCIONES"
mediaPREACC[["encuesta"]] <- "PREVIA"
mediaPREACC[["CategriaCA"]] <- "ACCIONES"

# Promedios de ONL para cada individuo
media <- rowMeans(Tue1y2[,c(17:20)], na.rm=TRUE)      #CON
mediaONLCON <-as.data.frame(media)
mediaONLCON[["tipo"]] <- "3 CONOCIMIENTO"
mediaONLCON[["encuesta"]] <- "POSTERIOR"
mediaONLCON[["CategriaCA"]] <- "CON"
media <- rowMeans(Tue1y2[,c(21:29)], na.rm=TRUE)      #ACCIONES
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


