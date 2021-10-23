#############################################################################################################################################
# 0  - Librerias 
#############################################################################################################################################
rm(list=ls())
library(rvest)
library(tidyr)
library(beepr)
library(ggplot2)

#############################################################################################################################################
# 1- web scraping
#############################################################################################################################################
# url de la web que dice la fecha de nacimiento de famosos----------------------------------------------------------------------------------
url <- "https://es.famousbirthdays.com/profession/"
web <- read_html(url)

# obtenemos los enlaces de todas las categorias que tienen
url_tematic <- web %>% html_nodes(".group-item") %>% html_attr('href')

# sacamos los enlaces de todas las perosnas que tienen por cada tematica----------------------------------------------------------------------------------
web_tematic <- read_html(url_tematic[1])
url_tematic_person <- web_tematic %>% html_nodes(".clearfix") %>% html_attr('href')

# bucle para el resto
for (i in 2:48){
  web_tematic <- read_html(url_tematic[i])
  url_i <- web_tematic %>% html_nodes(".clearfix") %>% html_attr('href')
  url_tematic_person <- c(url_tematic_person, url_i )
}
# quitamos posible duplicados
url_tematic_person <- unique(url_tematic_person)

# sacamos la información de cada persona ----------------------------------------------------------------------------------

# se crea un df para llenarlo con los datos del bucle
df <- data.frame()

# bucle para sacar los datos
for (i in 1:2522){
  
  web_persona <- read_html(url_tematic_person[i])
  
  info <-  html_nodes(web_persona,'.col-sm-12')%>% html_text()
  info <- gsub("\n", "", info)
  info <- info[5]
  
  info <- c(strsplit(info, " "))
  texto_columnas <- data.frame(unlist(info))
  texto_columnas[,1] <- as.character(texto_columnas[,1])
  nombre <- paste(texto_columnas[2,1], texto_columnas[3,1])   
  
  texto_columnas[,1] <- tolower(texto_columnas[,1])
  
  mes <- ifelse("enero" %in% texto_columnas$unlist.info.  , "enero",
                ifelse("febrero" %in% texto_columnas$unlist.info. , "febrero",
                       ifelse("marzo" %in% texto_columnas$unlist.info. , "marzo",
                              ifelse("abril" %in% texto_columnas$unlist.info., "abril",
                                     ifelse("mayo" %in% texto_columnas$unlist.info. , "mayo",
                                            ifelse("junio" %in% texto_columnas$unlist.info. , "junio",
                                                   ifelse("julio" %in% texto_columnas$unlist.info. , "julio",
                                                          ifelse("agosto" %in% texto_columnas$unlist.info., "agosto",
                                                                 ifelse("septiembre" %in% texto_columnas$unlist.info.  , "septiembre",
                                                                        ifelse("octubre" %in% texto_columnas$unlist.info. , "octubre",
                                                                               ifelse("noviembre" %in% texto_columnas$unlist.info.  , "noviembre",
                                                                                      ifelse("diciembre" %in% texto_columnas$unlist.info., "diciembre",
                                                                                             "NADA"))))))))))))
  
  famoso_x <- data.frame("nombre" = nombre,
                         "mes" = mes)
  
  df <- rbind(df,famoso_x )
  Sys.sleep(2)
  
}
beep(sound = 3)

df$mes <- as.factor(df$mes)

df$mes <- factor(df$mes, levels=c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto",
                                  "septiembre", "octubre", "noviembre", "diciembre"))

#############################################################################################################################################
# 3 - gráfico 
#############################################################################################################################################

graph_evol = data.frame(table(df$mes ))
colnames(graph_evol) = c("Mes",'Total')

ggplot(graph_evol)+
  geom_bar(aes(x = Mes,
               y = Total,
               fill = I('blue')),
           stat = 'identity',
           alpha = 0.75,
           show.legend = FALSE)+
  #scale_y_continuous(breaks=seq(0, 4500, 1000), limit = c(0,4500)) +
  geom_hline(yintercept = mean(graph_evol$Total),
             col = I('grey'),
             size = 1)+
  
  labs(title = 'Mes de nacimiento de famosos',
       
       caption = "@_Daniel_Nunez" )+
  
  xlab(NULL)+
  ylab(NULL)+
  scale_fill_brewer(palette = 'Dark2')+
  theme_classic()

ggsave(file="grafico_famosos.png", bg = "transparent", width = 12, height = 7, type = "cairo")



