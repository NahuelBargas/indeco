# Ejemplos usando Treemap

## Bases

## Cargar librerías

library(grid)
library(openxlsx)
library(sf)
library(tmap)
library(tmaptools)
library(viridisLite)


## Bases



### Cargar base con datos seleccionados del siguiente archivo:
### https://www.indec.gob.ar/ftp/cuadros/economia/CNA2018_resultados_preliminares_ganaderia.xls


censo<-read.xlsx("https://nahuelbargas.github.io/indeco/ganprovincia.xlsx")


### Descargar el shapefile del país desde http://www.indec.gov.ar/ftp/cuadros/territorio/codgeo/Codgeo_Pais_x_prov_datos.zip


unzip("Codgeo_Pais_x_dpto_con_datos.zip", exdir=getwd())

argentina<-st_read("pxdptodatosok.shp")

p1<-tm_shape(argentina)+tm_fill(col='skyblue')+tm_style("col_blind") #mapa del país completo


### A continuación se quitan las áreas de la Capital Federal, Antártida e Islas del Atlántico Sur

argentina2<-argentina[!(argentina$provincia=="Ciudad Autónoma de Buenos Aires"),]

argentina2<- argentina2[-425,] #Acá quitas la Antártida

argentina2<- argentina2[-511,] #Acá quitas las islas


### Se unen las bases 

argentinacenso<- merge(argentina2,censo, by= "provincia",all.x=TRUE,all.y=TRUE)



## La hora de los gráficos...


  		  
m1<-tm_shape(argentinacenso) + tm_polygons(c ("Bovinos","Caprinos"),
legend.format=list(text.separator="hasta"), n = 5, 
style="quantile",palette=list(inferno(5,direction=-1),viridis(5,direction=-1)))+
 tm_compass(type = "8star", position = c(0.05,0.88 ),size=2) +
  tm_scale_bar(breaks = c(0, 100, 200),position = c(0.05, 0.04))+
  tm_credits("Fuente: INDEC. Censo Agropecuario",position = c(0.4, 0),size = 0.4)+
  tm_layout(frame.lwd=2,
           main.title = "N°de EAP con orientación productiva-comercial",
           main.title.size= 1,
		main.title.position = "center",
		main.title.color = "darkblue",
		main.title.fontfamily="Comic Sans MS",
          legend.position=c(0.5,0.1),
		  legend.width=0.5,
		  frame = FALSE,
		  outer.margins=c(0,0, 0, 0)
          )		  

tmap_save(m1,"Bovinos-Caprinos.png", dpi = 300, asp = 0, outer.margins = 0)
		  
		  
		  
m2<- tm_shape(argentinacenso) + tm_polygons(c ("Equinos","Porcinos"),
border.col="gray80",
legend.format=list(text.separator="hasta"), n = 6, 
style="quantile",palette=list(cividis(5,direction=-1),plasma(5,direction=-1)))+
 tm_compass(type = "8star", position = c(0.05,0.88 ),size=2) +
  tm_scale_bar(breaks = c(0, 100, 200),position = c(0.05, 0.04))+
  tm_credits("Fuente: INDEC. Censo Agropecuario",position = c(0.4, 0),size = 0.4)+
  tm_layout(frame.lwd=2,
           main.title = "N°de EAP con orientación productiva-comercial",
           main.title.size= 1,
		   main.title.position = "center",
		   main.title.color = "darkblue",
		   main.title.fontfamily="Comic Sans MS",
           legend.position=c(0.5,0.1),
		   legend.width=0.5,
		   frame = FALSE,
		   outer.margins=c(0,0, 0, 0)
          )		  		  
		  
tmap_save(m2,"Equinos-Porcinos.png", dpi = 300, asp = 0, outer.margins = 0)

		  
m3<-tm_shape(argentinacenso) + tm_polygons("Ovinos",
legend.format=list(text.separator="hasta"), n = 5, 
style="quantile",palette= viridis(5,direction=-1))+
 tm_compass(type = "8star", position = c(0.05,0.80 ),size=2) +
  tm_scale_bar(breaks = c(0, 100, 200),position = c(0.05, 0.04),size=2)+
  tm_credits("Fuente: INDEC. Censo Agropecuario",position = c(0.6, 0),size=0.6)+
  tm_layout(frame.lwd=2,
           main.title = "N°de EAP con orientación productiva-comercial",
           main.title.size= 1,
		   main.title.position = "center",
		   		   main.title.color = "darkblue",
		   main.title.fontfamily="Comic Sans MS",
           legend.position=c(0.5,0.1),
		   legend.width=0.5,
		   frame = FALSE,
		   outer.margins=c(0,0, 0, 0)
          )		  		  		  


tmap_save( m3,"Ovinos.png", dpi = 300, asp = 0, outer.margins = 0,
insets_tm=p1,insets_vp=(viewport(0.80, 0.40, width = 0.4, height = 0.2)))

		  