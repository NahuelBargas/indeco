#Configuración de la base

options(
  shinymanager.db.timeout = 600000,  # 10 minutos - 30 segundos (ajusta según necesites)
  shinymanager.db.share = TRUE
)


#Librerías


library(shiny)
library(bslib)
library(tidyverse)
library(DBI)
#library(RODBC)
library(odbc)
library(dbplyr)
#library(GWalkR)
library(shinycssloaders)
library(highcharter)
library(janitor)
library(shinyjs)
library(DT)
library(jsonlite)
library(gt)
library(shinymanager)
library(RPostgres)


# Temas

contenido_scss <- readLines("mi_tema_app.scss", 
                           encoding = "UTF-8")
contenido_scss <- paste(contenido_scss, collapse = "\n")

# Objetos iniciales


## Establezco la conexión con la abse de datos
con <- DBI::dbConnect(odbc::odbc(), "consulta_soco")



vista_indices<-tbl(src = con, in_schema("consulta_soco", "vista_indices"))%>% collect()

aperturas_a_filtrar<- unique(vista_indices$apertura) 

grupos_a_filtrar <- vista_indices %>% filter(nivel==2) %>% arrange(codigo_pba_scrap)%>% pull(codigo_pba_scrap) %>% unique()

names(grupos_a_filtrar)<-vista_indices %>% filter(nivel==2) %>% arrange(codigo_pba_scrap)%>% pull(apertura) %>% unique()

clases_a_filtrar <- vista_indices %>% filter(nivel==3) %>% arrange(codigo_pba_scrap)%>% pull(codigo_pba_scrap) %>% unique()

names(clases_a_filtrar) <- vista_indices %>% filter(nivel==3) %>% arrange(codigo_pba_scrap)%>% pull(apertura) %>% unique()

vista_precios<- tbl(src = con, in_schema("consulta_soco", "vista_precios"))

periodos_a_filtrar<-vista_precios %>% select(periodo,fecha) %>% distinct() %>% collect() %>% arrange(fecha) %>% pull(periodo) %>% unique() 

periodos_a_filtrar_2<-vista_precios  %>% select(periodo,fecha) %>% distinct() %>% collect() %>%
                       group_by(periodo)%>% slice(1)%>% ungroup() %>% arrange(fecha)


periodos_a_filtrar_con_semanas<- vista_precios %>% 
  select(periodo, fecha) %>% 
  distinct() %>%
  collect() %>%
  group_by(periodo) %>%
  summarise(
    desde = min(fecha),
    hasta = max(fecha),
    .groups = 'drop'
  ) %>%
  mutate(
    semana = case_when(
      desde == hasta ~ paste0(day(desde), " de ", format(desde, "%B"), " de ", format(desde, "%Y")),
      month(desde) == month(hasta) ~ paste0("del ", day(desde), " al ", day(hasta), " de ", format(desde, "%B"), " de ", format(desde, "%Y")),
      TRUE ~ paste0("del ", day(desde), " de ", format(desde, "%B"), " al ", day(hasta), " de ", format(hasta, "%B"), " de ", format(desde, "%Y"))
    )
  ) %>%
  arrange(desde) %>%
  select(periodo, semana)

nombres_para_periodo <- setNames(periodos_a_filtrar_con_semanas$periodo, periodos_a_filtrar_con_semanas$semana)
  
##################### Opciones Highcharter

hcopts <- getOption("highcharter.lang")


hcopts$drillUpText<- "Volver"
hcopts$downloadCSV <- "Descargar CSV"
hcopts$downloadJPEG <- "Descargar imágen JPEG"
hcopts$downloadPDF <- "Descargar documento PDF"
hcopts$downloadPNG  <- "Descargar imágen PNG"
hcopts$downloadSVG  <- "Descargar imágen SVG"
hcopts$downloadXLS  <- "Descargar XLS"
hcopts$loading  <- "Procesando..."
hcopts$months<-c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
hcopts$noData <- "No existen datos para mostrar"
hcopts$numericSymbols <-c("mil", "M", "G", "T", "P", "E") 
hcopts$printChart <- "Imprimir Gráfico"
hcopts$shortMonths<-c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
hcopts$resetZoom <- "Restablecer zoom"
hcopts$resetZoomTitle <- "Restablecer nivel del zoom 1:1"
hcopts$thousandsSep <- ","
hcopts$viewData <- "Mostrar/ocultar tabla de datos"
hcopts$viewFullscreen <- "Ver en pantalla completa"
hcopts$exitFullscreen <- "Salir de la pantalla completa"
hcopts$contextButtonTitle<- "Ver opciones"
hcopts$weekdays  <- c("Domingo", "Lunes", "Martes", "Miércoles","Jueves","Viernes","Sábado")
hcopts$shortWeekdays  <- c("Sáb", "Dom", "Lun", "Mar", "Miér", "Jue", "Vie")

options(highcharter.lang = hcopts)

temaclásico<- hc_theme(
  colors = c("#7cb5ec","#434348","#90ed7d","#f7a35c","#8085e9","#f15c80","#e4d354","#2b908f","#f45b5b","#91e8e1"),
  chart = list(
    backgroundColor = "white"
  ))
  
temanegro<- hc_theme_db()

#### Función a utilizar en un gráfico:

stacked_bar_chart <- function(data = NA,
                              categories_column = NA,
                              measure_columns = NA,
                              stacking_type = NA,
                              ordering_function = c,
                              explicit_order = NA) {
  ordered_measure <-
    order(unlist(lapply(measure_columns, function(x) {
      ordering_function(data[, x])
    })),
    decreasing = TRUE) - 1
  
  chart <- highchart() %>%
    hc_xAxis(categories = data[, categories_column],
             title = categories_column)
  #Se modificó la función original en la parte de indec, tiraba error el script al actualizar las librerías, para lo anterior ver dash-ipc9.rmd
  invisible(lapply(1:length(measure_columns), function(colNumber) {
    chart <<-
      hc_add_series(
        hc = chart,
        name = measure_columns[colNumber],
        data = data[, measure_columns[colNumber]],
        index = explicit_order[colNumber],
		dataLabels=  list(enabled = TRUE,overflow="allow",crop=FALSE,inside=TRUE,format='{point.y:.1f}',style=list(fontSize='10px'))
      )
  }))
  
  chart %>%
    hc_chart(type = "bar") %>%
    hc_plotOptions(bar = list(stacking = as.character(stacking_type),enableMouseTracking = TRUE)) %>%
    hc_legend(reversed = TRUE)
}


## Toolpit

x <- c("Precio","Precio Tachado","Nombre informante","Producto super","Fecha","Sucursal")
x2 <- c("precio","precio_tachado","nombre_informante","producto_super","fecha","sucursal")

y<-as.character()
y[1:2] <- str_c("{point.", x2[1:2],":  ,.0f","}")
y[3:6] <- str_c("{point.", x2[3:6],":","}")

tt <- tooltip_table(x, y) 
  
########### Opciones DT
options(DT.options = list(pageLength = 7))

spanish<-'{
	"sProcessing":     "Procesando...",
	"sLengthMenu":     "Mostrar _MENU_ filas",
	"sZeroRecords":    "No se encontraron resultados",
	"sEmptyTable":     "Ningún dato disponible en esta tabla",
	"sInfo":           "Mostrando fila número _START_ a la _END_ de un total de _TOTAL_ filas",
	"sInfoEmpty":      "Mostrando filas del 0 al 0 de un total de 0 filas",
	"sInfoFiltered":   "(filtrado de un total de _MAX_ filas)",
	"sInfoPostFix":    "",
	"sSearch":         "Buscar:",
	"sUrl":            "",
	"sInfoThousands":  ",",
	"sLoadingRecords": "Cargando...",
     	"oPaginate": {
		"sFirst":    "Primero",
		"sLast":     "Último",
		"sNext":     "Siguiente",
		"sPrevious": "Anterior"
		},
	"oAria": {
		"sSortAscending":  ": Activar para ordenar la columna de manera ascendente",
		"sSortDescending": ": Activar para ordenar la columna de manera descendente"
	}	
}'
#####################

## Shiny AUTH


#
#passwords  <- c(
#"c2NyeXB0AA8AAAAIAAAAAQkvNibMs8sR5prFgQypTRpJ5C+4Vr3/5JRCKh+F69z1ItMIBJzNf5QOoNcvdQvD6iXxTuP7lPppI/QMuHCLbL60zQFJX/jfmCoSsvCahrRj",
#"c2NyeXB0AA8AAAAIAAAAAa9KwzvY0XQKoIVFE6rwKfU2IJlmL+Hin0vhe6bx3xwW1Fv1FfFHHurl4h7535EbjJZ5D3ZfabsXbHVooXBkf/wyfZ/C3XenGgnKRnvRQh3d"
#
#)
#
#credentials <- data.frame(
#  user = c("soco_vis", "soco_admin"),
#  password = passwords,
#  is_hashed_password = TRUE,
#  admin = c(FALSE, TRUE),
#  stringsAsFactors = FALSE
#)
#
#
#create_db(
#  credentials_data = credentials,
#  sqlite_path = "base/database.sqlite"
#)

inactivity <- "function idleTimer() {
var t = setTimeout(logout, 6600000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 6600000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"

set_labels(
  language = "en",
  "Please authenticate" = "Por favor, ingrese sus datos",
  "Username:" = "Su nombre de usuario:",
  "Password:" = "Su contraseña:"
)

######################### Spinner

options(spinner.type = 8,
        spinner.caption= div(strong("Cargando...."), br(), "Espere por favor"),
		spinner.size=1.6)
		
###################################################

 
 
cards_graf_serie_indices <- list(
  card(
    full_screen = TRUE,
    highchartOutput("hcvarsem")
  ),
  card(
    full_screen = TRUE,
    highchartOutput("hcindsem")
  ),
  card(
    full_screen = TRUE,
    highchartOutput("hcvaracu")
  ),
  card(
    full_screen = TRUE,
    highchartOutput("hcindci")
  )
)

sidebar_aperturas<- selectizeInput(
                 "sel_apertura",
                 "Selecciona una apertura:",
                 choices = aperturas_a_filtrar,multiple=TRUE,
				 options = list(plugins = "remove_button")) 

sidebar_periodo<-selectizeInput(
                  "sel_periodo",
                  "Selecciona una semana:",
                  choices = nombres_para_periodo,multiple=FALSE,
  options = list(plugins = "remove_button"))
  
sidebar_periodo2<-selectizeInput(
                  "sel_periodo2",
                  "Selecciona una semana:",
                  choices = nombres_para_periodo,multiple=TRUE,
  options = list(plugins = "remove_button"))  


link_dpe <- tags$a(
   href = "https://www.estadistica.ec.gba.gov.ar/dpe/",
   tags$img(src = "logomil2.png", 
                             alt = "DPE", 
                             width = "100px")
)

ima_dpe<- tags$head(
					  
			  tags$script(
                 HTML('$(document).ready(function() {
                        $(".navbar .container-fluid")
                          .append("<a href=\'https://www.estadistica.ec.gba.gov.ar/dpe\' target=\'_blank\'>" +
                                   "<img id = \'myImage\' src=\'logomil5.png\' align=\'right\' height = \'57.5px\'>" +
                                   "</a>");
                       });')),
              tags$style(
                HTML('@media (max-width:992px) { #myImage { position: fixed; right: 10%; top: 0.5%; }}')
                # or: 
                # HTML('@media (max-width:992px) { #myImage { position: fixed; right: 1%; top: 0.5%; }
                #                                 div.navbar-header > button {margin-right: 175px}}')
              ))

ui <- page_navbar(
  title = "VIZ-PBA-SCRAP",
  nav_spacer(), # push nav items to the right
  nav_panel(title = "Índices y variaciones por apertura",
    layout_sidebar(
       sidebar = sidebar_aperturas,
       layout_columns(cards_graf_serie_indices[[1]], cards_graf_serie_indices[[2]]),
       layout_columns(cards_graf_serie_indices[[3]],cards_graf_serie_indices[[4]])  
                   )
            ),
  nav_panel(title = "Índices y variaciones por periodo",
  card(
    height = 300,
    full_screen = TRUE,
    layout_sidebar(sidebar = sidebar_periodo, shinycssloaders::withSpinner(
                DTOutput("varaperturasDT"), 
               ))
  ),
  layout_columns(
    card(
        full_screen = TRUE,
      card_header(
        class = "d-flex justify-content-between",
        selectizeInput(
                        "sel_grupo",
                        "Selecciona un grupo:",
                        choices = grupos_a_filtrar,multiple=FALSE,
        options = list(plugins = "remove_button"))
      ),
      card_body(
        class = "align-items-center",
        highchartOutput('hcclases')
      )
        ),
	card(
        full_screen = TRUE,
      card_header(
        class = "d-flex justify-content-between",
        selectizeInput(
                        "sel_clase",
                        "Selecciona una clase:",
                        choices = clases_a_filtrar,multiple=FALSE,
        options = list(plugins = "remove_button"))
      ),
      card_body(
        class = "align-items-center",
        highchartOutput('hcsubclases')
      )
        )
		
		)
		),
   nav_panel('Consulta de precios',
    layout_sidebar(
       sidebar = sidebar_periodo2,
       layout_columns(
          card(
                 full_screen = TRUE,
                 shinycssloaders::withSpinner(
                   DTOutput("my_dt_precios")
                                             )
				), 
		  layout_columns(
		     card(
			   card( full_screen = TRUE,
			         shinycssloaders::withSpinner(highchartOutput("hc_precios"))
					 ),
			   card( full_screen = TRUE,
			         shinycssloaders::withSpinner(DTOutput("my_dt_comparacion"))
			   )
			   )
			           )
                       )
              )
			 ),
	nav_panel('Consulta de comportamiento semanal',
    	
			 card(
        full_screen = TRUE,
      card_header(
        class = "d-flex justify-content-between",
        selectizeInput(
                  "sel_periodo3",
                  "Selecciona una semana:",
                  choices = nombres_para_periodo[-1],multiple=FALSE,#selected=as.character(nombres_para_periodo[length(nombres_para_periodo[-1])]),#last(periodos_a_filtrar),
        options = list(plugins = "remove_button")),
		selectizeInput(
                        "sel_concepto",
                        "Seleccionar concepto a visualizar",
                        choices = c('Divisiones','Grupos','Supermercados','Partidos'),multiple=FALSE,
        options = list(plugins = "remove_button"))
                  ),
      card_body(
        layout_columns(
		       col_widths=c(6,6),
			   card( full_screen = TRUE,
			         shinycssloaders::withSpinner(highchartOutput('hcbarras'))
					 ),
			   card( full_screen = TRUE,
			         shinycssloaders::withSpinner(gt_output('cuadrocomportamiento'))
			   )
			   )
			           )
      )
	  ),
	
        
     
   nav_item(
    input_dark_mode(id = "dark_mode", mode = "light")
  ),
  nav_item(
    ima_dpe 
  ),
   theme= bs_theme(base_font = font_google("Encode Sans", local = TRUE),bootswatch='cerulean') %>%
    bs_add_rules(sass::sass(contenido_scss))
  )

ui <-  secure_app(ui, enable_admin = TRUE,head_auth = tags$script(inactivity),
                    background  = "linear-gradient(to right, #E81F76 0%, #00AEC3 100%)")
					
server <- function(input, output,session) {

res_auth <- secure_server(
      check_credentials = check_credentials("base/database.sqlite")#,
	  #passphrase = "passphrase_wihtout_keyring"
	                     
    )

output$auth_output <- renderPrint({
  reactiveValuesToList(res_auth)
})

serie_indices <- reactiveValues(data = NULL)

observeEvent(input$sel_apertura, {
  serie_indices$data <- vista_indices %>% filter( apertura %in% input$sel_apertura) %>% rename(nombre=apertura)

})

serie_indices2 <- reactiveValues(data = NULL)

observeEvent(input$sel_periodo, {

  
  serie_indices2$data <- vista_indices %>% filter( periodo %in% input$sel_periodo) #%>%
                          #mutate_at(c('periodo','fecha'),~as.factor)
                          
clases_segun_grupo$data <- serie_indices2$data %>% filter(nivel==3)%>%
         filter(str_detect(codigo_pba_scrap,input$sel_grupo))%>%
         arrange(desc(incidencia))

subclases_segun_clase$data <- serie_indices2$data %>% filter(nivel==4)%>%
         filter(str_detect(codigo_pba_scrap,input$sel_clase))%>%
         arrange(desc(incidencia))

})




tema_elegido <- reactiveValues(data = NULL)


observeEvent(input$dark_mode, {
    #if(length(input$dark_mode) == 0){
	#stop("El argumento tiene longitud cero")
	#} else{
	
	req(input$dark_mode)
	
    if(input$dark_mode == "dark"){
      tema_elegido$data <- temanegro
	  } else {
	  tema_elegido$data <- temaclásico
	  }
    #}
  })

output$hcvarsem <- renderHighchart({
     
     req(serie_indices$data)
     
     highchart() %>%
                hc_add_series(
                serie_indices$data, "line",
     							  hcaes(x=fecha,y=variacion_sem,group=nombre)
     							  )%>%
     		  hc_xAxis(type = "datetime") %>%		  
                hc_title(text="Aperturas seleccionadas- var. semanal")%>%
     #hc_chart(zoomType = "xy") %>%
	#hc_plotOptions( series = list( 
    #  dataLabels=list(enabled=TRUE,
    #           #inside=FALSE,
    #           #padding= 5,
    #           #shadow= TRUE,
    #           style=list(
    #               fontWeight= 'bold'
    #           ),
	#			format="{point.y:.2f}%"#, 
    #           #y= -6)
	#			)))%>%
     hc_tooltip(pointFormat = "{series.name} <br> {point.y:.1f}%") %>%
     hc_exporting(
         enabled = TRUE, 
     	scale= 1,
         chartOptions = list(plotOptions =list(series = list( dataLabels = list( enabled=TRUE,format='{point.y:.1f}')))),
     	printMaxWidth =900,
     	fallbackToExportServer = FALSE,
     	csv=list(decimalPoint = ','),
     	filename="datos del gráfico",
         buttons = list(contextButton = list( menuItems= c('viewFullscreen','printChart','downloadCSV','downloadPNG'
     	)))
         ) %>%
     
       hc_add_theme(tema_elegido$data) 
       })
	   
output$hcindsem <- renderHighchart({
     
     req(serie_indices$data)
	 
     highchart() %>%
                hc_add_series(
                serie_indices$data, "line",
     							  hcaes(x=fecha,y=indice,group=nombre)
     							  )%>%
     		  hc_xAxis(type = "datetime") %>%		  
                hc_title(text="Aperturas seleccionadas- Índice. semanal")%>%
     hc_chart(zoomType = "xy") %>%
	#hc_plotOptions( series = list( 
    #  dataLabels=list(enabled=TRUE,
    #           #inside=FALSE,
    #           #padding= 5,
    #           #shadow= TRUE,
    #           style=list(
    #               fontWeight= 'bold'
    #           ),
	#			format="{point.y:.2f}%"#, 
    #           #y= -6)
	#			)))%>%
     hc_tooltip(pointFormat = "{series.name} <br> {point.y:.1f}") %>%
           hc_exporting(
         enabled = TRUE, 
     	scale= 1,
         chartOptions = list(plotOptions =list(series = list( dataLabels = list( enabled=TRUE,format='{point.y:.1f}')))),
     	printMaxWidth =900,
     	fallbackToExportServer = FALSE,
     	csv=list(decimalPoint = ','),
     	filename="datos del gráfico",
         buttons = list(contextButton = list( menuItems= c('viewFullscreen','printChart','downloadCSV','downloadPNG'
     	)))
         ) %>%
     
       hc_add_theme(tema_elegido$data) 	
       })
	   
output$hcindci <- renderHighchart({
     
     req(serie_indices$data)
	 
     highchart() %>%
                hc_add_series(
                serie_indices$data, "line",
     							  hcaes(x=fecha,y=incidencia,group=nombre)
     							  )%>%
     		  hc_xAxis(type = "datetime") %>%		  
                hc_title(text="Aperturas seleccionadas- Íncidencia. semanal")%>%
     hc_chart(zoomType = "xy") %>%
	#hc_plotOptions( series = list( 
    #  dataLabels=list(enabled=TRUE,
    #           #inside=FALSE,
    #           #padding= 5,
    #           #shadow= TRUE,
    #           style=list(
    #               fontWeight= 'bold'
    #           ),
	#			format="{point.y:.2f}%"#, 
    #           #y= -6)
	#			)))%>%
     hc_tooltip(pointFormat = "{series.name} <br> {point.y:.1f} p.p.") %>%
           hc_exporting(
         enabled = TRUE, 
     	scale= 1,
         chartOptions = list(plotOptions =list(series = list( dataLabels = list( enabled=TRUE,format='{point.y:.1f}')))),
     	printMaxWidth =900,
     	fallbackToExportServer = FALSE,
     	csv=list(decimalPoint = ','),
     	filename="datos del gráfico",
         buttons = list(contextButton = list( menuItems= c('viewFullscreen','printChart','downloadCSV','downloadPNG'
     	)))
         ) %>%
     
       hc_add_theme(tema_elegido$data) 	
       })
	   
output$hcvaracu <- renderHighchart({
     
	 req(serie_indices$data)
     
     highchart() %>%
                hc_add_series(
                serie_indices$data, "line",
     							  hcaes(x=fecha,y=variacion_acu_4_semanas,group=nombre)
     							  )%>%
     		  hc_xAxis(type = "datetime") %>%		  
                hc_title(text="Aperturas seleccionadas- Variación acumulada (ult. 4 semanas)")%>%
     hc_chart(zoomType = "xy") %>%
	#hc_plotOptions( series = list( 
    #  dataLabels=list(enabled=TRUE,
    #           #inside=FALSE,
    #           #padding= 5,
    #           #shadow= TRUE,
    #           style=list(
    #               fontWeight= 'bold'
    #           ),
	#			format="{point.y:.2f}%"#, 
    #           #y= -6)
	#			)))%>%
     hc_tooltip(pointFormat = "{series.name} <br> {point.y:.1f} %") %>%
           hc_exporting(
         enabled = TRUE, 
     	scale= 1,
         chartOptions = list(plotOptions =list(series = list( dataLabels = list( enabled=TRUE,format='{point.y:.1f}')))),
     	printMaxWidth =900,
     	fallbackToExportServer = FALSE,
     	csv=list(decimalPoint = ','),
     	filename="datos del gráfico",
         buttons = list(contextButton = list( menuItems= c('viewFullscreen','printChart','downloadCSV','downloadPNG'
     	)))
         ) %>%
     
       hc_add_theme(tema_elegido$data) 	
       })


output$varaperturasDT <- renderDT({
  
    req(serie_indices2$data)
	 
    base_ind_var_ult_mes <- serie_indices2$data %>% arrange(codigo_pba_scrap)%>%
                              mutate(codigo_pba_scrap= as.factor(codigo_pba_scrap))%>%
                              mutate(apertura= as.factor(apertura))

    
	datatable(base_ind_var_ult_mes ,
   filter = list(position ='top',plain = TRUE), #server = TRUE, 
   rownames = FALSE,extensions = 'KeyTable', selection = "multiple", class="compact display",
	  plugins = 'input',
     callback = JS("$(\"input[type='search']\").attr('placeholder','Todos');"),
	 options = list(pagingType = "input",keys = TRUE,
	 columnDefs = list(list(className = 'dt-center', targets = c(1,3:9))),autoWidth = TRUE, language = fromJSON(spanish),
         initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': 'bisque', 'color': 'black'});",
           "}")
          )
	)%>% formatRound(
         c(4:ncol( base_ind_var_ult_mes)-1),
         digits = 2,
         interval = 3,
         mark = ",",
         dec.mark = getOption("OutDec")
                    )
	})
    
  
clases_segun_grupo <- reactiveValues(data = NULL)

observeEvent(input$sel_grupo , {
  clases_segun_grupo$data <- serie_indices2$data %>% filter(nivel==3)%>%
         filter(str_detect(codigo_pba_scrap,input$sel_grupo))%>%
         arrange(desc(incidencia))

})

nombre_grupo_seleccionado <- reactiveValues(data = NULL)

observeEvent(input$sel_grupo , {

nombre_grupo_seleccionado$data <- serie_indices2$data %>% filter(codigo_pba_scrap==input$sel_grupo) %>% pull(apertura) %>% unique()
})





  
output$hcclases <-   
renderHighchart({
 
 req(clases_segun_grupo$data)
 
 highchart() %>%
hc_plotOptions( series =list( 
 dataLabels=list(enabled=TRUE,inside=TRUE) )) %>%
 hc_xAxis(type="category",categories=clases_segun_grupo$data$apertura,
 lineWidth= 0, tickWidth= 0,labels= list( style = list(color= "#003136",fill="#003136",fontSize="13px"))
         )        %>% 
   hc_add_series(clases_segun_grupo$data,
    "bar",   
   hcaes(
      x= apertura,
      name = apertura,
      y = variacion_sem,
      variacion= variacion_sem), colorByPoint = FALSE,showInLegend=FALSE,
dataLabels=list(enabled=TRUE,align= 'right',x = 30,format='{point.y:.1f}',color='#00aec3'))%>%
hc_add_series(clases_segun_grupo$data,
    "bar",   
   hcaes(
      x= apertura,
      name = apertura,
      y = incidencia,
      incidencia= incidencia,
      variacion= variacion_sem), colorByPoint = FALSE,showInLegend=FALSE,
dataLabels=list(enabled=TRUE,align= 'right',x = 30,format='{point.y:.2f}',color='#39519F'))%>%
hc_tooltip( pointFormat = 
		  "Variación semanal: {point.variacion:.1f} % <br>
		  Incidencia semanal: {point.incidencia:.2f} p.p. ",crosshairs = FALSE, shared = FALSE)%>%
hc_colors(c('#00aec3','#39519F'))%>%
hc_yAxis(labels=list (formatter = JS(paste0("function() {
                       return this.value + '%'
					   }"
					  ))))%>%
hc_title(text = paste0("<span style='color: #00aec3; font-weight: bold'>Variación </span> e <span style='color: #39519F; font-weight: bold'>incidencia</span> semanal de las clases del grupo <span style='font-weight: bold'>",nombre_grupo_seleccionado$data, '</span>'),x=-30)%>% 
hc_exporting(
    enabled = TRUE, 
	scale= 1,
	printMaxWidth =900,
	fallbackToExportServer = FALSE,
	csv=list(columnHeaderFormatter=JS(paste0("function(item, key) {
                if (!item || item instanceof Highcharts.Axis) {
                    return ''
                } else {
                    return item.name;
                }
            }"
			)),	decimalPoint = ',' , dateFormat= '%d-%m-%y',itemDelimiter=';'),
	filename="datos del gráfico",
    buttons = list(contextButton = list( menuItems= c('viewFullscreen','printChart','downloadCSV','downloadPNG'
	)))
    ) %>%
  hc_credits(enabled=TRUE)%>%
  hc_add_theme(tema_elegido$data) 
})

observeEvent(input$sel_grupo, {
#if(length(input$sel_grupo) == 0){
#	stop("El argumento tiene longitud cero")
#	} else{
req(input$sel_grupo)
    if (input$sel_grupo == "A011") {
      updateSelectizeInput(inputId="sel_clase", choices = clases_a_filtrar[str_detect(clases_a_filtrar,'A011')])
    } else if (input$sel_grupo == "A012") {
      updateSelectizeInput(inputId="sel_clase", choices = clases_a_filtrar[str_detect(clases_a_filtrar,'A012')])
    } else if (input$sel_grupo == "A021") {
      updateSelectizeInput(inputId="sel_clase", choices = clases_a_filtrar[str_detect(clases_a_filtrar,'A021')])
    } else if (input$sel_grupo == "A022") {
      updateSelectizeInput(inputId="sel_clase", choices = clases_a_filtrar[str_detect(clases_a_filtrar,'A022')])
    } else if (input$sel_grupo == "A031") {
      updateSelectizeInput(inputId="sel_clase", choices = clases_a_filtrar[str_detect(clases_a_filtrar,'A031')])
    } else if (input$sel_grupo == "A032") {
      updateSelectizeInput(inputId="sel_clase", choices = clases_a_filtrar[str_detect(clases_a_filtrar,'A032')])
    } else if (input$sel_grupo == "A041") {
      updateSelectizeInput(inputId="sel_clase", choices = clases_a_filtrar[str_detect(clases_a_filtrar,'A041')])
    } else if (input$sel_grupo == "A042") {
      updateSelectizeInput(inputId="sel_clase", choices = clases_a_filtrar[str_detect(clases_a_filtrar,'A042')])
    } else if (input$sel_grupo == "A043") {
      updateSelectizeInput(inputId="sel_clase", choices = clases_a_filtrar[str_detect(clases_a_filtrar,'A043')])
    }
	#}
  })


subclases_segun_clase <- reactiveValues(data = NULL)




observeEvent(input$sel_clase , {
  subclases_segun_clase$data <- serie_indices2$data %>% filter(nivel==4)%>%
         filter(str_detect(codigo_pba_scrap,input$sel_clase))%>%
         arrange(desc(incidencia))

})

nombre_clase_seleccionada <- reactiveValues(data = NULL)

observeEvent(input$sel_clase , {

nombre_clase_seleccionada$data <- serie_indices2$data %>% filter(codigo_pba_scrap==input$sel_clase) %>% pull(apertura) %>% unique()
})
   
output$hcsubclases <-   
renderHighchart({
 
 req(subclases_segun_clase$data)
 
 highchart() %>%
hc_plotOptions( series =list( 
 dataLabels=list(enabled=TRUE,inside=TRUE) )) %>%
 hc_xAxis(type="category",categories=subclases_segun_clase$data$apertura,
 lineWidth= 0, tickWidth= 0, labels= list( style = list(color= "#003136",fill="#003136",fontSize="13px"))) %>% 
   hc_add_series(subclases_segun_clase$data,
    "bar",   
   hcaes(
      x= apertura,
      name = apertura,
      y = variacion_sem,
      variacion= variacion_sem), colorByPoint = FALSE,showInLegend=FALSE,
dataLabels=list(enabled=TRUE,align= 'right',x = 30,format='{point.y:.1f}',color='#00aec3'))%>%
hc_add_series(subclases_segun_clase$data,
    "bar",   
   hcaes(
      x= apertura,
      name = apertura,
      y = incidencia,
      incidencia= incidencia,
      variacion= variacion_sem), colorByPoint = FALSE,showInLegend=FALSE,
dataLabels=list(enabled=TRUE,align= 'right',x = 30,format='{point.y:.2f}',color='#39519F'))%>%
hc_tooltip( pointFormat = 
		  "Variación semanal: {point.variacion:.1f} % <br>
		  Incidencia semanal: {point.incidencia:.2f} p.p. ",crosshairs = FALSE, shared = FALSE)%>%
hc_colors(c('#00aec3','#39519F'))%>%
hc_yAxis(labels=list (formatter = JS(paste0("function() {
                       return this.value + '%'
					   }"
					  ))))%>%
hc_title(text = paste("<span style='color: #00aec3; font-weight: bold'>Variación </span> e <span style='color: #39519F; font-weight: bold'>incidencia</span> semanal de las subclases de la clase <span style='font-weight: bold'>",nombre_clase_seleccionada$data, '</span>'),x=-30)%>% 
hc_exporting(
    enabled = TRUE, 
	scale= 1,
	printMaxWidth =900,
	fallbackToExportServer = FALSE,
	csv=list(columnHeaderFormatter=JS(paste0("function(item, key) {
                if (!item || item instanceof Highcharts.Axis) {
                    return ''
                } else {
                    return item.name;
                }
            }"
			)),	decimalPoint = ',' , dateFormat= '%d-%m-%y',itemDelimiter=';'),
	filename="datos del gráfico",
    buttons = list(contextButton = list( menuItems= c('viewFullscreen','printChart','downloadCSV','downloadPNG'
	)))
    ) %>%
  hc_credits(enabled=TRUE)%>%
  hc_add_theme(tema_elegido$data) 
})

serie_precios <- reactiveValues(data = NULL)

observeEvent(input$sel_periodo2, {
    periodo_elegido <- input$sel_periodo2
  
     serie_precios$data <- vista_precios %>% 
             
             filter(periodo %in% periodo_elegido) %>% collect() %>%
             mutate(producto_super= as.factor(producto_super))%>%
             mutate(sucursal= as.factor(sucursal))%>%
             mutate(nombre_informante= as.factor(nombre_informante))%>%
             mutate(partido=as.factor(partido))
   
})
	

	
output$my_dt_precios <- renderDataTable({
   
        req(serie_precios$data)
		
         datatable(serie_precios$data %>% 
		         #filter( fecha2 %in% input$sel_fecha_pre )%>%#, 
				         #nombre_informante %in% input$sel_informante_pre )%>%
				 select(fecha,nombre_informante,sucursal, partido, producto_super,				 
				          precio, precio_tachado, link,EAN=ean
						  
						  ), 
	   filter = list(position ='top',plain = TRUE), extensions = 'KeyTable', selection = "single",rownames = TRUE,
	  plugins = 'input',
     callback = JS("$(\"input[type='search']\").attr('placeholder','Todos');"),
	 options = list(#pagingType = "input",
	 keys = TRUE,
	 columnDefs = list(list(className = 'dt-center', targets = "_all")),autoWidth = TRUE, language = fromJSON(spanish),
         initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#bbcae3', 'color': 'black', 'font-size' : 'small'});",
           "}")
          )
		  )%>% formatStyle("link", width='80px') #%>%
               #formatRound("var_semanal", digits = 1,dec.mark=',')
     })
  


db_hc_precios <- reactiveValues(data = NULL)

    observeEvent(input$my_dt_precios_rows_selected, {
	  
	  link_seleccionado <- serie_precios$data[input$my_dt_precios_rows_selected, ]$link
	  sucursal_seleccionada <- serie_precios$data[input$my_dt_precios_rows_selected, ]$sucursal

	  
	  
      if( length(input$my_dt_precios_rows_selected) > 0){
           db_hc_precios$data <-
             vista_precios %>% # filtrando base de datos de acuerdo a las filas seleccionadas en  my_dt_precios
			 filter(link %in% link_seleccionado,
                    sucursal == sucursal_seleccionada			 )%>% arrange(fecha) %>% collect() 		 
         } else {
          db_hc_precios$data <-vista_precios %>% # filtrando base de datos de acuerdo a la primer fila de  my_dt_precios
			 filter(link %in% serie_precios$data[1, ]$link)%>% collect() 
       	} 
    
    })

output$hc_precios	<- renderHighchart({
  
       req(db_hc_precios$data)

       
       
       hc<-highchart() %>%
             
       	     hc_add_series( db_hc_precios$data ,"line", hcaes(y="precio",x="fecha"),name="Precio" ) %>%
             hc_add_series(db_hc_precios$data, "line", hcaes(y="precio_tachado",x="fecha"),name="Precio_tachado" ) %>%
       	     
             hc_xAxis(type = "datetime")
       
       hc%>%
       hc_title(text=paste("Serie histórica de precios para el:",hc$x$hc_opts$series[[1]]$data[[1]]$producto_super), style=list(useHTML = TRUE))%>%
       hc_tooltip(pointFormat = tt, useHTML = TRUE, headerFormat = "", crosshairs = TRUE)%>%
       hc_chart(zoomType = "xy") %>%
        hc_exporting(
           enabled = TRUE, 
        scale= 1,
        chartOptions = list(plotOptions =list(line = list( dataLabels = list( enabled=TRUE,format='{point.y:.1f}')))),
        printMaxWidth =900,
        fallbackToExportServer = FALSE,
        csv=list(decimalPoint = ','),
        filename="datos del gráfico",
           buttons = list(contextButton = list( menuItems= c('viewFullscreen','printChart','downloadCSV','downloadPNG'
       	)))
           )%>%
         hc_add_theme(tema_elegido$data) 		
})


base_comparacion_dt <- reactiveValues(data = NULL)
   
observeEvent(input$my_dt_precios_rows_selected, {

     informante_seleccionado <- serie_precios$data[input$my_dt_precios_rows_selected, ]$informante
     ean_seleccionado <- serie_precios$data[input$my_dt_precios_rows_selected, ]$ean
     precio_seleccionado  <-  serie_precios$data[input$my_dt_precios_rows_selected, ]$precio

    # Base precios para compararar

    base_comparacion_dt$data <-  serie_precios$data %>% 
             filter(!is.na(ean))%>%    
			 # filtrando base de datos de acuerdo a las filas seleccionadas en  my_dt_precios
			 filter(! (informante %in% informante_seleccionado),
                    ean %in% ean_seleccionado)%>%
			 mutate( brecha_precio_seleccionado = round(precio /precio_seleccionado,1))
					         }
	         )
   

	  
output$my_dt_comparacion <- renderDT({
   
        req(base_comparacion_dt$data)
	

		
		

			
    datatable(base_comparacion_dt$data %>% 
				 select(nombre_informante,producto_super,sucursal,				 
				          precio, precio_tachado,brecha_precio_seleccionado, link
						  ), 
	filter = list(position ='top',plain = TRUE), class="compact display",rownames = TRUE,
	  plugins = 'input',
     callback = JS("$(\"input[type='search']\").attr('placeholder','Todos');"),
	 options = list(pagingType = "input",keys = TRUE,dom = 'tp',
	 columnDefs = list(list(className = 'dt-center', targets = "_all")),autoWidth = TRUE, language = fromJSON(spanish),
         initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#d9a5d7', 'color': 'black', 'font-size' : 'small'});",
           "}")
          )
		  )%>% formatStyle("link", width='80px')# %>%
           #    formatRound("var_semanal", digits = 1,dec.mark=',')
                       })   
 	   
base_comportamiendo_p  <- reactiveValues(data = NULL)


observeEvent(input$sel_periodo3, {
  req(input$sel_periodo3)
  
  periodo_seleccionado <- input$sel_periodo3
  
  periodos_para_calcular <- periodos_a_filtrar_2 %>% 
    filter(fecha <= periodos_a_filtrar_2[periodos_a_filtrar_2$periodo == periodo_seleccionado, ]$fecha) %>%
    tail(2) %>%
    pull(periodo)
  
  # Chequeo defensivo: ¿hay dos periodos?
  if(length(periodos_para_calcular) < 2) {
    showNotification("Seleccioná un periodo con al menos uno anterior para comparar.", type = "error")
    base_comportamiendo_p$data <- NULL
    return()
  }
  
  base_comportamiendo_p$data <- vista_precios %>% 
    filter(periodo %in% periodos_para_calcular) %>%
    collect()
})

base_var_total_final_div  <- reactiveValues(data = NULL)

observeEvent(input$sel_periodo3, {

req(base_comportamiendo_p$data)
        
		
		
        
        # Se realiza la consulta y se filtra para tomar sólo las aperturas con clasificación 
        base_precios_con_variaciones <- base_comportamiendo_p$data %>% 
          filter(! apertura=='Sin asignar') %>%
          filter(! apertura=='OTROS-EXCLUIR') 
		  

		
		
          
        base_precios_con_variaciones <- base_precios_con_variaciones %>% 
           mutate(precio = case_when(
             !is.na(precio_tachado) ~ precio_tachado,
        	 TRUE ~ precio))
        
        #Hay precios que vienen más de una vez en la bajada, hago un promedio simple
        	 
        base_prom_simple<- base_precios_con_variaciones %>% group_by(periodo,informante,nombre_informante,link,producto_super,codigo_pba_scrap,apertura,sucursal) %>%
                summarise(precio=mean(precio)) %>% ungroup()
        

		# Separo el precio por periodo
        base_wide<- base_prom_simple  %>%
           filter(! (precio <= 0)) %>% #saco los precios menores o iguales a 0
           pivot_wider(names_from=periodo,values_from=precio) 
        
		print(nrow(base_wide))
        print(names(base_wide))

		
		# Detectar las columnas de precios (de periodos)
		
		cols_fijas <- c("periodo", "informante", "nombre_informante", "link", 
                "producto_super", "codigo_pba_scrap", "apertura", "sucursal")
		
        precio_cols <- setdiff(names(base_wide), cols_fijas)
        
        # Chequeo: ¿hay dos columnas de precio?
        if(length(precio_cols) != 2) {
          showNotification("No hay datos suficientes en ambos periodos para calcular la variación.", type = "error")
          base_var_total_final_div$data <- NULL
          return()
        }
        
        # Ordenar los periodos:
        precio_cols <- sort(precio_cols)
        
        # Renombrar explícitamente las columnas de periodo
        base_wide <- base_wide %>%
          rename(precio_0 = all_of(precio_cols[1]), precio_1 = all_of(precio_cols[2]))
		
		 
        
        # Calculo la variación para los precios que se encuentran en ambos periodos
        
        base_calc_apertura_inf <-  base_wide %>% 
          mutate(variacion=precio_1/precio_0-1) %>%
          filter(!is.na(precio_1) & !is.na(precio_0))%>%
                          mutate(
        				  nivel = substr(codigo_pba_scrap,1,3),
        				  categoria =
        				    case_when (
        				     nivel=='A01' ~ 'ALIMENTOS Y BEBIDAS NO ALCOHÓLICAS',
        				     nivel=='A02' ~ 'BEBIDAS ALCOHÓLICAS',
        				     nivel=='A03' ~ 'LIMPIEZA DEL HOGAR',
        				     nivel=='A04' ~ 'HIGIENE CORPORAL',
        				     TRUE ~ 'DESCARTE'
                                       )
        				  	     ) %>%
        				  arrange(nivel)%>%
        				  mutate ( categoria = factor(categoria, levels = 
        				  c('ALIMENTOS Y BEBIDAS NO ALCOHÓLICAS',
                          'BEBIDAS ALCOHÓLICAS',
                          'LIMPIEZA DEL HOGAR',
                          'HIGIENE CORPORAL',
        				  'DESCARTE'
                                  )
        						  ) 
        						  )
          
        # Separo la base según signo de variación y creo variable signo de variación
        
        base_var_total <- base_calc_apertura_inf %>%
                                     mutate(signo_variacion = case_when (
        							 variacion < 0     ~ 'Baja de precios',
        							 variacion == 0    ~ 'Sin variación',
        							 variacion >0      ~ 'Suba de precios')
        							       )  
        
        # Cuento el tipo de magnitud								   
        base_var_total_long<- base_var_total %>%
                                           group_by(categoria) %>%
                                           count(signo_variacion)
        
        # Paso formato a long y calculo totales por fila
        base_var_total_wide <- 	base_var_total_long %>%
                                             pivot_wider(names_from=signo_variacion,values_from= n) %>%
        									 ungroup()%>%
                                             rowwise() %>% 
        									 mutate(Total = sum(c_across(where(is.numeric)),na.rm = TRUE))%>%
                                             ungroup()
        
        									 
        # Paso a porcentaje sobre el total:
        
         base_var_total_wide[,-1] <- (base_var_total_wide[,-1]/as.numeric(unlist(base_var_total_wide[,'Total'])))*100
         
         
         # Reemplazo NA por cero y no tomo el % Total
         
         base_var_total_final <- base_var_total_wide %>%
                                              mutate_if(is.numeric, replace_na, 0)%>%
                                              select(-Total)%>%
                                              #arrange(desc(categoria))
        									  arrange(categoria)
         base_var_total_final_div$data<- as.data.frame( base_var_total_final)
 
}) 

base_var_total_final_grup  <- reactiveValues(data = NULL)

observeEvent(input$sel_periodo3, {

req(base_comportamiendo_p$data)
        
        
        # Se realiza la consulta y se filtra para tomar sólo las aperturas con clasificación 
        base_precios_con_variaciones <- base_comportamiendo_p$data %>% 
          filter(! apertura=='Sin asignar') %>%
          filter(! apertura=='OTROS-EXCLUIR') 
          
        base_precios_con_variaciones <- base_precios_con_variaciones %>% 
           mutate(precio = case_when(
             !is.na(precio_tachado) ~ precio_tachado,
        	 TRUE ~ precio))
        
        #Hay precios que vienen más de una vez en la bajada, hago un promedio simple
        	 
        base_prom_simple<- base_precios_con_variaciones %>% group_by(periodo,informante,nombre_informante,link,producto_super,codigo_pba_scrap,apertura,sucursal) %>%
                summarise(precio=mean(precio)) %>% ungroup()
        
        # Separo el precio por periodo
        base_wide<- base_prom_simple  %>%
           filter(! (precio <= 0)) %>% #saco los precios menores o iguales a 0
           pivot_wider(names_from=periodo,values_from=precio) 
           
        # Detectar las columnas de precios (de periodos)
		
		cols_fijas <- c("periodo", "informante", "nombre_informante", "link", 
                "producto_super", "codigo_pba_scrap", "apertura", "sucursal")
		
        precio_cols <- setdiff(names(base_wide), cols_fijas)
        
        # Chequeo: ¿hay dos columnas de precio?
        if(length(precio_cols) != 2) {
          showNotification("No hay datos suficientes en ambos periodos para calcular la variación.", type = "error")
          base_var_total_final_div$data <- NULL
          return()
        }
        
        # Ordenar los periodos:
        precio_cols <- sort(precio_cols)
        
        # Renombrar explícitamente las columnas de periodo
        base_wide <- base_wide %>%
          rename(precio_0 = all_of(precio_cols[1]), precio_1 = all_of(precio_cols[2])) 
        
        # Calculo la variación para los precios que se encuentran en ambos periodos
        
        base_calc_apertura_inf <-  base_wide %>% 
          mutate(variacion=precio_1/precio_0-1) %>%
          filter(!is.na(precio_1) & !is.na(precio_0))%>%
                          mutate(
        				  nivel = substr(codigo_pba_scrap,1,4),
        				  categoria =
        				    case_when (
        				     nivel=='A011' ~	'Alimentos',
                             nivel=='A012' ~	'Bebidas no alcohólicas',
                             nivel=='A021' ~	'Cervezas',
                             nivel=='A022' ~	'Vinos',
                             nivel=='A031' ~	'Jabones y otros limpiadores para la ropa',
                             nivel=='A032' ~	'Detergentes y otros limpiadores para baño y cocina',
                             nivel=='A033' ~	'Otros artículos de limpieza',
                             nivel=='A041' ~	'Jabones, champú, acondicionador y otros',
                             nivel=='A042' ~	'Descartables para el cuidado personal',
                             nivel=='A043' ~	'Otros productos para el cuidado personal',
        				     TRUE ~ 'DESCARTE'
                                       )
        				  	     ) %>%
        				  arrange(nivel)%>%
        				  mutate ( categoria = factor(categoria, levels = 
        				  c('Alimentos',
                            'Bebidas no alcohólicas',
                            'Cervezas',
                            'Vinos',
                            'Jabones y otros limpiadores para la ropa',
                            'Detergentes y otros limpiadores para baño y cocina',
                            'Otros artículos de limpieza',
                            'Jabones, champú, acondicionador y otros',
                            'Descartables para el cuidado personal',
                            'Otros productos para el cuidado personal',
        				    'DESCARTE'
                                  )
        						  ) 
        						  )
          
        # Separo la base según signo de variación y creo variable signo de variación
        
        base_var_total <- base_calc_apertura_inf %>%
                                     mutate(signo_variacion = case_when (
        							 variacion < 0     ~ 'Baja de precios',
        							 variacion == 0    ~ 'Sin variación',
        							 variacion >0      ~ 'Suba de precios')
        							       )  
        
        # Cuento el tipo de magnitud								   
        base_var_total_long<- base_var_total %>%
                                           group_by(categoria) %>%
                                           count(signo_variacion)
        
        # Paso formato a long y calculo totales por fila
        base_var_total_wide <- 	base_var_total_long %>%
                                             pivot_wider(names_from=signo_variacion,values_from= n) %>%
        									 ungroup()%>%
                                             rowwise() %>% 
        									 mutate(Total = sum(c_across(where(is.numeric)),na.rm = TRUE))%>%
                                             ungroup()
        
        									 
        # Paso a porcentaje sobre el total:
        
         base_var_total_wide[,-1] <- (base_var_total_wide[,-1]/as.numeric(unlist(base_var_total_wide[,'Total'])))*100
         
         
         # Reemplazo NA por cero y no tomo el % Total
         
         base_var_total_final <- base_var_total_wide %>%
                                              mutate_if(is.numeric, replace_na, 0)%>%
                                              select(-Total)%>%
                                              #arrange(desc(categoria))
        									  arrange(categoria)
         base_var_total_final_grup$data<- as.data.frame( base_var_total_final)
 
})

base_var_total_final_sup  <- reactiveValues(data = NULL)

observeEvent(input$sel_periodo3, {

req(base_comportamiendo_p$data)
        
        
        # Se realiza la consulta y se filtra para tomar sólo las aperturas con clasificación 
        base_precios_con_variaciones <- base_comportamiendo_p$data %>%
		 filter(informante %in% c(
		   'W01 ',
           'W02 ',
           'W03 ',
           'W04 ',
           'W06 ',
           'W07 ',
           'W08 ',
           'W09 ',
           'W05 ')
          )
		  
        base_precios_con_variaciones <- base_precios_con_variaciones %>% 
           mutate(precio = case_when(
             !is.na(precio_tachado) ~ precio_tachado,
        	 TRUE ~ precio))
        
        #Hay precios que vienen más de una vez en la bajada, hago un promedio simple
        	 
        base_prom_simple<- base_precios_con_variaciones %>% group_by(periodo,informante,nombre_informante,link,producto_super,codigo_pba_scrap,apertura,sucursal) %>%
                summarise(precio=mean(precio)) %>% ungroup()
        
        # Separo el precio por periodo
        base_wide<- base_prom_simple  %>%
           filter(! (precio <= 0)) %>% #saco los precios menores o iguales a 0
           pivot_wider(names_from=periodo,values_from=precio) 
           
        # Renombro
        
        # Detectar las columnas de precios (de periodos)
		
		cols_fijas <- c("periodo", "informante", "nombre_informante", "link", 
                "producto_super", "codigo_pba_scrap", "apertura", "sucursal")
		
        precio_cols <- setdiff(names(base_wide), cols_fijas)
        
        # Chequeo: ¿hay dos columnas de precio?
        if(length(precio_cols) != 2) {
          showNotification("No hay datos suficientes en ambos periodos para calcular la variación.", type = "error")
          base_var_total_final_div$data <- NULL
          return()
        }
        
        # Ordenar los periodos:
        precio_cols <- sort(precio_cols)
        
        # Renombrar explícitamente las columnas de periodo
        base_wide <- base_wide %>%
          rename(precio_0 = all_of(precio_cols[1]), precio_1 = all_of(precio_cols[2]))
        
        # Calculo la variación para los precios que se encuentran en ambos periodos
        
        base_calc_apertura_inf <-  base_wide %>% 
          mutate(variacion=precio_1/precio_0-1) %>%
          filter(!is.na(precio_1) & !is.na(precio_0))%>%
                          mutate(
        				  categoria =
        				    case_when (
        				     informante=='W01 ' ~	'Chango Más',
                             informante=='W02 ' ~	'Disco',
                             informante=='W03 ' ~	'Vea',
                             informante=='W04 ' ~	'Jumbo',
                             informante=='W06 ' ~	'Carrefour',
                             informante=='W07 ' ~	'Toledo',
                             informante=='W08 ' ~	'La Coope',
                             informante=='W09 ' ~	'Día',
                             informante=='W05 ' ~	'Coto',
        				     TRUE ~ 'DESCARTE'
                                       )
        				  	     ) %>%
        				  #arrange(nivel)%>%
        				  mutate ( categoria = factor(categoria, levels = 
        				  c('Chango Más',
                            'Disco',
                            'Vea',
                            'Jumbo',
                            'Carrefour',
                            'Toledo',
                            'La Coope',
                            'Día',
                            'Coto',
        				    'DESCARTE'
                                  )
        						  ) 
        						  )
          
        # Separo la base según signo de variación y creo variable signo de variación
        
        base_var_total <- base_calc_apertura_inf %>%
                                     mutate(signo_variacion = case_when (
        							 variacion < 0     ~ 'Baja de precios',
        							 variacion == 0    ~ 'Sin variación',
        							 variacion >0      ~ 'Suba de precios')
        							       )  
        
        # Cuento el tipo de magnitud								   
        base_var_total_long<- base_var_total %>%
                                           group_by(categoria) %>%
                                           count(signo_variacion)
        
        # Paso formato a long y calculo totales por fila
        base_var_total_wide <- 	base_var_total_long %>%
                                             pivot_wider(names_from=signo_variacion,values_from= n) %>%
        									 ungroup()%>%
                                             rowwise() %>% 
        									 mutate(Total = sum(c_across(where(is.numeric)),na.rm = TRUE))%>%
                                             ungroup()
        
        									 
        # Paso a porcentaje sobre el total:
        
         base_var_total_wide[,-1] <- (base_var_total_wide[,-1]/as.numeric(unlist(base_var_total_wide[,'Total'])))*100
         
         
         # Reemplazo NA por cero y no tomo el % Total
         
         base_var_total_final <- base_var_total_wide %>%
                                              mutate_if(is.numeric, replace_na, 0)%>%
                                              select(-Total)%>%
                                              #arrange(desc(categoria))
        									  arrange(categoria)
         base_var_total_final_sup$data<- as.data.frame( base_var_total_final)
 
})


base_var_total_final_part  <- reactiveValues(data = NULL)

observeEvent(input$sel_periodo3, {

req(base_comportamiendo_p$data)
        
        
        # Se realiza la consulta y se filtra para tomar sólo las aperturas con clasificación 
        base_precios_con_variaciones <- base_comportamiendo_p$data %>%
		 filter(partido != "Sin definir", partido != "San Isidro" 
          )
		  
        base_precios_con_variaciones <- base_precios_con_variaciones %>% 
           mutate(precio = case_when(
             !is.na(precio_tachado) ~ precio_tachado,
        	 TRUE ~ precio))
        
        #Hay precios que vienen más de una vez en la bajada, hago un promedio simple
        	 
        base_prom_simple<- base_precios_con_variaciones %>% group_by(periodo,informante,nombre_informante,link,producto_super,codigo_pba_scrap,sucursal,partido) %>%
                summarise(precio=mean(precio)) %>% ungroup()
        
        # Separo el precio por periodo
        base_wide<- base_prom_simple  %>%
           filter(! (precio <= 0)) %>% #saco los precios menores o iguales a 0
           pivot_wider(names_from=periodo,values_from=precio) 
           
        # Renombro
        
        # Detectar las columnas de precios (de periodos)
		
		cols_fijas <- c("periodo", "informante", "nombre_informante", "link", 
                "producto_super", "codigo_pba_scrap", "apertura", "sucursal","partido")
		
        precio_cols <- setdiff(names(base_wide), cols_fijas)
        
        # Chequeo: ¿hay dos columnas de precio?
        if(length(precio_cols) != 2) {
          showNotification("No hay datos suficientes en ambos periodos para calcular la variación.", type = "error")
          base_var_total_final_div$data <- NULL
          return()
        }
        
        # Ordenar los periodos:
        precio_cols <- sort(precio_cols)
        
        # Renombrar explícitamente las columnas de periodo
        base_wide <- base_wide %>%
          rename(precio_0 = all_of(precio_cols[1]), precio_1 = all_of(precio_cols[2])) 
        
        # Calculo la variación para los precios que se encuentran en ambos periodos
        
        base_calc_apertura_inf <-  base_wide %>% 
          mutate(variacion=precio_1/precio_0-1) %>%
          filter(!is.na(precio_1) & !is.na(precio_0))%>%
                          mutate(
        				  categoria = partido
        				    
                                       ) %>%
        				  #arrange(nivel)%>%
        				  mutate ( categoria = factor(categoria, levels = 
        				  c('Avellaneda',
                            'La Matanza',
                            'Lomas de Zamora',
                            'Moron',
                            'Tigre',
                            'Vicente López'
                                  )
        						  ) 
        						  )
          
        # Separo la base según signo de variación y creo variable signo de variación
        
        base_var_total <- base_calc_apertura_inf %>%
                                     mutate(signo_variacion = case_when (
        							 variacion < 0     ~ 'Baja de precios',
        							 variacion == 0    ~ 'Sin variación',
        							 variacion >0      ~ 'Suba de precios')
        							       )  
        
        # Cuento el tipo de magnitud								   
        base_var_total_long<- base_var_total %>%
                                           group_by(categoria) %>%
                                           count(signo_variacion)
        
        # Paso formato a long y calculo totales por fila
        base_var_total_wide <- 	base_var_total_long %>%
                                             pivot_wider(names_from=signo_variacion,values_from= n) %>%
        									 ungroup()%>%
                                             rowwise() %>% 
        									 mutate(Total = sum(c_across(where(is.numeric)),na.rm = TRUE))%>%
                                             ungroup()
        
        									 
        # Paso a porcentaje sobre el total:
        
         base_var_total_wide[,-1] <- (base_var_total_wide[,-1]/as.numeric(unlist(base_var_total_wide[,'Total'])))*100
         
         
         # Reemplazo NA por cero y no tomo el % Total
         
         base_var_total_final <- base_var_total_wide %>%
                                              mutate_if(is.numeric, replace_na, 0)%>%
                                              select(-Total)%>%
                                              #arrange(desc(categoria))
        									  arrange(categoria)
         base_var_total_final_part$data<- as.data.frame( base_var_total_final)
 
})


base_var_total_cuadro_div  <- reactiveValues(data = NULL)

observeEvent(input$sel_periodo3, {

req(base_comportamiendo_p$data)
        
        
        # Se realiza la consulta y se filtra para tomar sólo las aperturas con clasificación 
        base_precios_con_variaciones <- base_comportamiendo_p$data %>% 
          filter( apertura !='Sin asignar') %>%
          filter( apertura !='OTROS-EXCLUIR') 
          
        base_precios_con_variaciones <- base_precios_con_variaciones %>% 
          mutate(precio = case_when(
            !is.na(precio_tachado) ~ precio_tachado,
           TRUE ~ precio))

         #Hay precios que vienen más de una vez en la bajada, hago un promedio simple
         
         base_prom_simple<- base_precios_con_variaciones %>% group_by(periodo,informante,nombre_informante,link,producto_super,codigo_pba_scrap,apertura,sucursal) %>%
                 summarise(precio=mean(precio)) %>% ungroup()
				 


         # Separo el precio por periodo
         base_wide<- base_prom_simple  %>%
            filter(! (precio <= 0)) %>% #saco los precios menores o iguales a 0
            pivot_wider(names_from=periodo,values_from=precio) 
			
		 print(nrow(base_wide))
         print(names(base_wide))
            
         # Renombro
         
         # Detectar las columnas de precios (de periodos)
		
		 cols_fijas <- c("periodo", "informante", "nombre_informante", "link", 
                 "producto_super", "codigo_pba_scrap", "apertura", "sucursal")
		 
         precio_cols <- setdiff(names(base_wide), cols_fijas)
         
         # Chequeo: ¿hay dos columnas de precio?
         if(length(precio_cols) != 2) {
           showNotification("No hay datos suficientes en ambos periodos para calcular la variación.", type = "error")
           base_var_total_final_div$data <- NULL
           return()
         }
         
         # Ordenar los periodos:
         precio_cols <- sort(precio_cols)
         
         # Renombrar explícitamente las columnas de periodo
         base_wide <- base_wide %>%
           rename(precio_0 = all_of(precio_cols[1]), precio_1 = all_of(precio_cols[2]))
         
         # Calculo la variación para los precios que se encuentran en ambos periodos
         
         base_calc_apertura_inf <-  base_wide %>% 
           mutate(variacion=(precio_1/precio_0-1)*100) %>%
           filter(!is.na(precio_1) & !is.na(precio_0))%>%
                           mutate(
         				  nivel = substr(codigo_pba_scrap,1,3),
         				  categoria =
         				    case_when (
         				     nivel=='A01' ~ 'ALIMENTOS Y BEBIDAS NO ALCOHÓLICAS',
         				     nivel=='A02' ~ 'BEBIDAS ALCOHÓLICAS',
         				     nivel=='A03' ~ 'LIMPIEZA DEL HOGAR',
         				     nivel=='A04' ~ 'HIGIENE CORPORAL',
         				     TRUE ~ 'DESCARTE'
                                        )
         				  	     ) %>%
         				  arrange(nivel)%>%
         				  mutate ( categoria = factor(categoria, levels = 
         				  c('ALIMENTOS Y BEBIDAS NO ALCOHÓLICAS',
                           'BEBIDAS ALCOHÓLICAS',
                           'LIMPIEZA DEL HOGAR',
                           'HIGIENE CORPORAL',
         				  'DESCARTE'
                                   )
         						  ) 
         						  )
           
         # Separo la base según signo de variación y creo variable magnitud de variación
         
         base_var_positiva <- base_calc_apertura_inf %>%
                                      filter(variacion > 0) %>% 
                                      mutate(magnitud_variacion = case_when (
         							 variacion <= 5     ~ 'Hasta 5',
         							 variacion > 5 & variacion <= 10 ~ 'Entre 5 y 10',
         							 variacion >10 & variacion <= 30 ~ 'Entre 10 y 30',
                                      TRUE                 ~ 'Más del 30')
         							       )%>%
         						    mutate(magnitud_variacion = factor(magnitud_variacion, levels = 
            							                   c('Hasta 5',
         											   'Entre 5 y 10',
         											   'Entre 10 y 30',
         											   'Más del 30'
         											   )             )
         								   )  
         
         # Cuento el tipo de magnitud								   
         base_var_positiva_long<- base_var_positiva %>%
                                            group_by(categoria) %>%
                                            count(magnitud_variacion)
         
         # Paso formato a long y calculo totales por fila
         base_var_positiva_wide <- 	base_var_positiva_long %>%
                                              pivot_wider(names_from=magnitud_variacion,values_from= n) %>%
         									 ungroup()%>%
                                              rowwise() %>% 
         									 mutate(Total = sum(c_across(where(is.numeric)),na.rm = TRUE))%>%
                                              ungroup()
         
         									 
         # Paso a porcentaje sobre el total:
         
          base_var_positiva_wide[,-1] <-(base_var_positiva_wide[,-1]/as.numeric(unlist(base_var_positiva_wide[,'Total'])))*100
          
          
          # Reemplazo NA por cero y no tomo el % Total
          
          base_var_positiva_final <- base_var_positiva_wide %>%
                                               mutate_if(is.numeric, replace_na, 0)%>%
                                               select(-Total)
         
         
         base_var_negativa <- base_calc_apertura_inf %>%
                                      filter(variacion < 0) %>% 
                                      mutate(magnitud_variacion = case_when (
         							 variacion >= -5     ~ 'Hasta -5',
         							 variacion < -5 & variacion >= -10 ~ 'Entre -5 y -10',
         							 variacion < -10 & variacion >= -30 ~ 'Entre -10 y -30',
                                      TRUE                 ~ '-30 o más')
         							       )%>%
         						    mutate(magnitud_variacion = factor(magnitud_variacion, levels = 
            							                   c(
         											   '-30 o más',
         											   'Entre -10 y -30',
         											   'Entre -5 y -10',
         											   'Hasta -5'
         											   )             )
         								   )   
         
         # Cuento el tipo de magnitud								   
         base_var_negativa_long<- base_var_negativa %>%
                                            group_by(categoria) %>%
                                            count(magnitud_variacion)
         
         # Paso formato a long y calculo totales por fila
         base_var_negativa_wide <- 	base_var_negativa_long %>%
                                              pivot_wider(names_from=magnitud_variacion,values_from= n) %>%
         									 ungroup()%>%
                                              rowwise() %>% 
         									 mutate(Total = sum(c_across(where(is.numeric)),na.rm = TRUE))%>%
                                              ungroup()
         									 
         
         # Paso a porcentaje sobre el total:
         
          base_var_negativa_wide[,-1] <-(base_var_negativa_wide[,-1]/as.numeric(unlist(base_var_negativa_wide[,'Total'])))*100
          
          
          base_var_total_cuadro_div$data<-select(base_var_negativa_wide,-c(Total)) %>%
                          left_join(select(base_var_positiva_wide,-c(Total)))%>%
         				 relocate(categoria, .before='Hasta 5')%>%
                         mutate_if(is.numeric, replace_na, 0)
 
})

base_var_total_cuadro_grup  <- reactiveValues(data = NULL)

observeEvent(input$sel_periodo3, {

req(base_comportamiendo_p$data)
        
        
        # Se realiza la consulta y se filtra para tomar sólo las aperturas con clasificación 
        base_precios_con_variaciones <- base_comportamiendo_p$data %>% 
          filter(! apertura=='Sin asignar') %>%
          filter(! apertura=='OTROS-EXCLUIR') 
          
        base_precios_con_variaciones <- base_precios_con_variaciones %>% 
          mutate(precio = case_when(
            !is.na(precio_tachado) ~ precio_tachado,
           TRUE ~ precio))

         #Hay precios que vienen más de una vez en la bajada, hago un promedio simple
         
         base_prom_simple<- base_precios_con_variaciones %>% group_by(periodo,informante,nombre_informante,link,producto_super,codigo_pba_scrap,apertura,sucursal) %>%
                 summarise(precio=mean(precio)) %>% ungroup()
         
         # Separo el precio por periodo
         base_wide<- base_prom_simple  %>%
            filter(! (precio <= 0)) %>% #saco los precios menores o iguales a 0
            pivot_wider(names_from=periodo,values_from=precio) 
            
         # Renombro
         
         # Detectar las columnas de precios (de periodos)
		
		 cols_fijas <- c("periodo", "informante", "nombre_informante", "link", 
                 "producto_super", "codigo_pba_scrap", "apertura", "sucursal")
		 
         precio_cols <- setdiff(names(base_wide), cols_fijas)
         
         # Chequeo: ¿hay dos columnas de precio?
         if(length(precio_cols) != 2) {
           showNotification("No hay datos suficientes en ambos periodos para calcular la variación.", type = "error")
           base_var_total_final_div$data <- NULL
           return()
         }
         
         # Ordenar los periodos:
         precio_cols <- sort(precio_cols)
         
         # Renombrar explícitamente las columnas de periodo
         base_wide <- base_wide %>%
           rename(precio_0 = all_of(precio_cols[1]), precio_1 = all_of(precio_cols[2])) 
         
         # Calculo la variación para los precios que se encuentran en ambos periodos
         
         base_calc_apertura_inf <-  base_wide %>% 
           mutate(variacion=(precio_1/precio_0-1)*100) %>%
           filter(!is.na(precio_1) & !is.na(precio_0))%>%
                           mutate(
         				  nivel = substr(codigo_pba_scrap,1,4),
                          categoria= 
         				  case_when (
        				     nivel=='A011' ~	'Alimentos',
                             nivel=='A012' ~	'Bebidas no alcohólicas',
                             nivel=='A021' ~	'Cervezas',
                             nivel=='A022' ~	'Vinos',
                             nivel=='A031' ~	'Jabones y otros limpiadores para la ropa',
                             nivel=='A032' ~	'Detergentes y otros limpiadores para baño y cocina',
                             nivel=='A033' ~	'Otros artículos de limpieza',
                             nivel=='A041' ~	'Jabones, champú, acondicionador y otros',
                             nivel=='A042' ~	'Descartables para el cuidado personal',
                             nivel=='A043' ~	'Otros productos para el cuidado personal',
        				     TRUE ~ 'DESCARTE'
                                       )
         				  	     ) %>%
         				  arrange(nivel)%>%
         				  mutate ( categoria = factor(categoria, levels = 
         				  c('Alimentos',
                            'Bebidas no alcohólicas',
                            'Cervezas',
                            'Vinos',
                            'Jabones y otros limpiadores para la ropa',
                            'Detergentes y otros limpiadores para baño y cocina',
                            'Otros artículos de limpieza',
                            'Jabones, champú, acondicionador y otros',
                            'Descartables para el cuidado personal',
                            'Otros productos para el cuidado personal',
        				    'DESCARTE'
                                  )
         						  ) 
         						  )
           
         # Separo la base según signo de variación y creo variable magnitud de variación
         
         base_var_positiva <- base_calc_apertura_inf %>%
                                      filter(variacion > 0) %>% 
                                      mutate(magnitud_variacion = case_when (
         							 variacion <= 5     ~ 'Hasta 5',
         							 variacion > 5 & variacion <= 10 ~ 'Entre 5 y 10',
         							 variacion >10 & variacion <= 30 ~ 'Entre 10 y 30',
                                      TRUE                 ~ 'Más del 30')
         							       )%>%
         						    mutate(magnitud_variacion = factor(magnitud_variacion, levels = 
            							                   c('Hasta 5',
         											   'Entre 5 y 10',
         											   'Entre 10 y 30',
         											   'Más del 30'
         											   )             )
         								   )  
         
         # Cuento el tipo de magnitud								   
         base_var_positiva_long<- base_var_positiva %>%
                                            group_by(categoria) %>%
                                            count(magnitud_variacion)
         
         # Paso formato a long y calculo totales por fila
         base_var_positiva_wide <- 	base_var_positiva_long %>%
                                              pivot_wider(names_from=magnitud_variacion,values_from= n) %>%
         									 ungroup()%>%
                                              rowwise() %>% 
         									 mutate(Total = sum(c_across(where(is.numeric)),na.rm = TRUE))%>%
                                              ungroup()
         
         									 
         # Paso a porcentaje sobre el total:
         
          base_var_positiva_wide[,-1] <-(base_var_positiva_wide[,-1]/as.numeric(unlist(base_var_positiva_wide[,'Total'])))*100
          
          
          # Reemplazo NA por cero y no tomo el % Total
          
          base_var_positiva_final <- base_var_positiva_wide %>%
                                               mutate_if(is.numeric, replace_na, 0)%>%
                                               select(-Total)
         
         
         base_var_negativa <- base_calc_apertura_inf %>%
                                      filter(variacion < 0) %>% 
                                      mutate(magnitud_variacion = case_when (
         							 variacion >= -5     ~ 'Hasta -5',
         							 variacion < -5 & variacion >= -10 ~ 'Entre -5 y -10',
         							 variacion < -10 & variacion >= -30 ~ 'Entre -10 y -30',
                                      TRUE                 ~ '-30 o más')
         							       )%>%
         						    mutate(magnitud_variacion = factor(magnitud_variacion, levels = 
            							                   c(
         											   '-30 o más',
         											   'Entre -10 y -30',
         											   'Entre -5 y -10',
         											   'Hasta -5'
         											   )             )
         								   )   
         
         # Cuento el tipo de magnitud								   
         base_var_negativa_long<- base_var_negativa %>%
                                            group_by(categoria) %>%
                                            count(magnitud_variacion)
         
         # Paso formato a long y calculo totales por fila
         base_var_negativa_wide <- 	base_var_negativa_long %>%
                                              pivot_wider(names_from=magnitud_variacion,values_from= n) %>%
         									 ungroup()%>%
                                              rowwise() %>% 
         									 mutate(Total = sum(c_across(where(is.numeric)),na.rm = TRUE))%>%
                                              ungroup()
         									 
         
         # Paso a porcentaje sobre el total:
         
          base_var_negativa_wide[,-1] <-(base_var_negativa_wide[,-1]/as.numeric(unlist(base_var_negativa_wide[,'Total'])))*100
          
          
          base_var_total_cuadro_grup$data<-select(base_var_negativa_wide,-c(Total)) %>%
                          left_join(select(base_var_positiva_wide,-c(Total)))%>%
         				 relocate(categoria, .before='Hasta 5')%>%
                         mutate_if(is.numeric, replace_na, 0)
 
})

base_var_total_cuadro_sup  <- reactiveValues(data = NULL)

observeEvent(input$sel_periodo3, {

req(base_comportamiendo_p$data)
        
        
        # Se realiza la consulta y se filtra para tomar sólo las aperturas con clasificación 
        base_precios_con_variaciones <- base_comportamiendo_p$data %>%
		 filter(informante %in% c(
		   'W01 ',
           'W02 ',
           'W03 ',
           'W04 ',
           'W06 ',
           'W07 ',
           'W08 ',
           'W09 ',
           'W05 ')
          )
          
        base_precios_con_variaciones <- base_precios_con_variaciones %>% 
          mutate(precio = case_when(
            !is.na(precio_tachado) ~ precio_tachado,
           TRUE ~ precio))

         #Hay precios que vienen más de una vez en la bajada, hago un promedio simple
         
         base_prom_simple<- base_precios_con_variaciones %>% group_by(periodo,informante,nombre_informante,link,producto_super,codigo_pba_scrap,apertura,sucursal) %>%
                 summarise(precio=mean(precio)) %>% ungroup()
         
         # Separo el precio por periodo
         base_wide<- base_prom_simple  %>%
            filter(! (precio <= 0)) %>% #saco los precios menores o iguales a 0
            pivot_wider(names_from=periodo,values_from=precio) 
            
         # Renombro
         
         # Detectar las columnas de precios (de periodos)
		
		 cols_fijas <- c("periodo", "informante", "nombre_informante", "link", 
                 "producto_super", "codigo_pba_scrap", "apertura", "sucursal")
		 
         precio_cols <- setdiff(names(base_wide), cols_fijas)
         
         # Chequeo: ¿hay dos columnas de precio?
         if(length(precio_cols) != 2) {
           showNotification("No hay datos suficientes en ambos periodos para calcular la variación.", type = "error")
           base_var_total_final_div$data <- NULL
           return()
         }
         
         # Ordenar los periodos:
         precio_cols <- sort(precio_cols)
         
         # Renombrar explícitamente las columnas de periodo
         base_wide <- base_wide %>%
           rename(precio_0 = all_of(precio_cols[1]), precio_1 = all_of(precio_cols[2]))
         
         # Calculo la variación para los precios que se encuentran en ambos periodos
         
         base_calc_apertura_inf <-  base_wide %>% 
           mutate(variacion=(precio_1/precio_0-1)*100) %>%
           filter(!is.na(precio_1) & !is.na(precio_0))%>%
                           mutate(
         				  nivel = substr(codigo_pba_scrap,1,4),
                          categoria= 
         				   case_when (
        				     informante=='W01 ' ~	'Chango Más',
                             informante=='W02 ' ~	'Disco',
                             informante=='W03 ' ~	'Vea',
                             informante=='W04 ' ~	'Jumbo',
                             informante=='W06 ' ~	'Carrefour',
                             informante=='W07 ' ~	'Toledo',
                             informante=='W08 ' ~	'La Coope',
                             informante=='W09 ' ~	'Día',
                             informante=='W05 ' ~	'Coto',
        				     TRUE ~ 'DESCARTE'
                                       )
         				  	     ) %>%
         				  #arrange(nivel)%>%
         				  mutate ( categoria = factor(categoria, levels = 
         				  c('Chango Más',
                            'Disco',
                            'Vea',
                            'Jumbo',
                            'Carrefour',
                            'Toledo',
                            'La Coope',
                            'Día',
                            'Coto',
        				    'DESCARTE'
                                  )
         						  ) 
         						  )
           
         # Separo la base según signo de variación y creo variable magnitud de variación
         
         base_var_positiva <- base_calc_apertura_inf %>%
                                      filter(variacion > 0) %>% 
                                      mutate(magnitud_variacion = case_when (
         							 variacion <= 5     ~ 'Hasta 5',
         							 variacion > 5 & variacion <= 10 ~ 'Entre 5 y 10',
         							 variacion >10 & variacion <= 30 ~ 'Entre 10 y 30',
                                      TRUE                 ~ 'Más del 30')
         							       )%>%
         						    mutate(magnitud_variacion = factor(magnitud_variacion, levels = 
            							                   c('Hasta 5',
         											   'Entre 5 y 10',
         											   'Entre 10 y 30',
         											   'Más del 30'
         											   )             )
         								   )  
         
         # Cuento el tipo de magnitud								   
         base_var_positiva_long<- base_var_positiva %>%
                                            group_by(categoria) %>%
                                            count(magnitud_variacion)
         
         # Paso formato a long y calculo totales por fila
         base_var_positiva_wide <- 	base_var_positiva_long %>%
                                              pivot_wider(names_from=magnitud_variacion,values_from= n) %>%
         									 ungroup()%>%
                                              rowwise() %>% 
         									 mutate(Total = sum(c_across(where(is.numeric)),na.rm = TRUE))%>%
                                              ungroup()
         
         									 
         # Paso a porcentaje sobre el total:
         
          base_var_positiva_wide[,-1] <-(base_var_positiva_wide[,-1]/as.numeric(unlist(base_var_positiva_wide[,'Total'])))*100
          
          
          # Reemplazo NA por cero y no tomo el % Total
          
          base_var_positiva_final <- base_var_positiva_wide %>%
                                               mutate_if(is.numeric, replace_na, 0)%>%
                                               select(-Total)
         
         
         base_var_negativa <- base_calc_apertura_inf %>%
                                      filter(variacion < 0) %>% 
                                      mutate(magnitud_variacion = case_when (
         							 variacion >= -5     ~ 'Hasta -5',
         							 variacion < -5 & variacion >= -10 ~ 'Entre -5 y -10',
         							 variacion < -10 & variacion >= -30 ~ 'Entre -10 y -30',
                                      TRUE                 ~ '-30 o más')
         							       )%>%
         						    mutate(magnitud_variacion = factor(magnitud_variacion, levels = 
            							                   c(
         											   '-30 o más',
         											   'Entre -10 y -30',
         											   'Entre -5 y -10',
         											   'Hasta -5'
         											   )             )
         								   )   
         
         # Cuento el tipo de magnitud								   
         base_var_negativa_long<- base_var_negativa %>%
                                            group_by(categoria) %>%
                                            count(magnitud_variacion)
         
         # Paso formato a long y calculo totales por fila
         base_var_negativa_wide <- 	base_var_negativa_long %>%
                                              pivot_wider(names_from=magnitud_variacion,values_from= n) %>%
         									 ungroup()%>%
                                              rowwise() %>% 
         									 mutate(Total = sum(c_across(where(is.numeric)),na.rm = TRUE))%>%
                                              ungroup()
         									 
         
         # Paso a porcentaje sobre el total:
         
          base_var_negativa_wide[,-1] <-(base_var_negativa_wide[,-1]/as.numeric(unlist(base_var_negativa_wide[,'Total'])))*100
          
          
          base_var_total_cuadro_sup$data<-select(base_var_negativa_wide,-c(Total)) %>%
                          left_join(select(base_var_positiva_wide,-c(Total)))%>%
         				 relocate(categoria, .before='Hasta 5')%>%
                         mutate_if(is.numeric, replace_na, 0)
 
})

base_var_total_cuadro_part  <- reactiveValues(data = NULL)

observeEvent(input$sel_periodo3, {

req(base_comportamiendo_p$data)
        
        
        # Se realiza la consulta y se filtra para tomar sólo las aperturas con clasificación 
        base_precios_con_variaciones <- base_comportamiendo_p$data %>%
		 filter(informante %in% c(
		   'W01 ',
           'W02 ',
           'W03 ',
           'W04 ',
           'W06 ',
           'W07 ',
           'W08 ',
           'W09 ',
           'W05 ')
          )
          
        base_precios_con_variaciones <- base_precios_con_variaciones %>% 
          mutate(precio = case_when(
            !is.na(precio_tachado) ~ precio_tachado,
           TRUE ~ precio))

         #Hay precios que vienen más de una vez en la bajada, hago un promedio simple
         
         base_prom_simple<- base_precios_con_variaciones %>% group_by(periodo,informante,nombre_informante,link,producto_super,codigo_pba_scrap,sucursal,partido) %>%
                 summarise(precio=mean(precio)) %>% ungroup()
         
         # Separo el precio por periodo
         base_wide<- base_prom_simple  %>%
            filter(! (precio <= 0)) %>% #saco los precios menores o iguales a 0
            pivot_wider(names_from=periodo,values_from=precio) 
            
         # Renombro
         
         # Detectar las columnas de precios (de periodos)
		
		 cols_fijas <- c("periodo", "informante", "nombre_informante", "link", 
                 "producto_super", "codigo_pba_scrap", "apertura", "sucursal","partido")
		 
         precio_cols <- setdiff(names(base_wide), cols_fijas)
         
         # Chequeo: ¿hay dos columnas de precio?
         if(length(precio_cols) != 2) {
           showNotification("No hay datos suficientes en ambos periodos para calcular la variación.", type = "error")
           base_var_total_final_div$data <- NULL
           return()
         }
         
         # Ordenar los periodos:
         precio_cols <- sort(precio_cols)
         
         # Renombrar explícitamente las columnas de periodo
         base_wide <- base_wide %>%
           rename(precio_0 = all_of(precio_cols[1]), precio_1 = all_of(precio_cols[2])) 
         
         # Calculo la variación para los precios que se encuentran en ambos periodos
         
         base_calc_apertura_inf <-  base_wide %>% 
           mutate(variacion=(precio_1/precio_0-1)*100) %>%
           filter(!is.na(precio_1) & !is.na(precio_0))%>%
                           mutate(
         				  nivel = substr(codigo_pba_scrap,1,4),
                          categoria= partido
         				   
         				  	     ) %>%
         				  #arrange(nivel)%>%
         				  mutate ( categoria = factor(categoria, levels = 
         				   c('Avellaneda',
                            'La Matanza',
                            'Lomas de Zamora',
                            'Moron',
                            'Tigre',
                            'Vicente López'
                                  )
         						  ) 
         						  )
           
         # Separo la base según signo de variación y creo variable magnitud de variación
         
         base_var_positiva <- base_calc_apertura_inf %>%
                                      filter(variacion > 0) %>% 
                                      mutate(magnitud_variacion = case_when (
         							 variacion <= 5     ~ 'Hasta 5',
         							 variacion > 5 & variacion <= 10 ~ 'Entre 5 y 10',
         							 variacion >10 & variacion <= 30 ~ 'Entre 10 y 30',
                                      TRUE                 ~ 'Más del 30')
         							       )%>%
         						    mutate(magnitud_variacion = factor(magnitud_variacion, levels = 
            							                   c('Hasta 5',
         											   'Entre 5 y 10',
         											   'Entre 10 y 30',
         											   'Más del 30'
         											   )             )
         								   )  
         
         # Cuento el tipo de magnitud								   
         base_var_positiva_long<- base_var_positiva %>%
                                            group_by(categoria) %>%
                                            count(magnitud_variacion)
         
         # Paso formato a long y calculo totales por fila
         base_var_positiva_wide <- 	base_var_positiva_long %>%
                                              pivot_wider(names_from=magnitud_variacion,values_from= n) %>%
         									 ungroup()%>%
                                              rowwise() %>% 
         									 mutate(Total = sum(c_across(where(is.numeric)),na.rm = TRUE))%>%
                                              ungroup()
         
         									 
         # Paso a porcentaje sobre el total:
         
          base_var_positiva_wide[,-1] <-(base_var_positiva_wide[,-1]/as.numeric(unlist(base_var_positiva_wide[,'Total'])))*100
          
          
          # Reemplazo NA por cero y no tomo el % Total
          
          base_var_positiva_final <- base_var_positiva_wide %>%
                                               mutate_if(is.numeric, replace_na, 0)%>%
                                               select(-Total)
         
         
         base_var_negativa <- base_calc_apertura_inf %>%
                                      filter(variacion < 0) %>% 
                                      mutate(magnitud_variacion = case_when (
         							 variacion >= -5     ~ 'Hasta -5',
         							 variacion < -5 & variacion >= -10 ~ 'Entre -5 y -10',
         							 variacion < -10 & variacion >= -30 ~ 'Entre -10 y -30',
                                      TRUE                 ~ '-30 o más')
         							       )%>%
         						    mutate(magnitud_variacion = factor(magnitud_variacion, levels = 
            							                   c(
         											   '-30 o más',
         											   'Entre -10 y -30',
         											   'Entre -5 y -10',
         											   'Hasta -5'
         											   )             )
         								   )   
         
         # Cuento el tipo de magnitud								   
         base_var_negativa_long<- base_var_negativa %>%
                                            group_by(categoria) %>%
                                            count(magnitud_variacion)
         
         # Paso formato a long y calculo totales por fila
         base_var_negativa_wide <- 	base_var_negativa_long %>%
                                              pivot_wider(names_from=magnitud_variacion,values_from= n) %>%
         									 ungroup()%>%
                                              rowwise() %>% 
         									 mutate(Total = sum(c_across(where(is.numeric)),na.rm = TRUE))%>%
                                              ungroup()
         									 
         
         # Paso a porcentaje sobre el total:
         
          base_var_negativa_wide[,-1] <-(base_var_negativa_wide[,-1]/as.numeric(unlist(base_var_negativa_wide[,'Total'])))*100
          
          
          base_var_total_cuadro_part$data<-select(base_var_negativa_wide,-c(Total)) %>%
                          left_join(select(base_var_positiva_wide,-c(Total)))%>%
         				 relocate(categoria, .before='Hasta 5')%>%
                         mutate_if(is.numeric, replace_na, 0)
 
})

base_var_total_final_graf <- reactiveValues(data = NULL)
base_var_total_cuadro<- reactiveValues(data = NULL)

cambios <- reactive({
    req(input$sel_periodo3)
	
    list(input$sel_periodo3,input$sel_concepto)
  })

observeEvent(cambios(), {
#if(length(cambios()) == 0){
#	stop("El argumento tiene longitud cero")
#	} else{
req(cambios())
 if (input$sel_concepto == "Divisiones") {
   base_var_total_final_graf$data <-  base_var_total_final_div$data
     } else if (input$sel_concepto == "Grupos") {
     base_var_total_final_graf$data <-  base_var_total_final_grup$data
    } else if (input$sel_concepto == "Supermercados") {
     base_var_total_final_graf$data <-  base_var_total_final_sup$data
    } else if (input$sel_concepto == "Partidos") {
    base_var_total_final_graf$data <-  base_var_total_final_part$data
    }
    }
	#}
    )
    
observeEvent(cambios(), {
#if(length(cambios()) == 0){
#	stop("El argumento tiene longitud cero")
#	} else{
req(cambios())
 if (input$sel_concepto == "Divisiones") {
   base_var_total_cuadro$data <- base_var_total_cuadro_div$data
     } else if (input$sel_concepto == "Grupos") {
     base_var_total_cuadro$data  <-  base_var_total_cuadro_grup$data
    } else if (input$sel_concepto == "Supermercados") {
     base_var_total_cuadro$data <-  base_var_total_cuadro_sup$data
    } else if (input$sel_concepto == "Partidos") {
    base_var_total_cuadro$data <-  base_var_total_cuadro_part$data
    }
    }
	#}
    )

output$hcbarras	<- renderHighchart({

req(base_var_total_final_graf$data)


        
        
        
columnas<- "categoria"
barras<-  names(base_var_total_final_graf$data)[-1]

orden_barras <- list(
   "Baja de precios"= 2,                  
   "Sin variación"= 1,                                                 
 "Suba de precios" = 0
)

orden_barras_v <- as.numeric(orden_barras)

stacked_bar_chart(
  data = base_var_total_final_graf$data,
  categories_column = columnas,
  measure_columns = barras,
  stacking_type = "normal",
  explicit_order = orden_barras_v
) %>% 
  hc_plotOptions(bar = list(
    stacking = "normal",
    enableMouseTracking = TRUE))%>%
  hc_tooltip(pointFormat =  "{series.name} <br>
      {point.y:.1f} p.p. ")%>%
	  hc_title(text = paste0("Precios online por ",input$sel_concepto,". Comportamiento en las variaciones de precios. Semana selecionada. En porcentaje"),style = list(color= "#003136", useHTML = TRUE))%>%
	  hc_xAxis(type="category",labels=list(align='right',autoRotation=FALSE,padding=3,style=list(fontWeight='bold'),margin=4,useHTML=TRUE)         
         ,visible= TRUE,title=list(text=""))%>%
	  hc_yAxis(visible= FALSE
					  ) %>% hc_colors(c('#E81F8A','#00AEC3','#584596'))  %>%
hc_legend(layout="horizontal",align='center')%>% 
hc_exporting(
    enabled = TRUE, 
	scale= 1,
	printMaxWidth =900,
	fallbackToExportServer = FALSE,
	csv=list(columnHeaderFormatter=JS(paste0("function(item, key) {
                if (!item || item instanceof Highcharts.Axis) {
                    return ''
                } else {
                    return item.name;
                }
            }"
			)),	decimalPoint = ',' , dateFormat= '%d-%m-%y',itemDelimiter=';'),
	filename="datos del gráfico",
    buttons = list(contextButton = list( menuItems= c('viewFullscreen','printChart','downloadCSV','downloadPNG'
	)))
    ) %>%
  hc_credits(enabled=TRUE)%>%
  hc_add_theme(tema_elegido$data) 
})


output$cuadrocomportamiento <- render_gt({
    titcuadrocolor<-paste0("<span style='color:#00aec3'><strong>Precios online por ",input$sel_concepto, ". Distribución de caídas y aumentos de precios por rangos de variaciones. Semana seleccionada. En porcentaje </strong></span>")

 
 base_var_total_cuadro$data %>% # base
 gt(id = "cuadrocategoria")%>% # se crea la tabla
 data_color(direction='row',columns=c(1:4),palette= c("#74689E", "#D12E7E","#E81F76"))%>% # formato de color condicional para las var negativas
  data_color(direction='row',columns=c(6:9),palette= c("#417099", "#3C5B9D","#584596"))%>% # formato de color condicional para las var positivas
  fmt_number(
    columns = c(1:4,6:9),
    decimals = 1,
    dec_mark = ",",
    sep_mark = "."
  )%>% # se pone un decimal a los números
	tab_options(
	column_labels.border.top.width=3,
	column_labels.border.top.color="white",
	column_labels.border.bottom.width=3,
	column_labels.border.bottom.color="white",
	table.border.top.width = 1,
	table.border.top.color="white",
	table.border.bottom.width = 3,
	table.border.bottom.color="white",
	table_body.hlines.style = "none",
	data_row.padding = px(4)
  )%>% # opciones sobre los bordes
  tab_style(
    style = list(
      cell_text(weight = "bold",
                color = "white",
                v_align = "middle",
                align = "center",
				size= px(12))
				),
	locations= cells_body(columns =c(1:4,6:9)
				)
				)%>% # aplicación sobre las columnas numéricas
  cols_width(categoria~px(190),
             everything()~px(75))%>% # ancho de columnas
  tab_style(
    style = cell_text(align = "center", color='#595959',size= px(13)),  
    locations = cells_column_labels(everything())
  )%>% # formato para los nombres de las columnas
  tab_style(
    style = cell_text(align = "center", color='white'),  
    locations = cells_column_labels('categoria')
  )%>% # se esconde el nombre de una columna
  tab_style(
    style = list(
      cell_text(
                color = '#595959',
                v_align = "middle",
                align = "center",
				size= px(13))
				),
	locations= cells_body(columns =c(5)
				)
				)%>%				# aplicación sobre los nombres de las categorías
 tab_header(
   title = html(titcuadrocolor)
 )%>%
  opt_css(                                 
    css = "
    #cuadrocategoria .gt_title {
    font-size: 100%;
	text-align: left;
    font-weight: initial;
    padding-top: 1px;
    padding-bottom: 15px;
    border-bottom-color: #FFFFFF;
    border-bottom-width: 0;
    }
	")
    })
    

}	  


shinyApp(ui = ui, server = server)