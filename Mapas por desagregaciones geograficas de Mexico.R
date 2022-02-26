################################################################################
#########  Mapas por desagregaciones geográficas de México          ############
################################################################################


##### Paqueterías que se usaron en el trabajo 
#install.packages("pacman")
require(pacman)
# Se instalan los paquetes necesarios para trabajar con los mapas 
p_load(showtext, dplyr, forcats, ggplot2, ggthemes, ggmap, knitr, kableExtra, 
       openxlsx, readxl, RColorBrewer, rgal, sp, spdplyr, tidyverse, tmap, 
       tmaptools, tibble, unikn, viridisLite)


require(dplyr)          #A Grammar of Data Manipulation 
require(forcats)        #Tools for Working with Categorical Variables (Factors)
require(ggplot2)        # Generar gráficos ggplot y la geometría de un mapa
require(ggthemes)       # Extra Themes, scale and Geoms for ggplot2
require(ggmap)          #Spatial Visualization with ggplot2 
require(ggpubr)         #Based Publication Ready Plots with ggplot2
require(knitr)
require(kableExtra)
require(openxlsx)
require(readxl)
require(RColorBrewer)   #ColorBrewer Palettes 
require(rgdal)          #Para importar shapefiles. 
require(sp)             # Classes and Methos for Spatial Data
require(spdplyr)        #Data manipulation verbs for the sptial classes
require(tidyverse) 
require(tmap)
require(tmaptools)
require(tibble)
require(unikn)          # Paleta de colores
require(viridisLite)    # Paleta de colores 


# Objetivo 

#Se presentan algunos ejercicios básicos de la cartografía digital con `R` y 
#haciendo uso de algunas paqueterías para integrar herramientas geográficas y 
#estadísticas. Así bien, para visualizar y modelar datos geográficos con el 
#software de código abierto y tratar de entender principalmente el manejo de 
#Sistemas de Información Geográfica (`SIG`) y visualizar datos e información que 
#requiera de una localización específica.    

# Sistema de Información Geográfica    

#El Instituto Nacional de Estadística, Geografía e Informática (INEGI) tiene 
#puestos a disposición del público los shapefiles con los distintos niveles de 
#información geográfica. Estos archivos digitales se encuentran disponibles en el 
#[`Marco Geoestadístico Nacional (MGN)`](https://www.inegi.org.mx/temas/mg/#Descargas), 
#el cual es un sistema único y se presenta la división del territorio nacional 
#en diferentes niveles de desagregación para referir geográficamente la 
#información estadística de los censos y encuestas. Se integra al Sistema 
#Nacional de Información Estadística y Geográfica (SNIEG).   

#Este producto integra información vectorial, tablas de atributos y catálogos. 
#Muestra la división geoestadística del territorio nacional en sucesivos niveles 
#del territorio. Esta división está dada por los llamados **límites estadísticos**, 
#que pueden coincidir con los límites político-administrativos oficiales, 
#los cuales tienen sustento legal.   

tabla <- data.frame(Division = c("Estatales", "Municipales", "Localidades rurales amanzanadas", "Localidades urbanas", "Localidades rurales amanzanadas y no amanzanadas", "Áreas geoestadísticas básicas rurales", "Áreas geoestadísticas básicas urbanas", "Manzanas urbanas y rurales (incluyendo caserío disperso)", "Territorio insular"),
                    Unidades_territoriales= c("32", "2 469","45 397","4 911", "295 779", "17 469", "63 982", "2 513 853", "350"),
                    Datos_vectoriales = c(rep("Polígonos",4), "Puntos", rep("Polígonos",4))) 

kable(tabla, 
      col.names = c("División", "Unidades territoriales", "Datos vectoriales"), 
      align = "c", 
      caption = c("Tabla: Subdivisiones territoriales básicas del INEGI.")) %>%
  kable_styling(position = "center",
                bootstrap_options = c("condensed", "responsive", "bordered", "hover")) %>%
  kable_classic(full_width = F, html_font = "montserrat") %>%
  row_spec(0, color = "black", bold = TRUE) %>% 
  collapse_rows(columns = 1, valign = "middle") %>%
  pack_rows("AGEE", 1, 1, label_row_css = "background-color: #DFE5E5; font-size: 10px; color: #030557; padding: 3px 10px;") %>%
  pack_rows("AGEM", 2, 2, label_row_css = "background-color: #DFE5E5; font-size: 10px; color: #030557; padding: 3px 10px;") %>%
  pack_rows("Localidades", 3, 5, label_row_css = "background-color: #DFE5E5; font-size: 10px; color: #030557; padding: 3px 10px;") %>%
  pack_rows("AGEB", 6, 7, label_row_css = "background-color: #DFE5E5; font-size: 10px; color: #030557; padding: 3px 10px;") %>% 
  pack_rows("MZA",8, 8, label_row_css = "background-color: #DFE5E5; font-size: 10px; color: #030557; padding: 3px 10px;") %>% 
  pack_rows("TI", 9, 9, label_row_css = "background-color: #DFE5E5; font-size: 10px; color: #030557; padding: 3px 10px;")                                                                                                                                                                                                                                                                                      
 
### Especificación del sistema de coordenadas   

#Ocurre a menudo que las capas de información vectorial se han obtenido de fuentes 
#con sistemas de coordenadas distintas; por lo que se debe transformar la 
#información para homogenizarla y representarla en el proyecto.      

#La [`Norma Técnica para el Sistema Geodésico Nacional`](https://www.inegi.org.mx/contenidos/temas/MapaDigital/Doc/asignar_sistema_coordenadas.pdf) 
#define las disposiciones con el fin de que el marco sea homogéneo, compatible y 
#comparable; y establece que el Marco de Referencia oficial para los Estados 
#Unidos Mexicanos es el International Terrestrial Reference Frame 2008, con datos 
#de época 2010.0 (ITRF08 ) asociado al elipsoide de referencia definido en el GRS80.            

#A continuación se enlista los datos del sistema de coordenadas para la estandarización de la información: 

tabla <- data.frame(Parametros = c("Cónica Conforme de Lambert (`CCL`)", "Dátum:","Elipsoide","Meridiano origen", "Latitud origen","Primer paralelo estándar", "Segundo paralelo estándar", "Falso Este","Falso Norte"), 
                    Datos = c("", "ITRF2008", "GRS80","102° 00' 00' W.", "12° 00' 00' N.", "17° 30' N", "29° 30'N", "2 500 000", "0"))

kable(tabla, 
      col.names = c("Parámetros", ""), 
      align = "c", 
      caption = c("Tabla: Características técnicas de la información")) %>%
  kable_styling(position = "center",
                bootstrap_options = c("condensed", "responsive", "bordered", "hover"), 
                font_size = 10) %>%
  kable_classic(full_width = F, html_font = "montserrat") %>%
  row_spec(0, color = "white", background = "#050844", bold = TRUE) 


### Geospatial Data Abstraction Library (`GDAL`)  

#En este documento se va a hacer uso del paquete `rgdal` el cual permite importar, 
#exportar o bien manipular los datos especiales:   
  
#- `readOGR()` y `writeOGR()`: para datos vectoriales   
#- `readGDAL()` y `writeGDAL()`: para rasters    

#También el paquete `gdalUtils` permite reproyectar, transformar, reclasificar, etc.   

#La función `ogrDrivers` permite visualizar los tipos de formatos que el paquete `rgdal` permite trabajar:  


kable(ogrDrivers(), 
      align = "c") %>%
  kable_styling(position = "center",
                bootstrap_options = c("condensed", "responsive", "bordered", "hover"),
                font_size = 10) %>%
  kable_classic(full_width = TRUE, html_font = "montserrat") %>%
  row_spec(0, color = "black", bold = TRUE) %>%
  scroll_box(width = "100%", height = "300px")


## Base de datos 

### Índice de marginación 

#Para conocer las desigualdades territoriales, desde 1993 la Secretaría General 
#del Consejo Nacional de Población (CONAPO) realiza un ejercicio que permite 
#construir el Índice de marginación con base en los resultados censales, una 
#medida que permite identificar las zonas y regiones con más carencias dentro del 
#país. Este índice resume y permite diferenciar los estados, municipios, 
#localidades y áreas geoestadísticas básicas del país según el impacto global de 
#las carencias que padece la población como resultado de la falta de acceso a 
#la educación, la residencia en viviendas inadecuadas, la percepción de ingresos 
#monetarios insuficientes y las relacionadas con la residencia en localidades pequeñas.   

#Hoy este índice se ha convertido en una de las principales herramientas analíticas 
#y operativas para la definición y focalización de políticas públicas enfocadas 
#al abatimiento de las carencias socioeconómicas de la población mexicana.      


#Para este ejercicio se hace uso de las bases de datos que contienen las 
#estimaciones del índice de marginación en los diferentes niveles de desagregación 
#geográfica censal. Permitiendo de esta manera hacer de manera visual los territorios 
#de desigualdad que enfrenta el país.  

#La base de datos del índice de marginación en los diferentes niveles de desagregación 
#se encuentra disponible en la página de [datos abiertos](https://www.gob.mx/conapo/documentos/indices-de-marginacion-2020-284372).  


################################################################################
############################  Entidad Federativa  ##############################
################################################################################

############################### Shapefile ######################################

#La función `readOGR` del paquete `rgdal`, extrae automáticamente la información 
#utilizada por otros paquetes `SIG` de código abierto como QGIS y permite a R 
#manejar una gama más amplia de formatos de datos espaciales. Esta función lee 
#datos `OGR` y datos vectoriales, pero solamente permite manejar capas con 
#características geométricas (no mezcla puntos, líneas o polígonos en una sola capa) 
#y a su vez establecerá un sistema de referencia espacial si la capa tiene dichos 
#metadatos.      
#Para leer un archivo `shapefile`, se establecen los siguientes argumentos, 
#como `dsn`, en donde se indica el directorio que contiene los shapes y `layer` 
#que es el nombre explícito de la capa a trabajar y dichas capas deben de ir sin 
#la extensión `.shp`.   

#A continuación se lee el archivo .shp que contiene de manera integrada la 
#división de el área geoestadística estatal `agee`.     


require(rgdal)
shape_estados <- readOGR(dsn ="/MGN 2020/conjunto_de_datos", 
                         layer = "00ent",
                         encoding = "UTF-8",
                         use_iconv = TRUE)

# La función `rename()` del paquete `dplyr` permite cambiar el nombre de la columna 
#de la clave geoestadística a nivel estatal dentro de la base de datos del shape.

shape_estados@data <- shape_estados@data %>% 
                        rename("CVE_GEO" = "CVEGEO")  


#Un objeto `SpatialPolygons` se puede combinar con una data.frame para crear lo 
#que se llama `SpatialPolygonsDataFrame`. 
#La diferencia está en los atributos asociados con los polígonos. 
#Un objeto `SpatialPolygonsDataFrame` tiene información adicional asociada a los 
#polígonos (ejemplo, nombre de la entidad, población, etc.) mientras que 
#`SpatialPolygons` contiene solo la información espacial (vértices) sobre el polígono.     

class(shape_estados)

#Si el archivo no tuviera Sitema de Referencia de Coordenadas `CRS` asignado se 
#debería utilizal la función `proj4string()` para asignar el `CRS` adecuado. 
#En cambio, si se quisiera cambiar el sistema de referencia (transormar) debe 
#utilizarse la función `spTransform()` del paquete `sp`.     

#La función `proj4string()`representa el sistema de referencia de coordenadas 
#utilizado en los datos. 

proj4string(shape_estados)  


################ Índice de marginación a nivel estatal #########################

#La base de datos del índice de marginación por estados se encuentra disponible 
#en la página oficial de [CONAPO]( https://www.gob.mx/conapo/documentos/indices-de-marginacion-2020-284372) 
#o bien se puede consultar en la página de [Datos Abiertos](https://datos.gob.mx/) 
#y se presenta en formato `.xlsx` [Consulta](https://datos.gob.mx/busca/dataset/indice-de-marginacion-carencias-poblacionales-por-localidad-municipio-y-entidad).  

#Para poder trabajar con la base de datos del índice de marginación a nivel 
#estatal, se elimina la fila que contiene los datos a nivel nacional con la 
#función `::filter()` del paquete `dplyr` y  por otro lado se cambia el nombre 
#de la columna `CVE_ENT`  que hace referencia a la clave geográfica del estado por 
#`CVE_GEO` para fines prácticos.  

IME_2020 <- read_xlsx("Data/IME_2020.xlsx", sheet = "IME_2020") %>% 
              filter(NOM_ENT != "Nacional") %>% 
               mutate(CVE_GEO = CVE_ENT) %>% 
                as.data.frame()

#Ahora bien, para la realización de los *mapas cloropléticos* que muestran áreas 
#geográficas divididas o regiones que están coloreadas en relación con una 
#variable de interés. Para este documento se decide hacer uso del **grado de marginación** 
#que permite englobar a las unidades geográficas que presentan las mismas 
#condiciones de exclusión social en cinco categorías.  

############################### Layers #########################################

#Existen dos maneras de trabajar los datos de los mapas:  
  
#- Es trabajar con los objetos `SpatialPolygonsDataFrame` o bien `spdf` y tratar 
#de unir los datos que se desean analizar, llamando algunas de las funciones de 
#`join_` o `merge` del paquete `dplyr`.   

#- O bien utilizar la función `fortify()`  del paquete `ggplot2`, que permite 
#convertir un objeto (ej.`spdf`) a un *data.frame*. Al hacer este tipo de conversión 
#permite aplicar un `join` para unir ambas estructuras de datos al nuevo formato del data.frame.   


#################### SpatialPolygons -> Data.frame #############################

#Cuando es necesario cambiar los ID's de las características en los objetos 
#`SpatialLines` o `SpatialPolygons`, se puede usar la función `spChFIDs()` de la 
#paquetería `sp`. Donde los nuevos ID's deben ser un vector de caracteres únicos 
#de la longitud correcta.   

#La función `fortify()` convierte un objeto S3 para convertir objetos diversos 
#a un data.frame para `ggplot2`. El data.frame resultante de la conversión de un 
#objeto *spdf* es sumamente largo, tiene una fila por cada cambio en un polígono 
#y un columna de grupo para separar cada polígono.  

capas_estados <- shape_estados %>%
                  sp::spChFIDs(., str_pad(shape_estados@data$CVE_GEO, 2, "left", pad = "0")) %>%
                   fortify(., id = "CVE_GEO") %>%
                    left_join(., IME_2020 %>% dplyr::select(CVE_GEO, NOM_ENT, GM_2020),
                               by = c("id" = "CVE_GEO"))   

############ SpatialPolygons -> SpatialPolygonsDataFrame #######################

layer_estados <- merge(shape_estados,
                       IME_2020 %>%
                         dplyr::select(c(-CVE_ENT, -NOM_ENT)) %>%
                          mutate(GM_2020 = fct_relevel(.$GM_2020,"Muy alto", "Alto", "Medio", "Bajo", "Muy bajo")),
                       by = "CVE_GEO")  


############ SpatialPolygons -> tbl_df/ tbl / data.frame ####################### 

#Conversión de datos: `st_as_sf`, convierte un objeto externo en un spatial object. 
#A continuación, se muestra un ejemplo de cómo visualizar objetos `sf`. 
#Este tipo de función es un poco más amigable de usar.  

#La función `as_tibble` de la paquetería `tibble`, genera una base de datos 
#genérica de S3 con métodos más eficientes para matrices y data.frame. 
#Su envoltorio delgado permite trabajar de manera más rápida que un `data.frame`.  

# Paquetes 
require(tibble)
require(sf)

tbl_estados <- as_tibble(st_as_sf(shape_estados)) %>%
                merge(., IME_2020 %>% dplyr::select(CVE_GEO, GM_2020))  

###################### Mapas a nivel estatal ###################################

########################## ggmap::geom_map() ###################################

#La función `ggmap()` devuelve un objeto `ggplot`. Puede usar este objeto `ggplot` 
#resultante como lo haría con cualquier otro objeto ggplot (ejemplo, agregar geoms, 
#cambiar tema). [GitHub: fresques/ggmap](https://github.com/fresques/ggmap)   

#Una de las bondades del paquete `ggmap`, permite hacer mediante el uso de 
#herramientas de *Google Maps* directamente desde `R`. Este paquete permite extraer 
#un mapa base de *Google Maps* y algunos otros servidores de mapas, para que luego 
#se puedan trazar y agregar puntos, polígonos, líneas y otros elementos usando 
#funciones `ggplot2`. Este paquete usa la `API` de *Google Maps*.  Otras funciones 
#en el paquete `ggmap`, necesita una clave `API` de Google Maps para que funcione.

require(ggmap) 

p <- IME_2020 %>% 
      mutate(GM_2020 = fct_relevel(.$GM_2020,"Muy alto","Alto","Medio","Bajo","Muy bajo")) %>%
       ggplot() + 
        geom_map(map = capas_estados, 
                  aes(map_id = CVE_GEO, fill = GM_2020,  color = "GM_2020")) +
         expand_limits(x = capas_estados$long, 
                        y = capas_estados$lat) + 
          theme_transparent() + 
           theme(plot.title = element_text(size = 22, hjust = 0.15, family = "montserrat", face = "bold"),
                  plot.caption = element_text(size = 10, hjust = 0.2, vjust = 1, family = "montserrat"),
                   legend.key.size = unit(0.5, "cm"),
                    legend.text = element_text(size = 10, family = "montserrat"), 
                     legend.title = element_text(size = 12, hjust = 0.5, family = "montserrat", face = "bold"),
                      legend.position = c(0.8, 0.7)) + 
            scale_fill_viridis_d(option = "A", begin = 0.3, end = 0.8) + 
             scale_color_manual(values = c("#BDBDBD")) + 
              guides(color = 'none') +
               labs(title = "Índice de marginación a nivel estatal, 2020",
                    fill = stringr::str_wrap("Grado de marginación", 10),
                     caption = expression(paste("Fuentes: Estimaciones del CONAPO con base en el INEGI, Censo de Población y Vivienda 2020")))

#path = "Images/IME_2020_gmap.pdf"
#ggexport (p, width = 8, height = 5, res = 400, filename = path)

p

########################## ggplot2::geom_polygons ##############################

#La función `geom_polygon()` de la paquetería `ggplot2`, devuelve un objeto 
#`ggplot` en el cual dibuja líneas entre puntos y los cierra ( es decir, dibuja 
#una línea desde el último punto hasta el primer punto). En el cual es importante 
#tener en cuenta que el data.frame incluye columnas con la ubicación de las áreas 
#geográficas y se agregan como argumentos en la función `aes(x = long,  y = lat)`. 
#También se incluye una columna que describe el orden en que se deben conectar 
#estos puntos para formar un polígono (`order`), el nombre del estado (`id`) y 
#una columna de grupo que separa los puntos en polígonos únicos que se deben 
#trazar `aes (group = group)`.  


# Paleta de colores
require(unikn) 

p <- ggplot() + 
      geom_polygon(data = capas_estados,
                   mapping = aes(x = long, 
                                 y = lat, 
                                 group = group,
                                 fill = fct_relevel(GM_2020,"Muy alto","Alto","Medio","Bajo","Muy bajo")),
                   color = "dark grey",
                   size = 0.5) +
       expand_limits(x = capas_estados$long,  
                     y = capas_estados$lat) + 
        theme_transparent() + 
         theme(plot.title = element_text(size = 21, hjust = 0.15, family = "montserrat", face = "bold"),
               plot.caption = element_text(size = 11, hjust = 0.2, vjust = 1, family = "montserrat"), 
               legend.key.size = unit(0.5, "cm"),
               legend.text = element_text(size = 10, family = "montserrat"), 
               legend.title = element_text(size = 12, hjust = 0.5, family = "montserrat", face = "bold"),
               legend.position = c(0.8, 0.7)) + 
          scale_fill_manual(values = rev(usecol(pal = pal_petrol, n = 5))) + 
           scale_color_manual(values = c("#BDBDBD")) +  
            guides(color = guide_legend(override.aes = list(fill = usecol(pal = pal_petrol, n = 5)))) +
     labs(title = "Índice de marginación a nivel estatal, 2020",
           fill = stringr::str_wrap("Grado de marginación", 10),
            caption = expression(paste("Fuentes: Estimaciones del CONAPO con base en el INEGI, Censo de Población y Vivienda 2020")))

#path = "Images/IME_2020_geom_polygon.pdf"
#ggexport (p, width = 8, height = 5, res = 400, filename = path)

p


########################## tmap::tm_shape() ####################################

#Con el paquete `tmap` se pueden generar mapas temáticos con gran flexibilidad. 
#La sintaxis para crear gráficos es similar a la de *ggplot2*, pero adaptada a los 
#mapas.  Además, ofrece un sistema de trazado coherente para mapas temáticos que 
#se basan en la estructura de las capas en los gráficos. Los mapas temáticos 
#se crean apilando capas, donde cada capa y cada dato se pueden asignar a una o 
#más `aes ()`. También es posible escalarlos y agregando posibles atributos del 
#mapa, como una barra de escala o una brújula. 

#Una de las ventajas a diferencia de ggplot(), es más rápido de generar los 
#gráficos y permite trabajar con múltiples capas a la vez. Además, permite hacerlo 
#de manera interactiva como un widget HTML.  

#La función `tm_shape`, crea un elemento que especifica un objeto de 
#`SpatialPolygonsDataFrame`, sin la necesidad de convertir los datos a un data.frame. 
#[GitHub: r-tmap/tmap](https://github.com/r-tmap/tmap)

# Paquetes 
require(tmap)
require(tmaptools)

p <- tm_shape(layer_estados) + 
      tm_polygons("GM_2020", 
                  title = stringr::str_wrap("Grado de marginación", 10),
                  #palette = hcl.colors(18, palette = "Inferno")[3:9], 
                  palette = "BuPu") + 
       tm_borders(col = "grey20", lwd = 0.5) + 
        tm_layout(title.snap.to.legend = TRUE,
                   title = "Índice de marginación a nivel estatal, 2020",
                    title.fontfamily = "montserrat",
                     title.fontface = "bold",
                      title.size = 2,
                       main.title.fontface = "bold",
                        main.title.position = c("center", "top"),
                         main.title.size = 7, 
                          main.title.fontfamily = "montserrat",
                           legend.outside.position = "right",
                            legend.title.fontfamily = "montserrat",
                             legend.title.fontface = "bold", 
                              legend.text.fontfamily = "montserrat",
                               legend.title.size = 1.2, 
                                legend.text.size = 1, 
                                 inner.margins = c(t = 0.05, r = 0.02, b = 0.11, l = 0.15),
                                  fontfamily = "montserrat",
                                   frame = FALSE) + 
          tm_legend(legend.position = c("right", "center")) +
           tm_credits(expression(paste("Fuentes: Estimaciones del CONAPO con base en el INEGI, Censo de Población y Vivienda 2020")),
                        position=c("left", "bottom"), 
                         size = 1.1)

#path = "Images/IME_2020_tmap.pdf"
#ggexport (p, width = 8, height = 5, res = 400, filename = path)

p

##################### ggspatial::layer_saptial() ###############################

#Cualquier capa espacial se puede agregar a un `ggplot()` usando la función 
#`layer_spatial()` del paquete `ggspatial` (bueno, cualquier objeto de los 
#paquetes `sf`, `sp` o `raster`...). Estas capas entrenarán las escalas y a 
#diferencia de las funciones `geom_` o `stat_`, `layer_spatial()` siempre toma 
#sus datos primero sin necesidad de especificar los límites de las escalas. 
#[Estructura ggspatial](https://paleolimbot.github.io/ggspatial/articles/ggspatial.html).

# Paquete 
require(ggspatial)

p <- ggplot() + 
      layer_spatial(layer_estados, aes(fill = GM_2020)) + 
       theme_transparent() + 
        theme(plot.title = element_text(size = 22, hjust = 0.15, family = "montserrat", face = "bold"),
               plot.caption = element_text(size = 11, hjust = 0.2, vjust = 1, family = "montserrat"), 
                legend.key.size = unit(0.5, "cm"),
                 legend.text = element_text(size = 12, family = "montserrat"), 
                  legend.title = element_text(size = 10, hjust = 0.5, family = "montserrat", face = "bold"),
                   legend.position = c(0.8, 0.7)) + 
         scale_fill_brewer(palette = rev("BuPu")) +
          scale_color_manual(values = c("#BDBDBD")) + 
           guides(color = guide_legend(override.aes = list(fill = usecol(pal = pal_petrol, n = 5)))) +
      labs(title = "Índice de marginación a nivel estatal, 2020",
            fill = stringr::str_wrap("Grado de marginación", 10), 
             caption = expression(paste("Fuentes: Estimaciones del CONAPO con base en el INEGI, Censo de Población y Vivienda 2020")))

#path = "Images/IME_2020_ggspatial.pdf"
#ggexport (p, width = 8, height = 5, res = 400, filename = path)

p

########################## sf::geom_sf() #######################################

#Conversión de datos: `st_as_sf`, convierte un objeto externo en un spatial object.        

#Los objetos espaciales creados por el paquete `sp` (que hasta hace poco era la 
#forma estándar de manejar datos espaciales en `R)` se pueden formatear en un 
#formato de características simples usando la función `st_as_sf()`: 

#Paquetes
require(sf)
estados_sf <- st_as_sf(capas_estados, 
                       coords = c("long","lat"),
                       crs = "+proj=lcc +lat_0=12 +lon_0=-102 +lat_1=17.5 +lat_2=29.5 +x_0=2500000 +y_0=0 +ellps=GRS80 +units=m +no_defs") 

#A continuación, se muestra un ejemplo de cómo visualizar objetos `sf`. Este 
#tipo de función es un poco más amigable de usar.  

#La función `as_tibble` de la paquetería `tibble`, genera una base de datos 
#genérica de S3 con métodos más eficientes para matrices y data.frame. 
#Su envoltorio delgado permite trabajar de manera más rápida que un `data.frame`.  

# Paquetes 
require(ggplot2)
require(tibble)
require(sf)
require(unikn) # Paleta de colores 

p <- ggplot(data = tbl_estados) +  
      geom_sf(mapping = aes(geometry = geometry, 
                             fill = fct_relevel(GM_2020,"Muy alto","Alto","Medio","Bajo","Muy bajo")),
              color = "#BDBDBD", 
              size = 0.1)  + 
       theme_transparent() + 
        theme(plot.title = element_text(size = 16, hjust = 0.15, family = "montserrat", face = "bold"),
               plot.caption = element_text(size = 9, hjust = 0.2, vjust = 1, family = "montserrat"),  
                legend.key.size = unit(0.5, "cm"),
                 legend.text = element_text(size = 8, family = "montserrat"),   
                  legend.title = element_text(size = 10, hjust = 0.5, family = "montserrat", face = "bold"),
                   legend.position = c(0.8, 0.7)) + 
         scale_fill_manual(values = rev(usecol(pal = pal_petrol, n = 5))) + 
          scale_color_manual(values = rev(usecol(pal = pal_petrol, n = 5))) + 
           guides(color = guide_legend(override.aes = list(fill = rev(usecol(pal = pal_petrol, n = 5))))) +
            labs(title = "Índice de marginación a nivel estatal, 2020",
                  fill = stringr::str_wrap("Grado de marginación", 10),
                   caption = expression(paste("Fuentes: Estimaciones del CONAPO con base en el INEGI, Censo de Población y Vivienda 2020")))

#path = "Images/IME_2020_geom_sf.pdf"
#ggexport (p, width = 8, height = 5, res = 400, filename = path)

p

################################################################################
############################  Nivel municipal  #################################
################################################################################

############################### Shapefile ######################################

#La función `readOGR` del paquete `rgdal`, extrae automáticamente la información 
#utilizada por otros paquetes `SIG` de código abierto como QGIS y permite a R 
#manejar una gama más amplia de formatos de datos espaciales. Esta función lee 
#datos `OGR` y datos vectoriales, pero solamente permite manejar capas con 
#características geométricas (no mezcla puntos, líneas o polígonos en una sola capa) 
#y a su vez establecerá un sistema de referencia espacial si la capa tiene dichos 
#metadatos.      
#Para leer un archivo `shapefile`, se establecen los siguientes argumentos, 
#como `dsn`, en donde se indica el directorio que contiene los shapes y `layer` 
#que es el nombre explícito de la capa a trabajar y dichas capas deben de ir sin 
#la extensión `.shp`.   

#A continuación se lee el archivo .shp que contiene de manera integrada la 
#división de el área geoestadística municipal `agem`.     


require(rgdal)
shape_municipios <- readOGR(dsn ="MGN 2020/conjunto_de_datos", 
                            layer = "00mun",
                            encoding = "UTF-8",
                            use_iconv = TRUE)


# La función `rename()` del paquete `dplyr` permite cambiar el nombre de la columna 
#de la clave geoestadística a nivel municipal dentro de la base de datos del shape.

shape_municipios@data <- shape_municipios@data %>%
                           rename("CVE_GEO" = "CVEGEO") 

################ Índice de marginación a nivel municipal #######################

#La base de datos del índice de marginación por estados se encuentra disponible 
#en la página oficial de [CONAPO]( https://www.gob.mx/conapo/documentos/indices-de-marginacion-2020-284372) 
#o bien se puede consultar en la página de [Datos Abiertos](https://datos.gob.mx/) 
#y se presenta en formato `.xlsx` [Consulta](https://datos.gob.mx/busca/dataset/indice-de-marginacion-carencias-poblacionales-por-localidad-municipio-y-entidad).  

#Para poder trabajar con la base de datos del índice de marginación a nivel 
#municipal, se elimina la fila que contiene los datos a nivel nacional con la 
#función `::filter()` del paquete `dplyr` y  por otro lado se cambia el nombre 
#de la columna `CVE_MUN`  que hace referencia a la clave geográfica del municipio por 
#`CVE_GEO` para fines prácticos.  

IMM_2020 <- read_xlsx("Data/IMM_2020.xlsx", sheet = "IMM_2020") %>%
             filter(NOM_ENT != "Nacional") %>%
              mutate(CVE_GEO = CVE_MUN) %>%
               as.data.frame()

#Ahora bien, para la realización de los *mapas cloropléticos* que muestran áreas 
#geográficas divididas o regiones que están coloreadas en relación con una 
#variable de interés. Para este documento se decide hacer uso del **grado de marginación** 
#que permite englobar a las unidades geográficas que presentan las mismas 
#condiciones de exclusión social en cinco categorías.  

############################### Layers #########################################

#Existen dos maneras de trabajar los datos de los mapas:  

#- Es trabajar con los objetos `SpatialPolygonsDataFrame` o bien `spdf` y tratar 
#de unir los datos que se desean analizar, llamando algunas de las funciones de 
#`join_` o `merge` del paquete `dplyr`.   

#- O bien utilizar la función `fortify()`  del paquete `ggplot2`, que permite 
#convertir un objeto (ej.`spdf`) a un *data.frame*. Al hacer este tipo de conversión 
#permite aplicar un `join` para unir ambas estructuras de datos al nuevo formato 
#del data.frame.   


#################### SpatialPolygons -> Data.frame #############################

#Cuando es necesario cambiar los ID's de las características en los objetos 
#`SpatialLines` o `SpatialPolygons`, se puede usar la función `spChFIDs()` de la 
#paquetería `sp`. Donde los nuevos ID's deben ser un vector de caracteres únicos 
#de la longitud correcta.   

#La función `fortify()` convierte un objeto S3 para convertir objetos diversos 
#a un data.frame para `ggplot2`. El data.frame resultante de la conversión de un 
#objeto *spdf* es sumamente largo, tiene una fila por cada cambio en un polígono 
#y un columna de grupo para separar cada polígono.  

capas_municipio <-  shape_municipios %>%
                      sp::spChFIDs(., str_pad(shape_municipios@data$CVE_GEO, 5, "left", pad = "0")) %>%
                       fortify(., id = "CVE_GEO") %>%
                        left_join(., IMM_2020 %>% dplyr::select(CVE_GEO, CVE_ENT, NOM_ENT, NOM_MUN, GM_2020), 
                                   by = c("id" = "CVE_GEO"))    

############ SpatialPolygons -> SpatialPolygonsDataFrame #######################

layer_municipios <- merge(shape_municipios,
                           IMM_2020 %>% dplyr::select(c(-CVE_ENT, -CVE_MUN)) %>%
                             mutate(GM_2020 = fct_relevel(.$GM_2020,"Muy alto", "Alto", "Medio", "Bajo", "Muy bajo")),
                           by = "CVE_GEO")


############ SpatialPolygons -> tbl_df/ tbl / data.frame ####################### 

#Conversión de datos: `st_as_sf`, convierte un objeto externo en un spatial object. 
#A continuación, se muestra un ejemplo de cómo visualizar objetos `sf`. 
#Este tipo de función es un poco más amigable de usar.  

#La función `as_tibble` de la paquetería `tibble`, genera una base de datos 
#genérica de S3 con métodos más eficientes para matrices y data.frame. 
#Su envoltorio delgado permite trabajar de manera más rápida que un `data.frame`.  

# Paquetes 
require(tibble)
require(sf)

tbl_municipios <- as_tibble(st_as_sf(shape_municipios)) %>%
                    merge(., IMM_2020 %>% dplyr::select(CVE_GEO, GM_2020))


###################### Zonas Metropolitanas 2015 ###############################

#La base de datos de las zonas metropolitanas se encuentra disponible  en la 
#página oficial de [CONAPO]

ZM_2015 <- read_xlsx("Data/ZM_2015.xlsx", sheet = "ZM_2015") 

kable(head(ZM_2015, 10),
      align = "c", 
      caption = c("Base: Zonas Metropolitanas 2015.")) %>%
  kable_styling(position = "center",
                font_size = 11,
                bootstrap_options = c("condensed", "responsive", "bordered")) %>%
  kable_classic(full_width = T, html_font = "montserrat") %>%
  row_spec(0, color = "black", bold = TRUE) %>%
  footnote(footnote_as_chunk = T,
           general_title = "Fuentes:",
           general = "Estimaciones del CONAPO con base en el INEGI, Censo de Población y Vivienda 2020") 

###################### Mapas a nivel municipal #################################

########################## ggmap::geom_map() ###################################

#La función `ggmap()` devuelve un objeto `ggplot`. Puede usar este objeto `ggplot` 
#resultante como lo haría con cualquier otro objeto ggplot (ejemplo, agregar geoms, 
#cambiar tema). [GitHub: fresques/ggmap](https://github.com/fresques/ggmap)   

#Una de las bondades del paquete `ggmap`, permite hacer mediante el uso de 
#herramientas de *Google Maps* directamente desde `R`. Este paquete permite extraer 
#un mapa base de *Google Maps* y algunos otros servidores de mapas, para que luego 
#se puedan trazar y agregar puntos, polígonos, líneas y otros elementos usando 
#funciones `ggplot2`. Este paquete usa la `API` de *Google Maps*.  Otras funciones 
#en el paquete `ggmap`, necesita una clave `API` de Google Maps para que funcione.  

p <- IMM_2020 %>% 
      mutate(GM_2020 = fct_relevel(.$GM_2020,"Muy alto", "Alto", "Medio", "Bajo", "Muy bajo")) %>%
       ggplot() + 
        geom_map(map = capas_municipio, 
                  aes(map_id = CVE_GEO, fill = GM_2020), 
                   color = 'transparent')  +
         expand_limits(x = capas_municipio$long, y = capas_municipio$lat) + 
          theme_transparent() + 
           theme(plot.title = element_text(size = 20, hjust = 0.15, family = "montserrat", face = "bold"), 
                  plot.caption = element_text(size = 10, hjust = 0.2, vjust = 1, family = "montserrat"), 
                   legend.key.size = unit(0.5, "cm"),
                    legend.text = element_text(size = 10, family = "montserrat"), 
                     legend.title = element_text(size = 12, hjust = 0.5, family = "montserrat", face = "bold"),
                      legend.position = c(0.8, 0.7)) + 
            scale_fill_viridis_d(option = "A", begin = 0.3, end = 0.8) + 
             scale_color_manual(values = c("#BDBDBD")) + 
              guides(color = 'none') +
         labs(title = "Índice de marginación a nivel municipal, 2020",
               fill = stringr::str_wrap("Grado de marginación", 10),
                caption = expression(paste("Fuentes: Estimaciones del CONAPO con base en el INEGI, Censo de Población y Vivienda 2020")))

#path = "Images/IMM_2020_gmap.pdf"
#ggexport (p, width = 8, height = 5, res = 400, filename = path)

p

########################## ggplot2::geom_polygons ##############################

#La función `geom_polygon()` de la paquetería `ggplot2`, devuelve un objeto 
#`ggplot` en el cual dibuja líneas entre puntos y los cierra ( es decir, dibuja 
#una línea desde el último punto hasta el primer punto). En el cual es importante 
#tener en cuenta que el data.frame incluye columnas con la ubicación de las áreas 
#geográficas y se agregan como argumentos en la función `aes(x = long,  y = lat)`. 
#También se incluye una columna que describe el orden en que se deben conectar 
#estos puntos para formar un polígono (`order`), el nombre del estado (`id`) y 
#una columna de grupo que separa los puntos en polígonos únicos que se deben 
#trazar `aes (group = group)`.  


# Paleta de colores 
require(RColorBrewer)

p <- ggplot() + 
      geom_polygon(capas_municipio,
                   mapping = aes(x = long, 
                                 y = lat, 
                                 group = group, 
                                 fill = fct_relevel(GM_2020, "Muy alto", "Alto", "Medio", "Bajo", "Muy bajo")),
                   color = "transparent",  
                   size = .15,
                   alpha = 0.9) +
      geom_polygon(capas_estados,
                   mapping = aes(x = long, 
                                 y = lat, 
                                 group = group),
                   color = "dark grey",
                   fill = "transparent",
                   size = 0.6) +
       coord_equal() +
        theme_transparent() + 
         theme(plot.title = element_text(size = 21, hjust = 0.15, family = "montserrat", face = "bold"),     
                plot.caption = element_text(size = 10, hjust = 0.2, vjust = 1, family = "montserrat"),
                 legend.key.size = unit(0.5, "cm"), 
                  legend.text = element_text(size = 10, family = "montserrat"), 
                   legend.title = element_text(size = 12, hjust = 0.5, family = "montserrat", face = "bold"),
                    legend.position = c(0.8, 0.7)) + 
          scale_fill_brewer(palette = "YlGnBu") +
           guides(color = 'none') +
            labs(title = expression(paste("Índice de marginación a nivel municipal, 2020")),
                  fill = stringr::str_wrap("Grado de marginación", 10),
                   caption = expression(paste("Fuentes: Estimaciones del CONAPO con base en el INEGI, Censo de Población y Vivienda 2020")))

#path = "Images/IMM_2020_geom_polygon.pdf"
#ggexport (p, width = 8, height = 5, res = 400, filename = path)

p



########################## tmap::tm_shape() ####################################

#Con el paquete `tmap` se pueden generar mapas temáticos con gran flexibilidad. 
#La sintaxis para crear gráficos es similar a la de *ggplot2*, pero adaptada a los 
#mapas.  Además, ofrece un sistema de trazado coherente para mapas temáticos que 
#se basan en la estructura de las capas en los gráficos. Los mapas temáticos 
#se crean apilando capas, donde cada capa y cada dato se pueden asignar a una o 
#más `aes ()`. También es posible escalarlos y agregando posibles atributos del 
#mapa, como una barra de escala o una brújula. 

#Una de las ventajas a diferencia de ggplot(), es más rápido de generar los 
#gráficos y permite trabajar con múltiples capas a la vez. Además, permite hacerlo 
#de manera interactiva como un widget HTML.  

#La función `tm_shape`, crea un elemento que especifica un objeto de 
#`SpatialPolygonsDataFrame`, sin la necesidad de convertir los datos a un data.frame. 
#[GitHub: r-tmap/tmap](https://github.com/r-tmap/tmap)

# Paquetes
require(tmap)
require(tmaptools)

p <- tm_shape(layer_municipios, id = "CVE_GEO") +
       tm_polygons("GM_2020", 
                   title = stringr::str_wrap("Grado de marginación", 10),
                   palette = "BuPu", 
                   border.col = 'transparent') + 
     tm_shape(layer_estados) + 
       tm_borders("grey20", lwd = 0.5) + 
        tm_layout(title.snap.to.legend = TRUE,
                   title = "Índice de marginación a nivel municipal, 2020",
                    title.fontfamily = "montserrat",
                     title.fontface = "bold",
                      title.size = 2,
                       main.title.fontface = "bold",
                        main.title.position = c("center", "top"),
                         main.title.size = 7, 
                          main.title.fontfamily = "montserrat",
                           legend.outside.position = "right",
                            legend.title.fontfamily = "montserrat",
                             legend.title.fontface = "bold", 
                              legend.text.fontfamily = "montserrat",
                               legend.title.size = 1.5, 
                                legend.text.size = 1.2, 
                                 inner.margins = c(t = 0.06, r = 0.04, b = 0.13, l = 0.15),
                                  fontfamily = "montserrat",
                                   frame = FALSE) + 
          tm_legend(legend.position = c("right", "center")) +
           tm_credits(expression(paste("Fuentes: Estimaciones del CONAPO con base en el INEGI, Censo de Población y Vivienda 2020")),
                      position=c("left", "bottom"), 
                      size = 1.4)

#path = "Images/IMM_2020_tmap.pdf"
#ggexport (p, width = 8, height = 5, res = 400, filename = path)

p

########################## sf::geom_sf() #######################################

#Conversión de datos: `st_as_sf`, convierte un objeto externo en un spatial object.        

#Los objetos espaciales creados por el paquete `sp` (que hasta hace poco era la 
#forma estándar de manejar datos espaciales en `R)` se pueden formatear en un 
#formato de características simples usando la función `st_as_sf()`: 


# Paquetes
require(ggplot2)
require(tibble)
require(sf)

p <- ggplot() +
      geom_sf(data = tbl_municipios,
              mapping = aes(geometry = geometry, 
                            fill = fct_relevel(GM_2020, "Muy alto","Alto","Medio","Bajo","Muy bajo"),
                            color = fct_relevel(GM_2020, "Muy alto","Alto","Medio","Bajo","Muy bajo")),
              size = 0.1) + 
      geom_sf(data = tbl_estados,
              mapping = aes(geometry = geometry), 
              fill = 'transparent',
              color = "black") +
       coord_sf() +
        theme_transparent() + 
          theme(plot.title = element_text(size = 22, hjust = 0.15, family = "montserrat", face = "bold"),
                 plot.caption = element_text(size = 10, hjust = 0.2, vjust = 1, family = "montserrat"),  
                  legend.key.size = unit(0.5, "cm"),
                   legend.text = element_text(size = 10, family = "montserrat"),   
                    legend.title = element_text(size = 12, hjust = 0.5, family = "montserrat", face = "bold"),
                     legend.position = c(0.8, 0.7)) + 
           scale_fill_manual(values = usecol(pal_bordeaux, n = 5)) + 
            scale_color_manual(values = usecol(pal_bordeaux, n = 5)) + 
             labs(title = "Índice de marginación a nivel municipal, 2020",
                   fill = stringr::str_wrap("Grado de marginación", 10),
                    color = stringr::str_wrap("Grado de marginación", 10),
                     caption = expression(paste("Fuentes: Estimaciones del CONAPO con base en el INEGI, Censo de Población y Vivienda 2020")))

#path = "Images/IMM_2020_geom_sf.pdf"
#ggexport (p, width = 8, height = 5, res = 400, filename = path)

p

################################################################################
############################  Nivel localidad  #################################
################################################################################

############################### Shapefile ######################################

#La función `readOGR` del paquete `rgdal`, extrae automáticamente la información 
#utilizada por otros paquetes `SIG` de código abierto como QGIS y permite a R 
#manejar una gama más amplia de formatos de datos espaciales. Esta función lee 
#datos `OGR` y datos vectoriales, pero solamente permite manejar capas con 
#características geométricas (no mezcla puntos, líneas o polígonos en una sola capa) 
#y a su vez establecerá un sistema de referencia espacial si la capa tiene dichos 
#metadatos.      
#Para leer un archivo `shapefile`, se establecen los siguientes argumentos, 
#como `dsn`, en donde se indica el directorio que contiene los shapes y `layer` 
#que es el nombre explícito de la capa a trabajar y dichas capas deben de ir sin 
#la extensión `.shp`.   

#A continuación se lee el archivo .shp que contiene de manera integrada la 
#división de el área geoestadística a nivel localidad.

require(rgdal)
shape_localidad <- readOGR(dsn ="MGN 2020/conjunto_de_datos", 
                            layer = "00l",
                             encoding = "UTF-8",
                              use_iconv = TRUE)


# La función `rename()` del paquete `dplyr` permite cambiar el nombre de la columna 
#de la clave geoestadística a nivel localidad dentro de la base de datos del shape.

shape_localidad@data <- shape_localidad@data %>%
                          rename("CVE_GEO" = "CVEGEO")

################ Índice de marginación a nivel municipal #######################

#La base de datos del índice de marginación por localidades se encuentra 
#disponible en la página oficial de [CONAPO]
#( https://www.gob.mx/conapo/documentos/indices-de-marginacion-2020-284372) o 
#bien se puede consultar en la página de [Datos Abiertos](https://datos.gob.mx/) 
#y se presenta en formato `.xlsx` [Consulta](https://datos.gob.mx/busca/dataset/indice-de-marginacion-carencias-poblacionales-por-localidad-municipio-y-entidad).   

#Para poder trabajar con la base de datos del índice de marginación a nivel 
#localidad, se cambia el nombre de la columna `CVE_LOC`  que hace referencia a 
#la clave geográfica de la localidad por `CVE_GEO` para fines prácticos.  

IML_2020 <- read_xlsx("Data/IML_2020.xlsx", sheet = "IML_2020") %>%
               mutate(CVE_GEO = CVE_LOC) %>% 
                 as.data.frame()

#Ahora bien, para la realización de los *mapas cloropléticos* que muestran áreas 
#geográficas divididas o regiones que están coloreadas en relación con una 
#variable de interés. Para este documento se decide hacer uso del **grado de marginación** 
#que permite englobar a las unidades geográficas que presentan las mismas 
#condiciones de exclusión social en cinco categorías.  


tabla <- IML_2020 %>%
          mutate(GM_2020 = fct_relevel(GM_2020, levels = c("Muy alto", "Alto", "Medio", "Bajo", "Muy bajo"))) %>%
           dplyr::select(GM_2020) %>%
            group_by(GM_2020) %>%
             summarise(N = n()) %>%
              ungroup() %>% 
               as.data.frame()

kable(tabla,
      col.names = c("Grado de \n marginación", "Localidades"), 
      align = "c", 
      caption = c("Tabla: Localidades según el grado de marginación.")) %>%
  kable_styling(position = "center",
                bootstrap_options = c("condensed", "responsive", "bordered", "hover"), 
                font_size = 10) %>%
   kable_classic(full_width = F, html_font = "montserrat") %>%
    row_spec(0, color = "black", bold = TRUE) %>% 
     gsub("font-size: initial !important;", "font-size: 11pt !important;", .) %>%
      gsub("text-align: initial !important;", "text-align: justify !important;", .)

############################### Layers #########################################

#Existen dos maneras de trabajar los datos de los mapas:  

#- Es trabajar con los objetos `SpatialPolygonsDataFrame` o bien `spdf` y tratar 
#de unir los datos que se desean analizar, llamando algunas de las funciones de 
#`join_` o `merge` del paquete `dplyr`.   

#- O bien utilizar la función `fortify()`  del paquete `ggplot2`, que permite 
#convertir un objeto (ej.`spdf`) a un *data.frame*. Al hacer este tipo de conversión 
#permite aplicar un `join` para unir ambas estructuras de datos al nuevo formato 
#del data.frame.   


#################### SpatialPolygons -> Data.frame #############################

#Cuando es necesario cambiar los ID's de las características en los objetos 
#`SpatialLines` o `SpatialPolygons`, se puede usar la función `spChFIDs()` de la 
#paquetería `sp`. Donde los nuevos ID's deben ser un vector de caracteres únicos 
#de la longitud correcta.   

#La función `fortify()` convierte un objeto S3 para convertir objetos diversos 
#a un data.frame para `ggplot2`. El data.frame resultante de la conversión de un 
#objeto *spdf* es sumamente largo, tiene una fila por cada cambio en un polígono 
#y un columna de grupo para separar cada polígono.  

capas_localidad <- shape_localidad %>%
                    sp::spChFIDs(., str_pad(shape_localidad@data$CVE_GEO, 9, "left", pad = "0")) %>%
                      fortify(., id = c("CVE_GEO")) %>%
                       right_join(., 
                                  IML_2020 %>% dplyr::select(CVE_GEO, ENT, NOM_ENT, MUN, NOM_MUN, NOM_LOC, GM_2020), 
                                   by = c("id" = "CVE_GEO")) 
length(unique(capas_localidad$id))

############ SpatialPolygons -> SpatialPolygonsDataFrame #######################

layer_localidad <- merge(shape_localidad,
                          IML_2020 %>%
                            dplyr::select(c(-CVE_LOC)) %>%
                             mutate(GM_2020 = fct_relevel(.$GM_2020,"Muy alto", "Alto", "Medio", "Bajo", "Muy bajo")),
                         by = "CVE_GEO")


############ SpatialPolygons -> tbl_df/ tbl / data.frame ####################### 

#Conversión de datos: `st_as_sf`, convierte un objeto externo en un spatial object. 
#A continuación, se muestra un ejemplo de cómo visualizar objetos `sf`. 
#Este tipo de función es un poco más amigable de usar.  

#La función `as_tibble` de la paquetería `tibble`, genera una base de datos 
#genérica de S3 con métodos más eficientes para matrices y data.frame. 
#Su envoltorio delgado permite trabajar de manera más rápida que un `data.frame`.  

# Paquetes 
require(tibble)
require(sf)

tbl_localidad <- as_tibble(st_as_sf(shape_localidad)) %>%
                   merge(., IML_2020 %>% dplyr::select(CVE_GEO, GM_2020))


###################### Mapas a nivel localidad #################################

########################## ggplot2::geom_polygons ##############################

#La función `geom_polygon()` de la paquetería `ggplot2`, devuelve un objeto 
#`ggplot` en el cual dibuja líneas entre puntos y los cierra ( es decir, dibuja 
#una línea desde el último punto hasta el primer punto). En el cual es importante 
#tener en cuenta que el data.frame incluye columnas con la ubicación de las áreas 
#geográficas y se agregan como argumentos en la función `aes(x = long,  y = lat)`. 
#También se incluye una columna que describe el orden en que se deben conectar 
#estos puntos para formar un polígono (`order`), el nombre del estado (`id`) y 
#una columna de grupo que separa los puntos en polígonos únicos que se deben 
#trazar `aes (group = group)`.  


# Paleta de colores 
require(RColorBrewer)

p <- ggplot() + 
      geom_polygon(capas_estados,
                   mapping = aes(x = long, 
                                 y = lat,  
                                 group = group), 
                   fill = 'transparent',
                   color = "black", 
                   size = .15,
                   alpha = 0.9) +
      geom_polygon(capas_localidad,
                   mapping = aes(x = long, 
                                 y = lat, 
                                 group = group, 
                                 fill = fct_relevel(GM_2020, "Muy alto","Alto","Medio","Bajo","Muy bajo"), 
                                 color = fct_relevel(GM_2020, "Muy alto", "Alto", "Medio", "Bajo", "Muy bajo")),
                   size = .15,
                   alpha = 0.9) +
       coord_equal() +
        theme_transparent() + 
         theme(plot.title = element_text(size = 24, hjust = 0.15, family = "montserrat", face = "bold"),
                plot.caption = element_text(size = 12, hjust = 0.2, vjust = 1, family = "montserrat"),  
                 legend.key.size = unit(0.5, "cm"),
                  legend.text = element_text(size = 12, family = "montserrat"), 
                   legend.title = element_text(size = 14, hjust = 0.5, family = "montserrat", face = "bold"),
                    legend.position = c(0.8, 0.7)) + 
           scale_fill_viridis_d(option = "A", begin = 0.3, end = 0.8) + 
            scale_color_viridis_d(option = "A", begin = 0.3, end = 0.8) + 
             guides(color = 'none') +
     labs(title = "Índice de marginación a nivel localidad, 2020",
           fill = stringr::str_wrap("Grado de marginación", 10),
            caption = expression(paste("Fuentes: Estimaciones del CONAPO con base en el INEGI, Censo de Población y Vivienda 2020")))

#path = "Images/IML_2020_geom_polygon.pdf"
#ggexport (p, width = 8, height = 5, res = 400, filename = path)

p

###########  Mapas de localidad por estado y municipios ########################

# Clave única de estados 
estados <- unique(IML_2020$ENT)
nom_ent <- unique(IML_2020$NOM_ENT)

p <- NULL 
q <- NULL
#for(i in 1:32){
for(i in 22){
  
  municipio <- IML_2020 %>% 
                mutate(CVE_MUN = paste0(ENT, MUN)) %>%
                 filter(ENT == estados[i]) %>% 
                  dplyr::select(CVE_MUN) %>%
                   unique()
  
  nom_mun <- IML_2020 %>% 
              filter(ENT == estados[i]) %>% 
               dplyr::select(NOM_MUN) %>%
                unique()
  
  p[[i]] <- ggplot() + 
              geom_polygon(capas_estados %>% filter(id == estados[i]),
                           mapping = aes(x = long, 
                                         y = lat, 
                                         group = group), 
                           fill = 'transparent',
                           color = "black", 
                           size = .5,
                           alpha = 1) +
              geom_polygon(capas_municipio %>% filter(CVE_ENT == estados[i]),
                           mapping = aes(x = long, 
                                         y = lat, 
                                         group = group), 
                           fill = 'transparent',
                           color = "#7A7A7A", 
                           size = .2,
                           alpha = 0.9) +
              geom_polygon(capas_localidad %>% filter(ENT == estados[[i]]),
                           mapping = aes(x = long, 
                                         y = lat, 
                                         group = group, 
                                         fill = fct_relevel(GM_2020, "Muy alto", "Alto", "Medio", "Bajo", "Muy bajo"),
                                         color = fct_relevel(GM_2020, "Muy alto", "Alto", "Medio", "Bajo", "Muy bajo")),
                           size = 0.5,
                           alpha = 0.6) +
               coord_equal() +
                theme_transparent() + 
                 theme(plot.title = element_text(size = 15, hjust = 0.5, family = "montserrat", face = "bold"),
                        plot.caption = element_text(size = 9, hjust = 0, family = "montserrat"), 
                         legend.key.size = unit(0.3, "cm"),
                          legend.text = element_text(size = 10, family = "montserrat"), 
                           legend.title = element_text(size = 12, hjust = 0.5, family = "montserrat", face = "bold"),
                            legend.position = "bottom") + 
                  scale_fill_viridis_d(option = "A", begin = 0.3, end = 0.8) + 
                   scale_color_viridis_d(option = "A", begin = 0.3, end = 0.8) + 
                    guides(color = 'none', 
                           fill = guide_legend(override.aes = list(alpha = 1))) +
            labs(title = stringr::str_wrap(paste(nom_ent[i], ": índice de marginación a nivel localidad, 2020"), 80),
                  fill = stringr::str_wrap("Grado de marginación", 10),
                  #caption = expression(paste("Fuentes: Estimaciones del CONAPO con base en el INEGI, Censo de Población y Vivienda 2020"))
                 )
  
  for(j in 1:nrow(municipio)){
    
    q[[j]] <- ggplot() + 
                geom_polygon(data = capas_municipio %>% filter(id == municipio$CVE_MUN[j]), 
                             mapping = aes(x = long, 
                                           y = lat, 
                                           group = group), 
                             fill = 'transparent',
                             color="black", 
                             size = .15,
                             alpha = 0.9) +
                geom_polygon(data = capas_localidad %>% 
                               mutate(CVE_MUN = paste0(ENT, MUN)) %>% 
                               filter(CVE_MUN == municipio$CVE_MUN[j]),
                             mapping = aes(x = long, 
                                           y = lat, 
                                           group = group, 
                                           fill = fct_relevel(GM_2020, "Muy alto", "Alto", "Medio", "Bajo", "Muy bajo"),
                                           color = fct_relevel(GM_2020, "Muy alto", "Alto", "Medio", "Bajo", "Muy bajo")),
                             size = .15,
                             alpha = 0.9) +
                 coord_equal() +
                  theme_transparent() + 
                    theme(plot.title = element_text(size = 18, hjust = 0.5, family = "montserrat", face = "bold"),
                           plot.caption = element_text(size = 9, hjust = 0, family = "montserrat"), 
                            legend.key.size = unit(0.3, "cm"),
                             legend.text = element_text(size = 10, family = "montserrat"), 
                              legend.title = element_text(size = 12, hjust = 0.5, family = "montserrat", face = "bold"),
                               legend.position = "right") + 
                     scale_fill_viridis_d(option = "A", begin = 0.3, end = 0.8) + 
                      scale_color_viridis_d(option = "A", begin = 0.3, end = 0.8) + 
                       guides(color = 'none', fill = "none") + 
              labs(title = str_wrap(paste(nom_mun$NOM_MUN[j]),50),
                 fill = stringr::str_wrap("Grado de marginación", 10))
  }
}

# Se genera un gráfico con su estado y municipio 

### Se extrae la estructura de  la legenda del gráfico de entidad 

legend <- get_legend(p[[22]] + theme(legend.key.size = unit(0.5, "cm"),
                                     legend.text = element_text(size = 14, family = "montserrat"), 
                                     legend.title = element_text(size = 16, hjust = 0.5, family = "montserrat", face = "bold"),                                legend.position = "right"))

tabla <- lapply(1:nrow(municipio), function(x){
                                    ggarrange(p[[22]] + guides(fill = 'none') + labs(title = ""),
                                              q[[x]], 
                                              legend , 
                                              ncol = 3, nrow = 1,
                                              widths = c(1, 1, 0.5),
                                              heights = c(1, 0.3, 1))
})

### Se generan los títulos y los pie de página del gráfico 

tabla <- lapply(1:nrow(municipio), function(x){
                    annotate_figure(tabla[[x]], 
                                    top = text_grob(stringr::str_wrap(paste(nom_ent[22], ": índice de marginación a nivel localidad, 2020"), 50),
                                                    family = "montserrat", 
                                                    face = "bold", 
                                                    size = 22), 
                                    bottom = text_grob(expression(paste("Fuentes: Estimaciones del CONAPO con base en el INEGI, Censo de Población y Vivienda 2020")),
                                                       family = "montserrat", 
                                                       face = "plain", 
                                                       size = 12,
                                                       hjust = 0.5))
})

#path = "Images/IML_2020_por estado_geom_polygon.pdf"
#ggexport (list = tabla, width = 8, height = 5, res = 400, filename = path)

tabla[[1]]

########################## sf::geom_sf() #######################################

#Conversión de datos: `st_as_sf`, convierte un objeto externo en un spatial object.        

#Los objetos espaciales creados por el paquete `sp` (que hasta hace poco era la 
#forma estándar de manejar datos espaciales en `R)` se pueden formatear en un 
#formato de características simples usando la función `st_as_sf()`: 


# Paquetes
require(ggplot2)
require(tibble)
require(sf)

p <- list()
#for(i in 1:32){
for(i in 7){
  limites <- capas_estados %>%
              filter(id == estados[i]) %>%
               summarise(min.x = min(.$long),
                         max.x = max(.$long),
                         min.y = min(.$lat),
                         max.y = max(.$lat))
  
  p[[i]] <-  ggplot() + 
              geom_sf(data = tbl_localidad,
                      mapping = aes(geometry = geometry, 
                                    fill = fct_relevel(GM_2020, "Muy alto","Alto","Medio","Bajo","Muy bajo"),
                                    color = fct_relevel(GM_2020, "Muy alto","Alto","Medio","Bajo","Muy bajo"))) +
              geom_sf(data = tbl_municipios,
                      mapping = aes(geometry = geometry), 
                      fill = 'transparent',
                      color = "#D4D4D4",
                      size = 0.3) + 
              geom_sf(data = tbl_estados,
                      mapping = aes(geometry = geometry), 
                      fill = 'transparent',
                      color = "black") +
               coord_sf(xlim = c(limites$min.x,limites$max.x),
                        ylim = c(limites$min.y,limites$max.y)) +
                theme_transparent() + 
                 theme(plot.title = element_text(size = 22, hjust = 0, family = "montserrat", face = "bold"),
                        plot.caption = element_text(size = 9, hjust = 0.2, vjust = 1, family = "montserrat"),  
                         plot.margin = unit(c(t = 0.5, r = 0, b = 0, l = 0), "cm"),
                          legend.key.size = unit(0.5, "cm"),
                           legend.text = element_text(size = 10, family = "montserrat"),   
                            legend.title = element_text(size = 12, hjust = 0.5, family = "montserrat", face = "bold"),
                             legend.position = "right") + 
                  scale_fill_manual(values = usecol(pal_bordeaux, n = 5)) + 
                   scale_color_manual(values = usecol(pal_bordeaux, n = 5)) + 
              labs(title = paste(nom_ent[i]),
                    fill = stringr::str_wrap("Grado de marginación", 10),
                     color = stringr::str_wrap("Grado de marginación", 10),
                      caption = expression(paste("Fuentes: Estimaciones del CONAPO con base en el INEGI, Censo de Población y Vivienda 2020")))
}

#path = "Images/IML_2020_geom_sf.pdf"
#ggexport (list = p, width = 8, height = 5, res = 400, filename = path)

p[[7]]

##### Con Filtro  ##############################################################

p <- list()
#for(i in 1:32){
for(i in 7){
  limites <- capas_estados %>%
    filter(id == estados[i]) %>%
    summarise(min.x = min(.$long),
              max.x = max(.$long),
              min.y = min(.$lat),
              max.y = max(.$lat))
  
  p[[i]] <-  ggplot() + 
    geom_sf(data = tbl_localidad %>% filter(CVE_ENT == estados[i]),
            mapping = aes(geometry = geometry, 
                          fill = fct_relevel(GM_2020, "Muy alto","Alto","Medio","Bajo","Muy bajo"),
                          color = fct_relevel(GM_2020, "Muy alto","Alto","Medio","Bajo","Muy bajo"))) +
    geom_sf(data = tbl_municipios %>% filter(CVE_ENT == estados[i]),
            mapping = aes(geometry = geometry), 
            fill = 'transparent',
            color = "#D4D4D4",
            size = 0.3) + 
    geom_sf(data = tbl_estados %>% filter(CVE_ENT == estados[i]),
            mapping = aes(geometry = geometry), 
            fill = 'transparent',
            color = "black") +
    coord_sf(xlim = c(limites$min.x,limites$max.x),
             ylim = c(limites$min.y,limites$max.y)) +
    theme_transparent() + 
    theme(plot.title = element_text(size = 22, hjust = 0, family = "montserrat", face = "bold"),
          plot.caption = element_text(size = 9, hjust = 0.2, vjust = 1, family = "montserrat"),  
          plot.margin = unit(c(t = 0.5, r = 0, b = 0, l = 0), "cm"),
          legend.key.size = unit(0.5, "cm"),
          legend.text = element_text(size = 10, family = "montserrat"),   
          legend.title = element_text(size = 12, hjust = 0.5, family = "montserrat", face = "bold"),
          legend.position = "right") + 
    scale_fill_manual(values = usecol(pal_bordeaux, n = 5)) + 
    scale_color_manual(values = usecol(pal_bordeaux, n = 5)) + 
    labs(title = paste(nom_ent[i]),
         fill = stringr::str_wrap("Grado de marginación", 10),
         color = stringr::str_wrap("Grado de marginación", 10),
         caption = expression(paste("Fuentes: Estimaciones del CONAPO con base en el INEGI, Censo de Población y Vivienda 2020")))
}

#path = "Images/IML_2020_geom_sf.pdf"
#ggexport (list = p, width = 8, height = 5, res = 400, filename = path)

p[[7]]

################################################################################
############################  Nivel AGEB   #####################################
################################################################################

############################### Shapefile ######################################

#La función `readOGR` del paquete `rgdal`, extrae automáticamente la información 
#utilizada por otros paquetes `SIG` de código abierto como QGIS y permite a R 
#manejar una gama más amplia de formatos de datos espaciales. Esta función lee 
#datos `OGR` y datos vectoriales, pero solamente permite manejar capas con 
#características geométricas (no mezcla puntos, líneas o polígonos en una sola capa) 
#y a su vez establecerá un sistema de referencia espacial si la capa tiene dichos 
#metadatos.      
#Para leer un archivo `shapefile`, se establecen los siguientes argumentos, 
#como `dsn`, en donde se indica el directorio que contiene los shapes y `layer` 
#que es el nombre explícito de la capa a trabajar y dichas capas deben de ir sin 
#la extensión `.shp`.   

#A continuación se lee el archivo .shp que contiene de manera integrada la 
#división de el área geoestadística a nivel ageb `ageb`.   

require(rgdal)
shape_ageb <- readOGR(dsn ="MGN 2020/conjunto_de_datos", 
                       layer = "00a",
                        encoding = "UTF-8",
                         use_iconv = TRUE)


# La función `rename()` del paquete `dplyr` permite cambiar el nombre de la columna 
#de la clave geoestadística a nivel localidad dentro de la base de datos del shape.

shape_ageb@data <- shape_ageb@data %>%
                     rename("CVE_GEO" = "CVEGEO")

################ Índice de marginación a nivel ageb ###########################

#La base de datos del índice de marginación por localidades se encuentra 
#disponible en la página oficial de [CONAPO]
#( https://www.gob.mx/conapo/documentos/indices-de-marginacion-2020-284372) o 
#bien se puede consultar en la página de [Datos Abiertos](https://datos.gob.mx/) 
#y se presenta en formato `.xlsx` [Consulta](https://datos.gob.mx/busca/dataset/indice-de-marginacion-carencias-poblacionales-por-localidad-municipio-y-entidad).   

#Para poder trabajar con la base de datos del índice de marginación a nivel 
#ageb, se cambia el nombre de la columna `CVE_AGEB`  que hace referencia a 
#la clave geográfica de la ageb por `CVE_GEO` para fines prácticos.  

IMU_2020 <- read_xlsx("Data/IMU_2020.xlsx", sheet = "IMU_2020") %>%
             mutate(CVE_GEO = CVE_AGEB) %>% 
               as.data.frame()

#Ahora bien, para la realización de los *mapas cloropléticos* que muestran áreas 
#geográficas divididas o regiones que están coloreadas en relación con una 
#variable de interés. Para este documento se decide hacer uso del **grado de marginación** 
#que permite englobar a las unidades geográficas que presentan las mismas 
#condiciones de exclusión social en cinco categorías.  


tabla <- IMU_2020 %>%
          mutate(GM_2020 = fct_relevel(GM_2020, levels = c("Muy alto", "Alto", "Medio", "Bajo", "Muy bajo"))) %>%
           dplyr::select(GM_2020) %>%
            group_by(GM_2020) %>%
             summarise(N = n()) %>%
              ungroup() %>% 
               as.data.frame()

kable(tabla,
      col.names = c("Grado de \n marginación", "Ageb"), 
      align = "c", 
      caption = c("Tabla: Agebs según el grado de marginación.")) %>%
  kable_styling(position = "center",
                bootstrap_options = c("condensed", "responsive", "bordered", "hover"), 
                font_size = 10) %>%
   kable_classic(full_width = F, html_font = "montserrat") %>%
    row_spec(0, color = "black", bold = TRUE) %>% 
     gsub("font-size: initial !important;", "font-size: 11pt !important;", .) %>%
      gsub("text-align: initial !important;", "text-align: justify !important;", .)


############################### Layers #########################################

#Existen dos maneras de trabajar los datos de los mapas:  

#- Es trabajar con los objetos `SpatialPolygonsDataFrame` o bien `spdf` y tratar 
#de unir los datos que se desean analizar, llamando algunas de las funciones de 
#`join_` o `merge` del paquete `dplyr`.   

#- O bien utilizar la función `fortify()`  del paquete `ggplot2`, que permite 
#convertir un objeto (ej.`spdf`) a un *data.frame*. Al hacer este tipo de conversión 
#permite aplicar un `join` para unir ambas estructuras de datos al nuevo formato 
#del data.frame.   


#################### SpatialPolygons -> Data.frame #############################

#Cuando es necesario cambiar los ID's de las características en los objetos 
#`SpatialLines` o `SpatialPolygons`, se puede usar la función `spChFIDs()` de la 
#paquetería `sp`. Donde los nuevos ID's deben ser un vector de caracteres únicos 
#de la longitud correcta.   

#La función `fortify()` convierte un objeto S3 para convertir objetos diversos 
#a un data.frame para `ggplot2`. El data.frame resultante de la conversión de un 
#objeto *spdf* es sumamente largo, tiene una fila por cada cambio en un polígono 
#y un columna de grupo para separar cada polígono.  

capas_ageb <- shape_ageb %>%
                sp::spChFIDs(., str_pad(shape_ageb@data$CVE_GEO, 13, "left", pad = "0")) %>%
                 fortify(., id = "CVE_GEO") %>%
                  right_join(., IMU_2020 %>% dplyr::select(CVE_GEO, ENT, NOM_ENT, MUN, NOM_MUN, LOC, NOM_LOC, GM_2020), 
                              by = c("id" = "CVE_GEO"))

############ SpatialPolygons -> SpatialPolygonsDataFrame #######################

layer_ageb <- merge(shape_ageb,
                     IMU_2020 %>%
                       dplyr::select(c(-CVE_AGEB)) %>%
                        mutate(GM_2020 = fct_relevel(.$GM_2020,"Muy alto", "Alto", "Medio", "Bajo", "Muy bajo")),
                      by = "CVE_GEO")
   

############ SpatialPolygons -> tbl_df/ tbl / data.frame ####################### 

#Conversión de datos: `st_as_sf`, convierte un objeto externo en un spatial object. 
#A continuación, se muestra un ejemplo de cómo visualizar objetos `sf`. 
#Este tipo de función es un poco más amigable de usar.  

#La función `as_tibble` de la paquetería `tibble`, genera una base de datos 
#genérica de S3 con métodos más eficientes para matrices y data.frame. 
#Su envoltorio delgado permite trabajar de manera más rápida que un `data.frame`.  

# Paquetes 
require(tibble)
require(sf)

tbl_ageb <- as_tibble(st_as_sf(shape_ageb)) %>%
              merge(., IMU_2020 %>% dplyr::select(CVE_GEO, GM_2020))  


###################### Zonas Metropolitanas 2015 ###############################

#La base de datos de las zonas metropolitanas se encuentra disponible  en la 
#página oficial de [CONAPO]

capas_zm <- shape_ageb %>%
              sp::spChFIDs(., str_pad(shape_ageb@data$CVE_GEO, 13, "left", pad = "0")) %>%
               fortify(., id = "CVE_GEO") %>%
                right_join(., 
                            IMU_2020 %>% dplyr::select(CVE_GEO, ENT, NOM_ENT, MUN, NOM_MUN, LOC, NOM_LOC, GM_2020), 
                             by = c("id" = "CVE_GEO")) %>% 
                 mutate(CVE_MUN = paste0(ENT, MUN)) %>%
                   right_join(., ZM_2015 %>% dplyr::select(CVE_MUN, CVE_ZM, NOM_ZM), 
                               by = "CVE_MUN")

###################### Mapas a nivel ageb ######################################

########################## ggplot2::geom_polygons ##############################

#La función `geom_polygon()` de la paquetería `ggplot2`, devuelve un objeto 
#`ggplot` en el cual dibuja líneas entre puntos y los cierra ( es decir, dibuja 
#una línea desde el último punto hasta el primer punto). En el cual es importante 
#tener en cuenta que el data.frame incluye columnas con la ubicación de las áreas 
#geográficas y se agregan como argumentos en la función `aes(x = long,  y = lat)`. 
#También se incluye una columna que describe el orden en que se deben conectar 
#estos puntos para formar un polígono (`order`), el nombre del estado (`id`) y 
#una columna de grupo que separa los puntos en polígonos únicos que se deben 
#trazar `aes (group = group)`.  


p <- ggplot() + 
      geom_polygon(capas_estados,
                   mapping = aes(x = long, 
                                 y = lat, 
                                 group = group), 
                   fill = 'transparent',
                   color="black", 
                   size = .15,
                   alpha = 0.9) +
      geom_polygon(capas_ageb,
                   mapping = aes(x = long, 
                                 y = lat, 
                                 group = group, 
                                 fill = fct_relevel(GM_2020, "Muy alto", "Alto", "Medio", "Bajo", "Muy bajo"), 
                                 color = fct_relevel(GM_2020, "Muy alto", "Alto", "Medio", "Bajo", "Muy bajo")),
                   size = .15,
                   alpha = 0.9) +
        coord_equal() +
         theme_transparent() + 
          theme(plot.title = element_text(size = 24, hjust = 0.15, family = "montserrat", face = "bold"), 
                 plot.caption = element_text(size = 12, hjust = 0.2, vjust = 1, family = "montserrat"), 
                  legend.key.size = unit(0.5, "cm"),
                   legend.text = element_text(size = 12, family = "montserrat"), 
                    legend.title = element_text(size = 14, hjust = 0.5, family = "montserrat", face = "bold"),
                     legend.position = c(0.8, 0.7)) + 
           scale_fill_viridis_d(option = "A", begin = 0.3, end = 0.8) + 
            scale_color_viridis_d(option = "A", begin = 0.3, end = 0.8) +  
             guides(color = 'none', 
                    fill = guide_legend(override.aes = list(alpha = 1))) +
          labs(title = "Índice de marginación a nivel AGEB, 2020",
                fill = stringr::str_wrap("Grado de marginación", 10),
                 caption = expression(paste("Fuentes: Estimaciones del CONAPO con base en el INEGI, Censo de Población y Vivienda 2020")))
    
#path = "Images/IMU_2020_geom_polygon.pdf"
#ggexport (p, width = 8, height = 5, res = 400, filename = path)

p

###########  Zona Metropolitana del Valle de México (ZMVM) #####################

tabla <- ZM_2015 %>%
           filter(CVE_ZM == "09.01")

p <-  ggplot() + 
        geom_polygon(capas_estados %>% filter(id == c("09", "13", "15")),
                     mapping = aes(x = long, 
                                   y = lat, 
                                   group = group), 
                     fill = 'transparent',
                     color = "black", 
                     size = .5,
                     alpha = 0.9) +
        geom_polygon(capas_municipio %>% filter(id == tabla$CVE_MUN),
                     mapping = aes(x = long, 
                                   y = lat, 
                                   group = group), 
                     fill = 'transparent',
                     color = "#C7C7C7", 
                     size = .3,
                     alpha = 0.9) +
        geom_polygon(capas_zm %>% filter(CVE_ZM == "09.01"),
                     mapping = aes(x = long, 
                                   y = lat, 
                                   group = group, 
                                   fill = fct_relevel(GM_2020, "Muy alto", "Alto", "Medio", "Bajo", "Muy bajo"),
                                   color = fct_relevel(GM_2020, "Muy alto", "Alto", "Medio", "Bajo", "Muy bajo")),
                     size = .15,
                     alpha = 0.7) +
         coord_equal() +
          theme_transparent() + 
           theme(plot.title = element_text(size = 22, hjust = 0.15, family = "montserrat", face = "bold"), 
                  plot.caption = element_text(size = 11, hjust = 0.2, vjust = 1, family = "montserrat"), 
                   legend.key.size = unit(0.5, "cm"),
                    legend.text = element_text(size = 12, family = "montserrat"), 
                     legend.title = element_text(size = 14, hjust = 0.5, family = "montserrat", face = "bold"),
                      legend.position = "right") + 
             scale_fill_viridis_d(option = "A", begin = 0.3, end = 0.8) + 
              scale_color_viridis_d(option = "A", begin = 0.3, end = 0.8) +  
               guides(color = 'none', 
                      fill = guide_legend(override.aes = list(alpha = 1))) +
        labs(title = "ZMVM: Índice de marginación a nivel AGEB, 2020",
              fill = stringr::str_wrap("Grado de marginación", 10),
               caption = expression(paste("Fuentes: Estimaciones del CONAPO con base en el INEGI, Censo de Población y Vivienda 2020")))

#path = "Images/IMU_2020_ZMVM_geom_polygon.pdf"
#ggexport (p, width = 8, height = 5, res = 400, filename = path)

p


########################## sf::geom_sf() #######################################

#Conversión de datos: `st_as_sf`, convierte un objeto externo en un spatial object.        

#Los objetos espaciales creados por el paquete `sp` (que hasta hace poco era la 
#forma estándar de manejar datos espaciales en `R)` se pueden formatear en un 
#formato de características simples usando la función `st_as_sf()`: 


# Paquetes
require(ggplot2)
require(tibble)
require(sf)

q <- NULL
p <- list()
#for(i in 1:32){
for(i in 6){
  limites <- capas_estados %>%
               filter(id == estados[i]) %>%
                summarise(min.x = min(.$long),
                          max.x = max(.$long),
                          min.y = min(.$lat),
                          max.y = max(.$lat))
  q[[i]] <-  ggplot() + 
              geom_sf(data = tbl_ageb %>% filter(CVE_ENT == estados[i]),
                      mapping = aes(geometry = geometry, 
                                    fill = fct_relevel(GM_2020, "Muy alto","Alto","Medio","Bajo","Muy bajo"),
                                    color = fct_relevel(GM_2020, "Muy alto","Alto","Medio","Bajo","Muy bajo"),
                                    linetype = "Line1"),
                      size = 0.7) +
              geom_sf(data = tbl_localidad %>% filter(CVE_ENT == estados[i]),
                      mapping = aes(geometry = geometry, linetype = "Line2"), 
                      fill = 'transparent',
                      color = "#C70E8E", 
                      size = 0.03) + 
              geom_sf(data = tbl_municipios %>% filter(CVE_ENT == estados[i]),
                      mapping = aes(geometry = geometry), 
                      fill = 'transparent',
                      color = "#D4D4D4",
                      size = 0.3) + 
              geom_sf(data = tbl_estados %>% filter(CVE_ENT == estados[i]),
                      mapping = aes(geometry = geometry), 
                      fill = 'transparent',
                      color = "black") +
                coord_sf(xlim = c(limites$min.x,limites$max.x),
                         ylim = c(limites$min.y,limites$max.y)) +
                 theme_transparent() + 
                  theme(plot.title = element_text(size = 22, hjust = 0.15, family = "montserrat", face = "bold"),
                         plot.caption = element_text(size = 9, hjust = 0.2, vjust = 1, family = "montserrat"),  
                          legend.key.size = unit(0.5, "cm"),
                           legend.text = element_text(size = 10, family = "montserrat"),   
                            legend.title = element_text(size = 12, hjust = 0.5, family = "montserrat", face = "bold"),
                             legend.position = "right") + 
                   scale_fill_manual(values = usecol(pal_bordeaux, n = 5)) + 
                    scale_color_manual(values = usecol(pal_bordeaux, n = 5)) +  
                     scale_linetype_manual("Nivel", labels = c("AGEB", "Localidad"), values = c("Line1" = "solid", "Line2" = "solid")) +
                      guides(linetype = guide_legend(override.aes = list(linetype = "solid", fill = 'transparent', color = c("#771434", "#C70E8E"), size = c(0.7, 0.03)))) +
              labs(title = paste(nom_ent[i]),
                    fill = stringr::str_wrap("Grado de marginación", 10),
                     color = stringr::str_wrap("Grado de marginación", 10),
                      caption = expression(paste("Fuentes: Estimaciones del CONAPO con base en el INEGI, Censo de Población y Vivienda 2020")))
  if(i == 6){
    p[[i]] <- q[[i]] + 
      coord_sf(xlim = c(limites$min.x + 1060000, limites$max.x),
               ylim = c(limites$min.y, limites$max.y)) 
  } else {
    p[[i]] <- q[[i]]
  }
}

#path = "Images/IMU_2020_geom_sf.pdf"
#ggexport (list = p, width = 8, height = 5, res = 400, filename = path)

p[[6]]

##################################### Librerías#################################  

##Librerías que se usaron en el trabajo    
  
names(sessionInfo()$otherPkgs)

###################################### Referencias #############################  

#Datos Abiertos de México - Índice de marginación (carencias poblacionales) por localidad, municipio y entidad. (2021). Retrieved February 13, 2022, from https://datos.gob.mx/busca/dataset/indice-de-marginacion-carencias-poblacionales-por-localidad-municipio-y-entidad    


#Wickham, H. (2022). A Grammar of Data Manipulation • dplyr. Retrieved February 12, 2022, from https://dplyr.tidyverse.org/    
  
#Garnett, R. (2018). cheatsheets/sf.pdf at main · rstudio/cheatsheets · GitHub. Retrieved February 12, 2022, from https://github.com/rstudio/cheatsheets/blob/main/sf.pdf    

#Lovelace, R., Nowosad, J., & Muenchow, J. (2019). Geocomputation with R. CRC Press. https://geocompr.robinlovelace.net/    
  


