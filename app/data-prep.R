# L I B R E R I A S
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)
if (!require("tidyr")) install.packages("tidyr")
library(tidyr)
if (!require("httr")) install.packages("httr")
library(httr)
if (!require("readr")) install.packages("readr")
library(readr)

# L L A V E
readRenviron(".Renviron")
noco_key <- Sys.getenv("API_TOKEN")


# C O M P R O M I S O S
urlCompromisos <- "https://datos-prueba.paga.datasketch.co/nc/avances_le91/api/v1/info-general-compromisos?limit=false"
infoCompromisos <- httr::GET(urlCompromisos, add_headers("xc-auth" = noco_key))
compromisos <- httr::content(infoCompromisos) %>% dplyr::bind_rows()
compromisos <- Filter(function(x) !all(is.na(x)), compromisos)
compromisos <- compromisos %>% separate_rows(Entidad, Contraparte, sep = "--")
unique(compromisos$Contraparte)
compromisos <- compromisos %>% dplyr::rename(c( "compromiso" = "Nombre_compromisos",
                                                "hito" = "Hitos",
                                                "tematica" = "Tematica",
                                                "entidad" = "Entidad",
                                                "contraparte" = "Contraparte",
                                                "valores_ogp" = "Valores_OGP",
                                                "vinculación_ods" = "Vinculacion_ODS",
                                                "fecha_inicio" = "Fecha_inicio_hito",
                                                "fecha_finalizacion" = "Fecha_finalizacion_hito",
                                                "contacto" = "Nombre_contacto",
                                                "corre_contacto" = "Correo_contacto",
                                                "IdCompromisos" = "id",
                                                "CreatedAtCompromiso" = "created_at",
                                                "UpdatedAtCompromiso" = "updated_at"
)) 

l <- purrr::map(1:ncol(compromisos), function(i) {
  compromisos[[i]] <<- trimws(gsub("  ", " ",gsub("\t ", "", compromisos[[i]])))
})


compromisos <- compromisos %>% separate_rows(contacto, corre_contacto, sep = ",")
compromisos$contacto <- trimws(compromisos$contacto) 
compromisos$corre_contacto <- trimws(compromisos$corre_contacto) 
compromisos <- compromisos %>% filter(!(contacto == "Lorena Raquel Escobar Perez" & contraparte == "Fundación Diálogo Diverso"))
compromisos <- compromisos %>% filter(!(contacto == "Felipe Ochoa" & contraparte == "Fundación Esquel"))
compromisos <- compromisos %>% filter(!(contacto == "Polo Fabian Iñiguez Matute" & entidad == "Presidencia de la República del Ecuador"))
compromisos <- compromisos %>% filter(!(contacto == "Mario Paúl Cabezas" & entidad == "Ministerio de Telecomunicaciones y de la Sociedad de la Información"))
compromisos$hito[compromisos$hito == "Hito 1: Validación de la política de datos abiertos, elaborada con insumos recibidos de participantes de los sectores público, privado, academia, sociedad civil y ciudadanía, que fueron obtenidos en mesas realizadas de manera previa a la formalización del presente compromiso."] <- "Hito 1: Validación de la política de datos abiertos, elaborada con insumos recibidos de participantes de los sectores público, privado, academia, sociedad civil y ciudadanía"


# E N T I D A D E S
urlEntidades <- "https://datos-prueba.paga.datasketch.co/nc/avances_le91/api/v1/entidades?limit=false"#"https://datos-prueba.paga.datasketch.co/nc/avances_le91/api/v1/Entidadess"
infoEntidades <- httr::GET(urlEntidades, add_headers(`xc-auth` = noco_key))
dataEntidades <- httr::content(infoEntidades) %>% dplyr::bind_rows()


indHito <- grep("Hito", names(dataEntidades))

dataEntidades <- dataEntidades %>% group_by(Compromiso) %>% mutate(idF = cur_group_id())


dataEntidades <- dataEntidades %>% 
  tidyr::gather("numHito","hito", indHito) %>% 
  tidyr::drop_na(hito) %>% dplyr::filter(hito != "") %>% dplyr::select(-numHito) 


dataEntidades <- dataEntidades %>% dplyr::rename(c( "compromiso" = "Compromiso",
                                                    "entidad" = "Entidad_responsable_de_registrar_el_avance",
                                                    "entidad_persona_formulario" = "Funcionario",
                                                    "fecha_registro_entidades" = "Fecha de registro",
                                                    "avance_descripcion" = "Descripción",
                                                    "avance" = "Indicador 1",
                                                    "estado" = "Indicador 2",
                                                    "contraparte_responsable" = "Indicador 3 - contraparte",
                                                    "actividades" = "Indicador 4",
                                                    "actividades_descripcion" = "Indicador 4 - justificación",
                                                    "participantes" = "Indicador 5",
                                                    "sectores" = "Indicador 6",
                                                    "realidad_inicial" = "Indicador 7 - realidad inicial",
                                                    "realidad_descripcion" = "Indicador 7 - realidad posterior",
                                                    "relacion_internacional" = "Indicador 8",
                                                    "relacion_internacional_descripcion" = "Indicador 8 - iniciativas",
                                                    "relacion_internacional_punto" = "Indicador 8 - especificación",
                                                    "relacion_internacional_justificacion" = "Indicador 8 - justificación",
                                                    "ind_6_consultor" = "Indicador 6 - nuevo",
                                                    "nuevas_iniciativas" = "Indicador 8 - nuevo",
                                                    "justificacion_entidades"= "Indicador 3 - justificación")) 

dataEntidades <- dataEntidades |> dplyr::filter(fecha_registro_entidades  <= Sys.time())
dataEntidades$hito[dataEntidades$hito == "Hito 2: Capacitación sobre estándares de Open Contracting\nData Estándar (OCDS) y Open Contracting for Infraestructure\nData Estándar (OC4IDS) dirigida a los responsables de\ncompras públicas y actores clave, previamente identificados"] <- "Hito 2: Capacitación sobre estándares de Open Contracting\nData Estándar (OCDS) y Open Contracting for Infraestructure\nData Estándar (OC4IDS) dirigida a los responsables de\ncompras públicas y actores clave, previamente identificados."


#dataEntidades <- dataEntidades %>% dplyr::inner_join(dicHitos) #%>% dplyr::select(-idF)
l <- purrr::map(1:ncol(dataEntidades), function(i) {
  dataEntidades[[i]] <<-  trimws(gsub("\n", " ",dataEntidades[[i]]))
  dataEntidades[[i]][dataEntidades[[i]] == ""] <<- NA
})
dataEntidades$compromiso <- gsub("  ", " ", dataEntidades$compromiso)
dataEntidades$relacion_internacional_descripcion <- trimws(dataEntidades$relacion_internacional_descripcion)
dataEntidades <- dataEntidades %>% rename("IdEntidades" = "id",
                                          "CreatedAtEntidad" = "created_at",
                                          "UpdatedAtEntidad" = "updated_at")
dataEntidades$fecha_registro_entidades <- lubridate::as_date(dataEntidades$fecha_registro_entidades)
dataEntidades$entidad <- gsub("Secretaria", "Secretaría", dataEntidades$entidad)
dataEntidades$hito[dataEntidades$hito == "Hito 1: Validación de la política de datos abiertos, elaborada con insumos recibidos de participantes de los sectores público, privado, academia, sociedad civil y ciudadanía, que fueron obtenidos en mesas realizadas de manera previa a la formalización del presente compromiso."] <- "Hito 1: Validación de la política de datos abiertos, elaborada con insumos recibidos de participantes de los sectores público, privado, academia, sociedad civil y ciudadanía"
#dataEntidades <- dataEntidades[ !duplicated(dataEntidades[, c("compromiso", "hito")], fromLast=T),]
dataEntidades  <- dataEntidades[ !duplicated(dataEntidades[, c("compromiso", "hito")], fromLast=T),]


### base de datos que une la base de compromisos con entidades
data_all <- dataEntidades |> inner_join(compromisos)#compromisos %>% inner_join(dataEntidades)
data_all  <- data_all[ !duplicated(data_all[, c("compromiso", "hito")], fromLast=T),]

# los compromisos que no contienen informacion de avance (estan en na) se dejan con un avance el 0%
data_all$avance[is.na(data_all$avance)] <- 0 
data_all$avance <- as.numeric(data_all$avance)

# ####### GRAFICO 1
# 
# avance <- mean(as.numeric(data_all$avance), na.rm = T)
# dfViz <- data.frame(
#   etiquetas = c("Porcentaje total de cumplimiento del plan", "Porcentaje que falta para el cumplimiento total del plan"),
#   id_T = c("a", "a"),
#   avance = c(avance, 100-avance)
# )
# 
# 
# df_avance <- data_all %>% group_by(compromiso) %>% summarise(avance = mean(as.numeric(avance), na.rm = T))
# df_avance$order <- paste0("Compromiso ", 1:nrow(df_avance))
# df_avance <- df_avance %>% dplyr::select(order, avance, compromiso)

# C O N T R A P A R T E

urlContraparte <- "https://datos-prueba.paga.datasketch.co/nc/avances_le91/api/v1/contrapartes?limit=false"#"https://datos-prueba.paga.datasketch.co/nc/avances_le91/api/v1/contrapartes"
infoContraparte <- httr::GET(urlContraparte, add_headers(`xc-auth` = noco_key))
dataContraparte <- httr::content(infoContraparte) %>% dplyr::bind_rows()

indHito <- grep("Hito", names(dataContraparte))

dataContraparte <- dataContraparte %>% 
  tidyr::gather("numHito","hito", indHito) %>% 
  tidyr::drop_na(hito) %>% dplyr::filter(hito != "") 


dataContraparte <- dataContraparte %>% dplyr::rename(c( "estado_contraparte" = "Indicador 2",
                                                        "entidad_responsable" = "Indicador 3 - entidad",
                                                        "entidad_responsable_justificacion" = "Indicador 3 - justificación",
                                                        "resultados" = "Indicador 7",
                                                        "compromiso" = "Compromiso",
                                                        "fecha_registro_contraparte" = "Fecha de registro",
                                                        "contraparte_persona_formulario" = "Funcionario",
                                                        "contraparte" = "Organización",
                                                        "justificacion_contraparte" = "Indicador 3 - justificación")) 

#dataContraparte <- dataContraparte %>% dplyr::inner_join(dicHitos) 
dataContraparte <- dataContraparte |> dplyr::filter(fecha_registro_contraparte  <= Sys.time())

l <- purrr::map(1:ncol(dataContraparte), function(i) {
  dataContraparte[[i]] <<-  trimws( gsub("\n", " ",dataContraparte[[i]]))
  dataContraparte[[i]][dataContraparte[[i]] == ""] <<- NA
})
dataContraparte$compromiso <- gsub("  ", " ", dataContraparte$compromiso)
dataContraparte <- dataContraparte %>% rename("IdContraparte" = "id",
                                              "CreatedAtContraparte" = "created_at",
                                              "UpdatedAtContraparte" = "updated_at")
dataContraparte$fecha_registro_contraparte <- lubridate::as_date(dataContraparte$fecha_registro_contraparte)
dataContraparte$hito[dataContraparte$hito == "Hito 1: Validación de la política de datos abiertos, elaborada con insumos recibidos de participantes de los sectores público, privado, academia, sociedad civil y ciudadanía, que fueron obtenidos en mesas realizadas de manera previa a la formalización del presente compromiso."] <- "Hito 1: Validación de la política de datos abiertos, elaborada con insumos recibidos de participantes de los sectores público, privado, academia, sociedad civil y ciudadanía"


# J O I N 
data_all2 <- dataContraparte %>% dplyr::inner_join(compromisos)#data_all %>% dplyr::left_join(dataContraparte)
data_all2 <- data_all2[ !duplicated(data_all2[, c("compromiso", "hito")], fromLast=T),]
data_all2 <- data_all2[,names(dataContraparte)]
data_all2 <- data_all2 |> dplyr::select(-contraparte)
data_all <- data_all %>% dplyr::left_join(data_all2)
#data_all <- data_all[ !duplicated(data_all[, c("compromiso", "hito", "contraparte", "entidad")], fromLast=T),]

data_all$avance[is.na(data_all$avance)] <- 0



# G R U P O N U C L E O

urlGrupoNucleo <- "https://datos-prueba.paga.datasketch.co/nc/avances_le91/api/v1/grupo-nucleo?limit=false"
infoGrupoNucleo <- httr::GET(urlGrupoNucleo, add_headers(`xc-auth` = noco_key))
dataGrupoNucleo <- httr::content(infoGrupoNucleo) %>% dplyr::bind_rows()

indHito <- grep("Hito", names(dataGrupoNucleo))
# dicHitos <- data_frame(compromiso = dataGrupoNucleo$Compromiso,
#                        idF = dataGrupoNucleo$id,
#                        dataGrupoNucleo[,indHito])

dataGrupoNucleo <- dataGrupoNucleo %>% 
  tidyr::gather("numHito","hito", indHito) %>% 
  tidyr::drop_na(hito) %>% dplyr::filter(hito != "") %>% dplyr::select(-numHito)

dataGrupoNucleo <- dataGrupoNucleo %>% dplyr::rename(c( "estado_grupoNucleo" = "Indicador 2",
                                                        "entidad_responsable_gn" = "Indicador 3 - entidad - grupo nucleo",
                                                        "contraparte_responsable_gn" = "Indicador 3 - contraparte - grupo nucleo",
                                                        "contraparte_grupoNucleo" = "Indicador 3 - contraparte",
                                                        "entidad_grupoNucleo" = "Indicador 3 - entidad", 
                                                        "resultados_grupoNucleo" = "Indicador 7",
                                                        "estrategias_grupoNucleo" = "Indicador 9",
                                                        "compromiso" = "Compromiso",
                                                        "fecha_registro_grupoNucleo" = "Fecha de registro")) 


dataGrupoNucleo <- dataGrupoNucleo |> dplyr::filter(fecha_registro_grupoNucleo  <= Sys.time())

l <- purrr:::map(1:ncol(dataGrupoNucleo), function(i) {
  dataGrupoNucleo[[i]] <<- trimws(gsub("\n", " ", trimws(dataGrupoNucleo[[i]])))
})
dataGrupoNucleo <- dataGrupoNucleo %>% rename("IdGrupoNucleo" = "id",
                                              "CreatedAtGrupoNucleo" = "created_at",
                                              "UpdatedAtGrupoNucleo" = "updated_at")
dataGrupoNucleo$hito[dataGrupoNucleo$hito == "Hito 1: Validación de la política de datos abiertos, elaborada con insumos recibidos de participantes de los sectores público, privado, academia, sociedad civil y ciudadanía, que fueron obtenidos en mesas realizadas de manera previa a la formalización del presente compromiso."] <- "Hito 1: Validación de la política de datos abiertos, elaborada con insumos recibidos de participantes de los sectores público, privado, academia, sociedad civil y ciudadanía"
dataGrupoNucleo$hito[dataGrupoNucleo$hito == "Hito 2: Articulación de organizaciones de la sociedad civil, academia y movimientos locales que promueven el Acuerdo de Escazú en una Plataforma u Observatorio Ambiental a nivel nacional formado virtual y/o presencialmente"] <- "Hito 2: Articulación de organizaciones de la sociedad civil, academia y movimientos locales que promueven el Acuerdo de Escazú en una Plataforma u Observatorio Ambiental a nivel nacional formado virtual y/o presencialmente para aportar al proceso de implementación del Acuerdo, así como vigilar el cumplimiento de los compromisos (hoja de ruta) que se determinen."
dataGrupoNucleo$hito[dataGrupoNucleo$hito == "Hito 4: Identificación y selección de indicadores para la medición de la implementación de la política de datos abiertos con la participación de actores mapeados"] <- "Hito 4: Identificación y selección de indicadores para la medición de la implementación de la política de datos abiertos con la participación de actores mapeados."
dataGrupoNucleo$hito[dataGrupoNucleo$hito == "Hito 9: Definición de indicadores que permitan la evaluación y mejoramiento de la calidad de los datos y la usabilidad, así como la medición del uso y monitoreo de los procedimientos de régimen común"] <- "Hito 9: Definición de indicadores que permitan la evaluación y mejoramiento de la calidad de los datos y la usabilidad, así como la medición del uso y monitoreo de los procedimientos de régimen común y el nivel de participación de los proveedores en los procedimientos de contratación pública, mismos que permitirán el mejoramiento continuo de los procesos del Sistema Nacional de Contratación Pública."

dataGrupoNucleo <- dataGrupoNucleo[ !duplicated(dataGrupoNucleo[, c("compromiso", "hito")], fromLast=T),]

dataGrupoNucleo$estrategias_grupoNucleo <- stringr::str_to_title(dataGrupoNucleo$estrategias_grupoNucleo)
dataGrupoNucleo$estrategias_grupoNucleo[dataGrupoNucleo$estrategias_grupoNucleo == ""] <- NA
dataGrupoNucleo$estrategias_grupoNucleo[dataGrupoNucleo$estrategias_grupoNucleo == "Si"] <- "Sí"
dataGrupoNucleo$contraparte_grupoNucleo <- stringr::str_to_title(dataGrupoNucleo$contraparte_grupoNucleo)
dataGrupoNucleo$contraparte_grupoNucleo[dataGrupoNucleo$contraparte_grupoNucleo == "Si"] <- "Sí"


data_fin <- data_all %>% dplyr::full_join(dataGrupoNucleo)
#data_fin <- data_fin[ !duplicated(data_fin[, c("compromiso", "hito", "contraparte", "entidad")], fromLast=T),]


data_fin$avance <- as.numeric(data_fin$avance)
data_fin$actividades <- as.numeric(data_fin$actividades)
data_fin$participantes <- as.numeric(data_fin$participantes)
data_fin$hito_id <- stringr::str_extract(data_fin$hito, "Hito [0-9]")
data_fin$cmp_esperado <- ifelse(lubridate::ymd(data_fin$fecha_finalizacion) < Sys.time(), "si", "no")
data_fin$sectores <- gsub(",", "|", data_fin$sectores)
data_fin$sectores <- gsub("entidad pública", "Entidad pública", data_fin$sectores)
data_fin$sectores <- gsub("Sociedad civil", "Sociedad Civil", data_fin$sectores)

order_comp <- dplyr::tibble(compromiso = unique(compromisos$compromiso))
data_fin <- order_comp |> left_join(data_fin)
data_fin
# save(data_fin, file = "data/all_data.RData")

