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
urlCompromisos <- "http://datos.paga.datasketch.co/nc/registro_de_avances_gzwk/api/v1/InformacionGeneralCompromisos?limit=100000"
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
                                                "IdCompromisos" = "Id",
                                                "CreatedAtCompromiso" = "CreatedAt",
                                                "UpdatedAtCompromiso" = "UpdatedAt"
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



# E N T I D A D E S
urlEntidades <- "https://datos.paga.datasketch.co/nc/registro_de_avances_gzwk/api/v1/Entidades?limit=100000"#"https://datos.paga.datasketch.co/nc/registro_de_avances_gzwk/api/v1/Entidadess"
infoEntidades <- httr::GET(urlEntidades, add_headers(`xc-auth` = noco_key))
dataEntidades <- httr::content(infoEntidades) %>% dplyr::bind_rows()

indHito <- grep("Hito", names(dataEntidades))
dicHitos <- data_frame(compromiso = dataEntidades$Compromiso,
                       idF = dataEntidades$Id,
                       dataEntidades[,indHito])
dicHitos <- dicHitos %>% 
  tidyr::gather("numHito","hito", -compromiso, -idF) %>% 
  tidyr::drop_na(hito) %>% dplyr::filter(hito != "") %>% dplyr::select(-numHito)

dicHitos <- dicHitos %>% arrange(-idF) %>% distinct(compromiso, hito, .keep_all = T)
dicHitos <- dicHitos %>% rename("Id" = "idF")

dataEntidades <- dataEntidades[,-indHito]

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
                                                    "nuevas_iniciativas" = "Indicador 8 - nuevo")) 



dataEntidades <- dataEntidades %>% dplyr::inner_join(dicHitos) #%>% dplyr::select(-idF)
l <- purrr::map(1:ncol(dataEntidades), function(i) {
  dataEntidades[[i]] <<-  trimws(gsub("\n", " ",dataEntidades[[i]]))
  dataEntidades[[i]][dataEntidades[[i]] == ""] <<- NA
})
dataEntidades$compromiso <- gsub("  ", " ", dataEntidades$compromiso)
dataEntidades$relacion_internacional_descripcion <- trimws(dataEntidades$relacion_internacional_descripcion)
dataEntidades <- dataEntidades %>% rename("IdEntidades" = "Id",
                                          "CreatedAtEntidad" = "CreatedAt",
                                          "UpdatedAtEntidad" = "UpdatedAt")
dataEntidades$fecha_registro_entidades <- lubridate::as_date(dataEntidades$fecha_registro_entidades)
dataEntidades <- dataEntidades %>% filter(!entidad_persona_formulario %in% c("Juliana Galvis", "test", "test3"))

data_all <- compromisos %>% left_join(dataEntidades)


# C O N T R A P A R T E

urlContraparte <- "https://datos.paga.datasketch.co/nc/registro_de_avances_gzwk/api/v1/contrapartes?limit=100000"#"https://datos.paga.datasketch.co/nc/registro_de_avances_gzwk/api/v1/contrapartes"
infoContraparte <- httr::GET(urlContraparte, add_headers(`xc-auth` = noco_key))
dataContraparte <- httr::content(infoContraparte) %>% dplyr::bind_rows()

indHito <- grep("Hito", names(dataContraparte))
dicHitos <- data_frame(compromiso = dataContraparte$Compromiso,
                       idF = dataContraparte$Id,
                       organizacion = dataContraparte$Organización,
                       dataContraparte[,indHito])

dicHitos <- dicHitos %>% 
  tidyr::gather("numHito","hito", -compromiso, -idF, -organizacion) %>% 
  tidyr::drop_na(hito) %>% dplyr::filter(hito != "") 
dicHitos <- dicHitos %>% arrange(-idF) %>% distinct(compromiso, organizacion, hito, .keep_all = T) %>% select(-organizacion)
dicHitos <- dicHitos %>% rename("Id" = "idF")
dataContraparte <- dataContraparte[,-indHito]

dataContraparte <- dataContraparte %>% dplyr::rename(c( "estado_contraparte" = "Indicador 2",
                                                        "entidad_responsable" = "Indicador 3 - entidad",
                                                        "entidad_responsable_justificacion" = "Indicador 3 - justificación",
                                                        "resultados" = "Indicador 7",
                                                        "compromiso" = "Compromiso",
                                                        "fecha_registro_contraparte" = "Fecha de registro",
                                                        "contraparte_persona_formulario" = "Funcionario",
                                                        "contraparte" = "Organización")) 

dataContraparte <- dataContraparte %>% dplyr::inner_join(dicHitos) 

l <- purrr::map(1:ncol(dataContraparte), function(i) {
  dataContraparte[[i]] <<-  trimws( gsub("\n", " ",dataContraparte[[i]]))
  dataContraparte[[i]][dataContraparte[[i]] == ""] <<- NA
})
dataContraparte$compromiso <- gsub("  ", " ", dataContraparte$compromiso)
dataContraparte <- dataContraparte %>% rename("IdContraparte" = "Id",
                                              "CreatedAtContraparte" = "CreatedAt",
                                              "UpdatedAtContraparte" = "UpdatedAt")
dataContraparte$fecha_registro_contraparte <- lubridate::as_date(dataContraparte$fecha_registro_contraparte)
dataContraparte <- dataContraparte %>% filter(!contraparte_persona_formulario %in% "test")
# J O I N 
data_all <- data_all %>% dplyr::full_join(dataContraparte)

data_all$avance[is.na(data_all$avance)] <- 0


unique(compromisos$tematica)


# G R U P O N U C L E O

urlGrupoNucleo <- "https://datos.paga.datasketch.co/nc/registro_de_avances_gzwk/api/v1/grupo-nucleo?limit=100000"
infoGrupoNucleo <- httr::GET(urlGrupoNucleo, add_headers(`xc-auth` = noco_key))
dataGrupoNucleo <- httr::content(infoGrupoNucleo) %>% dplyr::bind_rows()

indHito <- grep("Hito", names(dataGrupoNucleo))
dicHitos <- data_frame(compromiso = dataGrupoNucleo$Compromiso,
                       idF = dataGrupoNucleo$Id,
                       dataGrupoNucleo[,indHito])

dicHitos <- dicHitos %>% 
  tidyr::gather("numHito","hito", -compromiso, -idF) %>% 
  tidyr::drop_na(hito) %>% dplyr::filter(hito != "") %>% dplyr::select(-numHito)
dicHitos2 <- dicHitos %>% arrange(-idF) %>%  distinct(compromiso, hito, .keep_all = T)
dicHitos <- dicHitos %>% rename("Id" = "idF")

dataGrupoNucleo <- dataGrupoNucleo[,-indHito]

dataGrupoNucleo <- dataGrupoNucleo %>% dplyr::rename(c( "estado_grupoNucleo" = "Indicador 2",
                                                        "entidad_responsable_gn" = "Indicador 3 - entidad - grupo nucleo",
                                                        "contraparte_responsable_gn" = "Indicador 3 - contraparte - grupo nucleo",
                                                        "contraparte_grupoNucleo" = "Indicador 3 - contraparte",
                                                        "entidad_grupoNucleo" = "Indicador 3 - entidad", 
                                                        "resultados_grupoNucleo" = "Indicador 7",
                                                        "estrategias_grupoNucleo" = "Indicador 9",
                                                        "compromiso" = "Compromiso",
                                                        "fecha_registro_grupoNucleo" = "Fecha de registro")) 


#dataGrupoNucleo <- dataGrupoNucleo[,c(-1,-2,-3)]
dataGrupoNucleo <- dataGrupoNucleo %>% inner_join(dicHitos) 
l <- purrr:::map(1:ncol(dataGrupoNucleo), function(i) {
  dataGrupoNucleo[[i]] <<- trimws(gsub("\n", " ", trimws(dataGrupoNucleo[[i]])))
})
dataGrupoNucleo <- dataGrupoNucleo %>% dplyr::distinct(compromiso, hito, .keep_all = T)
unique(dataGrupoNucleo$hito)
dataGrupoNucleo <- dataGrupoNucleo %>% rename("IdGrupoNucleo" = "Id",
                                              "CreatedAtGrupoNucleo" = "CreatedAt",
                                              "UpdatedAtGrupoNucleo" = "UpdatedAt")




data_fin <- data_all %>% dplyr::full_join(dataGrupoNucleo)


data_fin$avance <- as.numeric(data_fin$avance)
data_fin$actividades <- as.numeric(data_fin$actividades)
data_fin$participantes <- as.numeric(data_fin$participantes)
data_fin$hito_id <- stringr::str_extract(data_fin$hito, "Hito [0-9]")
data_fin$cmp_esperado <- ifelse(lubridate::ymd(data_fin$fecha_finalizacion) < lubridate::ymd("2021-10-22"), "si", "no")
data_fin
#save(data_fin, file = "data/all_data.RData")

