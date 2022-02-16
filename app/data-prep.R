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
noco_key <- Sys.getenv("NOCO_API_KEY")



# C O N T R A P A R T E

urlContraparte <- "https://datos.paga.datasketch.co/nc/registro_de_avances_gzwk/api/v1/contrapartes?limit=100000"#"https://datos.paga.datasketch.co/nc/registro_de_avances_gzwk/api/v1/contrapartes"
infoContraparte <- httr::GET(urlContraparte, add_headers(`xc-auth` = noco_key))
dataContraparte <- httr::content(infoContraparte) %>% dplyr::bind_rows()
dataContraparte$idF <- paste0("ind_",1:nrow(dataContraparte))
indHito <- grep("Hito", names(dataContraparte))
dicHitos <- data_frame(compromiso = dataContraparte$Compromiso,
                       idF = dataContraparte$idF,
                       dataContraparte[,indHito])
dicHitos <- dicHitos %>% 
              tidyr::gather("numHito","hito", -compromiso, -idF) %>% 
               tidyr::drop_na(hito) %>% dplyr::filter(hito != "") %>% dplyr::select(-numHito)

dataContraparte <- dataContraparte[,-indHito]

# dataContraparte <- dataContraparte %>%
#   tidyr::unite("hito",c(`Hito segundo compromiso`, `Hito quinto compromiso`:`Hito séptimo compromiso`), na.rm = TRUE, sep = "")
dataContraparte <- dataContraparte %>% dplyr::rename(c( "estado_contraparte" = "Indicador 2",
                                                        #"entidad" = "Organización",
                                                        "entidad_responsable" = "Indicador 3 - entidad",
                                                        "entidad_responsable_justificacion" = "Indicador 3 - justificación",
                                                        "resultados" = "Indicador 7",
                                                        "compromiso" = "Compromiso",
                                                        "fecha_registro_contraparte" = "Fecha de registro",
                                                        "contraparte_persona_formulario" = "Funcionario",
                                                        "contraparte" = "Organización")) 
dataContraparte <- dataContraparte[,c(-1,-2,-3)]
dataContraparte <- dataContraparte %>% dplyr::inner_join(dicHitos) %>% dplyr::select(-idF)

l <- map(1:ncol(dataContraparte), function(i) {
  dataContraparte[[i]] <<-  trimws( gsub("\n", " ",dataContraparte[[i]]))
  dataContraparte[[i]][dataContraparte[[i]] == ""] <<- NA
})
dataContraparte$compromiso <- gsub("  ", " ", dataContraparte$compromiso)
dataContraparte$fecha_registro_contraparte <- lubridate::ymd(dataContraparte$fecha_registro_contraparte)

# E N T I D A D E S
urlEntidades <- "https://datos.paga.datasketch.co/nc/registro_de_avances_gzwk/api/v1/Entidades?limit=100000"#"https://datos.paga.datasketch.co/nc/registro_de_avances_gzwk/api/v1/Entidadess"
infoEntidades <- httr::GET(urlEntidades, add_headers(`xc-auth` = noco_key))
dataEntidades <- httr::content(infoEntidades) %>% dplyr::bind_rows()
dataEntidades$idF <- paste0("ind_",1:nrow(dataEntidades))
indHito <- grep("Hito", names(dataEntidades))
dicHitos <- data_frame(compromiso = dataEntidades$Compromiso,
                       idF = dataEntidades$idF,
                       dataEntidades[,indHito])
dicHitos <- dicHitos %>% 
  tidyr::gather("numHito","hito", -compromiso, -idF) %>% 
  tidyr::drop_na(hito) %>% dplyr::filter(hito != "") %>% dplyr::select(-numHito)

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



dataEntidades <- dataEntidades[,c(-1,-2,-3)]

dataEntidades <- dataEntidades %>% dplyr::inner_join(dicHitos) %>% dplyr::select(-idF)
l <- map(1:ncol(dataEntidades), function(i) {
  dataEntidades[[i]] <<-  trimws(gsub("\n", " ",dataEntidades[[i]]))
  dataEntidades[[i]][dataEntidades[[i]] == ""] <<- NA
})
dataEntidades$compromiso <- gsub("  ", " ", dataEntidades$compromiso)
dataEntidades$relacion_internacional_descripcion <- trimws(dataEntidades$relacion_internacional_descripcion)
dataEntidades <- dataEntidades %>% dplyr::distinct(compromiso, hito, .keep_all = TRUE)
dataEntidades$hito[dataEntidades$hito == "Hito 2: Capacitación sobre estándares de Open Contracting Data Estándar (OCDS) y Open Contracting for Infraestructure Data Estándar (OC4IDS) dirigida a los responsables de compras públicas y actores clave, previamente identificados"] <- "Hito 2: Capacitación sobre estándares de Open Contracting Data Estándar (OCDS) y Open Contracting for Infraestructure Data Estándar (OC4IDS) dirigida a los responsables de compras públicas y actores clave, previamente identificados."

# infoContra <- dataContraparte %>% select(compromiso, hito)
# infoContra$blabla <- "esta en contraparte"
# 
# infoEntid <- dataEntidades %>% select(compromiso, hito)
# infoEntid$sda <- "esto es entidad"
# 
# ddf <- infoContra %>% full_join(infoEntid)



# J O I N 
data_all <- dataContraparte %>% dplyr::full_join(dataEntidades)

data_all$avance[is.na(data_all$avance)] <- 0



# C O M P R O M I S O S
urlEntidades <- "https://datos.paga.datasketch.co/nc/registro_de_avances_gzwk/api/v1/Información general compromisos?limit=100000"#"https://datos.paga.datasketch.co/nc/registro_de_avances_gzwk/api/v1/Entidadess"
infoCompromisos <- httr::GET(urlEntidades, add_headers(`xc-auth` = noco_key))
compromisos <- httr::content(infoCompromisos) %>% dplyr::bind_rows()
compromisos <- Filter(function(x) !all(is.na(x)), compromisos)
compromisos <- compromisos %>% separate_rows(Entidad, Contraparte, sep = "--")
unique(compromisos$Contraparte)
#compromisos <- compromisos %>% select(-`¿El compromiso es prometedor según el IRM?`) # unique(compromisos$Entidad)
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
                                                "corre_contacto" = "Correo_contacto")) 
#compromisos$tematica <- trimws(compromisos$tematica)

compromisos <- compromisos %>% dplyr::select(-Id, -CreatedAt, -UpdatedAt)
l <- map(1:ncol(compromisos), function(i) {
  compromisos[[i]] <<- trimws(gsub("  ", " ",gsub("\t ", "", compromisos[[i]])))
})
unique(compromisos$tematica)

#setdiff(unique(data_all$compromiso), unique(compromisos$compromiso))
#setdiff(unique(data_all$hito), unique(compromisos$hito))
#setdiff(unique(data_all$entidad), unique(compromisos$entidad))
#setdiff(unique(data_all$contraparte), unique(compromisos$contraparte))

all_id_eit <- compromisos %>%
  dplyr::filter(compromiso == "Co-diseño de la hoja de ruta para la implementación del Estándar EITI para mejorar la transparencia en las industrias extractivas en Ecuador (petróleo, gas y minería)")



compromisos2 <- compromisos %>%
  dplyr::filter(compromiso != "Co-diseño de la hoja de ruta para la implementación del Estándar EITI para mejorar la transparencia en las industrias extractivas en Ecuador (petróleo, gas y minería)")

id_eit <- data.frame(hito = unique(all_id_eit$hito))
id_eit$compromiso <- unique(all_id_eit$compromiso)
id_eit$tematica <- unique(all_id_eit$tematica)
id_eit$entidad <- unique(all_id_eit$entidad)
id_eit$vinculación_ods <- unique(all_id_eit$vinculación_ods)
id_eit$valores_ogp <- unique(all_id_eit$valores_ogp)
compromisos2 <- compromisos2 %>% bind_rows(id_eit)
order_data <-  data.frame(compromiso = unique(compromisos$compromiso))
compromisos_toJoin <- order_data %>% dplyr::left_join(compromisos2) %>% tidyr::drop_na(contraparte)

data_all2 <- compromisos_toJoin %>% dplyr::left_join(data_all) 

# G R U P O N U C L E O

urlGrupoNucleo <- "https://datos.paga.datasketch.co/nc/registro_de_avances_gzwk/api/v1/grupo-nucleo?limit=100000"
infoGrupoNucleo <- httr::GET(urlGrupoNucleo, add_headers(`xc-auth` = noco_key))
dataGrupoNucleo <- httr::content(infoGrupoNucleo) %>% dplyr::bind_rows()
dataGrupoNucleo$idF <- paste0("ind_",1:nrow(dataGrupoNucleo))

indHito <- grep("Hito", names(dataGrupoNucleo))
dicHitos <- data_frame(compromiso = dataGrupoNucleo$Compromiso,
                       idF = dataGrupoNucleo$idF,
                       dataGrupoNucleo[,indHito])
dicHitos <- dicHitos %>% 
  tidyr::gather("numHito","hito", -compromiso, -idF) %>% 
  tidyr::drop_na(hito) %>% dplyr::filter(hito != "") %>% dplyr::select(-numHito)

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


dataGrupoNucleo <- dataGrupoNucleo[,c(-1,-2,-3)]
dataGrupoNucleo <- dataGrupoNucleo %>% inner_join(dicHitos) %>% dplyr::select(-idF)
l <- map(1:ncol(dataGrupoNucleo), function(i) {
  dataGrupoNucleo[[i]] <<- trimws(gsub("\n", " ", trimws(dataGrupoNucleo[[i]])))
})

unique(dataGrupoNucleo$hito)
# setdiff(unique(data_all2$compromiso), unique(dataGrupoNucleo$compromiso))
# setdiff(unique(data_all2$hito), unique(dataGrupoNucleo$hito))

data_fin <- data_all2 %>% dplyr::full_join(dataGrupoNucleo)


data_fin$avance <- as.numeric(data_fin$avance)
data_fin$actividades <- as.numeric(data_fin$actividades)
data_fin$participantes <- as.numeric(data_fin$participantes)


readr::write_rds(data_fin, "data/all_data.rds")