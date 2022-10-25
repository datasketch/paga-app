library(httr)
library(tidyr)
library(dplyr)

getData <- function() {
  readRenviron(".Renviron")
  noco_key <- Sys.getenv("API_TOKEN")
  
  
  # C O M P R O M I S O S
  urlCompromisos <-
    "https://datos-prueba.paga.datasketch.co/nc/avances_le91/api/v1/info-general-compromisos?limit=false"
  infoCompromisos <-
    httr::GET(urlCompromisos, add_headers("xc-auth" = noco_key))
  compromisos <-
    httr::content(infoCompromisos) %>% dplyr::bind_rows()
  compromisos <- Filter(function(x)
    ! all(is.na(x)), compromisos)
  compromisos <-
    compromisos %>% separate_rows(Entidad, Contraparte, sep = "--")
  unique(compromisos$Contraparte)
  compromisos <-
    compromisos %>% dplyr::rename(
      c(
        "compromiso" = "Nombre_compromisos",
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
      )
    )
  
  l <- purrr::map(1:ncol(compromisos), function(i) {
    compromisos[[i]] <<-
      trimws(gsub("  ", " ", gsub("\t ", "", compromisos[[i]])))
  })
  
  
  compromisos <-
    compromisos %>% separate_rows(contacto, corre_contacto, sep = ",")
  compromisos$contacto <- trimws(compromisos$contacto)
  compromisos$corre_contacto <- trimws(compromisos$corre_contacto)
  compromisos <-
    compromisos %>% filter(
      !(contacto == "Lorena Raquel Escobar Perez" & contraparte == "Fundación Diálogo Diverso")
    )
  compromisos <-
    compromisos %>% filter(!(contacto == "Felipe Ochoa" &
                               contraparte == "Fundación Esquel"))
  compromisos <-
    compromisos %>% filter(
      !(
        contacto == "Polo Fabian Iñiguez Matute" &
          entidad == "Presidencia de la República del Ecuador"
      )
    )
  compromisos <-
    compromisos %>% filter(
      !(
        contacto == "Mario Paúl Cabezas" &
          entidad == "Ministerio de Telecomunicaciones y de la Sociedad de la Información"
      )
    )
  compromisos$hito[compromisos$hito == "Hito 1: Validación de la política de datos abiertos, elaborada con insumos recibidos de participantes de los sectores público, privado, academia, sociedad civil y ciudadanía, que fueron obtenidos en mesas realizadas de manera previa a la formalización del presente compromiso."] <-
    "Hito 1: Validación de la política de datos abiertos, elaborada con insumos recibidos de participantes de los sectores público, privado, academia, sociedad civil y ciudadanía"
  
  
  # E N T I D A D E S
  urlEntidades <-
    "https://datos-prueba.paga.datasketch.co/nc/avances_le91/api/v1/entidades?limit=false"#"https://datos-prueba.paga.datasketch.co/nc/avances_le91/api/v1/Entidadess"
  infoEntidades <-
    httr::GET(urlEntidades, add_headers(`xc-auth` = noco_key))
  dataEntidades <-
    httr::content(infoEntidades) %>% dplyr::bind_rows()
  
  
  indHito <- grep("Hito", names(dataEntidades))
  
  dataEntidades <-
    dataEntidades %>% group_by(Compromiso) %>% mutate(idF = cur_group_id())
  
  
  dataEntidades <- dataEntidades %>%
    tidyr::gather("numHito", "hito", indHito) %>%
    tidyr::drop_na(hito) %>% dplyr::filter(hito != "") %>% dplyr::select(-numHito)
  
  
  dataEntidades <-
    dataEntidades %>% dplyr::rename(
      c(
        "compromiso" = "Compromiso",
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
        "nuevas_iniciativas" = "Indicador 8 - nuevo"
      )
    )
  
  
  
  #dataEntidades <- dataEntidades %>% dplyr::inner_join(dicHitos) #%>% dplyr::select(-idF)
  l <- purrr::map(1:ncol(dataEntidades), function(i) {
    dataEntidades[[i]] <<-  trimws(gsub("\n", " ", dataEntidades[[i]]))
    dataEntidades[[i]][dataEntidades[[i]] == ""] <<- NA
  })
  dataEntidades$compromiso <-
    gsub("  ", " ", dataEntidades$compromiso)
  dataEntidades$relacion_internacional_descripcion <-
    trimws(dataEntidades$relacion_internacional_descripcion)
  dataEntidades <- dataEntidades %>% rename(
    "IdEntidades" = "id",
    "CreatedAtEntidad" = "created_at",
    "UpdatedAtEntidad" = "updated_at"
  )
  dataEntidades$fecha_registro_entidades <-
    lubridate::as_date(dataEntidades$fecha_registro_entidades)
  dataEntidades$entidad <-
    gsub("Secretaria", "Secretaría", dataEntidades$entidad)
  dataEntidades$hito[dataEntidades$hito == "Hito 1: Validación de la política de datos abiertos, elaborada con insumos recibidos de participantes de los sectores público, privado, academia, sociedad civil y ciudadanía, que fueron obtenidos en mesas realizadas de manera previa a la formalización del presente compromiso."] <-
    "Hito 1: Validación de la política de datos abiertos, elaborada con insumos recibidos de participantes de los sectores público, privado, academia, sociedad civil y ciudadanía"
  dataEntidades <-
    dataEntidades[!duplicated(dataEntidades[, c("compromiso", "hito")], fromLast =
                                T),]
  
  
  ### base de datos que une la base de compromisos con entidades
  data_all <- compromisos %>% left_join(dataEntidades)
  # los compromisos que no contienen informacion de avance (estan en na) se dejan con un avance el 0%
  data_all$avance[is.na(data_all$avance)] <- 0
  data_all$avance <- as.numeric(data_all$avance)
  
  data_all
}

#* Check api status
#* @get /healthz
function() {
  list(msg = "Ok")
}

#* Get charts data
#* @get /charts
function() {
  data <- getData()
  avance <- mean(as.numeric(data$avance), na.rm = T)
  avance_general <- data.frame(
    etiquetas = c(
      "Porcentaje total de cumplimiento del plan",
      "Porcentaje que falta para el cumplimiento total del plan"
    ),
    avance = c(avance, 100 - avance)
  )
  
  avance_por_compromiso <-
    data %>% group_by(compromiso) %>% summarise(avance = mean(as.numeric(avance), na.rm = T))
  avance_por_compromiso$order <-
    paste0("Compromiso ", 1:nrow(avance_por_compromiso))
  avance_por_compromiso <-
    avance_por_compromiso %>% dplyr::select(order, avance, compromiso)
  
  list(avance_general = avance_general,
       avance_por_compromiso = avance_por_compromiso)
}