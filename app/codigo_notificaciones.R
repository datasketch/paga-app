# CARGA DE LIBRERIAS
library(httr)
library(jsonlite)
library(rjson)
library(dplyr)
library(htmltools)
library(lubridate)
library(mailR)


#### CARGA DE LA INFORMACIÓN ####
#load(file = "data/all_data.RData")
data_fin <- source("data-prep.R")$value
# TABLA LISTADO DE CORREOS
tabla_correo<-data.frame(entidad=c("Ministerio de Telecomunicaciones y de la Sociedad de la Información",
                                   "Secretaría Nacional de Planificación",                               
                                   "Servicio Nacional de Contratación Pública",                          
                                   "Ministerio del Ambiente, Agua y Transición Ecológica",               
                                   "Ministerio de Energía y Recursos Naturales No Renovables",           
                                   "Defensoría del Pueblo",                                              
                                   "Secretaría de Derechos Humanos",
                                   "Secretaría de Derechos Humanos",
                                   "Presidencia de la República del Ecuador",
                                   "Presidencia de la República del Ecuador - Compromiso 9",
                                   "Ministerio del Trabajo",
                                   "Red Ecuatoriana de Datos Abiertos y Metadatos (REDAM)",                                                          
                                   "Fundación de Ayuda por Internet",                                                                                
                                   "Fundación Ciudadanía y Desarrollo",
                                   "Universidad de Los Hemisferios, a través de Centro Internacional de Investigaciones sobre Ambiente y Territorio",
                                   "Corporación Participación Ciudadana, Grupo FARO",                                                                
                                   "Corporación Participación Ciudadana",                                                                            
                                   "Instituto de Altos Estudios Nacionales",
                                   "Instituto de Altos Estudios Nacionales",
                                   "Fundación Diálogo Diverso",
                                   "Fundación San Francisco Global",
                                   "Fundación Esquel",
                                   "Colegio de Administradores Públicos de Loja",
                                   "Grupo Nucleo",
                                   "IETI"),
                         correo=c("fabian.iniguez@mintel.gob.ec",
                                  "lguaman@planificacion.gob.ec",
                                  "christian.jarrin@sercop.gob.ec",
                                  "jose.naula@ambiente.gob.ec",
                                  "veronica.giler@recursosyenergia.gob.ec",
                                  "jaime.andrade@dpe.gob.ec",
                                  "lorena.escobar@derechoshumanos.gob.ec",
                                  "felipe.ochoa@derechoshumanos.gob.ec",
                                  "evelin.yandun@presidencia.gob.ec",
                                  "cabezasm@presidencia.gob.ec",
                                  "henry_valencia@trabajo.gob.ec",
                                  "julio@datalat.org",
                                  "edobejar@fundapi.org",
                                  "malarcon@ciudadaniaydesarrollo.org",
                                  "danielb@uhemisferios.edu.ec",
                                  "sjaramillo@participacionciudadana.org",
                                  "sjaramillo@participacionciudadana.org",
                                  "carlos.paladines@iaen.edu.ec",
                                  "irma.jara@iaen.edu.ec",
                                  "somos@dialogodiverso.org",
                                  "claudioarcos@sanfranciscoglobal.org",
                                  "bsalazar@esquel.org.ec",
                                  "luisjimenez859@gmail.com",
                                  "gobiernoabierto@presidencia.gob.ec",
                                  "secretariotecnico@eitiec.org"))



#### NOTIFICACIÓN FECHAS 2022-05-17 Y 2022-05-20 ####
if(Sys.Date()==lubridate::as_date("2022-05-17")|Sys.Date()==lubridate::as_date("2022-05-20")){
  #### PRIMERA NOTIFICACIÓN GENERAL 
  cuerpo_correo<- paste0("Estimada Entidad Responsable, entre el 23-05-2022 y 02-06-2022 se procederá con la carga de información del compromiso a su cargo. Por favor preparar, revisar y actualizar la información necesaria para demostrar el avance realizado y de igual manera gestionar la carga de las evidencias del avance de cumplimiento.
  Atentamente, Gobierno Abierto Ecuador")
  ######## ENVIO DE CORREO ELECTRONICO 
  sender <- "gizcontrol001@gmail.com"
  recipients <- c("flor.gonzalez@giz.de")#c(unique(tabla_correo$correo))
  mailR::send.mail(from = sender,
                   to = recipients,
                   cc = c("camila@datasketch.co"),#gobiernoabierto@presidencia.gob.ec
                   subject = "PAGA: Recordatorio para carga de información",
                   body = cuerpo_correo,
                   smtp = list(host.name = "smtp.gmail.com", port = 465,
                               user.name = "gizcontrol001@gmail.com",
                               passwd = "gizcontrol4752", ssl = TRUE),
                   authenticate = TRUE,
                   send = TRUE)
}


#### CORREOS RECORDATORIOS DE ALERTAS DE CARGA DE INFORMACIÓN ####
if(Sys.Date()>=lubridate::as_date("2022-05-23")&&Sys.Date()<=lubridate::as_date("2022-06-02")){
  split_1<- split(x = data_fin,f = data_fin$compromiso)
  # ANALISIS POR COMPROMISOS #
  for (i in 1:length(split_1)) {
    #i<-4
    tmp<-split_1[[i]]
    tmp<- tmp%>%arrange(hito)%>%as.data.frame()
    split_2<- split(x = tmp,f = tmp$entidad)
    ### ANALISIS POR COMPROMISO Y ENTIDAD
    for (k in 1:length(split_2)) {
      #k<-1
      tmp_2<- split_2[[k]]%>%as.data.frame()
      #### VERIFICACION DE HITOS DONDE LA FECHA ES NA ###
      envio<-tmp_2[is.na(tmp_2$fecha_registro_entidades)==T,c("compromiso","hito","fecha_registro_entidades")]
      if(nrow(envio)>0){
        compromiso<- unique(envio$compromiso)
        hitos<- sort(unique(envio$hito))
        cuerpo_correo<- paste0("Estimada Entidad Responsable, a la fecha del ",Sys.Date()," se identifica que no se ha procedido con la carga de información con respecto al compromiso: ",toupper(compromiso),", específicamente en: ",paste0(hitos,collapse = "; ")," Por favor completar la información requerida hasta el 2022-06-02. <br>Atentamente, Gobierno Abierto Ecuador.")
        ######## ENVIO DE CORREO ELECTRONICO #####
        if(compromiso=="Ciudadanización de la mejora de los trámites (simplificación)"){
          sender <- "gizcontrol001@gmail.com"
          recipients <- c("flor.gonzalez@giz.de")#c(tabla_correo[tabla_correo$entidad==unique(tmp_2$entidad),"correo"],"cabezasm@presidencia.gob.ec")
          mailR::send.mail(from = sender,
                           to = recipients,
                           cc = c("cabb96.cb@gmail.com"),#gobiernoabierto@presidencia.gob.ec
                           subject = "PAGA: Recordatorio para carga de información",
                           body = cuerpo_correo,
                           smtp = list(host.name = "smtp.gmail.com", port = 465,
                                       user.name = "gizcontrol001@gmail.com",
                                       passwd = "gizcontrol4752", ssl = TRUE),
                           authenticate = TRUE,
                           send = TRUE)
        }else{
          sender <- "gizcontrol001@gmail.com"
          recipients <- c("flor.gonzalez@giz.de")#c(tabla_correo[tabla_correo$entidad==unique(tmp_2$entidad),"correo"])
          mailR::send.mail(from = sender,
                           to = recipients,
                           cc = c("cabb96.cb@gmail.com"),#gobiernoabierto@presidencia.gob.ec
                           subject = "PAGA: Recordatorio para carga de información",
                           body = cuerpo_correo,
                           smtp = list(host.name = "smtp.gmail.com", port = 465,
                                       user.name = "gizcontrol001@gmail.com",
                                       passwd = "gizcontrol4752", ssl = TRUE),
                           authenticate = TRUE,
                           send = TRUE)
        }
      }
    }
    
    # ANALISIS POR COMPROMISO Y CONTRAPARTE
    split_3<-split(x = tmp,f = tmp$contraparte)
    for (k in 1:length(split_3)) {
      #k<-3
      tmp_2<- split_3[[k]]%>%as.data.frame()
      #### VERIFICACION DE HITOS DONDE LA FECHA ES NA ###
      envio<-tmp_2[is.na(tmp_2$fecha_registro_contraparte)==T,c("compromiso","hito","entidad","contraparte","fecha_registro_contraparte")]
      if(nrow(envio)>0){
        compromiso<- unique(envio$compromiso)
        hitos<- sort(unique(envio$hito))
        cuerpo_correo<- paste0("Estimada Contraparte, a la fecha del ",Sys.Date(),
                               " se identifica que no se ha procedido con la carga de información con repecto al compromiso: ",toupper(compromiso),
                               ", específicamente en: ",paste0(hitos,collapse = "; ")," Por favor completar la información requerida hasta el 2022-06-02. 
                             Atentamente, Gobierno Abierto Ecuador.")
        if(compromiso=="Co-diseño de la hoja de ruta para la implementación del Estándar EITI para mejorar la transparencia en las industrias extractivas en Ecuador (petróleo, gas y minería)"&&k==3){
          ####### ENVIO DE CORREO ELECTRONICO #####
          sender <- "gizcontrol001@gmail.com"
          recipients <- c("flor.gonzalez@giz.de")#c(tabla_correo[tabla_correo$entidad==unique(tmp_2$contraparte),"correo"],"secretariotecnico@eitiec.org")
          mailR::send.mail(from = sender,
                           to = recipients,
                           subject = "PAGA: Recordatorio para carga de información",
                           cc = c("cabb96.cb@gmail.com"),#gobiernoabierto@presidencia.gob.ec
                           
                           body = cuerpo_correo,
                           smtp = list(host.name = "smtp.gmail.com", port = 465,
                                       user.name = "gizcontrol001@gmail.com",
                                       passwd = "gizcontrol4752", ssl = TRUE),
                           authenticate = TRUE,
                           send = TRUE)
        }else{
          ######## ENVIO DE CORREO ELECTRONICO #####
          sender <- "gizcontrol001@gmail.com"
          recipients <- c("flor.gonzalez@giz.de")#c(tabla_correo[tabla_correo$entidad==unique(tmp_2$contraparte),"correo"])
          mailR::send.mail(from = sender,
                           to = recipients,
                           cc = c("cabb96.cb@gmail.com"),#gobiernoabierto@presidencia.gob.ec
                           
                           subject = "PAGA: Recordatorio para carga de información",
                           body = cuerpo_correo,
                           smtp = list(host.name = "smtp.gmail.com", port = 465,
                                       user.name = "gizcontrol001@gmail.com",
                                       passwd = "gizcontrol4752", ssl = TRUE),
                           authenticate = TRUE,
                           send = TRUE)
        }
      }
    }
  }
}

#### CORREOS RECORDATORIOS DE ALERTAS DE CARGA DE INFORMACIÓN GRUPO NUCLEO####
if(Sys.Date()>=lubridate::as_date("2022-05-23")&&Sys.Date()<=lubridate::as_date("2022-06-02")){
  grupo_nucleo<-data_fin%>%filter(is.na(fecha_registro_grupoNucleo))%>%as.data.frame()
  compro<- as.character(unique(grupo_nucleo[,c("compromiso")]))
  cuerpo_correo<- paste0("Estimado Grupo Nucleo, a la fecha del ",Sys.Date()," se identifica que no se ha procedido con la carga de información con repecto a los compromisos: ",paste0(compro,collapse = "; "),". Por favor completar la información requerida hasta el 2022-06-02.Atentamente, Gobierno Abierto Ecuador.")
  ######## ENVIO DE CORREO ELECTRONICO #####
  if (length(compro)>0) {
    sender <- "gizcontrol001@gmail.com"
    recipients <- c("flor.gonzalez@giz.de")#c(gobiernoabierto@presidencia.gob.ec)
    mailR::send.mail(from = sender,
                     to = recipients,
                     subject = "PAGA: Recordatorio para carga de información",
                     body = cuerpo_correo,
                     smtp = list(host.name = "smtp.gmail.com", port = 465,
                                 user.name = "gizcontrol001@gmail.com",
                                 passwd = "gizcontrol4752", ssl = TRUE),
                     authenticate = TRUE,
                     send = TRUE)
  }
}


#### CORREO RECORDATORIO GRUPO NUCLEO ####
if(Sys.Date()==lubridate::as_date("2022-05-30")){
  completos<- data_fin%>%filter(!is.na(fecha_registro_entidades)|!is.na(fecha_registro_contraparte)&!is.na(IdCompromisos))
  completos<- completos%>%select(compromiso,hito,entidad,contraparte)%>%as.data.frame()%>%unique()%>%arrange(compromiso,hito)
  if(length(completos)){
    cuerpo_correo<- paste0("Estimado Grupo Nucleo, a la fecha del ",Sys.Date(),", se adjunta el archivo CSV, que identifica a las Entidades y Contrapartes que han completado la información de los hitos de cada compromiso de forma total y parcial. Atentamente, Gobierno Abierto Ecuador.")
    # CREA ARCHIVO
    write.table(x = as.data.frame(completos),file = "/cloud/project/registros_completos.csv",sep = "|")
    # ENVIO CORREO
    sender <- "gizcontrol001@gmail.com"
    recipients <- c("flor.gonzalez@giz.de")#c(gobiernoabierto@presidencia.gob.ec)
    mailR::send.mail(from = sender,
                     to = recipients,
                     subject = "PAGA: Recordatorio para carga de información",
                     body = cuerpo_correo,
                     smtp = list(host.name = "smtp.gmail.com", port = 465,
                                 user.name = "gizcontrol001@gmail.com",
                                 passwd = "gizcontrol4752", ssl = TRUE),
                     authenticate = TRUE,
                     attach.files=c("/cloud/project/registros_completos.csv"),
                     send = TRUE)
    # REMUEVE ARCHIVO
    file.remove("/cloud/project/registros_completos.csv")
  }
}

rm(list=ls())





