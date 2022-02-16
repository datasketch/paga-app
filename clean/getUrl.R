#TOKEN
#eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6ImRhdmlkQGRhdGFza2V0Y2guY28iLCJmaXJzdG5hbWUiOm51bGwsImxhc3RuYW1lIjpudWxsLCJpZCI6MSwicm9sZXMiOiJ1c2VyIiwiaWF0IjoxNjM4NDU5NzI3fQ.O4NjFZowlheo-aVpEdYpB3Xvmrkm-olLdJh-y5O33Ww

#Entidades
#Sys.setenv(`xc-auth` = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6ImRhdmlkQGRhdGFza2V0Y2guY28iLCJmaXJzdG5hbWUiOm51bGwsImxhc3RuYW1lIjpudWxsLCJpZCI6MSwicm9sZXMiOiJ1c2VyIiwiaWF0IjoxNjM4NDU5NzI3fQ.O4NjFZowlheo-aVpEdYpB3Xvmrkm-olLdJh-y5O33Ww")
#https://datos.paga.datasketch.co/nc/registro_de_avances_gzwk/api/v1/entidades
library(httr)
key <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6ImRhdmlkQGRhdGFza2V0Y2guY28iLCJmaXJzdG5hbWUiOm51bGwsImxhc3RuYW1lIjpudWxsLCJpZCI6MSwicm9sZXMiOiJ1c2VyIiwiaWF0IjoxNjM4NDU5NzI3fQ.O4NjFZowlheo-aVpEdYpB3Xvmrkm-olLdJh-y5O33Ww"
#url <- "https://datos.paga.datasketch.co/nc/registro_de_avances_gzwk/api/v1/entidades"
url <- "https://datos.paga.datasketch.co/nc/registro_de_avances_gzwk/api/v1/grupo-nucleo"
a <- GET(url, add_headers(`xc-auth` = key))
http_status(a)

str(content(a))
a$request
a$all_headers
upload_file(url)
a$content
http_type(a)
#https://httr.r-lib.org/articles/api-packages.html

library(tidyverse)
info <- content(a) %>% bind_rows()
info <- info %>% unite("hito",c(`Hito segundo compromiso`, `Hito décimo compromiso`:`Hito tercer compromiso`), na.rm = TRUE)
info <- info %>% rename(c( "estado_grupoNucleo" = "Indicador 2",
                           "entidad_responsable_gn" = "Indicador 3 - entidad - grupo nucleo",
                           "contraparte_responsable_gn" = "Indicador 3 - contraparte - grupo nucleo",
                           "contraparte_grupoNucleo" = "Indicador 3 - contraparte",
                           "entidad_grupoNucleo" = "Indicador 3 - entidad", 
                           "resultados_grupoNucleo" = "Indicador 7",
                           "estrategias_grupoNucleo" = "Indicador 9",
                           "compromiso" = "Compromiso",
                           "fecha_registro_grupoNucleo" = "Fecha de registro"))
info <- info[,c(-1,-2,-3)]


url <- "https://datos.paga.datasketch.co/nc/registro_de_avances_gzwk/api/v1/grupo-nucleo-diccionario"
DIC <- GET(url, add_headers(`xc-auth` = key))
dic_c <- content(DIC)%>% bind_rows()

