#TOKEN
#eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6ImRhdmlkQGRhdGFza2V0Y2guY28iLCJmaXJzdG5hbWUiOm51bGwsImxhc3RuYW1lIjpudWxsLCJpZCI6MSwicm9sZXMiOiJ1c2VyIiwiaWF0IjoxNjM4NDU5NzI3fQ.O4NjFZowlheo-aVpEdYpB3Xvmrkm-olLdJh-y5O33Ww

#Entidades
#Sys.setenv(`xc-auth` = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6ImRhdmlkQGRhdGFza2V0Y2guY28iLCJmaXJzdG5hbWUiOm51bGwsImxhc3RuYW1lIjpudWxsLCJpZCI6MSwicm9sZXMiOiJ1c2VyIiwiaWF0IjoxNjM4NDU5NzI3fQ.O4NjFZowlheo-aVpEdYpB3Xvmrkm-olLdJh-y5O33Ww")
#https://datos.paga.datasketch.co/nc/registro_de_avances_gzwk/api/v1/entidades
library(httr)
key <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6ImRhdmlkQGRhdGFza2V0Y2guY28iLCJmaXJzdG5hbWUiOm51bGwsImxhc3RuYW1lIjpudWxsLCJpZCI6MSwicm9sZXMiOiJ1c2VyIiwiaWF0IjoxNjM4NDU5NzI3fQ.O4NjFZowlheo-aVpEdYpB3Xvmrkm-olLdJh-y5O33Ww"
url <- "https://datos.paga.datasketch.co/nc/registro_de_avances_gzwk/api/v1/entidades"
a <- GET(url, add_headers(`xc-auth` = key))
http_status(a)

str(content(a))
a$request
a$all_headers
upload_file(url)
