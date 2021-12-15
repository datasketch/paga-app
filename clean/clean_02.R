library(tidyverse)

dic_all <- read_csv("data/dic-Grid view.csv")[,-4]


# Entidades ---------------------------------------------------------------


entidades <- read_csv("data/formulario-entidades-Grid view.csv")
entidades <- entidades %>% drop_na(`Entidad responsable de registrar el avance`)
entidades <- Filter(function(x) !all(is.na(x)), entidades)
dic_entidades <- data_frame(label_original = names(entidades))
dic_entidades <- dic_entidades %>% left_join(dic_all)
names(entidades) <- dic_entidades %>% .$id
entidades$avance <- as.numeric(gsub("%", "", entidades$avance))
entidades <- entidades %>% distinct(compromiso, hito, .keep_all = TRUE)



# Compromisos -------------------------------------------------------------



compromisos <- read_csv("data/información-general-compromisos-Grid view.csv")[-1]
compromisos <- Filter(function(x) !all(is.na(x)), compromisos)
compromisos <- compromisos %>% separate_rows(Entidad, Contraparte, sep = "--")
compromisos <- compromisos %>% select(-`¿El compromiso es prometedor según el IRM?`) # unique(compromisos$Entidad)
# unique(compromisos$Contraparte)
dic_com <- data_frame(label_original = names(compromisos))
dic_com <- dic_com %>% left_join(dic_all)
names(compromisos) <- dic_com %>% .$id

unique(compromisos$compromiso)
all_id_eit <- compromisos %>%
  filter(compromiso == "Co-diseño de la hoja de ruta para la implementación del Estándar EITI para mejorar la transparencia en las industrias extractivas en Ecuador (petróleo, gas y minería)")

compromisos2 <- compromisos %>%
  filter(compromiso != "Co-diseño de la hoja de ruta para la implementación del Estándar EITI para mejorar la transparencia en las industrias extractivas en Ecuador (petróleo, gas y minería)")


id_eit <- data.frame(hito = unique(all_id_eit$hito))
id_eit$compromiso <- unique(all_id_eit$compromiso)
id_eit$tematica <- unique(all_id_eit$tematica)
id_eit$entidad <- unique(all_id_eit$entidad)
id_eit$vinculación_ods <- unique(all_id_eit$vinculación_ods)
id_eit$valores_ogp <- unique(all_id_eit$valores_ogp)
compromisos2 <- compromisos2 %>% bind_rows(id_eit)
order_data <-  data.frame(compromiso = unique(compromisos$compromiso))
compromisos_toJoin <- order_data %>% left_join(compromisos2)

data_first <- compromisos_toJoin %>% left_join(entidades)

end <- entidades %>%
  filter(compromiso == "Co-diseño de la hoja de ruta para la implementación del Estándar EITI para mejorar la transparencia en las industrias extractivas en Ecuador (petróleo, gas y minería)")

