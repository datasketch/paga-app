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




# Contrapartes ------------------------------------------------------------


contrapartes <- read_csv("data/formulario-contrapartes-Grid view.csv")
contrapartes <- contrapartes %>% drop_na(`Nombre del compromiso`)
contrapartes <- Filter(function(x) !all(is.na(x)), contrapartes)
dic_contra <- data_frame(label_original = names(contrapartes))
dic_all$id[dic_all$label_original == "¿Cuál es el estado de cumplimiento del hito?"] <- "estado_contraparte"
dic_all$id[dic_all$id == "fecha_registro"] <- "fecha_registro_contraparte"

#dic_all$id <- gsub("entidades")
dic_contra <- dic_contra %>% left_join(dic_all)
names(contrapartes) <- dic_contra %>% .$id
contrapartes <- contrapartes %>% distinct(compromiso, hito, .keep_all = TRUE)



data_all <- contrapartes %>% full_join(entidades)
data_all$avance[is.na(data_all$avance)] <- 0
#data_all <- data_all %>% distinct(compromiso, hito, .keep_all = TRUE)


# Compromisos -------------------------------------------------------------



compromisos <- read_csv("data/información-general-compromisos-Grid view.csv")[-1]
compromisos <- Filter(function(x) !all(is.na(x)), compromisos)
compromisos <- compromisos %>% separate_rows(Entidad, Contraparte, sep = "--")
compromisos <- compromisos %>% select(-`¿El compromiso es prometedor según el IRM?`)
# unique(compromisos$Entidad)
# unique(compromisos$Contraparte)
dic_com <- data_frame(label_original = names(compromisos))
dic_com <- dic_com %>% left_join(dic_all)
names(compromisos) <- dic_com %>% .$id
#compromisos <- compromisos %>% distinct(compromiso, hito, .keep_all = TRUE) %>% select(-entidad, -contraparte)
data_all <- compromisos %>% left_join(data_all) 
#data_all <- compromisos %>% full_join(data_all) 
# aa <- compromisos %>% filter(hito %in% "Hito 1: Diagnóstico de la situación actual de la plataforma de datos abiertos existente.")
# bb <- data_all %>% filter(hito %in% "Hito 1: Diagnóstico de la situación actual de la plataforma de datos abiertos existente.")

data_all <- data_all %>% filter(contraparte != "Co-diseño de la hoja de ruta para la implementación del Estándar EITI para mejorar la transparencia en las industrias extractivas en Ecuador (petróleo, gas y minería)")
#data_all <- data_all %>% drop_na(fecha_inicio)
#data_all$avance[is.na(data_all$avance)] <- 0
#write_rds(data_all, "all_data.rds")
#write_csv(data_all, "data_paga.csv", na = "")
# write_rds(dic_all, "all_dic.rds")
# 


library(hgchmagic)
dd <- compromisos %>% full_join(entidades)
dd$avance[is.na(dd$avance)] <- 0
df_1 <- data.frame(id = "Cumplimiento",name = "Porcentaje total de cumplimiento del Plan",b = "aaa jas", porcentaje = mean(dd$avance, na.rm = TRUE))
df_2 <- data.frame(id = "No cumplimiento",name = "Porcentaje que falta para el cumplimiento total del Plan",b = "aaa jas",  porcentaje =  100 - mean(dd$avance, na.rm = TRUE))
df <- df_1 %>% bind_rows(df_2)
df$name <- as.character(df$name)
df$b <- as.character(df$b)
df <- df %>% select(id, b, porcentaje, name)
df$porcentaje <- round(df$porcentaje, 2)
h1 <- hgch_bar_CatCatNum(df,
                         reversedYaxis = TRUE,
                         orientation = "hor",
                         graph_type = "stacked",
                         y_max = 100,
                         suffix = "%",
                         hor_title = " ",
                         ver_title = " ",
                         dataLabels_show = TRUE,
                         dataLabels_template = "{series.name}: {y}% <br/>",
                         grid_y_enabled = FALSE,
                         grid_x_enabled = FALSE,
                         #order = c("Cumplimiento", "No cumplimiento"),
                         #order_legend = c("Cumplimiento", "No cumplimiento"),
                         palette_colors = c("#fdd60e", "#ff7f00"),
                         tooltip = "{name}: {porcentaje}%",
                         background_color = "transparent",
                         dataLabels_text_outline = FALSE,
                         dataLabels_size = 15,
                         legend_show = FALSE
                         )%>%
hc_chart(height = 200)
h1
# htmlwidgets::saveWidget(h1, "porcentaje_cumplimiento.html", background = "transparent")
# 
# 
df_c <- dd %>% group_by(compromiso) %>%
  summarise(promedio = mean(avance, na.rm = TRUE))
library(dsvizprep)
df_c  <- dsvizprep::order_category(df_c, "compromiso", unique(compromisos$compromiso), label_wrap = 100)
df_c$name <- paste0("Compromiso ", 1:nrow(df_c))
df_c <- df_c %>% select(name, promedio, compromiso)
h2 <- hgch_bar_CatNum(df_c,
                label_wrap = 100,
                #orientation = "hor",
                order = unique(df_c$name),
                suffix = "%",
                hor_title = " ",
                ver_title = " ",
                dataLabels_show = TRUE,
                format_sample_num = "1,234.",
                palette_colors = "#0076b7",
                tooltip = "{compromiso} <br/>Avance: {promedio}%",
                background_color = "transparent")
h2
htmlwidgets::saveWidget(h2, "compromisos_cumplimiento.html", background = "transparent")

# # data_all <- compromisos %>% full_join(data_all)
# # 
# # data_all <- data_all %>% filter(contraparte != "Co-diseño de la hoja de ruta para la implementación del Estándar EITI para mejorar la transparencia en las industrias extractivas en Ecuador (petróleo, gas y minería)")
# 
# 
# 
# 
# # write_rds(data_all, "all_data.rds")
# # write_rds(dic_all, "all_dic.rds")
# 
# # grafico uno
# # de la base entidades filtro de compromiso
# 
# comp1 <- entidades %>% filter(`Nombre del compromiso` %in% "Elaboración colaborativa de la política de datos abiertos y la guía para su implementación")
# comp1$dummy <- "Si"#100 - as.numeric(comp1_hitos$`¿Cuál es el porcentaje de avance de implementación del hito?`)
# comp1_hitos <- comp1 %>% select(dummy, `Nombre del hito`, avance = `¿Cuál es el porcentaje de avance de implementación del hito?`)
# comp1_hitos_prep  <- comp1_hitos  %>%  select(`Nombre del hito`)
# comp1_hitos_prep$avance <- 100 - comp1_hitos$avance 
# comp1_hitos_prep$dummy <- "No"
# comp1_viz <- bind_rows(comp1_hitos, comp1_hitos_prep)
# 
# library(hgchmagic)
# hgch_bar_CatCatNum(comp1_viz, graph_type = "stacked", orientation = "hor", label_wrap = 150)
# 
# 
# ## grafico dos
# comp1_estado <- entidades %>% filter(`Nombre del compromiso` %in% "Elaboración colaborativa de la política de datos abiertos y la guía para su implementación")
# comp1_estado <- comp1_estado %>% select(estado = `¿Cuál es el estado de cumplimiento del hito?`, `Nombre del hito`, avance = `¿Cuál es el porcentaje de avance de implementación del hito?`)
# hgch_bar_CatCatNum(comp1_estado, orientation = "hor", label_wrap = 150)


