library(tidyverse)

entidades <- read_csv("data/formulario-entidades-Grid view.csv")
entidades <- entidades %>% drop_na(`Entidad responsable de registrar el avance`)
entidades$`¿Cuál es el porcentaje de avance de implementación del hito?` <- as.numeric(gsub("%", "", entidades$`¿Cuál es el porcentaje de avance de implementación del hito?`))
entidades <- Filter(function(x) !all(is.na(x)), entidades)

contrapartes <- read_csv("data/formulario-contrapartes-Grid view.csv")
contrapartes <- contrapartes %>% drop_na(`Nombre del compromiso`)
contrapartes <- Filter(function(x) !all(is.na(x)), contrapartes)

compromisos <- read_csv("data/información-general-compromisos-Grid view.csv")
compromisos <- Filter(function(x) !all(is.na(x)), compromisos)


# grafico uno
# de la base entidades filtro de compromiso

comp1 <- entidades %>% filter(`Nombre del compromiso` %in% "Elaboración colaborativa de la política de datos abiertos y la guía para su implementación")
comp1$dummy <- "Si"#100 - as.numeric(comp1_hitos$`¿Cuál es el porcentaje de avance de implementación del hito?`)
comp1_hitos <- comp1 %>% select(dummy, `Nombre del hito`, avance = `¿Cuál es el porcentaje de avance de implementación del hito?`)
comp1_hitos_prep  <- comp1_hitos  %>%  select(`Nombre del hito`)
comp1_hitos_prep$avance <- 100 - comp1_hitos$avance 
comp1_hitos_prep$dummy <- "No"
comp1_viz <- bind_rows(comp1_hitos, comp1_hitos_prep)

library(hgchmagic)
hgch_bar_CatCatNum(comp1_viz, graph_type = "stacked", orientation = "hor", label_wrap = 150)


## grafico dos
comp1_estado <- entidades %>% filter(`Nombre del compromiso` %in% "Elaboración colaborativa de la política de datos abiertos y la guía para su implementación")
comp1_estado <- comp1_estado %>% select(estado = `¿Cuál es el estado de cumplimiento del hito?`, `Nombre del hito`, avance = `¿Cuál es el porcentaje de avance de implementación del hito?`)
hgch_bar_CatCatNum(comp1_estado, orientation = "hor", label_wrap = 150)


