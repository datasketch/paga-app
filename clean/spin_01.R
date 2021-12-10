library(hgchmagic)

compromisos <- read_rds("all_data.rds")
compromisos <- compromisos %>% drop_na(entidad)
compromisos$avance[is.na(compromisos$avance)] <- 0
df_1 <- data.frame(name = "Porcentaje total del cumplimiento del Plan",b = "aaa jas", porcentaje = mean(compromisos$avance, na.rm = TRUE))
df_2 <- data.frame(name = "Porcentaje de NO cumplimiento del Plan",b = "aaa jas",  porcentaje =  100 - mean(compromisos$avance, na.rm = TRUE))
df <- df_1 %>% bind_rows(df_2)
df$name <- as.character(df$name)
df$b <- as.character(df$b)
hgch_bar_CatCatNum(df, 
                   orientation = "hor",
                   graph_type = "stacked",
                   y_max = 100, 
                   hor_title = " ",
                   ver_title = " ",
                   grid_y_enabled = FALSE,
                   grid_x_enabled = FALSE,
                   palette_colors = c("#ff7f00", "#fdd60e"),
                   legend_show = FALSE)


df_c <- compromisos %>% group_by(compromiso) %>% summarise(promedio = mean(avance, na.rm = TRUE))
