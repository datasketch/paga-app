library(hgchmagic)
load("data/all_data.RData")
dd <- data_fin %>% drop_na(avance)
df_1 <- data.frame(id = "Cumplimiento",name = "Porcentaje total de cumplimiento del Plan",b = "aaa jas", porcentaje = mean(dd$avance, na.rm = TRUE))
df_2 <- data.frame(id = "Por ejecutar",name = "Porcentaje que falta para el cumplimiento total del Plan",b = "aaa jas",  porcentaje =  100 - mean(dd$avance, na.rm = TRUE))
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
htmlwidgets::saveWidget(h1, "porcentaje_cumplimiento.html", background = "transparent")


df_c <- dd %>% group_by(compromiso) %>%
  summarise(promedio = mean(avance, na.rm = TRUE))
library(dsvizprep)
df_c  <- dsvizprep::order_category(df_c, "compromiso", unique(dd$compromiso), label_wrap = 100)
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
#htmlwidgets::saveWidget(h2, "compromisos_cumplimiento.html", background = "transparent")

