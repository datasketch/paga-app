webshot::install_phantomjs()
library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(shinydisconnect)
library(shinybusy)
library(dsmodules)
library(hgchmagic)
library(DT)


style <- "
@import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Sans&display=swap');

.panel,.panel-header,.panel-footer  {
  background: #f2f2f2;
}

.layout-panels {
    background-color: #ffffff;
}


@media screen and (max-width: 768px) { 
.layout-container{
  min-height: 600px !important;} 
}

.orientation-notice {
 display: none !important;
}

.panel {
 font-size: 0.85rem;
 height: 100%; /*90%;*/
}

.panel-body {
 padding-bottom: 2rem;
}


.shinybusy, .shinybusy-ready {
 top: 30% !important;
 right: 50% !important;
}

.text-malibu {
  color: #ff7f00;
}

.top-malibu {
  border-top: 1.5px solid #ff7f00;
}

.text-chardonnay {
  color: #ff7f00;
}

.top-chardonnay {
    border-top: 1.5px solid #ff7f00;
}

.btn-default {
 font-family: IBM Plex Sans;
 background: #f2f2f2 !important;
 border: 1px solid #6a6767;
 color: #6a6767;
 float: left;
 font-size: 0.82rem;
 font-weight: 400;
 text-transform: initial;
 letter-spacing: 0;
 margin-bottom: 3px;
}

.basic_active {
  color: #ffffff !important;
  background-color: #ff7f00 !important;
  border-color: #ff7f00 !important;
}


.needed {
 margin-bottom: 9px;
 max-width: 95%;
 text-align: left;
 white-space: normal;
 word-wrap: break-word;
}

.dropdown-action-trigger {
 background-color: #0076b7 !important;
}

.style_section {
 color: #ff7f00;
 font-size: 0.85rem !important;
 margin-bottom: 0rem;
 padding-top: 0rem;
}


#relacion {
 margin-bottom: 9%;
}

#variables_principales {
 margin-top: -3%;
 margin-bottom: 7%;
}


#ss-connect-dialog a::before {
background: #ff7f00 !important;
}


.buttons-group {
  display: inline-flex !important;
}

.buttons-group .button-style {
    width: 40px !important;
}

.buttons-group .button-style.active-btn {
  background-color: #0076b7 !important;
  width: 40px;
}


.buttons-group .button-style.active-btn:hover {
  background-color: #0076b7 !important;
}

.button-checkmark {
 display: none;
}

label.control-label {
    margin-bottom: 10px;
    margin-right: 20px;
}

.form-group {
    display: inline-flex !important;
    flex-direction: row !important;
    align-items: center !important;
}

.form-control.selectize-control {
  width: 600px !important;
}

.shiny-input-container:not(.shiny-input-container-inline) {
  max-width: 600px !important;
}

.selectize-input.full {
    background-color: #f2f2f2;
}

.radio {
  margin-bottom: 15px;
}
  
#viz_icons {
  margin-left: -33px;
}

.bodyModal {
padding: 5%;
}

.title-modal {
font-weight: 500;
}

.panel-footer {
    padding: 0rem 2rem !important;
}

.dropdown-action-container, .shiny-bound-input {
 width: 250px !important;
}

"

indicadores_data <- read_rds("data/all_data.rds")
indicadores_data$hito_id <- str_extract(indicadores_data$hito, "Hito [0-9]")
# indicadores_data$hito <- gsub("\\.", "",indicadores_data$hito)
# indicadores_data$hito <- paste0(indicadores_data$hito, ".")
# 
# indicadores_data$hito_id <-gsub("\\s*\\:[^\\)]+\\.","", indicadores_data$hito)
indicadores_data$cmp_esperado <- ifelse(lubridate::ymd(indicadores_data$fecha_finalizacion) < lubridate::ymd("2021-10-22"), "si", "no")
indicadores_dic <- read_rds("data/all_dic.rds")

ui <- panelsPage(
  shinypanels::modal(id = 'modal_extra_info', title = NULL, uiOutput("message_modal")),
  
  disconnectMessage(
    text = "Tu sesión ha finalizado, por favor haz click aquí para recargar vista",
    refresh = "RECARGAR",
    background = "#ffffff",
    colour = "#6a6767",
    size = 14,
    overlayColour = "#2a2e30",
    overlayOpacity = 0.85,
    refreshColour = "#ffffff",
    css = "padding: 4.8em 3.5em !important; box-shadow: 0 1px 10px 0 rgba(0, 0, 0, 0.1) !important;"
  ),
  tags$head(tags$head(
    includeScript("js/siscrimel.js")
  )),
  styles = style,
  panel(title = "Indicadores",
        id = "azul",
        width = 350,
        body = div(
          uiOutput("basicos")
        ),
        footer = tags$i("Fecha de actualización: Octubre 22 del 2021")
  ),
  panel(title = "Visualización",
        id = "naranja",
        header_right = uiOutput("descargas"),
        can_collapse = FALSE,
        color = "chardonnay", #div(add_busy_spinner(spin = "fading-circle"),uiOutput("final_viz"))
        body = div(
          uiOutput("commitment"),
          uiOutput("final_viz")
          #verbatimTextOutput("aver")
        ),# 
        footer =  div(class = "panel-header",
                      uiOutput("viz_icons"), 
                      tags$a(
                        href="https://www.datasketch.co", target="blank",
                        img(src='ds_logo.png', align = "right", width = 150, height = 110)))
  )
)

server <- function(input, output, session) {
  
  
  
  indicators_list <- reactive({
    
    basicos <- data.frame(id = c("avance", "estado", "cumplimiento", "actividades", "participantes", "sectores", "resultados", "relacion_internacional"),
                          indicadores = c("1. Porcentaje de avance de cada hito	Entidades responsables",
                                          "2. Estado actual de implementación del hito",
                                          "3. Cumplimiento de responsabilidades de la entidad responsable y la contraparte durante el cumplimiento del hito.",
                                          "4. Número de actividades realizadas para la inclusión de actores en el proceso",
                                          "5. Número de participantes en total en cada hito de los compromisos.",
                                          "6. Identificación de sectores de pertenencia de participantes en cada hito de los compromisos.",
                                          "7. Percepción de resultados de implementación del hito",
                                          "8. Cumplimiento con iniciativas internacionales.") 
                          
    )
    
    l <- purrr::map(1:nrow(basicos), function(z){
      actionButton(inputId = basicos[z,]$id, label = basicos[z,]$indicadores, class = "needed")
    })
    l
  })
  
  
  indicator_choose <- reactive({
    last_btn <- input$last_click
    if (is.null(last_btn)) last_btn <- "avance"
    last_btn
  })
  
  observe({
    if(is.null(indicators_list())) return()
    l <- indicators_list()
    last_btn <- indicator_choose()
    button_id <- which(c("avance", "estado", "cumplimiento", "actividades", "participantes", "sectores", "resultados", "relacion_internacional") %in% last_btn)
    l[[button_id]] <- gsub("needed", "needed basic_active", l[[button_id]])
    l[[button_id]] <- HTML(paste0(paste(l[[button_id]], collapse = '')))
    if (indicator_choose() == "cumplimiento")
      l[[3]] <- div(l[[3]],
                    radioButtons("sub_cumplimiento",
                                 " ", 
                                 setNames(c("contraparte_responsable", "entidad_responsable", "contraparte_nucleo", "entidad_nucleo"), 
                                          c("3.1 ¿La contraparte ha respondido con sus responsabilidades con la entidad Responsable durante el compromiso?",
                                            "3.2 ¿La entidad responsable ha respondido con sus responsabilidades con la contraparte?",
                                            "3.3 ¿La contraparte ha respondido con sus responsabilidades con el Grupo Núcleo durante el compromiso?",
                                            "3.4 ¿La entidad responsable ha respondido con sus responsabilidades con el Grupo Núcleo durante el compromiso?"
                                          )))
      )
    output$basicos <- renderUI({
      l
    })
  })
  
  
  possible_viz <- reactive({
    p <- indicator_choose()
    if (is.null(p)) return()
    v <- c("bar", "table")
    if (p %in% c("sectores", "relacion_internacional")) {
      v <- c("treemap", "table")
    }
    
    v
  })
  
  
  actual_but <- reactiveValues(active = NULL)
  
  observe({
    if (is.null(possible_viz())) return()
    viz_rec <- possible_viz()
    if (is.null(input$viz_selection)) return()
    
    if (input$viz_selection %in% viz_rec) {
      actual_but$active <- input$viz_selection
    } else {
      actual_but$active <- viz_rec[1]
    }
  })
  
  
  output$viz_icons <- renderUI({
    req(possible_viz())
    suppressWarnings(
      buttonImageInput('viz_selection',
                       " ",
                       images = possible_viz(),
                       path = 'icons/',
                       active = actual_but$active)
    )
  })
  
  
  id_viz <- reactive({
    id_viz <- actual_but$active
    if (is.null(id_viz)) id_viz <- "bar"
    id_viz
  })
  
  last_indicator <- reactive({
    l_i <- indicator_choose()
    if (l_i == "cumplimiento") {
      if (is.null(input$sub_cumplimiento)) return()
      l_i <- input$sub_cumplimiento
    }
    l_i
  })
  
  data_intial <- reactive({
    req(indicadores_data)
    req(last_indicator())
    df <- indicadores_data
    
    
    if (last_indicator() == "estado") {
      df <- indicadores_data[!(is.na(indicadores_data$estado) & is.na(indicadores_data$estado_contraparte)),]
    }
    if (last_indicator() == "resultados") {
      df <- indicadores_data %>% drop_na(resultados)
    }
    if (last_indicator() %in% "contraparte_responsable") {
      df <- indicadores_data %>% drop_na(contraparte_responsable)
    }
    if (last_indicator() %in% "entidad_responsable") {
      df <- indicadores_data %>% drop_na(entidad_responsable)
    }
    if (!(last_indicator() %in% c("estado", "resultados"))) {
      df <- indicadores_data %>%
        drop_na(estado)
    }
    
    
    df
    
  })
  
  output$commitment <- renderUI({
    req(data_intial())
    selectizeInput("compromiso_id", "COMPROMISO", unique(data_intial()$compromiso))
  })
  
  
  
  data_filter <- reactive({
    req(data_intial())
    if (is.null(input$compromiso_id)) return()
    
    df <- data_intial() %>%
      filter(compromiso %in% input$compromiso_id)
    
    
    df
  })
  
  
  
  
  data_select <- reactive({
    req(data_filter())
    df <- data_filter()
    var_s <- last_indicator()
    
    if (last_indicator() %in% c( "contraparte_nucleo", "entidad_nucleo")) return()
    # all_hitos <- data.frame()
    # print("aaaaa")
    # print(all_hitos)
    if (last_indicator() %in% "estado") {
      df <- df[,c(var_s, "hito_id", "estado_contraparte", "cmp_esperado", "hito")]
      df <- df %>% plyr::rename(c("estado" = "Entidad", "estado_contraparte" = "Contraparte"))
      df <- df %>% gather("tipo", "estado", c("Entidad", "Contraparte"))
      df <- df %>% bind_rows(data.frame(tipo = "Grupo Núcleo", hito_id = df$hito_id[1], estado = NA))
      df$estadoxx <- plyr::revalue(df$estado, c("Completado" = 5, "Ejecución" = 4, "Definición" = 3, "Planificación" = 2))
      df <- df %>% select(tipo, hito_id, estadoxx, estado, cmp_esperado, hito)
    } else if (last_indicator() %in% "avance") {
      df <- df[,c(var_s, "hito_id", "fecha_inicio", "fecha_finalizacion", "cmp_esperado", "hito")]
      df$`Porcentaje de no cumplimiento` <- (100 - df$avance)
      df <- df %>% plyr::rename(c("avance" = "Porcentaje de cumplimiento"))
      df <- df %>% gather("avance", "porcentaje", c("Porcentaje de cumplimiento", "Porcentaje de no cumplimiento"))
      df <- df %>% select(avance, hito_id, porcentaje, everything()) %>% filter(porcentaje>0)
      
    } else if (last_indicator() %in% "sectores") {
      df <- df[,c(var_s, "hito_id", "cmp_esperado", "hito")]
      df <- df %>% separate_rows(sectores, convert = TRUE, sep = ",")
    } else if (last_indicator() %in% c("contraparte_responsable", "entidad_responsable")) {
      df <- df[,c(var_s, "hito_id", "cmp_esperado", "hito")]
      df$tipo <- ifelse(last_indicator() == "contraparte_responsable", "Entidad", "Contraparte")
      df$value <- plyr::revalue(df[[last_indicator()]], c("Sí" = 4, "No" = 2))
      df <- df %>% select(tipo, hito_id, value, everything())
      df <- df %>% bind_rows(data.frame(tipo = "Grupo Núcleo", hito_id = df$hito_id[1], value = NA))
      
    } else if (last_indicator() %in%   "resultados") {
      df <- df[,c(var_s, "hito_id", "cmp_esperado", "hito")]
      df$value <- plyr::revalue(df$resultados, c("Se mantuvo igual" = 1, "Mejoró un poco" = 3, "Mejoró sustancialmente" = 5))
      df$tipo <- "Contraparte"
      df <- df %>% select(tipo, hito_id, value, everything())
      df <- df %>% bind_rows(data.frame(tipo = "Grupo Núcleo", hito_id = df$hito_id[1], value = NA))
    } else if (last_indicator() %in% "relacion_internacional") {
      df <- df[,c("relacion_internacional_descripcion", "hito_id", var_s, "cmp_esperado", "hito", "relacion_internacional_justificacion")]
      df <- df %>% separate_rows(relacion_internacional_descripcion, sep = ",")
      df$relacion_internacional_descripcion[is.na(df$relacion_internacional_descripcion)] <- "NA"
    } else {
      df <- df[,c("hito_id", var_s, "cmp_esperado", "hito")]
    }
    df
    
  })
  
  
  opts_plot <- reactive({
    
    fjs <- NULL
    order_s <- NULL
    yMax <- NULL
    yEnabled <- TRUE
    id_button <- last_indicator()
    colors <- c("#ff4e17", "#0076b7", "#78dda0", "#ff7f00", "#fdd60e")
    cursor <- NULL
    myFunc <- NULL
    
    if (id_button == "avance") {
      tx <- "Fecha de inicio: {fecha_inicio} <br/> Fecha de finalización: {fecha_finalizacion} <br/>{hito}<br/><b>{avance}: {porcentaje}%</b>"
      yMax <- 100
      order_s <- c("Porcentaje de cumplimiento", "Porcentaje de no cumplimiento")
      colors <- c("#0076b7", "#293662")
    } else if (id_button == "estado") {
      tx <- "<b>{tipo}</b> <br/>{hito}<br/> <b>Estado: {estado}</b>"
      fjs <- JS("function () {var arreglo = ['','','Planificación', 'Definición', 'Ejecución', 'Completado'];return arreglo[this.value];}")
      order_s <- c("Entidad", "Contraparte","Grupo Núcleo")
      yMax <- 5
      colors <- c("#0076b7","#ff4e17", "#78dda0")
    } else if (id_button == "contraparte_responsable") {
      tx <- "{hito}<br/> <b>La contraparte ha respondido con sus responsabilidades con la entidad Responsable durante el compromiso: {contraparte_responsable} </b> <br/><br/>Da click para más información"
      order_s <- c("Contraparte", "Grupo Núcleo")
      colors <- c("#0076b7", "#78dda0")
      cursor <- "pointer"
      yMax <- 4
      myFunc <- JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.category, cat:this.name, timestamp: new Date().getTime()});}")
      fjs <- JS("function () {var arreglo = ['','No', '' , 'Sí'];return arreglo[this.value];}")
    } else if (id_button == "entidad_responsable") {
      tx <- "{hito}<br/> <b>La entidad responsable ha responido con sus responsabilidades con la contraparte: {entidad_responsable} </b>  <br/><br/>Da click para más información"
      order_s <- c("Entidad", "Grupo Núcleo")
      colors <- c("#ff4e17", "#78dda0")
      cursor <- "pointer"
      yMax <- 4
      myFunc <- JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.category, cat:this.name, timestamp: new Date().getTime()});}")
      fjs <- JS("function () {var arreglo = ['','No', '', 'Sí'];return arreglo[this.value];}")
    } else if (id_button == "actividades") {
      tx <- "{hito} <br/> <b>Número de actividades {actividades}</b> <br/><br/>Da click para más información"
      colors  <- "#293662"
      yEnabled <- FALSE
      cursor <- "pointer"
      myFunc <- JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.name, timestamp: new Date().getTime()});}")
    } else if (id_button == "participantes") {
      tx <- "{hito} <br/> <b>Número de participantes {participantes}</b>"
      colors  <- "#293662"
      yEnabled <- FALSE
    } else if (id_button == "sectores") {
      tx <- "{hito} <br/> <b>Sector: {sectores}</b>" 
    } else if (id_button == "resultados") {
      tx <- "{hito} <br/> <b>Percepción de resultados: {resultados}</b>  <br/><br/>Da click para más información" 
      fjs <- JS("function () {var arreglo = ['','Se mantuvo igual', '','Mejoró un poco','' ,'Mejoró sustancialmente'];return arreglo[this.value];}")
      order_s <- c("Contraparte", "Grupo Núcleo")
      colors <- c("#0076b7", "#78dda0")
      cursor <- "pointer"
      myFunc <- JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.category, cat:this.name, timestamp: new Date().getTime()});}")
      yMax <- 5
    } else if (id_button == "relacion_internacional") {
      #tx <- "{hito} <br/> <b>Cumplimiento con iniciativas internacionales: {relacion_internacional_descripcion}</b> <br/> <br/> <b>Punto concreto de la relación con el hito</b> {relacion_internacional_justificacion}"
       tx <- "{hito} <br/> <b> Iniciativa internacional ligada con Gobierno Abierto: </b>{relacion_internacional_descripcion} <br/>
       <b>Punto concreto de la relación con el hito</b> {relacion_internacional_justificacion} <br/>
       <b>Cumplimiento con iniciativas internacionales: {relacion_internacional}</b>"
      #colors <- c("#293662", "#78dda0")
    } else {
      tx <- return()
    }
    
    list(
      tooltip = tx,
      formatterJS = fjs,
      orderLegend = order_s,
      yMax = yMax,
      colors = colors,
      yEnabled = yEnabled,
      cursor = cursor,
      clickFc = myFunc 
    )
  })
  
  
  hgch_viz <- reactive({
    if (is.null(data_select())) return()
    if (nrow(data_select()) == 0) return()
    print(data_select())
    if (actual_but$active == "table") return()
    format_x_js <- NULL
    axisColor <- data_select() %>% filter(cmp_esperado %in% "si")
    if (nrow(axisColor) != 0) {
    axisColor <- unique(axisColor$hito_id)
    hito_high <- paste0("\'",axisColor, "\'", collapse = ",")
    format_x_js <- JS(paste0("function () { var arr = [", hito_high,"]; if (arr.includes(this.value)) {return '<text style=\"color: #0076b7 !important;fill: #0076b7 !important;\">' + this.value + '</text>'; } else { return this.value}; }"))
    }
    
    viz <- "CatCatNum"
    showLabels <- FALSE
    
    if (last_indicator() %in% c("actividades", "participantes")) {
      viz <- "CatNum"
      showLabels <- TRUE
    }
    if (last_indicator() %in% c("relacion_internacional", "sectores")) {
      viz <- "CatCat"
    }
    
    graph_type <- "grouped"
    if (last_indicator() == "avance") {
      graph_type = "stacked"
      yEnabled <- TRUE
      showLabels <- TRUE
    }
    
    type_viz <- actual_but$active
    viz_sel <- paste0("hgch_", type_viz, "_", viz)
    print(viz_sel)
    do.call(viz_sel, list(data = data_select(),
                          graph_type = graph_type,
                          orientation = "hor", 
                          hor_title = " ",
                          ver_title = " ",
                          label_wrap = 100,
                          background_color = "transparent",
                          y_max = opts_plot()$yMax,
                          order_legend = opts_plot()$orderLegend,
                          y_axis_align = "right",
                          formatter_x_js = format_x_js,
                          formatter_js = opts_plot()$formatterJS,
                          dataLabels_show = showLabels,
                          label_wrap_legend = 150,
                          cursor = opts_plot()$cursor,
                          # drop_na_legend = TRUE,
                          # drop_na = TRUE,
                          grid_y_enabled = opts_plot()$yEnabled,
                          tooltip = opts_plot()$tooltip,
                          format_sample_num = "1,234.",
                          palette_colors = opts_plot()$colors,
                          clickFunction = opts_plot()$clickFc))
  })
  
  
  output$hgch_viz <- renderHighchart({
    hgch_viz()
  })
  
  
  
  
  textModal <- reactive({
    # if (last_indicator() != "actividades") {
    #   df <- df %>% filter(tipo %in% input$hcClicked$cat, hito %in% input$hcClicked$id)
    # }
    req(last_indicator())
    if (last_indicator() %in% c("avance", "estado",  "participantes", "sectores", "relacion_internacional")) return()
    if (is.null(input$hcClicked)) return()
    req(data_filter())
    df <- data_filter()
    
    
    hito_select <- gsub("<br/>", " ", input$hcClicked$id)
    hito_select <- gsub("co- creación", "co-creación", hito_select)
    df <- df %>% filter(hito_id %in% hito_select)
    
    
    if (last_indicator() == "actividades") {
      tx <- div(class = "bodyModal",
                HTML(
                  paste0("<p class = 'title-modal'>",
                         indicadores_dic$label_original[indicadores_dic$id == "actividades_descripcion"],
                         "</p><br/> <h3>Contraparte</h3><br/> <p class = 'description-modal'>",
                         df$actividades_descripcion, "</p>"
                  )
                )
      ) 
    }
    
    if (last_indicator() == "resultados") {
      tx <- div(class = "bodyModal",
                HTML(
                  paste0("<h3>Contraparte</h3><br/><p class = 'title-modal'>",
                         indicadores_dic$label_original[indicadores_dic$id == "realidad_inicial"],":</p><p class = 'description-modal'>",
                         df$realidad_inicial,"</p><br/><p class = 'title-modal'>",
                         indicadores_dic$label_original[indicadores_dic$id == "realidad_descripcion"],":</p>",
                         df$realidad_descripcion,"</p>"
                  )
                )
      )
    }
    
    if (last_indicator() %in% c("contraparte_responsable")) {
      tx <- div(class = "bodyModal",
                "Sin información de la justificación de cumplimiento de responsabilidades"
      )
    }
    
    if (last_indicator() %in% c("entidad_responsable")) {
      tx <- div(class = "bodyModal",
                HTML(
                  paste0("<p class = 'title-modal'>",
                         indicadores_dic$label_original[indicadores_dic$id == "entidad_responsable_justificacion"],
                         "</p><br/> <h3>Entidad</h3><br/> <p class = 'description-modal'>",
                         df$entidad_responsable_justificacion, "</p>"
                  )
                )
      ) 
    }
    
    tx
    
    
  })
  
  output$message_modal<- renderUI({
    textModal()
  })
  
  observeEvent(input$hcClicked, {
    shinypanels::showModal("modal_extra_info")
  })
  
  # output$aver <- renderPrint({
  #   list(
  #     textModal(),
  #     data_filter()$hito
  #   )
  # })
  
  
  output$table_view <- renderDataTable({
    req(data_filter())
    if (actual_but$active != "table") return()
    df <- data_filter()
    # df_dic <- data.frame(id = names(df))
    # df_dic <- df_dic %>% left_join(indicadores_dic)
    # names(df) <- df_dic$label
    DT::datatable(df,
                  rownames = F,
                  options = list(
                    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                    lengthChange = F,
                    pageLength = 15,
                    scrollX = T,
                    scrollY = T,
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#0a4a83', 'color': '#fff'});",
                      "}")
                  )) %>% 
      DT::formatStyle( 0 , target= 'row',color = '#0A446B', fontSize ='13px', lineHeight='15px')
  })
  
  
  
  output$final_viz <- renderUI({
    if (is.null(id_viz())) return()
    if (is.null(data_select())) return("No hay datos disponibles")
    if (id_viz() == "table") {
      v <- dataTableOutput("table_view", width = 900)
    } else {
      v <- highchartOutput("hgch_viz", height = 400)
    }
    
    if (id_viz() == "bar") {
      div (
        HTML("<div style=background:#cccccc;max-width:230px;padding:2px;margin-left:2%;>Hitos en desarrollo <span style=color:#0076b7;margin-left:3%;>Hitos finalizados</span></div>"),
        v
      )
    } else {
      v
    }

  })
  
  output$descargas <- renderUI({
    if (is.null(actual_but$active)) return()
    div (style = "display: grid;grid-template-columns: 1fr 1fr;grid-gap: 20px;",
         downloadImageUI("download_viz", dropdownLabel = "Descargar visualización", formats = c("jpeg", "pdf", "png", "html"), display = "dropdown"),
         downloadTableUI("dropdown_table", dropdownLabel = "Descargar Datos  ", formats = c("csv", "xlsx", "json"), display = "dropdown")
    )
  })
  
  downloadTableServer("dropdown_table", element = reactive(data_filter()), formats = c("csv", "xlsx", "json"))
  downloadImageServer("download_viz", element = reactive(hgch_viz()), lib = "highcharter", formats = c("jpeg", "pdf", "png", "html"), file_prefix = "plot")
  
  
  
}

shinyApp(ui, server)