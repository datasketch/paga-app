webshot::install_phantomjs()
library(shiny)
library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(shinydisconnect)
library(shinycustomloader)
library(shinybusy)
library(dsmodules)
library(hgchmagic)
library(DT)


style <- "
@import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Sans&display=swap');

.panel,.panel-header,.panel-footer  {
  background: #f2f2f2;
}

.panel-footer {
height: 100px;
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

#info_add, #ficha_add {
 background-color: #0076b7 !important;
 border: 1px solid #0076b7 !important;
 color: #ffffff;
 margin-right: -5%;
 width: 250px !important;
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
    color: #E4602A;
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
  max-width: 100% !important;
}

.selectize-input.full {
    background-color: #f2f2f2;
    border: 1px solid #E4602A !important;
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
#viz_selection {
 width: 180px !important;
}

#dropdown_allData-dropdown {
 width: 180px !important;
}

#dropdown_allData-DownloadTblcsv {
    background-color: #ff7f00 !important;
    color: #fff !important;
    border: 1px solid #ff7f00 !important;
}

/*
scroll STYLES
*/
::-webkit-scrollbar {
  width: 7px;
  height: 7px;
}

::-webkit-scrollbar-track {
  background: #ffffff !important;
}

::-webkit-scrollbar-thumb {
  background: #6a6767 !important;
}

::-webkit-scrollbar:focus {
  overflow: scroll;
  display: block;
  background: #6a6767 !important;
}


"

indicadores_dic <- read_rds("data/all_dic.rds")
indicadores_dic <- indicadores_dic %>% distinct(id, .keep_all = TRUE)

ui <- panelsPage(
  shinypanels::modal(id = 'modal_extra_info', title = NULL, uiOutput("message_modal")),
  shinypanels::modal(id = 'modal_viz_info', title = NULL, uiOutput("info_plots")),
  shinypanels::modal(id = 'modal_ficha_info', title = NULL, uiOutput("info_ficha")),
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
    includeScript("js/paga.js")
  )),
  styles = style,
  panel(title = "Indicadores",
        id = "azul",
        width = 300,
        body = div(
          uiOutput("basicos")
        ),
        footer = tags$i("Fecha de actualización: Octubre 22 del 2021")
  ),
  panel(title = "Visualización",
        id = "naranja",
        header_right = uiOutput("descargas"),
        can_collapse = FALSE,
        width = 900,
        color = "chardonnay", #div(add_busy_spinner(spin = "fading-circle"),uiOutput("final_viz"))
        body = div(
          uiOutput("commitment"),
          uiOutput("addFilters"),
          withLoader(
            uiOutput("final_viz"),
            type = "html", loader = "loader4")
        ),# 
        footer =  div(class = "panel-header",
                      uiOutput("viz_icons"),       
                      downloadTableUI("dropdown_allData", text = "DESCARGAR DATOS", formats = c("csv"), display = "buttons"),
                      div(style = "display: flex;gap:20px;",
                          actionButton("info_add", "DESCRIPCIÓN DE ESTA GRÁFICA"),
                          actionButton("ficha_add", "FICHA TÉCNICA DEL INDICADOR")
                      )
        )
  )
)

server <- function(input, output, session) {
  
  
  #  Actualizacion de que los datos -----------------------------------------
  
  
  dataOrigin <- "data/all_data.RData"
  
  data <- reactivePoll(1000, session,
                       
                       checkFunc = function() {
                         if (file.exists(dataOrigin))
                           file.info(dataOrigin)$mtime[1]
                         else
                           shinyalert(title = "file",text = "Archivo no encontrado")
                       },
                       
                       valueFunc = function() {
                         source("data-prep.R")$value
                       }
  )
  
  
  
  # Lista predeterminada de indicadores -------------------------------------
  ### Creacion del menu de indicadores
  indicators_list <- reactive({
    
    basicos <- data.frame(id = c("avance", "estado", "cumplimiento", "actividades", "participantes", "sectores", "resultados", "relacion_internacional", "estrategias_grupoNucleo"),
                          indicadores = c("1. Porcentaje de avance de cada hito	Entidades responsables.",
                                          "2. Estado actual de implementación del hito.",
                                          "3. Cumplimiento de responsabilidades de la entidad responsable y la contraparte durante el cumplimiento del hito.",
                                          "4. Número de actividades realizadas para la inclusión de actores en el proceso.",
                                          "5. Número de participantes en total en cada hito de los compromisos.",
                                          "6. Identificación de sectores de pertenencia de participantes en cada hito de los compromisos.",
                                          "7. Percepción de resultados de implementación del hito.",
                                          "8. Cumplimiento con iniciativas internacionales.",
                                          "9. Estrategias de Comunicación Cocreadas.") 
                          
    )
    
    l <- purrr::map(1:nrow(basicos), function(z){
      actionButton(inputId = basicos[z,]$id, label = basicos[z,]$indicadores, class = "needed")
    })
    l
  })
  
  # boton seleccionado por defecto  
  indicator_choose <- reactive({
    last_btn <- input$last_click
    if (is.null(last_btn)) last_btn <- "avance"
    last_btn
  })
  
  ind_table <- reactive({
    req(indicator_choose())
    idsInd <- c("avance", "estado", "cumplimiento", "actividades", "participantes",
                "sectores", "resultados", "relacion_internacional", "estrategias_grupoNucleo")
    ic <- indicator_choose()
    tx <- NULL
    
    for (i in 1:length(idsInd)) {
      if (ic == idsInd[i]) {
        tx <- paste0("Indicador", i)
      }
    }
    tx
  })
  
  # selector particular de radio botones   
  observe({
    if(is.null(indicators_list())) return()
    l <- indicators_list()
    last_btn <- indicator_choose()
    button_id <- which(c("avance", "estado", "cumplimiento", "actividades", "participantes", "sectores", "resultados", "relacion_internacional", "estrategias_grupoNucleo") %in% last_btn)
    l[[button_id]] <- gsub("needed", "needed basic_active", l[[button_id]])
    l[[button_id]] <- HTML(paste0(paste(l[[button_id]], collapse = '')))
    if (indicator_choose() == "cumplimiento")
      l[[button_id]] <- div(l[[button_id]],
                    radioButtons("sub_cumplimiento",
                                 " ", 
                                 setNames(c("contraparte_responsable", "entidad_responsable", "contraparte_grupoNucleo", "entidad_grupoNucleo"), 
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
  
  # Graficos disponibles 
  possible_viz <- reactive({
    p <- indicator_choose()
    if (is.null(p)) return()
    v <- c("bar", "table")
    if (p %in% c( "relacion_internacional", "sectores")) {
      v <- c("donut",v)
    }
    
    v
  })
  
  # hover de los iconos de los graficos disponibles
  hover_viz <- reactive({
    p <- indicator_choose()
    if (is.null(p)) return()
    v <- c("Barras", "Tabla")
    if (p %in% c("sectores", "relacion_internacional")) {
      v <- c("Dona", "Barras", "Tabla")
    }
    v
  })
  
  
  # guardar el ultimo indicador seleccionado  
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
  
  # renderizar los graficos en pantalla
  output$viz_icons <- renderUI({
    req(possible_viz())
    suppressWarnings(
      buttonImageInput('viz_selection',
                       " ",
                       images = possible_viz(),
                       tooltips = hover_viz(),
                       path = 'icons/',
                       active = actual_but$active)
    )
  })
  
  # guardar la ultima grafica seleccionada
  id_viz <- reactive({
    id_viz <- actual_but$active
    if (is.null(id_viz)) id_viz <- "bar"
    id_viz
  })
  
  last_indicator <- reactive({
    req(indicator_choose())
    l_i <- indicator_choose()
    if (l_i == "cumplimiento") {
      if (is.null(input$sub_cumplimiento)) return()
      l_i <- input$sub_cumplimiento
    }
    l_i
  })
  
  
  # Selector de compromiso en indicador especificoß
  output$commitment <- renderUI({
    req(data())
    req(last_indicator())
    if (last_indicator() == "estrategias_grupoNucleo") return()
    
    selectizeInput("compromiso_id", "COMPROMISO", unique(data()$compromiso))
  })
  
  
  
  # Filtro de base de datos segun indicador seleccionado
  data_filter <- reactive({
    req(data())
    req(last_indicator())
    if (is.null(input$compromiso_id)) return()
    
    df <- data() %>%
      filter(compromiso %in% input$compromiso_id)
    
    if (last_indicator() %in% "estrategias_grupoNucleo") {
      df <- data()
    }
    
    
    df
  })
  
  # seleccion de hitos en indicadores especificos
  output$addFilters <- renderUI({
    req(data_filter())
    req(last_indicator())
    df <- data_filter()
    var_s <- last_indicator()
    if (id_viz() != "donut") return()
    if (!(last_indicator() %in% c( "sectores", "relacion_internacional"))) return()
    selectizeInput("hitoSel",  "HITO DE INTERES", unique(df$hito))
  })
  
  # Preparacion de datos y graficos -----------------------------------------
  # Organizacion de base de datos segun selecciones apriori
  
  data_select <- reactive({
    req(data_filter())
    req(last_indicator())
    df <- data_filter()
    var_s <- last_indicator()
    #c("#ff4e17", "#0076b7", "#78dda0", "#ff7f00", "#fdd60e", "#a478dd")
    if (last_indicator() %in% "estado") {
      df <- df[,c(var_s, "hito_id", "estado_contraparte", "estado_grupoNucleo", "cmp_esperado", "hito")]
      
      df <- df %>% plyr::rename(c("estado" = "Entidad responsable", "estado_contraparte" = "Contraparte", "estado_grupoNucleo" = "Grupo Núcleo"))
      df <- df %>% gather("tipo", "estado", c("Entidad responsable", "Contraparte", "Grupo Núcleo"))
      df$estadoxx <- plyr::revalue(df$estado, c("Completado" = 4, "Ejecución" = 3, "Planificación" = 2, "Definición" = 1, "Detenido" = 0))
      df <- df %>% select(tipo, hito_id, estadoxx, estado, cmp_esperado, hito) %>% tidyr::drop_na(estado)
    } else if (last_indicator() %in% "avance") {
      
      df <- df[,c(var_s, "hito_id", "fecha_inicio", "fecha_finalizacion", "cmp_esperado", "hito")]
      df$avance[is.na(df$avance)] <- 0
      df$`Porcentaje de no cumplimiento` <- (100 - df$avance)
      df <- df %>% plyr::rename(c("avance" = "Porcentaje de cumplimiento"))
      df <- df %>% gather("avance", "porcentaje", c("Porcentaje de cumplimiento", "Porcentaje de no cumplimiento"))
      df <- df %>% select(avance, hito_id, porcentaje, everything()) #%>% filter(porcentaje>0)
      df$...colors <- ifelse(df$avance == "Porcentaje de cumplimiento", "#0076b7", "#293662")
      
    } else if (last_indicator() %in% "sectores") {
      #print("sectores")
      req(id_viz())
      df <- df[,c(var_s, "hito_id", "cmp_esperado", "hito")]
      #print(df)
      df <- df %>% separate_rows(sectores, convert = TRUE, sep = ",|\\-|\\|")
      if (id_viz() == "donut") {
        req(input$hitoSel)
        df <- df %>% filter(hito %in% input$hitoSel)
      }
      df$sectores <- trimws(df$sectores) 
    } else if (last_indicator() %in% c("contraparte_responsable", "entidad_responsable", "contraparte_grupoNucleo", "entidad_grupoNucleo")) {
      if (last_indicator() == "contraparte_responsable") {
        df <- df[,c(var_s,"entidad_responsable_gn" ,"hito_id", "cmp_esperado", "hito")]
        df <- df %>% dplyr::rename(c("Entidad responsable" = "contraparte_responsable",
                                     "Grupo Núcleo" = "entidad_responsable_gn"))
        df <- df %>% gather("tipo", "value", c("Entidad responsable", "Grupo Núcleo"))
        df$contraparte_responsable <- df$value
      }
      if (last_indicator() == "entidad_responsable") {
        df <- df[,c(var_s,"contraparte_responsable_gn" ,"hito_id", "cmp_esperado", "hito")]
        df <- df %>% dplyr::rename(c("Contraparte" = "entidad_responsable",
                                     "Grupo Núcleo" = "contraparte_responsable_gn"))
        df <- df %>% gather("tipo", "value", c("Contraparte", "Grupo Núcleo"))
        df$entidad_responsable <- df$value
      }
      if (last_indicator() %in% c("contraparte_grupoNucleo")) {
        df <- df[,c(var_s,"hito_id", "cmp_esperado", "hito")]
        df <- df %>% dplyr::rename(c("Grupo Núcleo" = "contraparte_grupoNucleo"))
        df$value <- ifelse(df$`Grupo Núcleo` == "si", "Sí", df$`Grupo Núcleo`)
        df$contraparte_responsable <- df$`Grupo Núcleo`
        df$tipo <- "Grupo Núcleo"
      }
      if (last_indicator() %in% c("entidad_grupoNucleo")) {
        df <- df[,c(var_s,"hito_id", "cmp_esperado", "hito")]
        df <- df %>% dplyr::rename(c("Grupo Núcleo" = "entidad_grupoNucleo"))
        df$value <- df$`Grupo Núcleo`
        df$entidad_responsable <- df$`Grupo Núcleo`
        df$tipo <- "Grupo Núcleo"
      }
      df$value <- as.numeric(plyr::revalue(df$value, c("Sí" = 4, "No" = 2)))
      df <- df %>% select(tipo, hito_id, value, everything())
    } else if (last_indicator() %in%   "resultados") {
      df <- df[,c(var_s, "resultados_grupoNucleo","hito_id", "cmp_esperado", "hito")]
      df <- df %>% dplyr::rename(c("Contraparte" = var_s,
                                   "Grupo Núcleo" = "resultados_grupoNucleo"))
      df <- df %>% gather("tipo", "resultados", c("Contraparte", "Grupo Núcleo"))
      df$value <- plyr::revalue(df$resultados, c("Se mantuvo igual" = 1, "Mejoró un poco" = 3, "Mejoró sustancialmente" = 5))
      df <- df %>% select(tipo, hito_id, value, everything())
      #df <- df %>% bind_rows(data.frame(tipo = "Grupo Núcleo", hito_id = df$hito_id[1], value = NA))
    } else if (last_indicator() %in% "relacion_internacional") {
      df <- df[,c("relacion_internacional_descripcion", "hito_id", var_s, "cmp_esperado", "hito", "relacion_internacional_justificacion")]
      df <- df %>% separate_rows(relacion_internacional_descripcion, sep = ",|\\-|\\|")
      df$relacion_internacional_descripcion <- trimws(df$relacion_internacional_descripcion)
      if (id_viz() == "donut") {
        req(input$hitoSel)
        df <- df %>% filter(hito %in% input$hitoSel)
      }
      #df$relacion_internacional_descripcion[is.na(df$relacion_internacional_descripcion)] <- "NA"
    } else {
      df <- df[,c("hito_id", var_s, "cmp_esperado", "hito")]
    }
    
    if (last_indicator() %in% "estrategias_grupoNucleo") {
      df <- data() %>% drop_na(estrategias_grupoNucleo)
      indComp <- data.frame(compromiso = unique(df$compromiso))
      indComp$idCom <- paste0("Compromiso ", 1:nrow(indComp))
      df <- df %>% left_join(indComp)
      #df <- df %>% dplyr::select(-Id)
      df$value <- as.numeric(plyr::revalue(df$estrategias_grupoNucleo, c("Sí" = 4, "No" = 2)))
      df <- df %>% select(idCom, value, "estrategias_grupoNucleo", everything())
    }
    
    req(id_viz())
    if (id_viz() != "donut") {
      if (nrow(df) != 0) {
        df_hitos <- data_filter() %>% select(hito_id, hito) %>% distinct()
        
        ind_hito <- setdiff(df_hitos$hito_id, unique(df$hito_id))
        
        if (!(identical(ind_hito, character()))) {
          df_hitos <- df_hitos %>% filter(hito_id %in% ind_hito)
          df <- df %>% bind_rows(df_hitos)
        }
      }
    }
    
    if ("tipo" %in% names(df)) {
      df$...colors <- dplyr::recode(df$tipo, "Entidad responsable" = "#0076b7", "Contraparte" = "#ff4e17", "Grupo Núcleo" = "#78dda0")
    }
    
    df
  })
  
  # estilos de visualizacion segun guia de paga 
  opts_plot <- reactive({
    req(data_select())
    req(id_viz())
    fjs <- NULL
    order_s <- NULL
    order_stacked <- NULL
    yMax <- NULL
    yEnabled <- TRUE
    id_button <- last_indicator()
    colors <- c("#ff4e17", "#0076b7", "#78dda0", "#ff7f00", "#fdd60e", "#a478dd")
    cursor <- NULL
    myFunc <- NULL
    df <- data_select()
    reverse_axis <- FALSE
    labelsRotationY <- NULL
    marginBottom <- NULL
    dataLabels_template <- NULL
    showLabels <- FALSE
    grid_y_enabled <- TRUE
    colorDonut <- NULL
    legendShow <- TRUE
    legendRev <- FALSE
    orderAxis <- NULL
    if (id_button == "avance") {
      tx <- "Fecha de inicio: {fecha_inicio} <br/> Fecha de finalización: {fecha_finalizacion} <br/>{hito}<br/><b>{avance}: {porcentaje}%</b>"
      yMax <- 100
      reverse_axis <- FALSE#TRUE
      yEnabled <- TRUE
      order_stacked <- rev(unique(df$avance))
      order_s <-  rev(unique(df$avance))
      legendRev <- TRUE
      showLabels <- TRUE
      #colors <- c("#0076b7", "#293662", "#0076b7", "#293662")
      # if (length(order_s) == 1) {
      #   if (order_s == "Porcentaje de cumplimiento") colors <- colors[1]
      #   if (order_s == "Porcentaje de no cumplimiento") colors <- colors[2]
      # }
    } else if (id_button == "estado") {
      tx <- "<b>{tipo}</b> <br/>{hito}<br/> <b>Estado: {estado}</b>"
      fjs <- JS("function () {var arreglo = ['Detenido','Definición','Planificación', 'Ejecución', 'Completado'];return arreglo[this.value];}")
      order_s <- c("Entidad responsable", "Contraparte","Grupo Núcleo")
      yMax <- 4
      #labelsRotationY <- 45
      #colors <- c("#0076b7","#ff4e17", "#78dda0")
    } else if (id_button == "contraparte_responsable") {
      tx <- "{hito}<br/> <b>La contraparte ha respondido con sus responsabilidades con la entidad Responsable durante el compromiso: {contraparte_responsable} </b> <br/><br/>Da click para más información"
      c("Entidad responsable", "Grupo Núcleo")
      #colors <- c("#0076b7", "#78dda0")
      cursor <- "pointer"
      yMax <- 4
      myFunc <- JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.category, cat:this.name, timestamp: new Date().getTime()});}")
      fjs <- JS("function () {var arreglo = ['','No', '' , 'Sí'];return arreglo[this.value];}")
    } else if (id_button == "entidad_responsable") {
      tx <- "{hito}<br/> <b>La entidad responsable ha responido con sus responsabilidades con la contraparte: {entidad_responsable} </b>  <br/><br/>Da click para más información"
      order_s <- order_s <- c("Contraparte", "Grupo Núcleo")
      #colors <- c("#ff4e17", "#78dda0")
      cursor <- "pointer"
      yMax <- 4
      myFunc <- JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.category, cat:this.name, timestamp: new Date().getTime()});}")
      fjs <- JS("function () {var arreglo = ['','No', '', 'Sí'];return arreglo[this.value];}")
    }  else if (id_button == "contraparte_grupoNucleo") {
      tx <- "{hito}<br/> <b>La contraparte ha respondido con sus responsabilidades con el Grupo Núcleo: {contraparte_responsable} </b>  <br/>"
      #colors <- c( "#78dda0")
      cursor <- "pointer"
      yMax <- 4
      fjs <- JS("function () {var arreglo = ['','No', '', 'Sí'];return arreglo[this.value];}")
    } else if (id_button == "entidad_grupoNucleo") {
      tx <- "{hito}<br/> <b>La entidad ha respondido con sus responsabilidades con el Grupo Núcleo: {entidad_responsable} </b>  <br/>"
      #colors <- c("#78dda0")
      cursor <- "pointer"
      yMax <- 4
      fjs <- JS("function () {var arreglo = ['','No', '', 'Sí'];return arreglo[this.value];}")
    } else if (id_button == "actividades") {
      tx <- "{hito} <br/> <b>Número de actividades {actividades}</b> <br/><br/>Da click para más información"
      colors  <- "#293662"
      yEnabled <- FALSE
      cursor <- "pointer"
      marginBottom <- 50
      showLabels <- TRUE
      myFunc <- JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.name, timestamp: new Date().getTime()});}")
    } else if (id_button == "participantes") {
      tx <- "{hito} <br/> <b>Número de participantes {participantes}</b>"
      colors  <- "#293662"
      yEnabled <- FALSE
      marginBottom <- 50
      showLabels <- TRUE
      #myFunc <- JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.name, timestamp: new Date().getTime()});}")
    } else if (id_button == "sectores") {
      tx <- "{hito} <br/> <b>Sector: {sectores}</b>"
      yMax <- 6
      yEnabled <- FALSE
      if (id_viz() == "donut") {
        colorDonut <- "sectores"
        legendShow <- FALSE
        showLabels <- TRUE
        dataLabels_template <- "{point.name}"
      }
    } else if (id_button == "resultados") {
      tx <- "{hito} <br/> <b>Percepción de resultados: {resultados}</b>  <br/><br/>Da click para más información"
      fjs <- JS("function () {var arreglo = ['','Se mantuvo igual', '','Mejoró un poco','' ,'Mejoró sustancialmente'];return arreglo[this.value];}")
      order_s <- c("Contraparte", "Grupo Núcleo")
      #colors <- c("#0076b7", "#78dda0")
      cursor <- "pointer"
      myFunc <- JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.category, cat:this.name, timestamp: new Date().getTime()});}")
      yMax <- 5
    } else if (id_button == "relacion_internacional") {
      #tx <- "{hito} <br/> <b>Cumplimiento con iniciativas internacionales: {relacion_internacional_descripcion}</b> <br/> <br/> <b>Punto concreto de la relación con el hito</b> {relacion_internacional_justificacion}"
      tx <- "{hito} <br/> <b> Iniciativa internacional ligada con Gobierno Abierto: </b>{relacion_internacional_descripcion} <br/>
       <b>Punto concreto de la relación con el hito</b> {relacion_internacional_justificacion} <br/>
       <b>Cumplimiento con iniciativas internacionales: {relacion_internacional}</b>"
      dataLabels_template <- "{point.name}"
      showLabels <- TRUE
      yEnabled <- FALSE
      #marginBottom <- 50
      if (id_viz() == "donut") {
        colorDonut <- "relacion_internacional"
        legendShow <- FALSE
      }
      #colors <- c("#293662", "#78dda0")
    } else if (id_button == "estrategias_grupoNucleo") {
      tx <- "{compromiso} <br/> <b>Hitos con estrategias de comunicación: </b><br/>{hito}<br/> <b>El compromiso cuenta con una estrategia de comunicación cocreada: {estrategias_grupoNucleo} </b>  <br/>"
      colors <- c("#0076b7", "#78dda0")
      yMax <- 4
      #colorDonut <- "value"
      orderAxis <- paste0("Compromiso ", 1:10)
      fjs <- JS("function () {var arreglo = ['','No', '', 'Sí'];return arreglo[this.value];}")
    } else {
      tx <- return()
    }
    list(
      tooltip = tx,
      formatterJS = fjs,
      orderLegend = order_s,
      order_stacked = order_stacked,
      order = orderAxis,
      yMax = yMax,
      palette_type = "categorical",
      labelsRotationY = labelsRotationY,
      colors = colors,
      yEnabled = yEnabled,
      reverse_axis = reverse_axis,
      cursor = cursor,
      clickFc = myFunc,
      marginBottom = marginBottom,
      dataLabels_template = dataLabels_template,
      showLabels = showLabels,
      grid_y_enabled =  grid_y_enabled,
      colorDonut  = colorDonut,
      legendShow = legendShow,
      legendRev = legendRev,
      dataLabels_align = "middle",
      dataLabels_inside = TRUE
    )
  })
  
  
  opts_viz <- reactive({
    req(data_select())
    req(last_indicator())
    req(opts_plot())
    if (nrow(data_select()) == 0) return()
    if (actual_but$active == "table") return()
    
    
    format_x_js <- NULL
    axisColor <- data_select() %>% filter(cmp_esperado %in% "si")
    #print(axisColor)
    if (nrow(axisColor) != 0) {
      axisColor <- unique(axisColor$hito_id)
      hito_high <- paste0("\'",axisColor, "\'", collapse = ",")
      format_x_js <- JS(paste0("function () { var arr = [", hito_high,"]; if (arr.includes(this.value)) {return '<text style=\"color:#109a4f !important;fill:#109a4f !important;font-weight: 500;\">' + this.value + '</text>'; } else { return this.value}; }"))
    }
    #print(format_x_js)
    
    graph_type <- "grouped"
    if (last_indicator() %in% c("avance", "sectores")) {
      graph_type = "stacked"
    }
    
    
    
    list(
      data = data_select(),
      plot_margin_bottom = opts_plot()$marginBottom,
      graph_type = graph_type,
      reversedYaxis = opts_plot()$reverse_axis,
      labelsRotationY = opts_plot()$labelsRotationY,
      color_by = opts_plot()$colorDonut,
      legend_show = opts_plot()$legendShow,
      orientation = "hor",
      drop_na = TRUE,
      agg = "mean",
      caption = "Fecha de actualización: Octubre 22 del 2021",
      legend_y_position = -30,
      hor_title = " ",
      ver_title = " ",
      label_wrap = 100,
      dataLabels_template = opts_plot()$dataLabels_template,
      dataLabels_size = 13,
      text_size = 14,
      dataLabels_align = "middle",
      dataLabels_inside = TRUE,
      #na_color = "red",
      background_color = "transparent",
      y_max = opts_plot()$yMax,
      order_legend = opts_plot()$orderLegend,
      order_stacked = opts_plot()$order_stacked,
      order = opts_plot()$order,
      y_axis_align = "right",
      formatter_x_js = format_x_js,
      formatter_js = opts_plot()$formatterJS,
      dataLabels_show = opts_plot()$showLabels,
      label_wrap_legend = 150,
      legend_reversed = opts_plot()$legendRev,
      cursor = opts_plot()$cursor,
      legend_align = "left",
      caption_align = "right",
      # drop_na_legend = TRUE,
      # drop_na = TRUE,
      grid_y_enabled = opts_plot()$yEnabled,
      tooltip = opts_plot()$tooltip,
      format_sample_num = "1,234.",
      palette_colors = opts_plot()$colors,
      clickFunction = opts_plot()$clickFc
    )
  })
  
  # tipo de datos a graficar
  viz_type <- reactive({
    req(last_indicator())
    req(id_viz())
    if (actual_but$active == "table") return()
    viz <- "CatCatNum"
    showLabels <- FALSE
    
    
    if (last_indicator() %in% c("actividades", "participantes", "estrategias_grupoNucleo")) {
      viz <- "CatNum"
    }
    if (last_indicator() %in% c("relacion_internacional", "sectores")) {
      viz <- "CatCat"
    }
    if (id_viz() == "donut") {
      viz <- "Cat"
    }
    
    type_viz <- actual_but$active
    viz_sel <- paste0("hgch_", type_viz, "_", viz)
    viz_sel
  })
  
  # guardar visualizacion segun datos
  hgch_viz <- reactive({
    req(input$compromiso_id)
    req(viz_type())
    req(opts_viz())
    mdf_opts <- opts_viz()
    data_mdf <-  mdf_opts$data
    axisInd <- data_mdf %>% filter(cmp_esperado %in% "si")
    if (nrow(axisInd) != 0) {
      axisInd <- unique(axisInd$hito_id)
      data_mdf$hito_id <- ifelse(data_mdf$hito_id %in% axisInd, data_mdf$hito_id, paste0(data_mdf$hito_id, "*"))
    }
    
    mdf_opts <- mdf_opts[-(grep("formatter_x_js", names(mdf_opts)))]
    mdf_opts$title <- input$compromiso_id
    mdf_opts$subtitle <- "Hitos en desarrollo*"
    mdf_opts$data <- data_mdf
    do.call(viz_type(), mdf_opts)
  })
  
  
  
  output$hgch_viz <- renderHighchart({
    tryCatch({
      req(viz_type())
      req(opts_viz())
      do.call(viz_type(), opts_viz())
    }, error = function(con) {
      return()
    }
    )
  })
  
  
  # Informacion de modal que da detalle especifico en un indicador predeterminado al dar click en 
  # la grafica
  
  textModal <- reactive({
    
    req(last_indicator())
    if (last_indicator() %in% c("avance", "estado",   "sectores", "relacion_internacional")) return()
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
                         df$actividades_descripcion, " </p>"
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
                         "</p><br/> <h3>Entidad Responsable</h3><br/> <p class = 'description-modal'>",
                         df$entidad_responsable_justificacion, "</p>"
                  )
                )
      )
    }
    
    if (last_indicator() %in% "estrategias_grupoNucleo") {
      tx <- "Sin detalle"
    }

    
    tx
    
    
  })
  
  output$message_modal<- renderUI({
    textModal()
  })
  
  observeEvent(input$hcClicked, {
    shinypanels::showModal("modal_extra_info")
  })
  
  # Renderizar tabla
  output$table_view <- renderDataTable({
    req(data_filter())
    
    df <- data_filter() %>% dplyr::select(-hito_id, -cmp_esperado)
    
    df_dic <- data.frame(id = names(df))
    df_dic <- df_dic %>% left_join(indicadores_dic)
    tooltip_names <- paste0("'",df_dic$label_original, "'", collapse = ",")
    
    
    headerCallback <- paste0(c(
      "function(thead, data, start, end, display){",
      "  var tooltips = [", tooltip_names ,"];",
      "  for(var i=0; i<37; i++){",
      "    $('th:eq('+i+')',thead).attr('title', tooltips[i]);",
      "  }",
      "}"
    ))
    dtable<- DT::datatable(df,
                           rownames = F,
                           escape = FALSE,
                           options = list(
                             mark = list(accuracy = "exactly"),
                             autoWidth = TRUE,
                             scrollX = TRUE,   ## enable scrolling on X axis
                             scrollY = TRUE,
                             headerCallback = JS(headerCallback),
                             columnDefs= list(
                               list(
                                 width = '350px', targets = c(0,1, 14, 19, 24)
                               )
                             ),
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
    
    
    
    dep1 <- htmltools::htmlDependency(
      "datatables.mark", "2.0.1",
      src = c(href = "https://cdn.datatables.net/plug-ins/1.10.19/features/mark.js"),
      script = "datatables.mark.min.js")
    dep2 <- htmltools::htmlDependency(
      "jquery.mark", "8.11.1",
      src = c(href = "https://cdnjs.cloudflare.com/ajax/libs/mark.js/8.11.1"),
      script = "jquery.mark.min.js")
    dtable$dependencies <- c(dtable$dependencies, list(dep1, dep2))
    dtable
  })
  
  
  # mostrar visualizacion en pantalla
  output$final_viz <- renderUI({
    
    if (is.null(id_viz())) return()
    if (is.null(data_select())) return("No hay datos disponibles")
    if (id_viz() == "table") {
      v <- dataTableOutput("table_view", width = 900)
      
    } else {
      v <- highchartOutput("hgch_viz", height = 500)
    }
    
    if (id_viz() == "bar") {
      req(last_indicator())
      tx <- HTML("<div style=background:#cccccc;width:250px;max-width:auto;padding:2px;margin-left:2%;font-weight:500;> <span style=color:#109a4f;>Hitos finalizados</span><span style=margin-left:3%;>Hitos en desarrollo</span></div>")
      if (last_indicator() == "estrategias_grupoNucleo") tx <- br()
      div (
        tx,
        v
      )
    } else {
      v
    }
    
    
  })
  
  # Informacion de modal que explica cada indicador
  textButtonInfo <- reactive({
    req(indicator_choose())
    info <- read_csv("data/plot_info.csv")
    tx <- HTML(info$tx[info$id == indicator_choose()])
    div(class = "bodyModal" ,tx)
  })
  
  
  output$info_plots <- renderUI({
    textButtonInfo()
  })
  
  observeEvent(input$info_add, {
    shinypanels::showModal("modal_viz_info")
  })
  
  
  
  textButtonFicha <- reactive({
    req(indicator_choose())
    info <- read_csv("data/ind_info.csv")
    tx <- HTML(info$tx[info$id == indicator_choose()])
    div(class = "bodyModal" ,tx)
  })
  
  
  output$info_ficha <- renderUI({
    textButtonFicha()
  })
  
  observeEvent(input$ficha_add, {
    shinypanels::showModal("modal_ficha_info")
  })
  
  # Descarga de viz y datos -------------------------------------------------
  
  output$descargas <- renderUI({
    if (is.null(actual_but$active)) return()
    if (actual_but$active != "table") {
      div (style = "display: grid;grid-template-columns: 1fr 1fr;grid-gap: 15px;",
           downloadImageUI("download_viz", dropdownLabel = "Descargar visualización", formats = c("jpeg", "pdf", "png", "html"), display = "dropdown"),
           #} else {
           downloadTableUI("dropdown_table", dropdownLabel = "Descargar Filtros  ", formats = c("csv", "xlsx", "json"), display = "dropdown")
      )
    } else {
      downloadTableUI("dropdown_table", dropdownLabel = "Descargar Datos  ", formats = c("csv", "xlsx", "json"), display = "dropdown")
    }
  })
  
  downloadTableServer2("dropdown_table", element = reactive(list("Data"=data_filter(), "Diccionario"=indicadores_dic)), formats = c("csv", "xlsx", "json"), zip = TRUE, file_prefix = reactive(ind_table()))
  downloadImageServer("download_viz", element = reactive(hgch_viz()), lib = "highcharter", formats = c("jpeg", "pdf", "png", "html"), file_prefix = "plot")
  
  downloadTableServer("dropdown_allData", element = reactive(data()), formats = c("csv", "xlsx", "json"), file_prefix = "allData")
  
  
  
 
  
}

shinyApp(ui, server)