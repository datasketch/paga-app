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
 background: #ffffff !important;
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
 background-color: #ff7f00 !important;
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

.radio {
  margin-bottom: 15px;
}
  
#viz_icons {
  margin-left: -33px;
}
"

indicadores_data <- read_rds("data/all_data.rds")
indicadores_dic <- read_rds("data/all_dic.rds")

ui <- panelsPage(
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
        )
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
                                 setNames(c("contraparte_responsable", "entidad_responsable"),# "contraparte_nucleo", "entidad_nucleo"), 
                                          c("3.1 ¿La contraparte ha respondido con sus responsabilidades con la entidad Responsable durante el compromiso?",
                                            "3.2 ¿La entidad responsable ha responido con sus responsabilidades con la contraparte?"#,
                                            #"3.3 ¿La contraparte ha respondido con sus responsabilidades con el Grupo Núcleo durante el compromiso?",
                                            #"3.4 ¿La entidad responsable ha respondido con sus responsabilidades con el Grupo Núcleo durante el compromiso?"
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
    id_viz <- input$viz_selection
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
    if (last_indicator() %in% "estado") {
      df <- df[,c(var_s, "hito", "estado_contraparte")]
      df <- df %>% plyr::rename(c("estado" = "Entidad", "estado_contraparte" = "Contraparte"))
      df <- df %>% gather("tipo", "estado", -hito)
      df$estado <- plyr::revalue(df$estado, c("Completado" = 0, "Ejecución" = 60, "Definición" = 40, "Planificación" = 10))
      df <- df %>% select(tipo, hito, estado)
    } else if (last_indicator() %in% "avance") {
      df <- df[,c(var_s, "hito")]
      df$`Porcentaje de no completitud` <- (100 - df$avance)
      df <- df %>% plyr::rename(c("avance" = "Porcentaje de completitud"))
      df <- df %>% gather("avance", "porcentaje", -hito)
      df <- df %>% select(avance, hito, porcentaje)
    } else if (last_indicator() %in% "sectores") {
      df <- df[,c(var_s, "hito")]
      df <- df %>% separate_rows(sectores, convert = TRUE, sep = ",")
    } else if (last_indicator() %in% c("contraparte_responsable", "entidad_responsable", "resultados", "relacion_internacional")) {
      df <- df[,c(var_s, "hito")]
    } else {
      df <- df[,c("hito", var_s)]
    }
    df
    
  })
  
  
  
  
  hgch_viz <- reactive({
    req(data_select())
    viz <- "CatCatNum"
    if (length(data_select()) == 2) {
      if (last_indicator() %in% c("actividades", "participantes")) {
        viz <- "CatNum"
      } else
        viz <- "CatCat"
    }
    
    graph_type <- "grouped"
    if (last_indicator() == "avance") {
      graph_type = "stacked"
    }
    
    do.call(paste0("hgch_bar_", viz), list(data = data_select(),
                                           graph_type = graph_type,
                                           orientation = "hor", 
                                           hor_title = " ",
                                           ver_title = " ",
                                           label_wrap = 150))
  })
  
  
  output$hgch_viz <- renderHighchart({
    hgch_viz()
  })
  
  
  
  output$table_view <- renderDataTable({
    req(data_filter())
    df <- data_filter()
    df_dic <- data.frame(id = names(df))
    df_dic <- df_dic %>% left_join(indicadores_dic)
    names(df) <- df_dic$label
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
    if (id_viz() == "table") {
      v <- dataTableOutput("table_view", width = 900)
    } else {
      v <- highchartOutput("hgch_viz")
    }
    v
  })
  
  output$descargas <- renderUI({
    if (is.null(actual_but$active)) return()
    if (actual_but$active != "table") {
      downloadImageUI("download_viz", dropdownLabel = "Descarga", formats = c("jpeg", "pdf", "png", "html"), display = "dropdown")
    } else {
      downloadTableUI("dropdown_table", dropdownLabel = "Descarga", formats = c("csv", "xlsx", "json"), display = "dropdown")
    }
  })
  
  downloadTableServer("dropdown_table", element = data_filter(), formats = c("csv", "xlsx", "json"))
  downloadImageServer("download_viz", element = hgch_viz(), lib = "highcharter", formats = c("jpeg", "pdf", "png", "html"), file_prefix = "plot")
  
  
  
}

shinyApp(ui, server)