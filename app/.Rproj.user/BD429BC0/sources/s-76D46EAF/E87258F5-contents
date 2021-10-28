webshot::install_phantomjs()
library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(shinydisconnect)
library(shinybusy)
library(dsmodules)
library(lfltmagic)
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

input[type='radio']:checked:after {
        width: 15px;
        height: 15px;
        border-radius: 15px;
        top: -1px;
        left: -1px;
        position: relative;
        background-color: #ff7f00;
        content: '';
        display: inline-block;
        visibility: visible;
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

.irs-bar {
    border-top: 1px solid #ff7f00 !important;
    border-bottom: 1px solid #ff7f00 !important;
    background: #ff7f00 !important;
}

.irs-from, .irs-to, .irs-single {
    color: #ff7f00 !important
}

.title-data-select {
    color: #ff7f00 !important;
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

.leaflet-control-attribution {
 display: none;
}
"

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
        body = uiOutput("final_viz"), # verbatimTextOutput("aver"),# 
        footer =  div(class = "panel-header",
                      uiOutput("viz_icons"), 
                      tags$a(
                        href="https://www.datasketch.co", target="blank",
                        img(src='ds_logo.png', align = "right", width = 150, height = 110)))
  )
)

server <- function(input, output, session) {
  

  
  indicators_list <- reactive({
     
    basicos <- data.frame(id = c("avance", "estado", "cumplimiento", "actividades", "participantes", "sectores", "resultados", "internacional"),
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
    button_id <- which(c("avance", "estado", "cumplimiento", "actividades", "participantes", "sectores", "resultados", "internacional") %in% last_btn)
    l[[button_id]] <- gsub("needed", "needed basic_active", l[[button_id]])
    l[[button_id]] <- HTML(paste0(paste(l[[button_id]], collapse = '')))
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
  

  
  
  
}

shinyApp(ui, server)