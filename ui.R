# UI definition
ui <- fluidPage(
  # Spinner overlay
  div(
    id = "loading-overlay",
    div(
      id = "loading",
      img(src = "loading.gif", height = 75, width = 75),
      style = "position: fixed; right: 50%; top: 50%; z-index: 3001;"
    ),
    style = "position: fixed; top: 0; left: 0; right: 0; bottom: 0; background-color: rgba(255, 255, 255, 0.5); z-index: 3000; display: none;"
  ),
  tags$script(
    'function checkifrunning() {
       var is_running = $("html").hasClass("shiny-busy");
       if (is_running){
         $("#loading-overlay").show();
       } else {
         $("#loading-overlay").hide();
       }
     }
     setInterval(checkifrunning, 100);'
  ),
  useShinyjs(),  # Initialize shinyjs
  titlePanel("Mapeamento"),
  sidebarPanel(
    actionButton("help_button", "Clique aqui para ajuda", icon = icon("info-circle")),
    selectInput("sistema", "Selecione o sistema de abastecimento de água:", choices = NULL, selected = NULL, multiple = FALSE),
    selectInput("MappingInput", "O que você deseja desenhar?",
                choices = c(
                  "Áreas verdes",
                  "Corpos d'água",
                  "Áreas urbanas",
                  "Escola",
                  "Indústria",
                  "Agricultura",
                  "Pastagem",
                  "Silvicultura"
                )),
    actionButton("calc_dist", "Calcular Distancia"),
    DTOutput("features_table"),
    downloadButton("download_shapefile", "Baixar camadas")  # Download button for shapefile
  ),
  mainPanel(
    leafletOutput("map", height = "85vh"),
    verbatimTextOutput("distance_output")
  )
)