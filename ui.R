# UI definition
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  tags$head(
    tags$style(HTML("
      .small-legend {
        background: white !important;
        padding: 8px;
        border-radius: 4px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.15);
      }
                summary .indicator {
      transition: transform 0.2s;
      display: inline-block;
      margin-left: 0.5em;
    }
    details[open] summary .indicator {
      transform: rotate(180deg);
    }
    "))
  ),
  # Spinner overlay
  div(
    id = "loading-overlay",
    div(
      id = "loading",
      img(src = "loading2.gif", height = 75, width = 75),
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
  titlePanel("Semeando Água"),
  sidebarPanel(
    actionButton("help_button", "Clique aqui para ajuda", icon = icon("info-circle")),
    selectInput("sistema", "Selecione o sistema de abastecimento de água:", choices = NULL, selected = NULL, multiple = FALSE),
    selectInput("MappingInput", "O que você deseja desenhar?",
                choices = c(
                  "Escola",
                  "Agricultura",
                  "Áreas urbanas",
                  "Áreas verdes",
                  "Corpos d'água",
                  "Indústria",
                  "Pastagem",
                  "Silvicultura"
                )),
    actionButton("calc_dist", "Calcular Distancia"),
    DTOutput("features_table"),
    downloadButton("download_shapefile", "Baixar camadas"),  # Download button for shapefile
    tags$details(
      open = NA, # starts expanded
      tags$summary(
        tags$b("Camadas"), 
        tags$span(class = "indicator", "\u25BC") # ▼, will rotate when open
      ),
      checkboxGroupInput(
        "overlays",
        label = NULL,
        choices = custom_control,
        selected = c("Sistema Cantareira")
      )
    )
  ),
  mainPanel(
    leafletOutput("map", height = "85vh"),
    verbatimTextOutput("distance_output")
  )
)