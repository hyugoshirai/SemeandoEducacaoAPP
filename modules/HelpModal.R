### 1.Help Modal
### This function creates a modal dialog with help information for the user.
createHelpModal <- function() {
  modalDialog(
    title = "Menu de Ajuda",
    easyClose = TRUE,
    footer = modalButton("Close"),
    size = "l",
    tags$div(
      # Tabset Panel with IDs for each tabPanel
      tabsetPanel(
        id = "help_tabs",
        tabPanel(
          "Navegação",
          tagList(
            h4("Instruções de Navegação no mapa"),  
            h6(tags$i("(Clique nas figuras para aumentar o tamanho. Clique novamente para retornar ao tamanho normal.)")),
            tags$ul(
              tags$li(
                strong("Desenhar Pontos no Mapa:"),
                " Use o ", tags$img(src = "Draw a marker.png", style = "height: 1em; vertical-align: middle;"), " ícone para desenhar seus pontos no mapa."
              )
            ),
            tags$img(src = "ponto.gif", style = "width: 100%;", onclick = "this.style.transform='scale(2)';", ondblclick = "this.style.transform='scale(1)';"),
            tags$ul(
              tags$li(
                strong("Editar Pontos:"),
                " Se precisar ajustar um ponto, clique no ícone ", tags$img(src = "Edit layers.png", style = "height: 1em; vertical-align: middle;"), ", selecione o ponto que deseja editar e salve a edição."
              )
            ),
            tags$img(src = "edit_point.gif", style = "width: 100%;", onclick = "this.style.transform='scale(2)';", ondblclick = "this.style.transform='scale(1)';"),
            tags$ul(
              tags$li(
                strong("Desenhar Polígonos no Mapa:"),
                " Use o ", tags$img(src = "Draw a polygon.png", style = "height: 1em; vertical-align: middle;"), " ícone para desenhar seus polígonos no mapa."
              )
            ),
            tags$img(src = "Polygon.gif", style = "width: 100%;", onclick = "this.style.transform='scale(2)';", ondblclick = "this.style.transform='scale(1)';"),
            tags$ul(
              tags$li(
                strong("Desenhar Linhas no Mapa:"),
                " Use o ", tags$img(src = "Draw a polyline.png", style = "height: 1em; vertical-align: middle;"), " ícone para desenhar suas linhas no mapa."
              )
            ),
            tags$img(src = "Line.gif", style = "width: 100%;", onclick = "this.style.transform='scale(2)';", ondblclick = "this.style.transform='scale(1)';"),
            tags$ul(
              tags$li(
                strong("Deletar Pontos:"),
                " Para remover um ponto, clique no ", tags$img(src = "Delete layers.png", style = "height: 1em; vertical-align: middle;"), " ícone e clique no ponto que deseja deletar."
              )
            ),
            tags$img(src = "delete_point.gif", style = "width: 100%;", onclick = "this.style.transform='scale(2)';", ondblclick = "this.style.transform='scale(1)';"),
            tags$ul(
              tags$li(
                strong("Pesquisar localidades:"),
                " Para pesquisar lugares pelo nome, clique em ", tags$img(src = "Search.png", style = "height: 1em; vertical-align: middle;"), " e clique no nome do local."
              )
            ),
            tags$ul(
              tags$li(
                strong("Alterar Basemap:"),
                " Você pode alterar o basemap nas opções disponíveis no mapa."
              )
            ),
            tags$img(src = "Basemap.gif", style = "width: 100%;", onclick = "this.style.transform='scale(2)';", ondblclick = "this.style.transform='scale(1)';"),
            tags$ul(
              tags$li(
                strong("Download:"),
                " Para baixar as camadas desenhadas, clique no botão de download "
              )
            ),
            tags$img(src = "download.gif", style = "width: 100%;", onclick = "this.style.transform='scale(2)';", ondblclick = "this.style.transform='scale(1)';"),
            p("Para navegar no mapa de forma mais eficaz, use os seguintes ícones:"),
            tags$ul(
              tags$li("Zoom In: ", tags$img(src = "Zoom in.png", style = "height: 1em; vertical-align: middle;"), " para dar zoom no mapa."),
              tags$li("Zoom Out: ", tags$img(src = "Zoom out.png", style = "height: 1em; vertical-align: middle;"), " para dar zoom out no mapa."),
              tags$li("Alternar Camadas: ", tags$img(src = "Layers.png", style = "height: 1em; vertical-align: middle;"), " para alternar camadas."),
              tags$li("Medir Distâncias: ", tags$img(src = "Measure distances and areas.png", style = "height: 1em; vertical-align: middle;"), " para medir distâncias no mapa.")
            )
          )
        ),
      )
    )
  )
}