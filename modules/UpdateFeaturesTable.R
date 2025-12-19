### 1. Create reactive values for the features table
### This code snippet creates a reactive value to store the features table data
features_table_data <- reactiveVal(
  data.frame(ID = character(),
             Nome = character(),
             Geometria = character(),
             Tipo = character(),
             Cor = character(),
             stringsAsFactors = FALSE)
)

### 2. Function to update the features table
### This function updates the features table with the current features
updateFeaturesTable <- function() {
  fl <- features_list()
  if (length(fl) == 0) {
    features_table_data(
      data.frame(ID = character(), Nome = character(), Geometria = character(),
                 Tipo = character(), Cor = character(), stringsAsFactors = FALSE)
    )
    message("updateFeaturesTable: no features.")
    return(invisible(NULL))
  }
  
  rows <- lapply(seq_along(fl), function(i) {
    x <- fl[[i]]              # one-row sf
    id <- if ("id" %in% names(x)) as.character(x$id[1]) else names(fl)[i]
    nm <- if ("name" %in% names(x)) as.character(x$name[1]) else NA_character_
    tp <- if ("Tipo" %in% names(x)) as.character(x$Tipo[1]) else NA_character_
    cl <- if ("color" %in% names(x)) as.character(x$color[1]) else NA_character_
    gt <- as.character(sf::st_geometry_type(sf::st_geometry(x)))[1]
    
    data.frame(
      ID = id, Nome = nm, Geometria = gt, Tipo = tp, Cor = cl,
      stringsAsFactors = FALSE
    )
  })
  
  tbl <- do.call(rbind, rows)
  features_table_data(tbl)
  
  message("updateFeaturesTable: rows=", nrow(tbl), " | cols=", paste(names(tbl), collapse = ", "))
  print(utils::head(tbl))
}
