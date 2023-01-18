#' Reducción de registros cercanos (thinning) para SDM
#' @description  Recibe una data.frame de puntos georreferenciados de ocurrencia, preferiblemente descargados con la función rgbif::occ_data(), que haya pasado por un proceso preliminar de limpieza de registros con baja confiabilidad (basado en opinión de experto). En el marco del pre-procesamiento de datos para modelamiento de distribución potencial de especies propuesto por Cobos et. al (2019) en el paquete `kuenm`, esta función realiza los siguientes procedimientos:
#' \itemize{
#'  \item Procedimiento 1: Reducción de puntos cercanos (Thinning), con la separación mínima entre puntos definida por el argumento distancia_km. Este proceso está activado por defecto.
#'  \item Procedimiento 2: Guardar los archivos .csv de los registros de ocurrencia completos, y las bases de datos de entrenamiento y testeo en la carpeta de trabajo actual, con los nombres "Sp_joint.csv", "Sp_train.csv" y "Sp_test.csv", respectivamente.
#'  \item Creación de las bases de entrenamiento (75\% de los registros) y testeo (25\% de los registros). Este proceso está activado por defecto.
#' }
#'
#' @param data_ocurrencias data.frame con los registros de ocurrencia georreferenciados (con las columna de latitud y longitud por separado).
#' @param distancia_km valor numérico para definir la cantidad de kilómetros a los que deben estar separados los registros de presencia como mínimo (aplicado para el procedimiento de thining).
#' @param semilla valor numérico, define una semilla para la selección.
#' @param porc_entrenamiento valor numérico, que expresa la proporción (0 a 1) de registros destinados a ser guardados en la base de entrenamiento.
#' @param mapview lógico, TRUE por defecto. Activa la visualización de los registros completos descargados y limpiados  con mapview en RStudio.
#' @param subcarpeta_sp NULL por defecto. String que determina la subcarpeta donde se guardarán los archivos .csv de ocurrencia para la especie de interés.
#' @param CRS Sistemas de Referencia de Coordenadas. Definido por defecto como "+proj=longlat +ellps=WGS84 +datum=WGS84".
#'
#' @return La función devuelve los archivos de registros con los nombres necesarios para ser usados con el paquete Kuenm (Cobos et al., 2019) con los nombres "Sp_joint.csv", "Sp_train.csv" y "Sp_test.csv".
#'
#' @export
#' @importFrom  magrittr %>%
#' @import scrubr
#' @import mapview
#' @importFrom sf st_as_sf
#' @importFrom sp CRS
#' @importFrom spThin thin
#' @importFrom dplyr filter
#' @importFrom dplyr select
#'
sp_thining_kuenm <- function(data_ocurrencias,
                             lat_col = "decimalLatitude",
                             long_col = "decimalLongitude",
                             especie_col = "scientificName",
                             subcarpeta_sp = NULL, distancia_km = 10, semilla = 123,
                             porc_entrenamiento = 0.75, mapview = TRUE,
                             CRS = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")) {
  # Reducción de puntos cercanos (Thining)
  suppressWarnings(
    spThin::thin(data_ocurrencias,
      lat.col = lat_col,
      long.col = long_col,
      spec.col = especie_col,
      thin.par = distancia_km,
      reps = 10, write.files = TRUE,
      max.files = 1, out.base = "",
      out.dir = paste0(getwd(), "/", subcarpeta_sp), write.log.file = FALSE,
      verbose = TRUE
    )
  )

  # Separar la base en entrenamiento 75% y testeo 25%
  thinned <- read.csv(paste0(subcarpeta_sp, "/", "_thin1.csv"))
  thinned[, especie_col] <- gsub(" ", "_", thinned[, especie_col])

  todo <- unique(thinned)
  todo$check <- paste(todo[, lat_col], todo[, long_col], sep = "_")

  set.seed(semilla)
  entrenamiento <- todo[sample(
    nrow(todo),
    round((length(todo[, 1]) * porc_entrenamiento))
  ), ]
  testeo <- todo[!todo[, 4] %in% entrenamiento[, 4], ]

  todo$check <- NULL
  entrenamiento$check <- NULL
  testeo$check <- NULL

  # Exportación de los archivos de registros
  # con los nombres necesarios para ser usados
  # con el paquete Kuenm (Cobos et al., 2019)
  write.csv(todo, paste0(subcarpeta_sp, "/", "Sp_joint.csv"),
    row.names = FALSE
  )
  write.csv(entrenamiento, paste0(subcarpeta_sp, "/", "Sp_train.csv"),
    row.names = FALSE
  )
  write.csv(testeo, paste0(subcarpeta_sp, "/", "Sp_test.csv"),
    row.names = FALSE
  )

  # Eliminar archivo _thin1.csv
  file.remove(paste0(subcarpeta_sp, "/", "_thin1.csv"))

  # Desplegar mapa con los registros luego del thinning
  if (mapview == TRUE) {
    coord_mapview <- sf::st_as_sf(
      thinned %>%
        dplyr::select(
          especie_col,
          long_col,
          lat_col
        ),
      coords = c(
        long_col,
        lat_col
      ),
      crs = CRS
    )

    mapview::mapview(coord_mapview,
      layer.name = unique(data_ocurrencias[, especie_col])[1]
    )
  }
}
