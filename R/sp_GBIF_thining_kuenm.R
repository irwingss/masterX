#' Descargar y visualizar puntos georreferenciados de presencia de especies desde GBIF
#' @description  Accede al repositorio de GBIF (Global Biodiversity Information
#'      Facility) y realiza la descarga de los registros georreferenciados de
#'      presencia de la especie solicitada. La base de datos descargada puede
#'      ser utilizada para el modelamiento de distribución potencial de la especie.
#'      Además de descargar los datos, la función realiza las siguientes acciones:
#' \itemize{
#'  \item Procedimiento 2: Eliminar registros duplicados.
#'  \item Procedimiento 3: Eliminar registros con coordenadas (0, 0).
#'  \item Procedimiento 4: Guardar los archivos .csv de los registros de ocurrencia en la carpeta de trabajo actual con el nombre "*Especie*_occ_full.csv".
#'  \item Procedimiento 5: Reducción de puntos cercanos (Thining), con la separación mínima entre puntos definida por el argumento **separacion_puntos**. Este proceso está activado por defecto.
#'  \item Procedimiento 6: Guardar los archivos .csv de los registros de ocurrencia completos, y las bases de datos de entrenamiento y testeo en la carpeta de trabajo actual, con los nombres "Sp_joint.csv", "Sp_train.csv" y "Sp_test.csv", respectivamente.
#'  \item Creación de las bases de entrenamiento (75\% de los registros) y testeo (25\% de los registros). Este proceso está activado por defecto.
#' }
#'
#' @param Especie string con el nombre científico de la especie de interés.
#' @param ocu_reg_limite valor numérico que especifíca la cantidad máxima (límite) de registros de ocurrencia descargados desde GBIF para la especie de interés.
#' @param separacion_puntos valor numérico para definir la cantidad de kilómetros a los que deben estar separados los registros de presencia como mínimo (aplicado para el procedimiento de thining).
#' @param semilla valor numérico, define una semilla para la selección.
#' @param porc_entrenamiento valor numérico, que expresa la proporción (0 a 1) de registros destinados a ser guardados en la base de entrenamiento.
#' @param limpieza lógico, TRUE por defecto. Activa el proceso de thining de la
#'    base de datos y exporta tres bases de datos (base completa, la de
#'    entrenamiento con 75\% de los registros, y la de testeo con el 25\% restante).
#' @param mapview lógico, TRUE por defecto. Activa la visualización de los registros completos descargados y limpiados  con mapview en RStudio.
#' @param subcarpeta_sp NULL por defecto. String que determina la subcarpeta donde se guardarán los archivos .csv de ocurrencia para la especie de interés.
#' @param CRS Sistemas de Referencia de Coordenadas. Definido por defecto como "+proj=longlat +ellps=WGS84 +datum=WGS84".
#'
#' @return La función devuelve los archivos de registros con los nombres necesarios para ser usados con el paquete Kuenm (Cobos et al., 2019) con los nombres "Sp_joint.csv", "Sp_train.csv" y "Sp_test.csv".
#'
#' @export
#' @importFrom  magrittr %>%
#' @import rgbif
#' @import scrubr
#' @import mapview
#' @importFrom sf st_as_sf
#' @importFrom sp CRS
#' @importFrom spThin thin
#' @importFrom dplyr filter
#' @importFrom dplyr select
#'
#' @details Confirmación de los nombres científicos:
#' Para corroborar que el nombre científico es correcto, y previsualizar la cantidad de registros disponibles para la especie, visita y realiza la búsqueda en: https://www.gbif.org/
#'
#' @examples
#' # Descarga con la configuración por defecto
#' sp_GBIF_thining_kuenm("Rupicola peruvianus")
#'
#' # Descargar los archivos .csv de registros hacia la subcarpeta de la especie
#' sp_GBIF_thining_kuenm(Especie = "Syndactyla ruficollis",
#' subcarpeta_sp = "Syndactyla ruficollis/")
#'
sp_GBIF_thining_kuenm <- function(Especie, ocu_reg_limite = 5000,
                           separacion_puntos = 10, semilla = 123,
                           porc_entrenamiento = 0.75, limpieza = TRUE,
                           mapview = TRUE, subcarpeta_sp = NULL,
                           CRS = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")) {

  # Descargar la data
  gbif_data <- rgbif::occ_data(
    scientificName = Especie,
    hasCoordinate = TRUE,
    limit = ocu_reg_limite
  )
  # Extraer las ocurrencias
  occurrences <- gbif_data$data

  # Eliminar registros sin coordenadas
  occurrences <- occurrences %>%
    dplyr::filter(!is.na(decimalLongitude) | !is.na(decimalLatitude))

  # Eliminar registros duplicados
  occurrences$code <- paste(occurrences$name, occurrences$decimalLongitude,
    occurrences$decimalLatitude,
    sep = "_"
  )
  occurrences <- occurrences[!duplicated(occurrences$code), ]

  # Eliminar registros con coordenadas (0, 0)
  occurrences <- occurrences %>%
    filter(decimalLongitude != 0 & decimalLatitude != 0) %>%
    mutate(scientificName = Especie)

  # Guardar las ocurrencias
  DF <- apply(occurrences, 2, as.character)

  write.csv(as.data.frame(DF), paste0(subcarpeta_sp, Especie, "_occ_full.csv"),
    row.names = FALSE
  )

  if(limpieza == TRUE){

    # Reducción de puntos cercanos (Thining)
    suppressWarnings(
      spThin::thin(occurrences,
       lat.col = "decimalLatitude",
       long.col = "decimalLongitude",
       spec.col = "scientificName",
       thin.par = separacion_puntos,
       reps = 10, write.files = TRUE,
       max.files = 1, out.base = Especie,
       out.dir = getwd(), write.log.file = FALSE,
       verbose = TRUE
       )
    )

    # Separar la base en entrenamiento 75% y testeo 25%
    occ_thinn <- read.csv(paste0(Especie, "_thin1.csv"))
    occ_thinn$scientificName <- gsub(" ", "_", occ_thinn$scientificName)

    todo <- unique(occ_thinn)
    todo$check <- paste(todo[, 2], todo[, 3], sep = "_")

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
    write.csv(todo, paste0(subcarpeta_sp, "Sp_joint.csv"), row.names = FALSE)
    write.csv(entrenamiento, paste0(subcarpeta_sp, "Sp_train.csv"), row.names = FALSE)
    write.csv(testeo, paste0(subcarpeta_sp, "Sp_test.csv"), row.names = FALSE)

    # Eliminar archivo "Especie_thin1.csv"
    file.remove(paste0(Especie, subcarpeta_sp, "_thin1.csv"))

  }

  if (mapview == TRUE) {
    coord_mapview <<- sf::st_as_sf(
      occurrences %>%
        dplyr::select(
          scientificName,
          decimalLongitude,
          decimalLatitude,
          eventDate,
          country,
          locality,
          elevation
        ),
      coords = c(
        "decimalLongitude",
        "decimalLatitude"
      ),
      crs = CRS
    )

    mapview::mapview(coord_mapview, layer.name = Especie)
  } else if (mapview == FALSE) {
    print("No se solicitó imprimir mapa")
  }
}
