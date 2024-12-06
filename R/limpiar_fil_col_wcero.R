#' Limpieza de Filas o Columnas con Sumatoria Cero
#'
#' Esta función elimina filas o columnas de una matriz o data frame cuya sumatoria es igual a cero
#' (dentro de una tolerancia especificada). Es útil para limpiar datos antes de realizar análisis multivariados.
#'
#' @param data Una matriz o data frame a limpiar.
#' @param by_row Lógico. Si es \code{TRUE}, se eliminarán filas; si es \code{FALSE}, se eliminarán columnas. Por defecto es \code{TRUE}.
#' @param tolerance Umbral de tolerancia para considerar una sumatoria como cero. Por defecto es \code{1e-8}.
#'
#' @return Un objeto de la misma clase que \code{data} con las filas o columnas especificadas eliminadas.
#'
#' @examples
#' # Crear una data frame de ejemplo
#' df <- data.frame(
#'   A = c(1, 0, 3),
#'   B = c(0, 0, 0),
#'   C = c(4, 5, 6)
#' )
#'
#' # Eliminar columnas con sumatoria cero
#' limpiar_fc_cero(df, by_row = FALSE)
#'
#' # Eliminar filas con sumatoria cero
#' limpiar_fc_cero(df, by_row = TRUE)
#'
#' @export
limpiar_fil_col_wcero <- function(data, by_row = TRUE, tolerance = 1e-8) {
  # Verificar si 'data' es una matriz o data frame
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("El argumento 'data' debe ser una matriz o un data frame.")
  }

  # Verificar si la tabla está vacía (sin filas o sin columnas)
  if (nrow(data) == 0 || ncol(data) == 0) {
    message("La tabla está vacía (sin filas o columnas). Se devuelve la tabla original.")
    return(data)
  }

  # Verificar si todos los elementos de la tabla son ceros
  if (all(data == 0)) {
    message("La tabla está completamente llena de ceros. Se devuelve la tabla original.")
    return(data)
  }

  # Función interna para identificar índices a eliminar
  identify_indices <- function(sums, tolerance) {
    which(abs(sums) < tolerance)
  }

  if (by_row) {
    # Analizar filas
    sums <- rowSums(data)
    operacion <- identify_indices(sums, tolerance)

    if (length(operacion) > 0) {
      # Eliminar filas cuya sumatoria es cercana a 0
      cleaned_data <- data[-operacion, , drop = FALSE]
      message(paste("Se eliminaron", length(operacion), "filas con sumatoria igual a 0."))
      return(cleaned_data)
    } else {
      message("Tu tabla no tiene filas que limpiar (sumatoria igual a 0). Se devuelve la tabla original.")
      return(data)
    }
  } else {
    # Analizar columnas
    sums <- colSums(data)
    operacion <- identify_indices(sums, tolerance)

    if (length(operacion) > 0) {
      # Eliminar columnas cuya sumatoria es cercana a 0
      cleaned_data <- data[, -operacion, drop = FALSE]
      message(paste("Se eliminaron", length(operacion), "columnas con sumatoria igual a 0."))
      return(cleaned_data)
    } else {
      message("Tu tabla no tiene columnas que limpiar (sumatoria igual a 0). Se devuelve la tabla original.")
      return(data)
    }
  }
}
