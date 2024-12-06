#' Limpieza de Filas o Columnas con Sumatoria Cero (Ampliada)
#'
#' Esta función elimina filas o columnas de una matriz o data frame cuya sumatoria es igual a cero
#' (dentro de una tolerancia especificada). Es útil para limpiar datos antes de realizar análisis multivariados.
#'
#' Además, se han agregado dos modos:
#' - \code{fila_solo_numerico = TRUE}: Si la tabla es mixta (contiene columnas numéricas y categóricas),
#'   se calcularán las sumas únicamente sobre las columnas numéricas. Las filas o columnas con sumatoria cero,
#'   considerando únicamente las columnas numéricas, serán eliminadas. La tabla resultante conservará el resto
#'   de las columnas (incluyendo las categóricas).
#'
#' - \code{matriz_mixta = TRUE}: Ignora la lógica de sumatorias y simplemente devuelve la tabla filtrada por
#'   \code{complete.cases}, es decir, solo las filas completas (sin valores faltantes).
#'   Este modo se prioriza sobre la limpieza por sumatorias.
#'
#' @param data Una matriz o data frame a limpiar.
#' @param by_row Lógico. Si es \code{TRUE}, se eliminarán filas; si es \code{FALSE}, se eliminarán columnas. Por defecto es \code{TRUE}.
#' @param tolerance Umbral de tolerancia para considerar una sumatoria como cero. Por defecto es \code{1e-8}.
#' @param fila_solo_numerico Lógico. Si es \code{TRUE}, al calcular sumatorias solo se considerarán las columnas numéricas.
#'                          Por defecto es \code{FALSE}.
#' @param matriz_mixta Lógico. Si es \code{TRUE}, se asume que la tabla es mixta y se ignoran sumatorias, filtrando solo por \code{complete.cases}.
#'                     Por defecto es \code{FALSE}.
#'
#' @return Un objeto de la misma clase que \code{data} con las filas o columnas especificadas eliminadas.
#'
#' @examples
#' # Data frame con datos mixtos
#' df <- data.frame(
#'   A = c(1, 0, 3),
#'   B = c(0, 0, 0),
#'   C = c(4, 5, 6),
#'   D = factor(c("a", "b", "b"))
#' )
#'
#' # Modo original: eliminar columnas con sumatoria cero (considerando todas las columnas numéricas)
#' limpiar_fil_col_wcero(df, by_row = FALSE)
#'
#' # Modo fila_solo_numerico = TRUE (solo se consideran columnas numéricas para sumatorias)
#' # Eliminar filas con sumatoria numérica cero
#' limpiar_fil_col_wcero(df, by_row = TRUE, fila_solo_numerico = TRUE)
#'
#' # Modo matriz_mixta = TRUE (ignora sumatorias y devuelve solo las filas completas)
#' df_mixta <- data.frame(
#'   A = c(1, NA, 3),
#'   B = c(2, 2, 2),
#'   C = factor(c("x", "y", NA))
#' )
#' limpiar_fil_col_wcero(df_mixta, matriz_mixta = TRUE)
#'
#' @export
limpiar_fil_col_wcero <- function(data, by_row = TRUE, tolerance = 1e-8,
                                  fila_solo_numerico = FALSE, matriz_mixta = FALSE) {
  # Función interna para identificar índices a eliminar
  identify_indices <- function(sums, tolerance) {
    which(abs(sums) < tolerance)
  }

  # Caso 1: Si la matriz es mixta (matriz_mixta = TRUE), ignoramos sumatorias y filtramos por complete.cases
  if (matriz_mixta) {
    # Filtrar filas completas
    new_data <- data[complete.cases(data), , drop = FALSE]
    message("Se devolvió la tabla considerando solo filas completas (complete.cases), ignorando sumatorias.")
    return(new_data)
  }

  # Caso 2: Si no es matriz mixta, aplicamos la lógica de sumatorias

  # Verificar si 'data' es una matriz o data frame
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("El argumento 'data' debe ser una matriz o un data frame.")
  }

  # Verificar si la tabla está vacía (sin filas o sin columnas)
  if (nrow(data) == 0 || ncol(data) == 0) {
    message("La tabla está vacía (sin filas o columnas). Se devuelve la tabla original.")
    return(data)
  }

  # Verificar si todos los elementos de la tabla son ceros (en las columnas numéricas si fila_solo_numerico=TRUE)
  if (fila_solo_numerico) {
    # Extraer solo las columnas numéricas
    numeric_data <- data[sapply(data, is.numeric)]
    # Si no hay columnas numéricas, no hay criterio de sumatoria que aplicar
    if (ncol(numeric_data) == 0) {
      message("No se encontraron columnas numéricas. Se devuelve la tabla original.")
      return(data)
    }
    if (all(numeric_data == 0)) {
      message("La tabla está completamente llena de ceros en las columnas numéricas. Se devuelve la tabla original.")
      return(data)
    }
  } else {
    # Modo original (sin fila_solo_numerico)
    if (all(data == 0)) {
      message("La tabla está completamente llena de ceros. Se devuelve la tabla original.")
      return(data)
    }
  }

  # Caso 2.1: Fila solo numérico = TRUE
  if (fila_solo_numerico) {
    numeric_data <- data[sapply(data, is.numeric)]
    if (by_row) {
      # Analizar filas sobre columnas numéricas
      sums <- rowSums(numeric_data)
      operacion <- identify_indices(sums, tolerance)

      if (length(operacion) > 0) {
        # Eliminar filas cuya sumatoria numérica es cercana a 0
        cleaned_data <- data[-operacion, , drop = FALSE]
        message(paste("Se eliminaron", length(operacion), "filas con sumatoria numérica igual a 0."))
        return(cleaned_data)
      } else {
        message("Tu tabla no tiene filas numéricas que limpiar (sumatoria igual a 0). Se devuelve la tabla original.")
        return(data)
      }
    } else {
      # Analizar columnas sobre columnas numéricas
      sums <- colSums(numeric_data)
      operacion <- identify_indices(sums, tolerance)

      if (length(operacion) > 0) {
        # Eliminar columnas numéricas cuya sumatoria es cercana a 0
        numeric_col_names <- colnames(numeric_data)[operacion]
        cleaned_data <- data[, !(colnames(data) %in% numeric_col_names), drop = FALSE]
        message(paste("Se eliminaron", length(operacion), "columnas numéricas con sumatoria igual a 0."))
        return(cleaned_data)
      } else {
        message("Tu tabla no tiene columnas numéricas que limpiar (sumatoria igual a 0). Se devuelve la tabla original.")
        return(data)
      }
    }

  } else {
    # Caso 2.2: Lógica original (fila_solo_numerico = FALSE)
    if (by_row) {
      # Analizar filas
      sums <- rowSums(data)
      operacion <- identify_indices(sums, tolerance)

      if (length(operacion) > 0) {
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
        cleaned_data <- data[, -operacion, drop = FALSE]
        message(paste("Se eliminaron", length(operacion), "columnas con sumatoria igual a 0."))
        return(cleaned_data)
      } else {
        message("Tu tabla no tiene columnas que limpiar (sumatoria igual a 0). Se devuelve la tabla original.")
        return(data)
      }
    }
  }
}
