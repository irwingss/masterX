#' Graficar MDS/PCoA con Detección Automática, Agrupamientos Genéricos y Opciones de Overlay
#'
#' Esta función toma un objeto resultante de un análisis MDS (ej. NMDS con `metaMDS`)
#' o PCoA (ej. `wcmdscale`) y genera un gráfico con los dos primeros ejes. Detecta automáticamente
#' el tipo de análisis (PCoA si el objeto tiene `eig`, NMDS en caso contrario).
#'
#' Permite incorporar información de agrupamiento de forma genérica:
#' - Si se proporciona `group_col`, se asume que esta columna en `meta_data` es categórica y se usa para el agrupamiento.
#' - Si no se proporciona `group_col` pero sí `numeric_col`, se transformará esa columna numérica en categorías
#'   según los cortes especificados en `breaks` y las etiquetas en `labels`.
#' - Si no se proporciona ninguna de las dos, no habrá agrupamiento.
#'
#' También se puede añadir una capa de superposición (`overlay_type`) sobre los puntos:
#' - "none": Ninguna capa adicional.
#' - "ellipses": Añade elipses por grupo.
#' - "density": Añade densidad 2D por grupo.
#'
#' @param object Objeto resultante de `vegan::wcmdscale` (PCoA) o `vegan::metaMDS` (NMDS).
#' @param titulo Título del gráfico.
#' @param meta_data (Opcional) Data frame con información adicional de las muestras. Debe tener el mismo número y orden de filas que `object` o rownames coincidentes.
#' @param group_col (Opcional) Nombre de la columna en `meta_data` que ya es categórica y se usará para agrupar.
#' @param numeric_col (Opcional) Nombre de la columna numérica en `meta_data` a partir de la cual se crearán categorías utilizando `breaks` y `labels`.
#' @param breaks (Opcional) Vector numérico de puntos de corte para `cut()`. Por defecto `c(-Inf, 100, 200, Inf)`.
#' @param labels (Opcional) Vector de etiquetas para las categorías. Debe tener longitud igual a `length(breaks)-1`.
#'                Por defecto `c("<100","100-200",">200")`.
#' @param overlay_type (Opcional) "none", "ellipses" o "density" para añadir capas sobre los puntos. Por defecto "none".
#'
#' @return Un objeto ggplot2 con el gráfico MDS/PCoA.
#'
#' @examples
#' \dontrun{
#' # PCoA con una columna categórica directa:
#' plot_mds(object = pcoa_obj, titulo = "Ejemplo PCoA", meta_data = meta_df, group_col = "Region")
#'
#' # NMDS con una columna numérica que se transformará en categorías:
#' plot_mds(object = nmds_obj, titulo = "Ejemplo NMDS", meta_data = meta_df,
#'          numeric_col = "TamañoParche", breaks = c(-Inf, 50, 150, Inf), labels = c("<50", "50-150", ">150"))
#'
#' # Sin agrupación:
#' plot_mds(object = pcoa_obj, titulo = "Sin grupos", meta_data = meta_df, overlay_type = "density")
#' }
#'
#' @importFrom vegan scores
#' @importFrom ggplot2 ggplot aes geom_point theme_minimal labs stat_ellipse stat_density_2d
#' @importFrom dplyr mutate
#' @export
plot_mds <- function(object,
                     titulo = NULL,
                     meta_data = NULL,
                     group_col = NULL,
                     numeric_col = NULL,
                     breaks = c(-Inf, 100, 200, Inf),
                     labels = c("<100","100-200",">200"),
                     overlay_type = "none") {

  # Extraer scores de sitios
  pcoa_scores <- vegan::scores(object, display = "sites")

  if (!is.data.frame(pcoa_scores)) {
    pcoa_scores <- as.data.frame(pcoa_scores)
  }

  # Detectar si es PCoA (tiene eig) o NMDS (no tiene eig)
  is_pcoa <- !is.null(object$eig)

  if (is_pcoa) {
    # PCoA
    pcoa_scores <- pcoa_scores[, 1:2, drop = FALSE]
    colnames(pcoa_scores) <- c("PCoA1", "PCoA2")
    eig <- object$eig
    var_explained <- eig / sum(eig) * 100
    var1 <- round(var_explained[1], 1)
    var2 <- round(var_explained[2], 1)
    xlab_text <- paste0("PCoA1 (", var1, "%)")
    ylab_text <- paste0("PCoA2 (", var2, "%)")
  } else {
    # NMDS
    pcoa_scores <- pcoa_scores[, 1:2, drop = FALSE]
    colnames(pcoa_scores) <- c("NMDS1", "NMDS2")
    xlab_text <- "NMDS1"
    ylab_text <- "NMDS2"
  }

  # Determinar variable de agrupación
  grouping_var <- NULL
  if (!is.null(meta_data)) {
    if (nrow(meta_data) != nrow(pcoa_scores)) {
      warning("El número de filas en 'meta_data' no coincide con el número de sitios en 'object'. Verifique el orden.")
    }

    # Prioridad: group_col > numeric_col
    if (!is.null(group_col) && group_col %in% names(meta_data)) {
      grouping_var <- group_col
    } else if (!is.null(numeric_col) && numeric_col %in% names(meta_data)) {
      # Transformar la columna numérica en categorías
      meta_data[[paste0(numeric_col, "_cat")]] <- cut(meta_data[[numeric_col]], breaks = breaks, labels = labels, include.lowest = TRUE)
      grouping_var <- paste0(numeric_col, "_cat")
    }

    pcoa_scores <- cbind(pcoa_scores, meta_data)
  }

  # Construcción del gráfico base
  p <- ggplot(data = pcoa_scores, aes_string(x = colnames(pcoa_scores)[1], y = colnames(pcoa_scores)[2]))

  # Añadir estéticas si hay variable de agrupación
  if (!is.null(grouping_var)) {
    p <- p + aes_string(color = grouping_var, fill = grouping_var, shape = grouping_var)
  }

  # Añadir capas sobre los puntos según overlay_type
  if (overlay_type == "ellipses") {
    p <- p + stat_ellipse(geom = "polygon", alpha = 0.2)
  } else if (overlay_type == "density") {
    p <- p + stat_density_2d(geom = "polygon", alpha = 0.07, aes(alpha = after_stat(level)))
  }

  # Añadir puntos
  p <- p + geom_point(size = 3, alpha = 0.8)

  # Tema minimal
  p <- p + theme_minimal()

  # Etiquetas de ejes y título
  p <- p + labs(
    title = titulo,
    x = xlab_text,
    y = ylab_text
  )

  return(p)
}
