#' Graficar Análisis RDA con Opciones Avanzadas
#'
#' Esta función permite visualizar objetos resultantes de un análisis RDA (Redundancy Analysis).
#' Ofrece opciones para personalizar la visualización de sitios, variables explicativas y predictoras,
#' así como la adición de capas adicionales como elipses y densidad 2D.
#'
#' @param object Objeto resultante de `vegan::rda`.
#' @param titulo (Opcional) Título del gráfico.
#' @param scaling Tipo de escalado a utilizar ("sites", "species", etc.). Por defecto "sites".
#' @param lc (Opcional) Lógico. Si `TRUE`, utiliza scores de "lc" (linear combination); si `FALSE`, utiliza scores de "wa" (weighted averages). Por defecto `FALSE`.
#' @param sitios_nombres (Opcional) Lógico. Si `TRUE`, añade etiquetas a los sitios (filas). Por defecto `FALSE`.
#' @param segmentos_Yvar (Opcional) Lógico. Si `TRUE`, dibuja segmentos para las variables explicativas (Yvar, columnas de la matriz Y). Por defecto `FALSE`.
#' @param overlay_type (Opcional) Tipo de capa a añadir sobre los puntos:
#'   - "none": Ninguna capa adicional.
#'   - "ellipses": Añade elipses por grupo.
#'   - "density": Añade densidad 2D por grupo.
#'   Por defecto "none".
#' @param meta_data (Opcional) Data frame con información adicional de las muestras. Debe tener el mismo número y orden de filas que las muestras en `object`.
#' @param group_col (Opcional) Nombre de la columna en `meta_data` que ya es categórica y se usará para agrupar.
#' @param numeric_col (Opcional) Nombre de la columna numérica en `meta_data` para crear categorías.
#' @param breaks (Opcional) Vector numérico de puntos de corte para `cut()`. Por defecto `c(-Inf, 100, 200, Inf)`.
#' @param labels (Opcional) Vector de etiquetas para las categorías. Por defecto `c("<100","100-200",">200")`.
#' @param repel (Opcional) Lógico. Si `TRUE`, usa `geom_text_repel` para las etiquetas de texto. Requiere el paquete `ggrepel`. Por defecto `FALSE`.
#' @param color_Yvar (Opcional) Color para las variables explicativas (Yvar). Por defecto `"#fc4903"`.
#' @param color_Xvar (Opcional) Color para las variables predictoras (Xvar). Por defecto `"#9500e6"`.
#' @param color_sitios (Opcional) Color para los puntos de los sitios. Por defecto `"#03a9fc"`.
#' @param alpha_segments_Yvar (Opcional) Transparencia para los segmentos de Yvar. Por defecto `0.5`.
#' @param alpha_segments_Xvar (Opcional) Transparencia para los segmentos de Xvar. Por defecto `0.5`.
#' @param alpha_sitios (Opcional) Transparencia para los puntos de los sitios. Por defecto `0.5`.
#' @param size_sitios (Opcional) Tamaño de los puntos de los sitios. Por defecto `3`.
#' @param size_Yvar (Opcional) Tamaño de las etiquetas de Yvar. Por defecto `3`.
#' @param size_Xvar (Opcional) Tamaño de las etiquetas de Xvar. Por defecto `3`.
#'
#' @return Un objeto `ggplot2` con el gráfico RDA.
#'
#' @examples
#' \dontrun{
#' library(vegan)
#' data(dune)
#' data(dune.env)
#'
#' # Análisis RDA
#' rda_obj <- rda(dune ~ ., data = dune.env)
#'
#' # Graficar RDA con etiquetas de sitios y segmentos para Yvar
#' plot_RDA(rda_obj, titulo = "RDA de Dune", sitios_nombres = TRUE, segmentos_Yvar = TRUE)
#'
#' # Graficar RDA con overlay de densidad 2D
#' plot_RDA(rda_obj, titulo = "RDA con Densidad", overlay_type = "density", meta_data = dune.env, group_col = "Management")
#' }
#'
#' @importFrom vegan scores
#' @importFrom ggplot2 ggplot aes geom_point geom_text geom_segment geom_hline geom_vline theme_minimal labs stat_ellipse stat_density_2d
#' @importFrom ggrepel geom_text_repel
#' @importFrom dplyr mutate
#' @export
plot_RDA <- function(object,
                     titulo = NULL,
                     scaling = "sites",
                     lc = FALSE,
                     sitios_nombres = FALSE,
                     segmentos_Yvar = FALSE,
                     overlay_type = "none",
                     meta_data = NULL,
                     group_col = NULL,
                     numeric_col = NULL,
                     breaks = c(-Inf, 100, 200, Inf),
                     labels = c("<100","100-200",">200"),
                     repel = FALSE,
                     color_Yvar = "red",
                     color_Xvar = "blue",
                     color_sitios = "black",
                     alpha_segments_Yvar = 0.5,
                     alpha_segments_Xvar = 0.5,
                     alpha_sitios = 0.5,
                     size_sitios = 3,
                     size_Yvar = 3,
                     size_Xvar = 3) {

  # Cargar paquetes necesarios
  if (!requireNamespace("ggrepel", quietly = TRUE) && repel) {
    stop("El paquete 'ggrepel' es necesario para usar 'repel = TRUE'. Instálalo con install.packages('ggrepel').")
  }

  # Validar el objeto
  if (!inherits(object, "rda")) {
    stop("El objeto proporcionado no es de clase 'rda'. Por favor, use un objeto adecuado.")
  }

  # Extraer scores de sitios
  sitios_scores <- vegan::scores(object, display = ifelse(lc, "lc", "wa"), choices = 1:2, scaling = scaling)
  sitios_df <- as.data.frame(sitios_scores)
  colnames(sitios_df) <- c("Dim1", "Dim2")

  # Extraer scores de variables explicativas (Yvar)
  Yvar_scores <- vegan::scores(object, display = "species", choices = 1:2, scaling = scaling)
  Yvar_df <- as.data.frame(Yvar_scores)
  colnames(Yvar_df) <- c("Dim1", "Dim2")
  Yvar_df$label <- rownames(Yvar_df)

  # Extraer scores de variables predictoras (Xvar)
  Xvar_scores <- vegan::scores(object, display = "bp", choices = 1:2, scaling = scaling)
  Xvar_df <- as.data.frame(Xvar_scores)
  colnames(Xvar_df) <- c("Dim1", "Dim2")
  Xvar_df$label <- rownames(Xvar_df)

  # Determinar variable de agrupación
  grouping_var <- NULL
  if (!is.null(meta_data)) {
    if (nrow(meta_data) != nrow(sitios_df)) {
      warning("El número de filas en 'meta_data' no coincide con el número de sitios en 'object'. Verifique el orden.")
    }

    # Prioridad: group_col > numeric_col
    if (!is.null(group_col) && group_col %in% names(meta_data)) {
      grouping_var <- group_col
    } else if (!is.null(numeric_col) && numeric_col %in% names(meta_data)) {
      # Transformar la columna numérica en categorías
      meta_data <- meta_data %>%
        mutate("{paste0(numeric_col, '_cat')}" := cut(.data[[numeric_col]], breaks = breaks, labels = labels, include.lowest = TRUE))
      grouping_var <- paste0(numeric_col, "_cat")
    }

    sitios_df <- cbind(sitios_df, meta_data)
    Yvar_df <- cbind(Yvar_df, meta_data)
    Xvar_df <- cbind(Xvar_df, meta_data)
  }

  # Construcción del gráfico base
  p <- ggplot()

  # Añadir capas según overlay_type
  if (overlay_type == "ellipses" && !is.null(grouping_var)) {
    p <- p + stat_ellipse(data = sitios_df, aes(x = Dim1, y = Dim2, color = .data[[grouping_var]], fill = .data[[grouping_var]]), geom = "polygon", alpha = 0.2)
  } else if (overlay_type == "density" && !is.null(grouping_var)) {
    p <- p + stat_density_2d(data = sitios_df, aes(x = Dim1, y = Dim2, color = .data[[grouping_var]], fill = .data[[grouping_var]], alpha = after_stat(level)), geom = "polygon")
  }

  # Añadir puntos de sitios
  if ("point" %in% sitios_nombres) {
    p <- p + geom_point(data = sitios_df, aes(x = Dim1, y = Dim2, color = if (!is.null(grouping_var)) .data[[grouping_var]] else NULL),
                        shape = 16, color = color_sitios, size = size_sitios, alpha = alpha_sitios)
  }

  # Añadir etiquetas de sitios
  if ("text" %in% sitios_nombres) {
    if (repel) {
      p <- p + ggrepel::geom_text_repel(data = sitios_df, aes(x = Dim1, y = Dim2, label = rownames(sitios_df)), color = color_sitios, size = size_sitios)
    } else {
      p <- p + geom_text(data = sitios_df, aes(x = Dim1, y = Dim2, label = rownames(sitios_df)), color = color_sitios, size = size_sitios, nudge_y = 0.2)
    }
  }

  # Añadir segmentos para Yvar si aplica
  if (segmentos_Yvar) {
    p <- p + geom_segment(data = Yvar_df, aes(x = 0, y = 0, xend = Dim1, yend = Dim2), color = color_Yvar, alpha = alpha_segments_Yvar,
                          arrow = arrow(length = unit(2, "mm"), type = "closed"))

    # Añadir etiquetas para Yvar
    p <- p + geom_text_repel(data = Yvar_df, aes(x = Dim1, y = Dim2, label = label), color = color_Yvar, size = size_Yvar)
  } else {
    # Añadir etiquetas para Yvar sin segmentos
    p <- p + geom_text_repel(data = Yvar_df, aes(x = Dim1, y = Dim2, label = label), color = color_Yvar, size = size_Yvar)
  }

  # Añadir segmentos para Xvar si aplica
  if (segmentos_Yvar) {
    p <- p + geom_segment(data = Xvar_df, aes(x = 0, y = 0, xend = Dim1, yend = Dim2), color = color_Xvar, alpha = alpha_segments_Xvar,
                          arrow = arrow(length = unit(2, "mm"), type = "closed"))

    # Añadir etiquetas para Xvar
    p <- p + geom_text_repel(data = Xvar_df, aes(x = Dim1, y = Dim2, label = label), color = color_Xvar, size = size_Xvar)
  } else {
    # Añadir etiquetas para Xvar sin segmentos
    p <- p + geom_text_repel(data = Xvar_df, aes(x = Dim1, y = Dim2, label = label), color = color_Xvar, size = size_Xvar)
  }

  # Añadir líneas de referencia
  p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray70")

  # Aplicar temas y etiquetas
  p <- p + theme_minimal() +
    labs(
      title = titulo,
      x = if (scaling == "sites" && is_pcoa) paste0("PCoA1 (", round(object$eig[1] / sum(object$eig) * 100, 1), "%)") else "Dim1",
      y = if (scaling == "sites" && is_pcoa) paste0("PCoA2 (", round(object$eig[2] / sum(object$eig) * 100, 1), "%)") else "Dim2",
      color = if (!is.null(grouping_var)) grouping_var else NULL,
      fill = if (!is.null(grouping_var)) grouping_var else NULL
    )

  return(p)
}
