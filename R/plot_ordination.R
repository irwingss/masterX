#' Graficar Análisis de Ordenación con Opciones Avanzadas
#'
#' Esta función permite visualizar objetos resultantes de análisis de ordenación como RDA (Redundancy Analysis),
#' CCA (Canonical Correspondence Analysis) y db-RDA (Distance-based RDA). Ofrece opciones para personalizar la
#' visualización de sitios, variables explicativas y predictoras, así como la adición de capas adicionales
#' como elipses y densidad 2D.
#'
#' @param object Objeto resultante de `vegan::rda`, `vegan::cca` o `vegan::capscale`.
#' @param titulo (Opcional) Título del gráfico.
#' @param scaling Tipo de escalado a utilizar ("sites", "species", etc.). Por defecto "sites".
#' @param lc (Opcional) Lógico. Si `TRUE`, utiliza scores de "lc" (linear combination); si `FALSE`, utiliza scores de "wa" (weighted averages). Por defecto `FALSE`.
#' @param sitios_nombres (Opcional) Vector de cadenas. Puede contener "point" para añadir puntos y/o "text" para añadir etiquetas a los sitios. Por defecto `FALSE`.
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
#' @param repel_sitios (Opcional) Lógico. Si `TRUE`, usa `geom_text_repel` para las etiquetas de texto de los sitios. Requiere el paquete `ggrepel`. Por defecto `FALSE`.
#' @param repel_var (Opcional) Lógico. Si `TRUE`, usa `geom_text_repel` para las etiquetas de texto de las variables (Yvar y Xvar). Requiere el paquete `ggrepel`. Por defecto `FALSE`.
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
#' @return Un objeto `ggplot2` con el gráfico de ordenación.
#'
#' @examples
#' \dontrun{
#' library(vegan)
#' library(ggrepel)
#' data(dune)
#' data(dune.env)
#'
#' # Análisis RDA
#' rda_obj <- rda(dune ~ ., data = dune.env)
#'
#' # Análisis CCA
#' cca_obj <- cca(dune ~ ., data = dune.env)
#'
#' # Análisis db-RDA (capscale)
#' db_rda_obj <- capscale(dune ~ ., data = dune.env, distance = "bray")
#'
#' # Graficar RDA con etiquetas de sitios y segmentos para Yvar
#' plot_ordination(rda_obj, titulo = "RDA de Dune", sitios_nombres = c("point", "text"), segmentos_Yvar = TRUE)
#'
#' # Graficar CCA con overlay de elipses
#' plot_ordination(cca_obj, titulo = "CCA de Dune", overlay_type = "ellipses", meta_data = dune.env, group_col = "Management")
#'
#' # Graficar db-RDA con etiquetas repelentes para sitios y variables
#' plot_ordination(db_rda_obj, titulo = "db-RDA de Dune", sitios_nombres = c("point", "text"),
#'                 segmentos_Yvar = TRUE, repel_sitios = TRUE, repel_var = TRUE)
#' }
#'
#' @importFrom vegan scores
#' @importFrom ggplot2 ggplot aes geom_point geom_text geom_segment geom_hline geom_vline theme_minimal labs stat_ellipse stat_density_2d
#' @importFrom ggrepel geom_text_repel
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @export
plot_ordination <- function(object,
                            titulo = NULL,
                            scaling = "sites",
                            lc = FALSE,
                            sitios_nombres = "point",
                            segmentos_Yvar = FALSE,
                            overlay_type = "none",
                            meta_data = NULL,
                            group_col = NULL,
                            numeric_col = NULL,
                            breaks = c(-Inf, 100, 200, Inf),
                            labels = c("<100","100-200",">200"),
                            repel_sitios = FALSE,
                            repel_var = FALSE,
                            color_Yvar = "black",
                            color_Xvar = "red",
                            color_sitios = "blue",
                            alpha_segments_Yvar = 0.5,
                            alpha_segments_Xvar = 0.5,
                            alpha_sitios = 0.5,
                            size_sitios = 1.5,
                            size_Yvar = 2,
                            size_Xvar = 2) {

  # Cargar paquetes necesarios
  if ((!requireNamespace("ggrepel", quietly = TRUE) && (repel_sitios || repel_var))) {
    stop("El paquete 'ggrepel' es necesario para usar 'repel_sitios = TRUE' o 'repel_var = TRUE'. Instálalo con install.packages('ggrepel').")
  }

  # Validar el objeto
  if (!inherits(object, c("rda", "cca", "capscale"))) {
    stop("El objeto proporcionado no es de clase 'rda', 'cca' o 'capscale'. Por favor, use un objeto adecuado de 'vegan'.")
  }

  # Función para obtener etiquetas de ejes basadas en eigenvalores
  get_axis_labels <- function(obj, axis1 = 1, axis2 = 2) {
    if (!is.null(obj$CCA$eig)) {
      eig <- obj$CCA$eig
      tot_eig <- sum(eig)
      percent1 <- round(eig[axis1] / tot_eig * 100, 1)
      percent2 <- round(eig[axis2] / tot_eig * 100, 1)
      label1 <- paste0("Axis ", axis1, " (", percent1, "%)")
      label2 <- paste0("Axis ", axis2, " (", percent2, "%)")
    } else if (!is.null(obj$CA$eig)) {
      eig <- obj$CA$eig
      tot_eig <- sum(eig)
      percent1 <- round(eig[axis1] / tot_eig * 100, 1)
      percent2 <- round(eig[axis2] / tot_eig * 100, 1)
      label1 <- paste0("Axis ", axis1, " (", percent1, "%)")
      label2 <- paste0("Axis ", axis2, " (", percent2, "%)")
    } else {
      label1 <- paste0("Dim ", axis1)
      label2 <- paste0("Dim ", axis2)
    }
    return(c(label1, label2))
  }

  # Obtener etiquetas de ejes
  axis_labels <- get_axis_labels(object)

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
      categoria_col <- paste0(numeric_col, "_cat")
      meta_data <- meta_data %>%
        mutate(!!categoria_col := cut(.data[[numeric_col]], breaks = breaks, labels = labels, include.lowest = TRUE))
      grouping_var <- categoria_col
    }

    sitios_df <- cbind(sitios_df, meta_data)
    Yvar_df <- cbind(Yvar_df, meta_data)
    Xvar_df <- cbind(Xvar_df, meta_data)
  }

  # Construcción del gráfico base
  p <- ggplot()

  # Añadir capas según overlay_type
  if (overlay_type == "ellipses" && !is.null(grouping_var)) {
    p <- p + stat_ellipse(data = sitios_df,
                          aes(x = Dim1, y = Dim2, color = .data[[grouping_var]], fill = .data[[grouping_var]]),
                          geom = "polygon", alpha = 0.2)
  } else if (overlay_type == "density" && !is.null(grouping_var)) {
    p <- p + stat_density_2d(data = sitios_df,
                             aes(x = Dim1, y = Dim2, color = .data[[grouping_var]], fill = .data[[grouping_var]], alpha = after_stat(level)),
                             geom = "polygon")
  }

  # Añadir puntos de sitios
  if (is.character(sitios_nombres) && "point" %in% sitios_nombres) {
    p <- p + geom_point(data = sitios_df,
                        aes(x = Dim1, y = Dim2, color = if (!is.null(grouping_var)) .data[[grouping_var]] else NULL),
                        shape = 16, size = size_sitios, alpha = alpha_sitios)
  }

  # Añadir etiquetas de sitios
  if (is.character(sitios_nombres) && "text" %in% sitios_nombres) {
    if (repel_sitios) {
      p <- p + ggrepel::geom_text_repel(data = sitios_df,
                                        aes(x = Dim1, y = Dim2, label = rownames(sitios_df)),
                                        color = color_sitios, size = size_sitios)
    } else {
      p <- p + geom_text(data = sitios_df,
                         aes(x = Dim1, y = Dim2, label = rownames(sitios_df)),
                         color = color_sitios, size = size_sitios, nudge_y = 0.2)
    }
  }

  # Añadir segmentos para Yvar si aplica
  if (segmentos_Yvar) {
    p <- p + geom_segment(data = Yvar_df,
                          aes(x = 0, y = 0, xend = Dim1, yend = Dim2),
                          color = color_Yvar, alpha = alpha_segments_Yvar,
                          arrow = arrow(length = unit(2, "mm"), type = "closed"))

    # Añadir etiquetas para Yvar
    if (repel_var) {
      p <- p + ggrepel::geom_text_repel(data = Yvar_df,
                                        aes(x = Dim1, y = Dim2, label = label),
                                        color = color_Yvar, size = size_Yvar)
    } else {
      p <- p + geom_text(data = Yvar_df,
                         aes(x = Dim1, y = Dim2, label = label),
                         color = color_Yvar, size = size_Yvar, nudge_y = 0.2)
    }
  } else {
    # Añadir etiquetas para Yvar sin segmentos
    if (repel_var) {
      p <- p + ggrepel::geom_text_repel(data = Yvar_df,
                                        aes(x = Dim1, y = Dim2, label = label),
                                        color = color_Yvar, size = size_Yvar)
    } else {
      p <- p + geom_text(data = Yvar_df,
                         aes(x = Dim1, y = Dim2, label = label),
                         color = color_Yvar, size = size_Yvar, nudge_y = 0.2)
    }
  }

  # Añadir segmentos para Xvar si aplica
  if (segmentos_Yvar) {  # Nota: `segmentos_Yvar` también controla Xvar
    p <- p + geom_segment(data = Xvar_df,
                          aes(x = 0, y = 0, xend = Dim1, yend = Dim2),
                          color = color_Xvar, alpha = alpha_segments_Xvar,
                          arrow = arrow(length = unit(2, "mm"), type = "closed"))

    # Añadir etiquetas para Xvar
    if (repel_var) {
      p <- p + ggrepel::geom_text_repel(data = Xvar_df,
                                        aes(x = Dim1, y = Dim2, label = label),
                                        color = color_Xvar, size = size_Xvar)
    } else {
      p <- p + geom_text(data = Xvar_df,
                         aes(x = Dim1, y = Dim2, label = label),
                         color = color_Xvar, size = size_Xvar, nudge_y = 0.2)
    }
  } else {
    # Añadir etiquetas para Xvar sin segmentos
    if (repel_var) {
      p <- p + ggrepel::geom_text_repel(data = Xvar_df,
                                        aes(x = Dim1, y = Dim2, label = label),
                                        color = color_Xvar, size = size_Xvar)
    } else {
      p <- p + geom_text(data = Xvar_df,
                         aes(x = Dim1, y = Dim2, label = label),
                         color = color_Xvar, size = size_Xvar, nudge_y = 0.2)
    }
  }

  # Añadir líneas de referencia
  p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray70")

  # Aplicar temas y etiquetas
  p <- p + theme_minimal() +
    labs(
      title = titulo,
      x = axis_labels[1],
      y = axis_labels[2],
      color = if (!is.null(grouping_var)) grouping_var else NULL,
      fill = if (!is.null(grouping_var)) grouping_var else NULL
    )

  return(p)
}
