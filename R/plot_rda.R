#' Graficar Análisis de Ordenación (RDA, CCA, dbRDA) con Opciones Avanzadas
#'
#' Esta función permite visualizar objetos resultantes de análisis de ordenación como RDA, CCA, dbRDA, NMDS, PCoA, entre otros.
#' Detecta automáticamente el tipo de análisis y ajusta la visualización en consecuencia.
#'
#' @param object Objeto resultante de `vegan::rda`, `vegan::cca`, `vegan::capscale`, `vegan::metaMDS`, `vegan::wcmdscale`, etc.
#' @param titulo (Opcional) Título del gráfico.
#' @param scaling Tipo de escalado a utilizar. Depende del análisis (por ejemplo, "sites" para RDA/CCA).
#' @param lc (Opcional) Lógico. Si `TRUE`, utiliza scores de "lc" (contraposición lineal); si `FALSE`, utiliza scores de "wa" (ponderados). Por defecto `FALSE`.
#' @param sitios_nombres (Opcional) Lógico. Si `TRUE`, añade etiquetas a los sitios. Por defecto `FALSE`.
#' @param segmentos_Yvar (Opcional) Lógico. Si `TRUE`, dibuja segmentos para las variables explicativas (ejes Y). Por defecto `FALSE`.
#' @param segmentos_Xvar (Opcional) Lógico. Si `TRUE`, dibuja segmentos para las variables predictoras (ejes X). Por defecto `FALSE`.
#' @param sitios (Opcional) Vector con "point", "text", o ambos. Controla la representación de los sitios. Por defecto `c("point")`.
#' @param repel (Opcional) Lógico. Si `TRUE`, usa `geom_text_repel` para las etiquetas de texto. Requiere el paquete `ggrepel`. Por defecto `FALSE`.
#' @param color_Yvar (Opcional) Color para las variables explicativas (ejes Y). Por defecto `"#fc4903"`.
#' @param color_Xvar (Opcional) Color para las variables predictoras (ejes X). Por defecto `"#9500e6"`.
#' @param color_sitios (Opcional) Color para los puntos de los sitios. Por defecto `"#03a9fc"`.
#' @param alpha_segments_Yvar (Opcional) Transparencia para los segmentos de variables Y. Por defecto `0.5`.
#' @param alpha_segments_Xvar (Opcional) Transparencia para los segmentos de variables X. Por defecto `0.5`.
#' @param alpha_sitios (Opcional) Transparencia para los puntos de los sitios. Por defecto `0.5`.
#' @param size_sitios (Opcional) Tamaño de los puntos de los sitios. Por defecto `3`.
#' @param size_Yvar (Opcional) Tamaño de las etiquetas de las variables Y. Por defecto `3`.
#' @param size_Xvar (Opcional) Tamaño de las etiquetas de las variables X. Por defecto `3`.
#'
#' @return Un objeto `ggplot2` con el gráfico de ordenación.
#'
#' @examples
#' \dontrun{
#' library(vegan)
#' data(dune)
#' data(dune.env)
#'
#' # Ejemplo con RDA
#' rda_obj <- rda(dune ~ ., data = dune.env)
#' plot_ordination(rda_obj, titulo = "RDA de Dune", sitios_nombres = TRUE, segmentos_Yvar = TRUE)
#'
#' # Ejemplo con NMDS
#' nmds_obj <- metaMDS(dune)
#' plot_ordination(nmds_obj, titulo = "NMDS de Dune", sitios = c("point", "text"), repel = TRUE)
#' }
#'
#' @importFrom vegan scores
#' @importFrom ggplot2 ggplot aes geom_point geom_text geom_segment geom_hline geom_vline theme_minimal labs
#' @importFrom ggrepel geom_text_repel
#' @export
plot_rda <- function(object,
                            titulo = NULL,
                            scaling = "sites",
                            lc = FALSE,
                            sitios_nombres = FALSE,
                            segmentos_Yvar = FALSE,
                            segmentos_Xvar = FALSE,
                            sitios = c("point"),
                            repel = FALSE,
                            color_Yvar = "#fc4903",
                            color_Xvar = "#9500e6",
                            color_sitios = "#03a9fc",
                            alpha_segments_Yvar = 0.5,
                            alpha_segments_Xvar = 0.5,
                            alpha_sitios = 0.5,
                            size_sitios = 3,
                            size_Yvar = 3,
                            size_Xvar = 3) {

  # Verificar que el objeto sea compatible
  if (!inherits(object, c("rda", "cca", "capscale", "metaMDS", "wcmdscale"))) {
    stop("El objeto proporcionado no es de una clase compatible. Debe ser 'rda', 'cca', 'capscale', 'metaMDS', o 'wcmdscale'.")
  }

  # Cargar ggplot2 y ggrepel si es necesario
  if (repel && !requireNamespace("ggrepel", quietly = TRUE)) {
    stop("El paquete 'ggrepel' es necesario para usar 'repel = TRUE'. Instálalo con install.packages('ggrepel').")
  }

  # Extraer scores de sitios
  sitios_scores <- vegan::scores(object, display = ifelse(lc, "lc", "wa"), choices = 1:2, scaling = scaling)
  sitios_df <- as.data.frame(sitios_scores)
  colnames(sitios_df) <- c("Dim1", "Dim2")

  # Detectar tipo de análisis
  tipo_analisis <- if (inherits(object, "rda")) {
    "RDA"
  } else if (inherits(object, "cca")) {
    "CCA"
  } else if (inherits(object, "capscale")) {
    "capscale"
  } else if (inherits(object, "metaMDS")) {
    "NMDS"
  } else if (inherits(object, "wcmdscale")) {
    "PCoA"
  } else {
    "Ordenación"
  }

  # Extraer scores de variables explicativas (Yvar) si aplica
  if (tipo_analisis %in% c("RDA", "CCA", "capscale")) {
    Yvar_scores <- vegan::scores(object, display = "species", choices = 1:2, scaling = scaling)
    Yvar_df <- as.data.frame(Yvar_scores)
    colnames(Yvar_df) <- c("Dim1", "Dim2")
    Yvar_df$label <- rownames(Yvar_df)
  }

  # Extraer scores de variables predictoras (Xvar) si aplica
  Xvar_scores <- vegan::scores(object, display = "bp", choices = 1:2, scaling = scaling)
  Xvar_df <- as.data.frame(Xvar_scores)
  colnames(Xvar_df) <- c("Dim1", "Dim2")
  Xvar_df$label <- rownames(Xvar_df)

  # Preparar datos para variables (Yvar)
  if (tipo_analisis %in% c("RDA", "CCA", "capscale")) {
    Yvar_df$color <- color_Yvar
  }

  # Preparar datos para variables predictoras (Xvar)
  Xvar_df$color <- color_Xvar

  # Construcción del gráfico base
  p <- ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray70")

  # Añadir segmentos para Yvar si aplica y está habilitado
  if (tipo_analisis %in% c("RDA", "CCA", "capscale") && segmentos_Yvar) {
    p <- p +
      geom_segment(data = Yvar_df, aes(x = 0, y = 0, xend = Dim1, yend = Dim2),
                   color = Yvar_df$color, alpha = alpha_segments_Yvar,
                   arrow = arrow(length = unit(2, "mm"), type = "closed"))
  }

  # Añadir segmentos para Xvar si está habilitado
  if (segmentos_Xvar) {
    p <- p +
      geom_segment(data = Xvar_df, aes(x = 0, y = 0, xend = Dim1, yend = Dim2),
                   color = Xvar_df$color, alpha = alpha_segments_Xvar,
                   arrow = arrow(length = unit(2, "mm"), type = "closed"))
  }

  # Añadir puntos de sitios si está habilitado
  if ("point" %in% sitios) {
    p <- p +
      geom_point(data = sitios_df, aes(x = Dim1, y = Dim2),
                 shape = 16, color = color_sitios,
                 size = size_sitios, alpha = alpha_sitios)
  }

  # Añadir etiquetas de sitios si está habilitado
  if ("text" %in% sitios) {
    if (repel) {
      p <- p +
        ggrepel::geom_text_repel(data = sitios_df,
                                 aes(x = Dim1, y = Dim2, label = rownames(sitios_df)),
                                 color = color_sitios,
                                 size = size_sitios)
    } else {
      p <- p +
        geom_text(data = sitios_df,
                  aes(x = Dim1, y = Dim2, label = rownames(sitios_df)),
                  color = color_sitios,
                  size = size_sitios,
                  nudge_y = 0.2)
    }
  }

  # Añadir etiquetas y segmentos de Yvar si aplica
  if (tipo_analisis %in% c("RDA", "CCA", "capscale") && segmentos_Yvar) {
    if (!is.null(Yvar_df)) {
      p <- p +
        geom_text_repel(data = Yvar_df, aes(x = Dim1, y = Dim2, label = label),
                        color = Yvar_df$color,
                        size = size_Yvar)
    }
  }

  # Añadir etiquetas y segmentos de Xvar si está habilitado
  if (segmentos_Xvar) {
    if (!is.null(Xvar_df)) {
      p <- p +
        geom_text_repel(data = Xvar_df, aes(x = Dim1, y = Dim2, label = label),
                        color = Xvar_df$color,
                        size = size_Xvar)
    }
  }

  # Añadir variables explicativas (Yvar) si aplica y no se están dibujando segmentos
  if (tipo_analisis %in% c("RDA", "CCA", "capscale") && !segmentos_Yvar) {
    if (!is.null(Yvar_df)) {
      p <- p +
        geom_text_repel(data = Yvar_df, aes(x = Dim1, y = Dim2, label = label),
                        color = Yvar_df$color,
                        size = size_Yvar)
    }
  }

  # Añadir variables predictoras (Xvar) si no se están dibujando segmentos
  if (!segmentos_Xvar) {
    if (!is.null(Xvar_df)) {
      p <- p +
        geom_text_repel(data = Xvar_df, aes(x = Dim1, y = Dim2, label = label),
                        color = Xvar_df$color,
                        size = size_Xvar)
    }
  }

  # Estilo y etiquetas finales
  p <- p +
    theme_minimal() +
    labs(
      title = titulo,
      x = paste0("Dim1"),
      y = paste0("Dim2")
    )

  # Añadir información de varianza explicada si es PCoA o similar
  if (tipo_analisis %in% c("RDA", "CCA", "capscale", "wcmdscale")) {
    if (!is.null(object$eig)) {
      var_explained <- object$eig / sum(object$eig) * 100
      var1 <- round(var_explained[1], 1)
      var2 <- round(var_explained[2], 1)
      p <- p + labs(
        x = paste0("Dim1 (", var1, "%)"),
        y = paste0("Dim2 (", var2, "%)")
      )
    }
  }

  # Devolver el gráfico
  return(p)
}
