#' Rellenado con las paletas de colores masterX
#' @description Función afín a ggplot2 para el rellenado de geométricas.
#'      Para conocer las paletas disponibles revisa la ayuda de la función
#'      masterx_palettes().
#' @param palette Nombre de la paleta de colores.
#'
#' @return la paleta de colores que será aplicada al gráfico
#' @export
#' @import ggplot2
#' @examples
#' library(ggplot2)
#' data(iris)
#' ggplot(data = iris,
#' aes(x = Species, fill = Species)) +
#'   geom_bar() +
#'   theme_bw() +
#'   theme(legend.position = "top") +
#'   labs(x = "Petal Length", y = "Sepal Length") +
#'   scale_fill_masterx(palette = "pal22")
scale_fill_masterx <- function(..., palette = "pal22") {
  discrete_scale("fill", "masterx", masterx_pal(palette), ...)
}
