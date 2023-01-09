#' Coloreo con las paletas de colores masterX
#' @description Función afín a ggplot2 para el coloreo de geométricas.
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
#' aes(x = Petal.Length, y = Sepal.Length, color = Species)) +
#'   geom_point(size = 4, alpha = 0.4) +
#'   theme_bw() +
#'   theme(legend.position = "top") +
#'   labs(x = "Petal Length", y = "Sepal Length") +
#'   scale_color_masterx(palette = "pal22")
scale_color_masterx <- function(..., palette = "pal22") {
  discrete_scale("color", "masterx", masterx_pal(palette), ...)
}
