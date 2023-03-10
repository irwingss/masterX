% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale_fill_masterx.R
\name{scale_fill_masterx}
\alias{scale_fill_masterx}
\title{Rellenado con las paletas de colores masterX}
\usage{
scale_fill_masterx(..., palette = "pal22")
}
\arguments{
\item{palette}{Nombre de la paleta de colores.}
}
\value{
la paleta de colores que será aplicada al gráfico
}
\description{
Función afín a ggplot2 para el rellenado de geométricas.
     Para conocer las paletas disponibles revisa la ayuda de la función
     masterx_palettes().
}
\examples{
library(ggplot2)
data(iris)
ggplot(data = iris,
aes(x = Species, fill = Species)) +
  geom_bar() +
  theme_bw() +
  theme(legend.position = "top") +
  labs(x = "Petal Length", y = "Sepal Length") +
  scale_fill_masterx(palette = "pal22")
}
