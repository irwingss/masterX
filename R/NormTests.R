#' Múltiples test de normalidad
#' @description Genera de manera rápida un gráfico comparativo con p-valor obtenido de diversas pruebas de normalidad, partir de un vector de datos numéricos
#' @param vector vector de elementos numéricos del cual se analizará su normalidad
#' @param significancia nivel de significancia para visualizar en el gráfico final. Valor por defecto: 0.05
#' @param color color de las barras del gráfico

#'
#' @return gráfico de ggplot2 mostrando los p-valores y el promedio de p-valores (línea) para identificar la congruencia de las pruebas de normalidad. Los p-valores se muestras numéricamente en la consola
#' @export
#'
#' @import ggpubr
#' @import ggplot2
#' @import forcats
#' @importFrom  magrittr %>%
#' @importFrom nortest lillie.test
#' @importFrom nortest ad.test
#' @importFrom nortest sf.test
#' @importFrom nortest cvm.test
#' @importFrom DistributionTest zc.test
#' @importFrom DistributionTest za.test
#' @importFrom forcats fct_reorder
#'
#' @section Descripción:
#' El gráfico final es generado con ggplot2, por lo que puede ser editado con cualquier función de dicho paquete. La línea de guiones roja define el nivel de significancia de 0.05
#'
#' @examples
#' library(ggplot2)
#' data("iris")
#'
#' # Resultado de las pruebas de normalidad
#' NormTests(iris$Sepal.Length)
#'
#' # Con modificaciones de ggplot2
#' NormTests(iris[,2], color = "forestgreen") +
#' labs(titule = "Título genial") +
#' theme_test()
#'
NormTests <- function(vector, significancia = 0.05, color = "deepskyblue1") {
    pvals <- c(
      shapiro.test(vector)$p.value,
      ks.test(vector, "pnorm")$p.value,
      lillie.test(vector)$p.value,
      ad.test(vector)$p.value,
      sf.test(vector)$p.value,
      cvm.test(vector)$p.value,
      zc.test(vector, "norm")$p.value,
      za.test(vector, "norm")$p.value
    )

  tit1 <- deparse(substitute(vector))
  tit2 <- strsplit(tit1, split="(\\$)")[[1]][2]

  if (is.na(tit2)) {
    titulo <- tit1
  } else if (!is.na(tit2)) {
    titulo <- tit2
  }

  names(pvals) <- c("SW", "KS", "LF", "AD", "SF", "CVM", "ZC", "ZA")
  nombres <- c("SW", "KS", "LF", "AD", "SF", "CVM", "ZC", "ZA")
  print(pvals)

  data.frame(nombres, pvals) %>%
    ggplot(aes(
      x = fct_reorder(nombres, pvals),
      y = pvals
    )) +
    geom_bar(
      stat = "identity",
      show.legend = FALSE,
      fill = color
    ) +
    theme_minimal() +
    labs(
      x = "Pruebas de Normalidad", y = "P valores",
      title = titulo
    ) +
    geom_hline(yintercept = significancia, lty = 2, color = "red") +
    geom_hline(yintercept = mean(pvals), lty = 3, color = "blue") +
    annotate(
      label = "Sig.level", geom = "label", size = 2.5,
      x = 1, y = 0.05, color = "red"
    ) +
    annotate(
      label = "Promedio", geom = "label", size = 2.5,
      x = 1, y = mean(pvals), color = "blue"
    )
}
