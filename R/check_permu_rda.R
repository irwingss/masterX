#' Revisar Estadísticas de Permutaciones (ANOVA) para RDA/CCA
#'
#' Esta función realiza un ANOVA para un objeto RDA o CCA (o similar, como capscale) usando `anova.cca`.
#' Además, muestra las estadísticas de permutaciones a través de `permustats`, su resumen, y opcionalmente
#' genera un gráfico de densidad de las distribuciones de las permutaciones.
#'
#' @param RDA Un objeto resultante de `rda`, `cca` o `capscale` (vegan).
#' @param by (Opcional) Argumento a pasar a `anova.cca` (por defecto NULL). Puede ser "terms", "axis", "margin", etc.
#' @param plot (Opcional) Lógico. Si TRUE (por defecto), se muestra el gráfico de densidad. Si FALSE, no se genera.
#' @param plot_args (Opcional) Lista de argumentos adicionales para `densityplot`. Por defecto `list()`.
#'
#' @return Invisiblemente, una lista con los siguientes elementos:
#' \itemize{
#'   \item{Anova}{Resultado del ANOVA (anova.cca).}
#'   \item{Permustats}{Objeto resultante de `permustats`.}
#'   \item{SummaryPermustats}{Resumen del objeto Permustats.}
#' }
#'
#' @examples
#' \dontrun{
#' # Suponiendo que tienes un objeto RDA (por ejemplo my_rda)
#' # check_permu(my_rda, by = "terms", plot = TRUE)
#' }
#'
#' @import vegan
#' @import lattice
#' @export
check_permu_rda <- function(RDA, by = NULL, plot = TRUE, plot_args = list()) {
  # Verificar que el objeto sea compatible con anova.cca
  # anova.cca funciona con objetos resultantes de rda, cca, capscale, etc.
  if (!inherits(RDA, c("rda", "cca", "capscale"))) {
    stop("El objeto proporcionado no es de clase 'rda', 'cca' o 'capscale'. Por favor, use un objeto adecuado.")
  }

  # Identificar método (RDA o CCA)
  method_type <- if (inherits(RDA, "rda")) {
    "RDA"
  } else if (inherits(RDA, "cca")) {
    "CCA"
  } else {
    "capscale"
  }

  # Mensajes informativos
  cat("------------------------ \n")
  cat("RESULTADOS ANOVA", method_type, "\n", sep = " ")
  cat("------------------------ \n\n")

  # Ejecutar ANOVA con permutaciones
  Anova_RDA <- anova.cca(RDA, by = by)

  # Imprimir resultado del ANOVA
  print(Anova_RDA)

  cat("\n\n--------------------------------------\n")
  cat("RESULTADOS ESTADÍSTICO F PERMUTACIONES\n")
  cat("--------------------------------------\n\n")

  # Obtener estadísticas de permustats
  pstat <- permustats(Anova_RDA)
  print(pstat)

  # Imprimir resumen de las permustats
  pstat_summary <- summary(pstat)
  print(pstat_summary)

  # Gráfico de densidad si se requiere
  if (plot) {
    do.call(densityplot, c(list(x = pstat), plot_args))
  }

  # Devolver invisiblemente una lista con resultados
  invisible(list(
    Anova = Anova_RDA,
    Permustats = pstat,
    SummaryPermustats = pstat_summary
  ))
}
