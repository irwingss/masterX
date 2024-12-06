#' Cargar R Scritp en blanco para iniciar proyecto
#' @description Esta función crea un R Script conteniendo la estructura mínima requerida para mantener ordenada la información de un proyecto o análisis.
#' @param modulo nombre del archivo .R que será cargado
#' @export
#'
#' @examples
#' \dontrun{
#'   cargar_rscript("proj_rscript")
#' }
#'
cargar_rscript <- function(modulo = "proj_rscript") {
  modulo <- paste0(modulo, ".R")
  suppressMessages(file.copy(
    from = system.file(package = "masterX",
                       paste0("scripts/", modulo)),
    to = getwd()
  ))
  file.edit(modulo)
}
