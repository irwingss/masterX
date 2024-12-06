#' Instalación del tema masterX Rstudio
#' @description Utiliza esta función para instalar el tema de colores de RStudio: masterX. No es requerido el uso de argumentos en la función. Al ejecutarla como `masterx_rstudio_theme()` ya procederá a descargar, instalar y aplicar el tema de manera automática. Si quieres utilizar esta función para instalar otros temas desde cualquier repositorio de GitHub, asegúrate que el tema tenga extensión .rstheme
#' @param URL_tema Enlace URL raw del tema a aplicar. Como ejemplo, utiliza:
#'     Enlace del tema masterX-RStudio.rstheme:
#'     "https://raw.githubusercontent.com/irwingss/masterX-RStudio-Theme/main/masterX-RStudio.rstheme"
#' @export
#' @import rstudioapi
#' @import xml2
#' @import fs
#' @importFrom rstudioapi addTheme
#'
#' @examples
#' masterx_rstudio_theme(
#'   "https://raw.githubusercontent.com/irwingss/masterX-RStudio-Theme/main/masterX-RStudio.rstheme"
#' )
#'
masterx_rstudio_theme <- function(URL_tema){
  addTheme(URL_tema, apply = TRUE)
  }
