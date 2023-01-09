#' Importar documentos de excel desde GitHub
#' @description Permite la carga de archivos de extensión .xlsx desde GitHub. Este proceso descargará el archivo de manera temporal en el equipo y luego lo cargará en RStudio
#' @param URL_raw enlace URL del archivo obtenido copiando el disponible en el botón "View raw" al acceder al archivo en GitHub
#'
#' @return objeto data.frame conteniendo la información del archivo .xlsx obtenido desde GitHub
#' @export
#' @importFrom openxlsx read.xlsx
#' @importFrom httr GET
#' @importFrom httr write_disk
#' @import openxlsx httr
#' @section Encontrando el enlace URL en GitHub:
#' Puedes encontrar el enlace URL siguiendo los pasos de las siguientes imágenes.
#'
#' Ingresar a la página del archivo .xlsx
#'
#' \if{html}{
#'
#'   \out{<div style="text-align: center">}\figure{excel_github_1.png}{options: style="width:750px;max-width:75\%;"}\out{</div>}
#'
#' }
#'
#' Obtener la URL del archivo .xlsx
#'
#' \if{html}{
#'
#'   \out{<div style="text-align: center">}\figure{excel_github_2.png}{options: style="width:750px;max-width:75\%;"}\out{</div>}
#'
#' }
#'
#'
#' @examples
#' # Descargar el archivo Owl2.xlsx desde GitHub
#' URL <- "https://github.com/irwingss/ArchivosCuestionarios/blob/main/Owls2.xlsx?raw=true"
#' excel_github(URL)
excel_github <- function(URL_raw){
  # Dirección temporal
  temp_file <- tempfile(fileext = ".xlsx")

  # Descargar el archivo
  httr::GET(URL_raw,
            httr::write_disk(path = temp_file))

  # Abrir el archivo
  read.xlsx(temp_file)
}
