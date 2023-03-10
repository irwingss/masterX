% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/excel_github.R
\name{excel_github}
\alias{excel_github}
\title{Importar documentos de excel desde GitHub}
\usage{
excel_github(URL_raw)
}
\arguments{
\item{URL_raw}{enlace URL del archivo obtenido copiando el disponible en el botón "View raw" al acceder al archivo en GitHub}
}
\value{
objeto data.frame conteniendo la información del archivo .xlsx obtenido desde GitHub
}
\description{
Permite la carga de archivos de extensión .xlsx desde GitHub. Este proceso descargará el archivo de manera temporal en el equipo y luego lo cargará en RStudio
}
\section{Encontrando el enlace URL en GitHub}{

Puedes encontrar el enlace URL siguiendo los pasos de las siguientes imágenes.

Ingresar a la página del archivo .xlsx

\if{html}{

  \out{<div style="text-align: center">}\figure{excel_github_1.png}{options: style="width:750px;max-width:75\%;"}\out{</div>}

}

Obtener la URL del archivo .xlsx

\if{html}{

  \out{<div style="text-align: center">}\figure{excel_github_2.png}{options: style="width:750px;max-width:75\%;"}\out{</div>}

}
}

\examples{
# Descargar el archivo Owl2.xlsx desde GitHub
URL <- "https://github.com/irwingss/ArchivosCuestionarios/blob/main/Owls2.xlsx?raw=true"
excel_github(URL)
}
