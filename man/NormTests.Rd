% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NormTests.R
\name{NormTests}
\alias{NormTests}
\title{Múltiples test de normalidad}
\usage{
NormTests(vector, significancia = 0.05, color = "deepskyblue1")
}
\arguments{
\item{vector}{vector de elementos numéricos del cual se analizará su normalidad}

\item{significancia}{nivel de significancia para visualizar en el gráfico final. Valor por defecto: 0.05}

\item{color}{color de las barras del gráfico}
}
\value{
gráfico de ggplot2 mostrando los p-valores y el promedio de p-valores (línea) para identificar la congruencia de las pruebas de normalidad. Los p-valores se muestras numéricamente en la consola
}
\description{
Genera de manera rápida un gráfico comparativo con p-valor obtenido de diversas pruebas de normalidad, partir de un vector de datos numéricos
}
\section{Descripción}{

El gráfico final es generado con ggplot2, por lo que puede ser editado con cualquier función de dicho paquete. La línea de guiones roja define el nivel de significancia de 0.05
}

\examples{
library(ggplot2)
data("iris")

# Resultado de las pruebas de normalidad
NormTests(iris$Sepal.Length)

# Con modificaciones de ggplot2
NormTests(iris[,2], color = "forestgreen") +
labs(titule = "Título genial") +
theme_test()

}
