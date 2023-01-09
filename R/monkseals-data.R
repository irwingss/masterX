#' Buceo de Focas Monje (Monk Seals)
#'
#' Data del artículo open access "Integrating multiple technologies to understand the foraging behaviour of Hawaiian monk seals
#'
#' @docType data
#'
#' @usage data(monkseals)
#'
#' @format An object of class \code{"cross"}; see \code{\link[qtl]{read.cross}}.
#'
#' @keywords datasets
#'
#' @references Wilson, K., Littnan, C., Halpin, P., & Read, A. (2017). Integrating multiple technologies to understand the foraging behaviour of Hawaiian monk seals. Royal Society open science, 4(3), 160703
#' (\href{https://royalsocietypublishing.org/doi/full/10.1098/rsos.160703}{Website})
#'
#' @source \href{http://dx.doi.org/10.5061/dryad.s0b80}{Dryad digital repository}
#'
#' @examples
#' data(monkseals)
#' mean(monkseals$odbaProm, na.omit = TRUE)
"monkseals"
