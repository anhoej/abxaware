#' AWaRe classification of ATC codes.
#'
#' A dataset containing ATC codes and the corresponding AWaRe categories.
#'
#' @format A data frame with 318 rows and 3 variables:
#' \describe{
#'   \item{atc}{ATC code, character}
#'   \item{text}{Drug name, character}
#'   \item{aware}{AWaRe group, access, watch or reserve, factor}
#'   ...
#' }
#' @source \url{https://adoptaware.org/}
"abx_aware"

#' AWaRe classification of ATC codes.
#'
#' A dataset containing antibiotic sales data from Danish hospitals 2015-2020.
#'
#' @format A data frame with 47551 rows and 7 variables:
#' \describe{
#'   \item{region}{Region, character}
#'   \item{hospital}{Hospital, character}
#'   \item{month}{Month, date}
#'   \item{atc}{ATC code, character}
#'   \item{drug}{Drug name, character}
#'   \item{ddd}{Amount purchased, Defined Daily Dosages (DDD)}
#'   \item{addd}{Amount purchased, DDDs adjusted to Danish recommendations}
#'   ...
#' }
#' @source \url{https://amgros.dk/}
"abx_sales"
