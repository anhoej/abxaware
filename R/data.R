#' AWaRe classification of ATC codes.
#'
#' A dataset containing ATC codes and the corresponding AWaRe categories.
#'
#' @format A data frame with 318 rows and 4 variables:
#' \describe{
#'   \item{atc}{ATC code, character}
#'   \item{atc_level}{ATC level, number}
#'   \item{generic_name}{Drug name, character}
#'   \item{aware_who}{AWaRe group, access, watch or reserve, factor,
#'         source: \url{https://www.who.int/publications/i/item/WHOEMPIAU2019.11}}
#'   \item{aware_uk}{AWaRe group from Budd 2019,
#'         source: \doi{https://doi.org/10.1093/jac/dkz321}}
#'   \item{aware_dk}{AWaRe group adjusted to Danish guidelines}
#' }
#' @source \url{https://adoptaware.org/}
"abx_aware"

#' Antibiotic sales data.
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
#' }
#' @source \url{https://amgros.dk/}
"abx_sales"
#'
#' #' Antibiotic usage.
#' #'
#' #' A dataset containing antibiotic usage data from from six hospitals from
#' #' varying time periods.
#' #'
#' #' @format A data frame with 8441 rows and 5 variables:
#' #' \describe{
#' #'   \item{hosp}{Hospital, character}
#' #'   \item{month}{Month, date}
#' #'   \item{atc}{ATC code, character}
#' #'   \item{drug}{Drug name, character}
#' #'   \item{days}{Number of antibiotic treatment days, 1 patient x 1 drug = 1 day}
#' #' }
#' "abx_days"
