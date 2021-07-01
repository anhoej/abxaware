#' Aggregate antibiotic usage data by AWaRe group
#'
#' @param df Data frame.
#' @param atc ATC code.
#' @param ddd Amount (usually Defined Daily Doses).
#' @param ... Grouping variables.
#' @param tall If TRUE (default) outputs data in tall format.
#' @param method 'dk' (default) or 'who' indicating the AWaRe classification to
#'        be used. The default may be changed within an R session using
#'         `options(abxaware.method = 'who')`.
#' @param ignore.other If TRUE, ignores drugs that have no AWaRe class.
#' @param silent If TRUE, prints method.
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' awr_aggregate(abx_sales)
#' awr_aggregate(abx_sales, atc, ddd)
#' awr_aggregate(abx_sales, atc, ddd, region)
#' awr_aggregate(abx_sales, atc, ddd, region, tall = TRUE)
#' awr_aggregate(abx_sales, atc, ddd, month)
#' awr_aggregate(abx_sales, atc, ddd, month, region)
#' awr_aggregate(abx_sales, atc, ddd, month, region, hospital)
#'
awr_aggregate <- function(df,
                          atc          = atc,
                          ddd          = ddd,
                          ...,
                          tall         = FALSE,
                          # method       = c('dk', 'who'),
                          method       = getOption('abxaware.method', 'dk'),
                          ignore.other = FALSE,
                          silent       = FALSE) {
  if(!silent) {
    message(paste0('Aggregating data using the "',
                   # match.arg(method),
                   method,
                   '" AWaRe classification'))
  }

  method <- paste0('aware_',
                   # match.arg(method)
                   method
                   ) %>%
    rlang::sym()

  d <- df %>%
    dplyr::select({{ atc }}, {{ ddd }}, ...) %>%
    dplyr::mutate(atc = {{ atc }}) %>%
    dplyr::left_join(abx_aware, by = 'atc') %>%
    dplyr::mutate(aware = !!method)

  if(ignore.other)
    d <- dplyr::filter(d, !is.na(aware))

  d <- d %>%
    dplyr::group_by(aware, ...) %>%
    dplyr::summarise('{{ddd}}' := sum({{ ddd }}),
                     .groups    = 'drop') %>%
    tidyr::complete(aware, ...) %>%
    dplyr::mutate('{{ddd}}' := tidyr::replace_na({{ ddd }}, 0)) %>%
    dplyr::group_by(...) %>%
    dplyr::mutate(total = sum({{ ddd }}, na.rm = TRUE),
                  p     = {{ ddd }} / total) %>%
    dplyr::ungroup() %>%
    dplyr::filter(total > 0)

  if(!ignore.other) {
    d <- d %>%
      dplyr::mutate(aware = forcats::fct_explicit_na(aware, 'other'),
                    aware = forcats::fct_relevel(aware, 'other'))
  }

  if(!tall) {
    d <- d %>%
      dplyr::select(-p) %>%
      tidyr::pivot_wider(names_from = aware,
                         values_from = {{ ddd }},
                         values_fill = 0)
  }

  d
}

#' Plot AWaRe data
#'
#' @param df Data frame.
#' @param atc ATC code.
#' @param ddd Amount.
#' @param time Time period.
#' @param unit Organisational unit.
#' @param ncol Integer, number of columns in faceted plots.
#' @param legend.position Character, where to put legend (e.g. 'none', 'right',
#'        'bottom').
#' @param ... Other arguments to awr_aggregate() and ggplot().
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' awr_plot(abx_sales, atc, ddd)
#' awr_plot(abx_sales, atc, ddd, unit = region)
#' awr_plot(abx_sales, atc, ddd, time = month)
#' awr_plot(abx_sales, atc, ddd, time = month, unit = region)
#' awr_plot(abx_sales, atc, ddd, time = month, unit = hospital, ncol = 4)
#' awr_plot(abx_days, atc, days, time = month)
#' awr_plot(abx_days, atc, days, unit = hosp)
#' awr_plot(abx_days, atc, days, unit = hosp, ignore.other = TRUE)
#'
awr_plot <- function(df,
                     atc             = atc,
                     ddd             = ddd,
                     time            = NULL,
                     unit            = NULL,
                     ncol            = NULL,
                     legend.position = NULL,
                     # method = c('dk', 'who'),
                     ...) {

  d <- awr_aggregate(df,
                     {{ atc }},
                     {{ ddd }},
                     {{ time }},
                     {{ unit }},
                     tall = TRUE,
                     ...)

  cols <- c(
    'reserve' = '#F07E6E', # red
    'watch'   = '#FBB258', # amber
    'access'  = '#90CD97'  # green
  )

  if('other' %in% levels(d$aware)) {
    cols <- c('other' = 'grey90', cols)
  }

  if(missing(time)) {
    if(missing(unit) ) {
      d <- dplyr::mutate(d, unit = '')
    } else {
      d <- dplyr::mutate(d, unit = {{ unit  }})
    }

    p <- ggplot2::ggplot(d,
                         ggplot2::aes(x    = unit,
                                      y    = p,
                                      fill = aware)) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::labs(x = NULL)
  } else {
    if (!inherits(dplyr::pull(df, {{ time }}), c('Date', 'POSIXt'))) {
      stop('Time must be a date or datetime variable',
           call. = FALSE)
    }

    p <- ggplot2::ggplot(d,
                         ggplot2::aes(x    = {{ time }},
                                      y    = p,
                                      fill = aware)) +
      ggplot2::geom_area()

    if(!missing(unit) )
      p <- p + ggplot2::facet_wrap(ggplot2::vars({{ unit }}),
                                   ncol = ncol)
  }

  p <- p +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid      = ggplot2::element_blank(),
                   legend.position = legend.position) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_fill_manual(values = cols) +
    ggplot2::labs(y    = NULL,
                  fill = NULL)

  p
}


.onAttach <- function(libname, pkgname) {
  options(abxaware.method = 'dk')
}

.onDetach <- function(libpath) {
  options(abxaware.method = NULL)
}

utils::globalVariables(c(
  ':=',
  'abx_aware',
  'aware',
  'total',
  'p'
))
