utils::globalVariables(c(
  ':=',
  'abx_aware',
  'aware',
  'total',
  'p'
))

#' Aggregate antibiotic usage data by AWaRe group
#'
#' @param df Data frame.
#' @param atc ATC code.
#' @param ddd Amount (usually Defined Daily Doses).
#' @param ... Grouping variables.
#' @param tall If TRUE (default) outputs data in tall format.
#' @param method 'dk' (default) or 'who' indicating the AWaRe classification to
#'        be used.
#' @param ignore.other If TRUE, ignores drugs that have no AWaRe class.
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' awr_aggregate(abx_sales)
#' awr_aggregate(abx_sales, atc, ddd)
#' awr_aggregate(abx_sales, atc, addd)
#' awr_aggregate(abx_sales, atc, ddd, region)
#' awr_aggregate(abx_sales, atc, ddd, region, tall = TRUE)
#' awr_aggregate(abx_sales, atc, ddd, month)
#' awr_aggregate(abx_sales, atc, ddd, month, region)
#' awr_aggregate(abx_sales, atc, ddd, month, region, hospital)
#' awr_aggregate(abx_days, atc, n, month, hosp)
awr_aggregate <- function(df,
                          atc = atc,
                          ddd = ddd,
                          ...,
                          tall = FALSE,
                          method = c('dk', 'who'),
                          ignore.other = FALSE) {
  method <- paste0('aware_',
                   match.arg(method)) %>%
    rlang::sym()

  d <- df %>%
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
    dplyr::mutate(aware = forcats::fct_explicit_na(aware, 'other'),
                  ) %>%
    dplyr::filter(total > 0) %>%
    identity()

  if(!ignore.other)
    d <- dplyr::mutate(d, aware = forcats::fct_relevel(aware, 'other'))

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
#' awr_plot(abx_sales, atc, addd)
#' awr_plot(abx_sales, atc, ddd, unit = region)
#' awr_plot(abx_sales, atc, ddd, time = month)
#' awr_plot(abx_sales, atc, ddd, time = month, unit = region)
#' awr_plot(abx_sales, atc, ddd, time = month, unit = hospital, ncol = 4)
#' awr_plot(abx_days, atc, n, time = month)
awr_plot <- function(df,
                     atc             = atc,
                     ddd             = ddd,
                     time            = NULL,
                     unit            = NULL,
                     ncol            = NULL,
                     legend.position = NULL,
                     # method = c('dk', 'who'),
                     ...) {
  cols <- c('other'   = 'grey90',
            'access'  = '#90CD97', #
            'watch'   = '#FBB258', # amber
            'reserve' = '#F07E6E'  # red
  )

  d <- awr_aggregate(df,
                     {{ atc }},
                     {{ ddd }},
                     {{ time }},
                     {{ unit }},
                     tall = TRUE,
                     ...)

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
