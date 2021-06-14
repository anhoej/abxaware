utils::globalVariables(c(
  ':=',
  'abx_aware',
  'aware',
  'total',
  'p'
))

#' Aggregate antibiotic usage data by AWaRe group
#'
#' @param df Data frame
#' @param atc ATC code
#' @param ddd Amount (usually Defined Daily Doses)
#' @param ... Grouping variables
#' @param tall If TRUE (default) outputs data in tall format
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' awr_aggregate(abx_sales)
#' awr_aggregate(abx_sales)
#' awr_aggregate(abx_sales, atc, ddd = addd)
#' awr_aggregate(abx_sales, atc, ddd)
#' awr_aggregate(abx_sales, atc, ddd, month)
#' awr_aggregate(abx_sales, atc, ddd, region)
#' awr_aggregate(abx_sales, atc, ddd, month, region)
#' awr_aggregate(abx_sales, atc, ddd, month, hospital)
#' awr_aggregate(abx_sales, atc, ddd, month, region, hospital)
#' awr_aggregate(abx_sales, atc, ddd, region, tall = FALSE)

awr_aggregate <- function(df,
                          atc = atc,
                          ddd = ddd,
                          ...,
                          tall = TRUE) {
  d <- df %>%
    dplyr::mutate(atc = {{ atc }}) %>%
    dplyr::left_join(abx_aware, by = 'atc')  %>%
    dplyr::group_by(aware, ...) %>%
    dplyr::summarise('{{ddd}}' := sum({{ ddd }}),
                     .groups    = 'drop') %>%
    tidyr::complete(aware, ...) %>%
    dplyr::mutate('{{ddd}}' := tidyr::replace_na({{ ddd }}, 0)) %>%
    dplyr::group_by(...) %>%
    dplyr::mutate(total = sum({{ ddd }}),
                  p     = {{ ddd }} / total) %>%
    dplyr::ungroup()

  if(!tall) {
    d <- d %>%
      dplyr::select(-p) %>%
      dplyr::mutate(aware = forcats::fct_explicit_na(aware, 'missing')) %>%
      tidyr::pivot_wider(names_from = aware,
                         values_from = {{ ddd }}) %>%
      tidyr::replace_na(list(missing = 0))
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
#' @param na.rm Logical, remove missing values?
#' @param ncol Integer, number of columns in faceted plots.
#' @param legend.position Character, where to put legend (e.g. 'none', 'right', 'bottom')
#' @param ... Other arguments to ggplot()
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' awr_plot(abx_sales, atc, ddd)
#' awr_plot(abx_sales, atc, addd)
#' awr_plot(abx_sales, atc, addd, na.rm = TRUE)
#' awr_plot(abx_sales, atc, ddd, unit = region)
#' awr_plot(abx_sales, atc, ddd, time = month)
#' awr_plot(abx_sales, atc, ddd, time = month, unit = region)
#' awr_plot(abx_sales, atc, ddd, time = month, unit = region, ncol = 1)
#' awr_plot(abx_sales, atc, ddd, time = month, unit = hospital,
#'          ncol = 4, na.rm = TRUE, legend.position = 'none')
awr_plot <- function(df,
                     atc             = atc,
                     ddd             = ddd,
                     time            = NULL,
                     unit            = NULL,
                     na.rm           = FALSE,
                     ncol            = NULL,
                     legend.position = NULL,
                     ...) {

  col_red   <- '#F07E6E'
  col_amber <- '#FBB258'
  col_green <- '#90CD97'

  d <- awr_aggregate(df,
                     {{ atc }},
                     {{ ddd }},
                     {{ time }},
                     {{ unit }})

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
      p <- p + ggplot2::facet_wrap(ggplot2::vars({{ unit }}), ncol = ncol)
  }

  p <- p +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   legend.position = legend.position) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_fill_manual(values = c(col_red,
                                          col_amber,
                                          col_green),
                               na.translate = !na.rm) +
    ggplot2::labs(y    = NULL,
                  fill = NULL)

  p
}
