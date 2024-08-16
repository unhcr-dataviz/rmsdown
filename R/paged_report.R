#' Function for RMS report paged template
#'
#' @param other_css Add extra css
#' @param toc Table of content
#' @param toc_depth Table of content depth
#' @param number_sections Number section headings
#' @param ... Arguments passed to pagedown::html_paged
#'
#' @return A pagedown report
#' @export
paged_report <- function(
    other_css = NULL,
    toc = TRUE,
    toc_depth = 2,
    number_sections = FALSE,
    ...) {
  # theme
  paged_theme <- bslib::bs_theme(
    version = 5,
    primary = "#0072bc",
    secondary = "#666666",
    warning = "#FAEB00",
    base_font = bslib::font_google("Lato"),
    font_scale = 0.875,
    "font-weight-base" = 300,
    "font-weight-bolder" = 700,
    "headings-font-weight" = 700,
    "headings-color" = "#0072bc",
    "lead-font-weight" = 400,
    "enable-grid-classes" = FALSE,
    "enable-cssgrid" = TRUE
  ) |>
    bslib::bs_remove(
      c(
        "_containers",
        "_forms",
        "_buttons",
        "_transitions",
        "_dropdown",
        "_button-group",
        "_nav",
        "_navbar",
        "_card",
        "_accordion",
        "_breadcrumb",
        "_pagination",
        "_badge",
        "_alert",
        "_progress",
        "_list-group",
        "_close",
        "_toasts",
        "_modal",
        "_tooltip",
        "_popover",
        "_carousel",
        "_spinners",
        "_offcanvas",
        "_placeholders",
        "bs3compat"
      )
    )

  # css file
  paged_report_css <- pkg_resource("scss/paged_report.scss")

  # template
  pagedown::html_paged(
    theme = paged_theme,
    css = c(paged_report_css, other_css),
    toc = toc,
    toc_depth = toc_depth,
    number_sections = number_sections,
    ...
  )
}
