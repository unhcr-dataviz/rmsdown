#' Create a simple horizontal bar chart
#'
#' @param df Formatted data frame
#' @param cat Category column
#' @param val Value column
#' @param title Chart title
#' @param caption Chart caption
#' @param chr_wrap Number of character to wrap long category labels
#' @param percentage Format axis and labels as percentage TRUE/FALSE
#' @param bar_color Specific color code for bars
#' @param lbl_threshold Value to switch labels from inside to outside the bars
#'
#' @return A plot object
#' @import ggplot2
#' @export
#'
#' @examples \dontrun{
#'
#' library(ggplot2)
#'
#' df_percent <- data.frame(
#'   category = c("Category A", "Category B", "Category C", "Category D"),
#'   value = c(0.3, 0.08, 0.6, 0.75)
#' )
#'
#' simple_bar_chart(df_percent, cat = "category", val = "value")
#' }
simple_bar_chart <- function(
    df, cat, val,
    title = "Bar Chart", caption = "Notes", chr_wrap = NULL,
    percentage = FALSE, bar_color = NULL, lbl_threshold = NULL) {
  # Wrap cat labels if chr_wrap is provided
  if (!is.null(chr_wrap)) {
    df[[cat]] <- stringr::str_wrap(df[[cat]], width = chr_wrap)
  }

  # Determine label threshold if not provided
  if (is.null(lbl_threshold)) {
    if (percentage) {
      lbl_threshold <- 0.1 # 10% for percentage data
    } else {
      lbl_threshold <- stats::quantile(df[[val]], 0.1) # 10th percentile for non-percentage data
    }
  }

  # Apply UNHCR blue if bar_color is not specified
  if (is.null(bar_color)) {
    bar_color <- unhcrthemes::unhcr_pal(
      n = 1,
      name = "pal_blue"
    )
  }

  # Basic plot
  p <- ggplot(df, aes(y = .data[[cat]], x = .data[[val]])) +
    scale_y_discrete(limits = rev) +
    labs(title = title, caption = caption)


  # Geoms, scale and theme
  if (percentage) {
    p <- p +
      geom_col(aes(x = 1), fill = "grey90", width = 0.7) +
      geom_col(width = 0.7, fill = bar_color) +
      geom_text(
        aes(
          label = scales::percent(.data[[val]], accuracy = 1),
          hjust = ifelse(.data[[val]] < lbl_threshold, -0.15, 1.15),
        ),
        color = ifelse(df[[val]] < lbl_threshold, "#0072bc", "white"),
        size = 8 / ggplot2::.pt,
      ) +
      scale_x_continuous(
        limits = c(0, 1.05),
        breaks = c(0, 1),
        expand = expansion(mult = c(0, 0.01)),
        labels = scales::percent
      ) +
      unhcrthemes::theme_unhcr(
        font_size = 9, plot_title_size = 11,
        caption_margin = 7, caption_size = 7,
        axis_title = FALSE, grid = "X", axis_text_size = 8,
        legend = FALSE,
        plot_margin = margin(rep(5, 4)), plot_background = "transparent"
      )
  } else {
    p <- p +
      geom_col(width = 0.7, fill = bar_color) +
      geom_text(
        aes(
          label = scales::number(.data[[val]], big.mark = ","),
          hjust = ifelse(.data[[val]] < lbl_threshold, -0.15, 1.15),
        ),
        color = ifelse(df[[val]] < lbl_threshold, "#0072bc", "white"),
        size = 8 / ggplot2::.pt,
      ) +
      scale_x_continuous(
        expand = expansion(mult = c(0, 0.01))
      ) +
      unhcrthemes::theme_unhcr(
        font_size = 9, plot_title_size = 11,
        caption_margin = 7, caption_size = 7,
        axis_title = FALSE, grid = FALSE,
        axis_text_size = 8, axis_text = "y", axis = "y",
        legend = FALSE,
        plot_margin = margin(rep(5, 4)), plot_background = "transparent"
      )
  }

  return(p)
}
