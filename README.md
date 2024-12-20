
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rmsdown

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The `rmsdown` package provides an R Markdown template and style based on
`pagedown`, allowing you to create high-quality PDFs that follow UNHCR
brand recommendations for the Results Monitoring Surveys (RMS).

## Installation

You can install the development version of `rmsdown` from GitHub using
the `pak` package:

``` r
# If pak is not yet installed, uncomment the following line:
# install.packages("pak")
pak::pkg_install("unhcr-dataviz/rmsdown")
```

## Usage

To start using the `rmsdown` template, create a new R Markdown document
and specify the `rmsdown::paged_report` output format in the YAML
header:

``` r
---
title: "Results Monitoring Surveys"
subtitle: "Country Year"
output: rmsdown::paged_report
---
```

You can also open a `rmsdown::paged_report` template using **RStudio**:

1.  Click the “File” menu then “New File” and choose “R Markdown”.
2.  In the “From Template” tab, choose the **RMS Paged Report** built-in
    template.

### CSS Classes

The `rmsdown` template uses Bootstrap 5 for styling. You can refer to
the [Bootstrap
documentation](https://getbootstrap.com/docs/5.3/getting-started/introduction/)
for guidance on using CSS classes.

Additionally, there is a specific format for creating section blue
pages:

``` r

###### [1.]{.big-num} Introduction {.section-title}

# Introduction {.hide-h1}

## The Forced Displacement Survey

Lorem ipsum...
```

### Plot Functions

The package includes a plot function to help you create consistent and
branded charts. More functions will be added in future updates.

``` r
library(ggplot2)

df_percent <- data.frame(
  category = c("Category A", "Category B", "Category C", "Category D"),
  value = c(0.3, 0.08, 0.6, 0.75)
)

simple_bar_chart(df_percent, cat = "category", val = "value")
```

## Acknowledgements

We extend our gratitude to the creators of the
[`pagedown`](https://github.com/rstudio/pagedown) package that
influenced the development of `rmsdown`.

## Contribution

Contributions to `rmsdown` are highly valued. If you have suggestions,
uncover bugs, or envision new features, kindly submit an [issue on
GitHub](https://github.com/unhcr-dataviz/rmsdown/issues). To contribute
code, don’t hesitate to fork the repository and create a pull request.

## License

This package is distributed under the [MIT
License](https://github.com/unhcr-dataviz/rmsdown/blob/master/LICENSE.md).
