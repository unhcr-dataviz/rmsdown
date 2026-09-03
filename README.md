
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rmsdown

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/vidonne/rmsdown/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/vidonne/rmsdown/actions/workflows/R-CMD-check.yaml)
<!-- [![CRAN status](https://www.r-pkg.org/badges/version/rmsdown)](https://CRAN.R-project.org/package=rmsdown) -->
<!-- badges: end -->

Create high-quality, UNHCR-branded PDFs for Results Monitoring Surveys
(RMS) with the `{rmsdown}` R package. This package leverages the
`{pagedown}` framework to deliver consistent and professional reports
aligned with **UNHCR’s brand guidelines**. Additionally, it includes a
dedicated plot function for branded charts.

## Features

- A specialized R Markdown template for RMS reports
- Custom CSS styling with Bootstrap 5 integration
- A plot function for consistent and branded visualizations
- Alignment with UNHCR’s branding and guidelines

## Installation

Install the development version of `{rmsdown}` from GitHub with:

``` r
# If pak is not yet installed, uncomment the following line:
# install.packages("pak")
pak::pkg_install("vidonne/rmsdown")
```

## Usage

### Getting Started

To use the `{rmsdown}` template, specify the `rmsdown::paged_report`
format in the YAML header of your R Markdown file:

``` yaml
---
title: "Results Monitoring Surveys"
subtitle: "Country Year"
output: rmsdown::paged_report
---
```

### Creating a New Document in RStudio

1.  File → New File → R Markdown
2.  Select “From Template”
3.  Choose the **RMS Paged Report** template

### Custom CSS Classes

The `{rmsdown}` template incorporates Bootstrap 5 for styling. Refer to
the [Bootstrap
documentation](https://getbootstrap.com/docs/5.3/getting-started/introduction/)
for details on available CSS classes.

For section-specific styling, use the following format:

``` r

###### [1.]{.big-num} Introduction {.section-title}

# Introduction {.hide-h1}

## The Forced Displacement Survey

The following content starts here.
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

## Additional UNHCR Tools

- [**unhcrthemes**](https://github.com/vidonne/unhcrthemes): UNHCR
  branded ggplot2 theme and color palettes
- [**unhcrdown**](https://github.com/vidonne/unhcrdown): UNHCR templates
  for R Markdown
- [**refugees**](https://github.com/PopulationStatistics/refugees):
  Access to [UNHCR Refugee Data
  Finder](https://www.unhcr.org/refugee-statistics) API
- [**quarto-html-unhcr**](https://github.com/vidonne/quarto-html-unhcr):
  Quarto extension for UNHCR branded HTML documents
- [**quarto-revealjs-unhcr**](https://github.com/vidonne/quarto-revealjs-unhcr):
  Quarto extension for UNHCR branded presentations
- [**Data Visualization Platform**](https://dataviz.unhcr.org/): UNHCR’s
  data visualization guidelines and tools

## Acknowledgements

We extend our gratitude to the creators of the
[`pagedown`](https://github.com/rstudio/pagedown) package that
influenced the development of `{rmsdown}`.

## Contribution and Code of Conduct

Contributions to `{rmsdown}` are highly valued. To ensure a welcoming
and inclusive community, we follow our [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
If you have suggestions, uncover bugs, or envision new features, kindly
submit an [issue on GitHub](https://github.com/vidonne/rmsdown/issues).

## License

This package is distributed under the [MIT
License](https://github.com/vidonne/rmsdown/blob/master/LICENSE.md).
