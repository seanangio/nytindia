
<!-- README.md is generated from README.Rmd. Please edit that file. You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/master/examples>. -->

# nytindia - Prepare and visualize India-tagged data from the NYT Article Search API

<!-- badges: start -->
<!-- badges: end -->

This package includes functions to query the [Article Search API of The
New York
Times](https://developer.nytimes.com/docs/articlesearch-product/1/overview)
for articles with an “India” location keyword. It also includes
functions to prepare this data to be ready for analysis, as well as a
[shiny app](https://seanangio.shinyapps.io/project/) to visualize the
output dataset.

## Installation

The impetus to restructure what was originally a data analysis project
as a package was:

-   to simplify updating the dataset over time
-   to learn how to make a package.

Accordingly, this is mostly a personal package, but it can be installed
from GitHub:

``` r
install.packages("devtools")
devtools::install_github("seanangio/nytindia")
```

## Query Data

To query all articles with an India location keyword from the NYT
Article Search API between two dates, run the following in the top level
folder of a new RStudio project.

``` r
library(nytindia)
nyt_get_data(begin_date = "YYYY-MM-DD", 
             end_date = "YYYY-MM-DD")
```

You’ll need environmental variables called “NYTIMES\_KEY” and
“NYT\_USER\_AGENT”.

If the directory is empty, leaving `begin_date` empty will default to
1851 (the earliest available date). Otherwise, it will begin from the
last date found. Leaving `end_date` empty will default to the current
date.

It queries up to the closest completed month.

The query searches for all articles with an “India” location keyword.
You should be able to change the default query with the parameters “q”
and “fq”. See the reference documentation
[here](https://seanangio.github.io/nytindia/reference/nyt_get_data.html).

## Prepare Data

Once you have your data from the API, there is a pipeline of functions
available to prepare the data. You can run the following to create the
prepared dataset.

``` r
nyt_build_data()
```

This should successfully output a dataset in a folder called
`nyt_shiny_app`, but there are a number of manual steps that should
ideally be included – as explained in the reference documentation and
the [Technical
Details](https://seanangio.github.io/nytindia/articles/technical-details.html)
vignette.

-   renaming news desks
-   renaming keyword values
-   renaming categories of keywords
-   geocoding locations

These are optional, but without them:

-   the map will be empty,
-   the keywords and news desk categories may be overlapping depending
    on the expanse of time you are querying,
-   the keyword heatmap may be broken.

That’s why `nyt_build_data()` is especially useful when just updating a
dataset with a new month, when the lookup tables and geocoding has
already been done.

To build the dataset step by step, use the following script. This is
essentially what `nyt_build_data()` is doing.

``` r
# 01-query-nyt-api.R
api_df <- nyt_bind_api_files()

# 02-prepare-nested.R
combined_df <- nyt_clean_api_tbl(api_df)

# 03-clean-news-desks.R
nested_df <- nyt_clean_news_desks(combined_df)

# 04-unnest-df.R
unnested_df <- nyt_unnest_df(nested_df)

# 05-clean-keywords.R
consolidated_unnested_df <- nyt_clean_keywords(unnested_df)

# 06-fix-keywords.R
unnested_df_values_fixed <- nyt_fix_keywords(consolidated_unnested_df)

# 07-query-mapquest.R
nyt_query_mapquest_api(unnested_df_values_fixed)

# 08-add-coords-countries.R
full_unnested_df <- nyt_join_coords_countries(unnested_df_values_fixed)

# 09-re-nest-keywords.R
full_nested_df <- nyt_re_nest_keywords(full_unnested_df)

# 10-write-final-nested-df.R
nyt_write_final_nested_df(full_nested_df)

# 11-download-shiny-files.R
nyt_download_shiny_files()
```

## Run the Shiny App

The package includes a [shiny
app](https://github.com/seanangio/nytindia/tree/master/inst/examples/nyt_india_app)
to visualize the results in many different ways.

<img src="/Users/seanangiolillo/Google Drive/nytindia/inst/examples/nyt_india_app/screenshots/nyt-india-app-homepage.png" width="100%" />

To run it locally, you’ll need the following packages included in the
“Suggests” section of this package’s DESCRIPTION file.

``` r
shiny_pkgs <- c("shiny","markdown","ggplot2","forcats","scales",
                "shinyWidgets","bsplus","shinycssloaders","DT",
                "ggiraph","tidytext","tsbox","dygraphs","gt",
                "leaflet","leaflet.extras","shinydashboard",
                "waiter")

install.packages(shiny_pkgs)

nyt_run_example("nyt_india_app")
```

## Analysis

You can find package vignettes [analyzing the
data](https://seanangio.github.io/nytindia/articles/analysis.html) and
summarizing the [technical
details](https://seanangio.github.io/nytindia/articles/technical-details.html).
