HW2
================
Xiangxiong Feng
2023-10-02

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

# clean the data in pols-month.csv

``` r
polsmonth_df = 
  read_csv('data/pols-month.csv') |>
  janitor:: clean_names() |>
  separate(mon, into = c('year', 'month'), sep = 4) |>
  separate(month, into = c('month', 'day'), sep = 3) |>
  mutate(
    month = replace(month, month == '-01', 'Jan'),
    month = replace(month, month == '-02', 'Feb'),
    month = replace(month, month == '-03', 'Mar'),
    month = replace(month, month == '-04', 'Apr'),
    month = replace(month, month == '-05', 'May'),
    month = replace(month, month == '-06', 'Jun'),
    month = replace(month, month == '-07', 'Jul'),
    month = replace(month, month == '-08', 'Aug'),
    month = replace(month, month == '-09', 'Sep'),
    month = replace(month, month == '-10', 'Oct'),
    month = replace(month, month == '-11', 'Noc'),
    month = replace(month, month == '-12', 'Dec')
  ) |>
  rename(president = prez_dem) |>
  mutate(
    president = replace(president, president == '0', 'gop'),
    president = replace(president, president == '1', '0'),
    president = replace(president, president == 'gop', '1')
    ) |>
  select(-prez_gop,-day)
```

    ## Rows: 822 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (8): prez_gop, gov_gop, sen_gop, rep_gop, prez_dem, gov_dem, sen_dem, r...
    ## date (1): mon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

# clean data in snp.csv

``` r
library(lubridate)
library(dplyr)
snp_df = 
  read_csv('data/snp.csv') |>
  janitor::clean_names() |>
  mutate(
    date = parse_date_time(date, order = c('mdy'))) |>
  separate(date, into = c('year', 'month'), sep = 4) |>
  separate(month, into = c('month', 'day'), sep = 3) |>
  mutate(
    month = replace(month, month == '-01', 'Jan'),
    month = replace(month, month == '-02', 'Feb'),
    month = replace(month, month == '-03', 'Mar'),
    month = replace(month, month == '-04', 'Apr'),
    month = replace(month, month == '-05', 'May'),
    month = replace(month, month == '-06', 'Jun'),
    month = replace(month, month == '-07', 'Jul'),
    month = replace(month, month == '-08', 'Aug'),
    month = replace(month, month == '-09', 'Sep'),
    month = replace(month, month == '-10', 'Oct'),
    month = replace(month, month == '-11', 'Noc'),
    month = replace(month, month == '-12', 'Dec')) |>
  select(-day)
```

    ## Rows: 787 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): date
    ## dbl (1): close
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
