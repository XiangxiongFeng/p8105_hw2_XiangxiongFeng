HW2
================
Xiangxiong Feng
2023-10-02

# Problem 1

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

## clean the data in pols-month.csv

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
    month = replace(month, month == '-12', 'Dec'),
    month = str_to_upper(month)
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

## clean data in snp.csv

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
    month = replace(month, month == '-12', 'Dec'),
    month = str_to_upper(month)) |>
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

## clean data in unemployment.csv

``` r
unemployment_df = 
  read_csv('data/unemployment.csv') |>
  janitor::clean_names() |>
  pivot_longer(
    jan:dec,
    names_to = 'month',
    values_to = 'unemployment_rate'
  ) |>
  mutate(
    month = str_to_upper(month),
  
  )
```

    ## Rows: 68 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (13): Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

## Joining dataset

``` r
pol_snp_df = 
  left_join(polsmonth_df,snp_df ) |>
  mutate(year = as.numeric(year))
```

    ## Joining with `by = join_by(year, month)`

``` r
final_df =
  left_join(pol_snp_df, unemployment_df)
```

    ## Joining with `by = join_by(year, month)`

The final dataset includes 11 variables, some important variables are
year, month and unemployment rate. The range of the year is from 1947 to
2015.

# Problem 2

\#import and clean Mr. Trash Wheel

``` r
library( readxl)
Mr.Trash_wheel_df = 
  read_excel('data/202309 Trash Wheel Collection Data.xlsx', sheet = 'Mr. Trash Wheel') |>
  janitor::clean_names() |>
  drop_na(dumpster) |>
  mutate(
    homes_powered = (weight_tons*500)/30,
    Wheel_Type = 'Mr.Trash Wheel',
    year = as.numeric(year)
  )
```

    ## New names:
    ## • `` -> `...15`
    ## • `` -> `...16`

# import and clean Professor Trash Wheel

``` r
Professor_Trash_df = 
  read_excel('data/202309 Trash Wheel Collection Data.xlsx', sheet = 'Professor Trash Wheel') |>
  janitor::clean_names() |>
  drop_na(dumpster) |>
  mutate(
    homes_powered = (weight_tons*500)/30,
    Wheel_Type = 'Professor Trash Wheel'
  )
```

# Import and clean

``` r
Gwynnda_Trash_df = 
  read_excel('data/202309 Trash Wheel Collection Data.xlsx', sheet = 'Gwynnda Trash Wheel') |>
  janitor::clean_names() |>
  drop_na(dumpster) |>
  mutate(
    homes_powered = (weight_tons*500)/30,
    Wheel_Type = 'Gwynnda Trash Wheel'
  )
```

# Combine dataset to produce a single tidydataset

``` r
# join Mr.Trash_wheel_df and Professor_Trash_df
T_TrashWheel_df = 
  full_join(Mr.Trash_wheel_df, Professor_Trash_df)
```

    ## Joining with `by = join_by(dumpster, month, year, date, weight_tons,
    ## volume_cubic_yards, plastic_bottles, polystyrene, cigarette_butts,
    ## glass_bottles, plastic_bags, wrappers, homes_powered, Wheel_Type)`

``` r
#join Gwynnda_Trash_df into result above to gain final data
Final_TrashWheel_df =
  full_join(T_TrashWheel_df, Gwynnda_Trash_df)
```

    ## Joining with `by = join_by(dumpster, month, year, date, weight_tons,
    ## volume_cubic_yards, plastic_bottles, polystyrene, cigarette_butts,
    ## plastic_bags, wrappers, homes_powered, Wheel_Type)`
