Pre-Processing of Handball-Bundesliga Data
================
Hansjörg Plieninger
2019-05-02

  - [Download Data](#download-data)
  - [Wrangle JSON data](#wrangle-json-data)
  - [Clean and Pre-Process Data](#clean-and-pre-process-data)
      - [Months](#months)

``` r
library(conflicted)
library(dplyr)
conflict_prefer("filter", "dplyr")
#> [conflicted] Will prefer dplyr::filter over any other package
library(purrr)
```

## Download Data

The data are provided on <https://www.dkb-handball-bundesliga.de> in
JSON format and are downloaded and saved as follows.

``` r
hbl_raw <- structure(
    list(season_id = c(33759L, 41816L, 55149L),
         name = structure(1:3,
                          .Label = c("Bundesliga 16/17", "Bundesliga 17/18",
                                     "Bundesliga 18/19"),
                          class = "factor"),
         year = structure(1:3, .Label = c("16/17", "17/18", "18/19"), 
                          class = "factor"),
         raw = list(NULL, NULL, NULL)),
    row.names = c(NA, -3L),
    class = c("tbl_df", "tbl", 
              "data.frame"))

for (ii in seq_len(nrow(hbl_raw))) {
    hbl_raw$raw[[ii]] <- jsonlite::fromJSON(
        paste0("https://hbl.fmp.sportradar.com/feeds/de/Europe:Berlin/gismo/fixtures/149/",
               hbl_raw$season_id[ii]))
    Sys.sleep(runif(1, 0, 3))
}

hbl_raw
#> # A tibble: 3 x 4
#>   season_id name             year  raw       
#>       <int> <fct>            <fct> <list>    
#> 1     33759 Bundesliga 16/17 16/17 <list [2]>
#> 2     41816 Bundesliga 17/18 17/18 <list [2]>
#> 3     55149 Bundesliga 18/19 18/19 <list [2]>

save(hbl_raw, file = "hbl_raw.rda", compress = "bzip2")
```

## Wrangle JSON data

The data for each season are stored in a long, nested list in JSON
format that needs to be converted into a data frame suitable for
analyses in R. Furthermore, the raw data contains many variables
irrelevant herein.

``` r
# 2 extractor functions
get_games <- function(x) {
    x %>%
        select("_id", "title", "round_number", "matches") %>%
        mutate_at("matches", function(x) map(x, "_id")) %>%
        tidyr::unnest(cols = matches)
}

get_season <- function(x) {
    x %>%
        magrittr::extract2("matches") %>%
        map(~select(.x, "_id", "_sid", "code",
                           starts_with("location"),
                           starts_with("play_date"),
                           starts_with("home_team"),
                           starts_with("away_team"),
                           starts_with("statistics"))) %>%
        map_dfr(~jsonlite::flatten(.))
}

### Extract relevant parts of the JSON lists and store them in column 'dat' ###

hbl_raw$raw2 <- map(hbl_raw$raw, c("doc", "data", "tournament", "phases")) %>%
    map(1) %>%
    map("matchdays") %>%
    map(1)

hbl_raw$tmp1 <- map(hbl_raw$raw2, get_games)
hbl_raw$tmp2 <- map(hbl_raw$raw2, get_season)

hbl_raw$dat <- map2(hbl_raw$tmp1, hbl_raw$tmp2,
                    ~left_join(.x, .y, by = c("matches" = "_id")))

hbl_raw
#> # A tibble: 3 x 8
#>   season_id name      year  raw     raw2       tmp1     tmp2       dat     
#>       <int> <fct>     <fct> <list>  <list>     <list>   <list>     <list>  
#> 1     33759 Bundesli~ 16/17 <list ~ <data.fra~ <tibble~ <data.fra~ <tibble~
#> 2     41816 Bundesli~ 17/18 <list ~ <data.fra~ <tibble~ <data.fra~ <tibble~
#> 3     55149 Bundesli~ 18/19 <list ~ <data.fra~ <tibble~ <data.fra~ <tibble~

hbl <- tidyr::unnest(hbl_raw, dat) %>%
    select_if(., ~!is.list(.x)) %>% 
    tibble::rowid_to_column() %>% 
    mutate(play_date.date  = lubridate::dmy(play_date.date))

# Rename variables
names(hbl) <- sub("^_", "", names(hbl)) %>%
    sub("^statistics.", "", .) %>%
    sub("^play_date[.]([a-z]+)", "\\1", .) %>%
    sub("_team.name", "_team", .) %>%
    sub("[.]_id", ".id", .) %>%
    sub("[.]_sid", ".sid", .) %>%
    sub("[.]|[.]_", "_", .) %>%
    make.names(unique = TRUE)

hbl$weekday <- factor(hbl$weekday, levels = 1:7)

# Lemgo changed its name some day
hbl$home_team[hbl$home_team == "TBV Lemgo"] <- "TBV Lemgo Lippe"
hbl$away_team[hbl$away_team == "TBV Lemgo"] <- "TBV Lemgo Lippe"
```

Now, `hbl` is in a format one can work with, see output below.

However, it is also clear that the data are not perfect, for example,
the correct capacity is missing in row 3 and the attendance is larger
than the capacity in row
13.

``` r
select(hbl, rowid, year, home_team, away_team, attendance, capacity) %>% 
    print(n = 13)
#> # A tibble: 918 x 6
#>    rowid year  home_team            away_team           attendance capacity
#>    <int> <fct> <chr>                <chr>                    <int>    <int>
#>  1     1 16/17 HSG Wetzlar          Füchse Berlin             4083     4412
#>  2     2 16/17 Rhein-Neckar Löwen   SC Magdeburg              7105    13200
#>  3     3 16/17 SG Flensburg-Handew~ HC Erlangen               5837        0
#>  4     4 16/17 VfL Gummersbach      HBW Balingen-Weils~       3288     4050
#>  5     5 16/17 TBV Lemgo Lippe      TSV GWD Minden            4150     4861
#>  6     6 16/17 FRISCH AUF! Göpping~ TSV Hannover-Burgd~       4400     5600
#>  7     7 16/17 SC DHfK Leipzig      Bergischer HC             3354     6500
#>  8     8 16/17 MT Melsungen         HSC 2000 Coburg           3268     4300
#>  9     9 16/17 TVB 1898 Stuttgart   THW Kiel                  6211     6211
#> 10    10 16/17 HSC 2000 Coburg      Rhein-Neckar Löwen        3274     3500
#> 11    11 16/17 TSV Hannover-Burgdo~ SG Flensburg-Hande~       4100     4150
#> 12    12 16/17 HBW Balingen-Weilst~ HSG Wetzlar               2250    10285
#> 13    13 16/17 HC Erlangen          MT Melsungen              3122     1400
#> # ... with 905 more rows
```

## Clean and Pre-Process Data

``` r
### Select subset of variables ###

df1 <- hbl %>%
    select(rowid, season_id, year, home_team, away_team,
           capacity, attendance, occupancy,
           round_number, date, weekday,
           # location_id, location_name, location_city,
           home_team_id, home_team_abbreviation,
           away_team_id, away_team_abbreviation)


### Fix attendance and capacity ###

# Fix missing capacity
df1[df1$rowid ==   3 & df1$capacity == 0, "capacity"] <- 6300

# Fix wrong capacity
df1[df1$rowid ==  13 & df1$capacity == 1400, "capacity"] <- 7850
df1[df1$rowid ==  52 & df1$capacity == 4150, "capacity"] <- 10000
df1[df1$rowid == 162 & df1$capacity == 4150, "capacity"] <- 10000
df1[df1$rowid == 160 & df1$capacity == 2251, "capacity"] <- 6211

df1$occupancy <- df1$attendance/df1$capacity*100

df1 <- filter(df1, occupancy < 120) %>% 
    filter(occupancy > 0 | season_id == 55149)

df1[df1$attendance > df1$capacity, "attendance"] <-
    df1[df1$attendance > df1$capacity, "capacity"]

df1$perc <- df1$attendance/df1$capacity


### Further wrangling ###

# Dummy coding of weekday with Saturday as the reference
contrasts(df1$weekday) <- contr.treatment(7, base = 6)

# Center variable 'machtday' (34 games per season)
df1$matchday <- df1$round_number - 17.5

# Sort teams according to final standings in season 2017/18
tmp1 <- c("SG Flensburg-Handewitt", "Rhein-Neckar Löwen", "Füchse Berlin",
          "SC Magdeburg", "THW Kiel", "TSV Hannover-Burgdorf", "MT Melsungen",
          "SC DHfK Leipzig", "TBV Lemgo Lippe", "FRISCH AUF! Göppingen",
          "HSG Wetzlar", "TSV GWD Minden", "HC Erlangen", "TVB 1898 Stuttgart",
          "VfL Gummersbach", "Die Eulen Ludwigshafen", "TuS N-Lübbecke",
          "TV 05/07 Hüttenberg")
df1 <- mutate_at(df1, vars(home_team, away_team),
                 .funs = forcats::fct_relevel, tmp1)
```

### Months

The categorical variable month has eight values, namely, September to
May. These could be used in a model via, for example, seven dummy-coded
variables. However, I want to make more tailored comparisons, namely, I
want to compare all matches in the first half of the season with all
matches in the second half. To this end, I create a dummy variable
`half2` in the following. With eight months, I need to make seven
comparisons and have alredy defined one. For the six remaining
contrasts, I employ comparisons of successive differences within each of
the two halves of the season.

``` r
# Wrangle month for better modeling and plotting
df1 <- df1 %>%
    group_by(year, round_number) %>% 
    mutate(
        # Calculate a common Date for all 9 matches in the same round_number for
        #  a better x-value for plotting
        Date = median(date),
        # Date6 = as.Date(cut.Date(Date, "week")) + 6,
        # Only few matches in Aug, Jan, & June, set those to Sep, Feb, & May
        Month = lubridate::month(Date) %>%
            factor(levels = c(8:12, 1:6)) %>%
            forcats::fct_relabel(.,
                                 car::recode,
                                 recodes = "8=9; 6=5; 1=2",
                                 as.factor = FALSE,
                                 as.numeric = FALSE)) %>% 
    ungroup

### Contrasts ###

contr.month <- cbind(rep(0:1, each = 4),           # 1st vs 2nd half
                     rbind(codingMatrices::code_diff(4),    # successive diffs
                           matrix(0, 4, 3)),
                     rbind(matrix(0, 4, 3),
                           codingMatrices::code_diff(4))    # successive diffs
)
colnames(contr.month) <- c("half2", "Oct", "Nov", "Dec", "Mar", "Apr", "May")
rownames(contr.month) <- NULL

# Add variable 'Month' for the left_join() below
contr.month <- data.frame(Month = forcats::as_factor(levels(df1$Month)), contr.month)
df1 <- left_join(df1, contr.month, by = "Month")

contr.month
#>   Month half2   Oct  Nov   Dec   Mar  Apr   May
#> 1     9     0 -0.75 -0.5 -0.25  0.00  0.0  0.00
#> 2    10     0  0.25 -0.5 -0.25  0.00  0.0  0.00
#> 3    11     0  0.25  0.5 -0.25  0.00  0.0  0.00
#> 4    12     0  0.25  0.5  0.75  0.00  0.0  0.00
#> 5     2     1  0.00  0.0  0.00 -0.75 -0.5 -0.25
#> 6     3     1  0.00  0.0  0.00  0.25 -0.5 -0.25
#> 7     4     1  0.00  0.0  0.00  0.25  0.5 -0.25
#> 8     5     1  0.00  0.0  0.00  0.25  0.5  0.75
```

The interpretation of the estimates pertaining to each of the contrasts
can be nicely illustrated using `mean_contrasts()`. As seen in the
following, the predictor `half2` (second row) tests whether the means in
last four months (i.e., Feb–May, i.e., `m5`–`m8`) are larger than those
in the first four months (Sep–Dec). Furthermore, the predictor `Oct`
tests if the mean in Oct is larger than that in Sep, and so on. Last,
the intercept—because of dummy coding used for `half2`—is equal to the
mean in the first half of the season.

``` r
codingMatrices::mean_contrasts(cbind(Intercept = 1, contr.month[, -1]))
#>           m1   m2   m3   m4   m5   m6   m7   m8  
#> Intercept  1/4  1/4  1/4  1/4    .    .    .    .
#> half2     -1/4 -1/4 -1/4 -1/4  1/4  1/4  1/4  1/4
#> Oct         -1    1    .    .    .    .    .    .
#> Nov          .   -1    1    .    .    .    .    .
#> Dec          .    .   -1    1    .    .    .    .
#> Mar          .    .    .    .   -1    1    .    .
#> Apr          .    .    .    .    .   -1    1    .
#> May          .    .    .    .    .    .   -1    1
```
