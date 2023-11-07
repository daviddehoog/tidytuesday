# Tidy Tuesday -
David de Hoog

``` r
library(tidytuesdayR)
library(tidyverse)
library(scales)
library(ggtext)

tuesdata <- tidytuesdayR::tt_load('2023-11-07')
```


        Downloading file 1 of 1: `house.csv`

``` r
house <- tuesdata$house
```

### Data

This week’s dataset is House of Representatives elections in the United
States from 1976 - 2022.

``` r
glimpse(house)
```

    Rows: 32,452
    Columns: 20
    $ year           <dbl> 1976, 1976, 1976, 1976, 1976, 1976, 1976, 1976, 1976, 1…
    $ state          <chr> "ALABAMA", "ALABAMA", "ALABAMA", "ALABAMA", "ALABAMA", …
    $ state_po       <chr> "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "…
    $ state_fips     <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    $ state_cen      <dbl> 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63,…
    $ state_ic       <dbl> 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41,…
    $ office         <chr> "US HOUSE", "US HOUSE", "US HOUSE", "US HOUSE", "US HOU…
    $ district       <chr> "001", "001", "001", "002", "002", "002", "003", "003",…
    $ stage          <chr> "GEN", "GEN", "GEN", "GEN", "GEN", "GEN", "GEN", "GEN",…
    $ runoff         <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,…
    $ special        <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,…
    $ candidate      <chr> "BILL DAVENPORT", "JACK EDWARDS", "WRITEIN", "J CAROLE …
    $ party          <chr> "DEMOCRAT", "REPUBLICAN", NA, "DEMOCRAT", "REPUBLICAN",…
    $ writein        <lgl> FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, T…
    $ mode           <chr> "TOTAL", "TOTAL", "TOTAL", "TOTAL", "TOTAL", "TOTAL", "…
    $ candidatevotes <dbl> 58906, 98257, 7, 66288, 90069, 5, 106935, 1111, 2, 3453…
    $ totalvotes     <dbl> 157170, 157170, 157170, 156362, 156362, 156362, 108048,…
    $ unofficial     <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,…
    $ version        <dbl> 20230706, 20230706, 20230706, 20230706, 20230706, 20230…
    $ fusion_ticket  <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,…

Unlike the United States (and many other places) Australia has
compulsory voting in elections. In House of Representatives elections in
Australia we use a preferential voting system, so once all the
preferences are distributed the final result is a two-candidate
preferred one. For example, in a safe seat, the winning candidate might
secure 41 per cent of the primary vote, and once preferences are
distributed and all but one other candidate is eliminated, the winner
might secure 58 per cent of the total votes.

I’m interested in how many votes (as a proportion) it takes to win a
seat in the US House of Representatives and how that might have changed
over time.

``` r
# Find the winning candidate in each election
# To start, make it a bit more manageable with only one state (Illinois, as the state that hosted this year's PositConf) and the general election stage (so omit the primaries).

house |>
  filter(
    state == "ILLINOIS" & 
    stage == "GEN" &
    is.na(party) != TRUE &
    party != "WRITE-IN" # &
    #special == FALSE &
    #runoff == FALSE
    ) |>
  mutate(
    party = as.factor(party),
    party = fct_lump_n(party, 2, other_level = "Other"),
    party = fct_recode(party, 
                       "Republican" = "REPUBLICAN",
                       "Democrat" = "DEMOCRAT")
    ) |>
  group_by(year, district, party) |>
  mutate(pct_vote = candidatevotes / totalvotes) |>
  select(year, state, district, party, candidatevotes, totalvotes, pct_vote) -> house_vis
```

``` r
house_vis |>
  filter(year == 1980) |>
  summarize(mean_pct_vote = mean(pct_vote) * 100)
```

    `summarise()` has grouped output by 'year', 'district'. You can override using
    the `.groups` argument.

    # A tibble: 49 × 4
    # Groups:   year, district [24]
        year district party      mean_pct_vote
       <dbl> <chr>    <fct>              <dbl>
     1  1980 001      Democrat           95.5 
     2  1980 001      Republican          4.52
     3  1980 002      Democrat           88.1 
     4  1980 002      Republican         11.8 
     5  1980 003      Democrat           68.9 
     6  1980 003      Republican         31.1 
     7  1980 004      Democrat           32.0 
     8  1980 004      Republican         68.0 
     9  1980 005      Democrat           79.6 
    10  1980 005      Republican         20.4 
    # ℹ 39 more rows

### Visualising

``` r
# Set up colour palette; # republican = red, democrat = blue, others = grey
party_palette <- c("blue", "red", "grey") 

# Averaging up districts to state level
house_vis |>
  group_by(year, party) |>
  summarize(.groups = "keep", mean_pct_vote = mean(pct_vote) * 100) |>
  ggplot(aes(x = year, y = mean_pct_vote, color = party)) +
    scale_colour_manual(values = party_palette) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = 50, linetype = "dotted") +
    labs(title = "<span style='color:red'>Republican</span> and <span style='color:blue'>Democrat</span> votes in House General Elections (Illinois: 1976 - 2022)") + 
    theme(
      axis.ticks = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 0.5),
      plot.title = element_markdown(),
      legend.position = "none"
    ) +
    scale_x_continuous(
      name = NULL,
      breaks = seq(1976, 2022, 4),
      labels = seq(1976, 2022, 4),
      minor_breaks = seq(1976, 2022, 2)
    ) +
    scale_y_continuous(
      name = NULL,
      breaks = seq(0, 60, 10),
      labels = label_percent(scale = 1),
      minor_breaks = NULL
    )
```

![Republican and Democrat votes in House General Elections (Illinois:
1976 - 2022)](analysis_files/figure-commonmark/vis-plot-1.png)

``` r
rm(tuesdata)
rm(house)
rm(house_vis)
rm(party_palette)
```

### References

(**data?**){DVN/IG0UN2_2017, author = {MIT Election Data and Science
Lab}, publisher = {Harvard Dataverse}, title = {{U.S. House 1976–2022}},
UNF = {UNF:6:A6RSZvlhh8eRZ4+mvT/HRQ==}, year = {2017}, version = {V12},
doi = {10.7910/DVN/IG0UN2}, url = {https://doi.org/10.7910/DVN/IG0UN2} }
