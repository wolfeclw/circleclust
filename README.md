# circleclust

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

`circleclust` is a collection of functions that facilitate the analysis of personal location data to decipher patterns in mobility using circular or [directional statistics](https://en.wikipedia.org/wiki/Directional_statistics). 

The `circleclust()` function classifies coordinates into distinct spatiotemporal clusters based on circular variance, or the variability in the bearing between sequential points or [dihedral angles](https://en.wikipedia.org/wiki/Dihedral_angle). The underlying algorithm calculates circular variance within a moving window and classifies coordinates as either 'static' or 'mobile' based on departures from a threshold value.

## Examples

The map below shows personal location data recorded while talking a stroll through the Cincinnati Zoo and Botanical Gardens.  Purple dots represent periods of mobile activity and yellow dots represent periods of static activity at various exhibits.

![](./man/figures/zoo_deck.gif)

Plotting the bearing (azimuth) between sequential points gives an under the hood look at how the algorithm classifies coordinates. We see a higher degree of variability in the bearings classified as 'static', while the purple 'mobile' bearings demonstrate a consistent trajectory.

![](./man/figures/hc_zoo.png)

#### A typical workflow looks something like this:

  1. Import location data
  2. Impute missing coordinates using `impute_coords()`
  3. Aggregate location data by the desired time unit using `dt_aggregate()`
  4. Calculate the speed and bearing between sequential coordinates with `move()`
  5. Apply the clustering algorithm with `circleclust()`
  6. Create an `sf` object with `wgs_sf()` and visualize with `cluster_deck()`
  
``` r
d_clusters <- zoo_trip %>% 
  impute_coords('Date_Time') %>%
  dt_aggregate('Date_Time') %>%
  move('Date_Time') %>%
  circleclust('Date_Time', pl_dist_threshold = 25, show_circvar = TRUE)

wgs_sf(d_clusters) %>% 
  cluster_deck(fill_colour = 'activity_status')
```

## Installation

To install `circleclust`, use the following code:

``` r
devtools::install_github("wolfeclw/circleclust")
```

## Citation

If you use this software in a scientific manuscript, please cite our publication:

  - *Turner A, Wolfe C, Ryan PH. Personal exposure to ultrafine particles in multiple microenvironments among adolescents. J Expo Sci Environ Epidemiol. 2024 Feb 28. doi: 10.1038/s41370-023-00638-7. PMID: 38418826.*

