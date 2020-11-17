# circleclust

`circleclust` is a collection of functions that facilitate the analysis of personal location data to decipher patterns in mobility using circular or [directional statistics](https://en.wikipedia.org/wiki/Directional_statistics). Though circular statistics have been broadly applied across scientific disciplines, `circleclust` is most closely related to applications in movement ecology, or the movement of organisms.

The `circleclust()` algorithm classifies coordinates into distinct spatiotemporal clusters based on circular variance, or the variability in the bearing between sequential points or [dihedral angles](https://en.wikipedia.org/wiki/Dihedral_angle).

## Examples

The algorithm calculates the circular variance within a moving window and classifies coordinates as either 'static' or 'mobile' based on departures from a threshold value.  The map below shows personal location data recorded while talking a stroll through the Cincinnati Zoo and Botanical Gardens.  Purple dots represent periods of mobile activity and yellow dots represent periods of static activity.

![](./docs/zoo_deck.gif)

Plotting the bearing (azimuth) between sequential points gives an under the hood look at how the algorithm classifies coordinates. We see a higher degree of variability in the bearings classified as 'static', while the purple 'mobile' bearings demonstrate a consistent trajectory.

![](./docs/hc_zoo.png)

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
install.packages("devtools")
devtools::install_github("wolfeclw/circleclust")
library(circleclust)
```

#### Related Packages

A significant portion of this codebase can be found in the [`pufpR`](https://github.com/wolfeclw/pufpR) package. `pufpR` is a collection of functions built to import, wrangle, and analyze data recorded by the Enmont personal ultrafine particle counter (PUFP).
