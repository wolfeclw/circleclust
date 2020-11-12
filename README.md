# circleclust

Circular or [directional statistics](https://en.wikipedia.org/wiki/Directional_statistics) is the study of directional or angular data.  Examples include data that is periodic in nature such as time (i.e. hours of the day, days of the year) and data that has no true zero such as compass bearing or wind direction. Circular statistics have been broadly applied across scientific disciplines. `circleclust` is most closely related to applications in movement ecology, or the movement of organisms.

`circleclust` is a collection of functions that facilitate the analysis of personal location data to decipher patterns in mobility.  The `circleclust::circleclust()` algorithm classifies coordinates into distinct spatiotemporal clusters based on circular variance, or the variability in the bearing between sequential points or [dihedral angles](https://en.wikipedia.org/wiki/Dihedral_angle).

`circleclust` also includes functions to aggregate data by time unit, impute missing location data, and spatial functions to transform and visualize clustered data.

## Installation

To install `circleclust`, use the following code:

``` r
install.packages("devtools")
devtools::install_github("wolfeclw/circleclust")
library(circleclust)
```
