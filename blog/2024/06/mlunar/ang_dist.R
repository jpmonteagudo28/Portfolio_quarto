##--------------------------------------------##
## Angular distances, length of synodic month
## Ascension & declination for Sun & Moon 
##--------------------------------------------##
options(digits = 9)
#> Angular distance formula for any two celestial bodies
#> when distance not near 0 or 180 degrees
#> @params: ra1(1hr = 15 deg of rotation),dec1(+,-) refers to the ascension 
#>          and declination angles in degrees.
#> @params: units refers to the units of your ascension and declination 
#>          measurements. Id `deg` is chosen, the function returns the angle 
#>          in degrees
angle.dist <- function(ra1,dec1,
                       ra2,dec2,
                       units = c("rad",
                                 "deg")){
stopifnot(is.numeric(c(ra1,dec1,
                       ra2,dec2)))
                         
  units <- match.arg(units)
  
  if (units == "deg") {
    ra1 <- deg2rad(ra1)
    dec1 <- deg2rad(dec1)
    ra2 <- deg2rad(ra2)
    dec2 <- deg2rad(dec2)
  }
  
  dist <- sin(dec1) * sin(dec2) + cos(dec1) * cos(dec2) * cos(ra1 - ra2)
  result <- acos(dist)
  
  if (units == "deg") {
    result <- rad2deg(result)
  }
  return(result)
}

