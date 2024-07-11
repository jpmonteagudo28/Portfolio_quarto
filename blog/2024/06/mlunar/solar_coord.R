##--------------------------------------------##
## Solar coordinates 
##--------------------------------------------##
options(digits = 9)
#> High accuracy Solar Coordinates
#> time is measured in Julian milennia (365250 days)
#> values for L and B are in radians
#> 

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

##-----------------------------------------##
## Low accuracy algorithm- to w/i 0".01
##-----------------------------------------##

#> Sun's geometric mean longitude - CHECK
geom.long <- function(time){
  l <- deg2rad(modr(280.46646 + 36000.76983*time + 0.0003032*time^2))
  return(l)
}

#> Mean anomaly of Sun is the same as mean anomaly of earth - CHECK
avg.sun.anomaly <- function(time) {
  m <- deg2rad(modr((357.52911 + 35999.05029*time - 0.0001537*time^2)))
  return(m)
}

#> eccentricity of earth's orbit - CHECK
earth.eccentricity <- function(time){
 e <- 0.016708634 - 0.000042037*time - 0.0000001267*time^2
 return(e)
}

#> Sun's equation of the center C - CHECK
center.sun <- function(time){
  m <- avg.sun.anomaly(time)
  
  c <- (deg2rad(1.914602) - deg2rad(0.004817)*time - 
        deg2rad(0.000014)*time^2)*
        sin(m) + 
       (deg2rad(0.019993) - deg2rad(0.000101)*time) * 
       sin(2*m) + 
       deg2rad(0.000289) * sin(3*m)
  return((c))
}

#> Sun's true longitude - CHECK
sun.true.long <- function(time){
  l <- geom.long(time)
  c <- center.sun(time)
  dot <- modr(l + c)
  return(dot)
}

#> Sun's true anomaly - CHECK
true.sun.anomaly <- function(time){
   m <- avg.sun.anomaly(time) 
   c <- center.sun(time)
   v <- modr(m + c)
   return(v)
}

#> Sun's radius vector - CHECK
find.radius <- function(time){
  v <- true.sun.anomaly(time)
  e <- earth.eccentricity(time)
  
  R <- (1.000001018 * (1 - e^2))/(1 + e*cos(v))
  return(R)
}

omega <- function(time){ #> CHECK
  om <- deg2rad(modr(125.04 - 1934.136*time))
  return(om)
}

#> Sun's apparent longitude - CHECK
sun.app.long <- function(time){
  dot <- sun.true.long(time)
  om <- omega(time)
  lambda <- dot - deg2rad(0.00569) - deg2rad(0.00478)*sin(om)
  return(lambda)
}

#> Nutation in obliquity - CHECK
nutation.obliq <- function(time){
  #> mean elongation of Moon from Sun
  d <- deg2rad(modr(297.85036 + 445267.111480*time - 
       0.0019142*time^2 + (time^3)/189474))
  #> Mean anomaly of Sun
  m <- deg2rad(modr(357.52772 + 35999.050340*time - 
       0.0001603*time^2 - (time^3)/300000))
  #> mean anomaly of moon
  mpr <- deg2rad(modr(134.96298 + 477198.867398*time +
       0.0086972*time^2 + (time^3)/56260))
  #>moon's argument of latitude
  f <- deg2rad(modr(93.27191 + 483202.017538 * time -
       0.0036825*time^2 + (time^3)/327270))
  #> long. of ascending node of moon
  om <- deg2rad(modr(125.04452 - 1934.136261*time +
       0.0020708*time^2 + (time^3)/450000))
  
  nutation_terms <- (
    (92025 + 8.9*time)*cos(om) +
      (5736 -3.1*time)*cos(-2*d + 2*f + 2*om) +
      (977 - 0.5*time)*cos(2*f + 2*om) -
      (895 + 0.5*time)*cos(2*om) +
      (54 - 0.1*time)*cos(m) -
      7*cos(mpr) +
      (224 - 0.6*time)*cos(-2*d + m + 2*f+ 2*om) +
      200 * cos(2*f + om) +
      (129 - 0.1*time)*cos(mpr + 2*f + 2*om) -
      (95 + 0.3*time)*cos(-2*d - m + 2*f + 2*om) +
      cos(-2*d + mpr) -
      70*cos(-2*d + 2*f + om) -
      53*cos(-mpr + 2*f + 2*om) +
      cos(2*d) -
      33*cos(mpr + om) +
      26*cos(2*d - mpr +2*f +2*om) +
      32*cos(-mpr + om) +
      27*cos(mpr + 2*f + om) +
      cos(-2*d + 2*mpr) -
      24*cos(-2*mpr + 2*f + om) +
      16*cos(2*d + 2*f +2*om) +
      13*cos(2*mpr + 2*f +2*om) +
      cos(2*mpr) -
      12*cos(-2*d + mpr + 2*f +2*om) +
      cos(2*f) +
      cos(-2*d + 2*f) -
      10*cos(-mpr + 2*f + om) +
      cos(2*m) -
      8*cos(2*d - mpr +  om) +
      7*cos(-2*d + 2*m + 2*f +2*om) +
      9*cos(m + om) +
      7*cos(-2*d + mpr + om) +
      6*cos(-m + om) +
      cos(2*mpr - 2*f) +
      5*cos(2*d - mpr + 2*f + om) +
      3*cos(2*d + mpr + 2*f + 2*om) -
      3*cos(m + 2*f + 2*om) +
      cos(-2*d + m + mpr) +
      3*cos(-m + 2*f + 2*om) +
      3*cos(2*d + 2*f + om) +
      cos(2*d + mpr) -
      3*cos(-2*d + 2*mpr + 2*f + 2*om) -
      3*cos(-2*d + mpr + 2*f + om) +
      3*cos(2*d -2*mpr + om) +
      3*cos(2*d + om) +
      cos(-m + mpr) +
      3*cos(-2*d -m + 2*f + om) +
      3*cos(-2*d + om) +
      3*cos(2*mpr + 2*f + om) +
      cos(-2*d + 2*mpr + om) +
      cos(-2*d + m + 2*f + om) +
      cos(mpr - 2*f) +
      cos(-d + mpr) +
      cos(-2*d + m) +
      cos(d) +
      cos( mpr + 2*f) +
      cos(-2*mpr + 2*f + 2*om) +
      cos(-d -m + mpr) +
      cos(m + mpr) +
      cos(-m + mpr + 2*f + 2*om) +
      cos(2*d - m - mpr + 2*f + 2*om) +
      cos(3*mpr + 2*f + 2*om) +
      cos(2*d - m + 2*f + 2*om)
  )
   arcsec_delta <- nutation_terms/10000
   deg_delta <- min2deg(0,0,arcsec_delta)
   return(deg_delta)
}

#> Obliquity of the ecliptic - CHECK
obliquity <- function(time){
  U <- time/100
  eps_subzero <- min2deg(23,26,21.448) - 
    min2deg(0,0,4680.93) * U -
    min2deg(0,0,1.55) * U^2 +
    min2deg(0,0,1999.25) * U^3 -
    min2deg(0,0,51.38) * U^4 -
    min2deg(0,0,249.67) * U^5 -
    min2deg(0,0,39.05) * U^6 +
    min2deg(0,0,7.12) * U^7 +
    min2deg(0,0,27.87) * U^8 +
    min2deg(0,0,5.79) * U^9 +
    min2deg(0,0,2.45) * U^10
  
  delta <- nutation.obliq(time)
  epsilon <- eps_subzero + delta
  return(epsilon)
}

#> Nutation in longitude - CHECK
#> it affects celestial longitude of all celestial bodies
nutation.long <- function(time){
  
  d <- deg2rad(modr(297.85036 + 445267.111480*time - 
                      0.0019142*time^2 + (time^3)/189474))
  #> Mean anomaly of Sun
  m <- deg2rad(modr(357.52772 + 35999.050340*time - 
                      0.0001603*time^2 - (time^3)/300000))
  #> mean anomaly of moon
  mpr <- deg2rad(modr(134.96298 + 477198.867398*time +
                        0.0086972*time^2 + (time^3)/56260))
  #>moon's argument of latitude
  f <- deg2rad(modr(93.27191 + 483202.017538 * time -
                      0.0036825*time^2 + (time^3)/327270))
  #> long. of ascending node of moon
  om <- deg2rad(modr(125.04452 - 1934.136261*time +
                       0.0020708*time^2 + (time^3)/450000))
  
  nutation_terms <- (
    (-171996 - 174.2*time)*sin(om) -
     (13187 - 1.6*time)*sin(-2*d + 2*f + 2*om) -
      (2274 - 0.2*time)*sin(2*f + 2*om) +
      (2062 + 0.2*time)*sin(2*om) +
      (1426 - 3.4*time)*sin(m) +
      (712 + 0.1*time)*sin(mpr) -
      (517 + 1.2*time)*sin(-2*d + m + 2*f + 2*om) -
      (386 - 0.4*time)*sin(2*f + om) -
      301*sin(mpr + 2*f + 2*om) +
      (217 - 0.5*time)*sin(-2*d - m + 2*f + 2*om) -
      158*sin(-2*d + mpr) +
      (129 + 0.1*time)*sin(-2*d + 2*f +om) +
      123*sin(-mpr + 2*f + 2*om) +
      63*sin(2*d) +
      (63 + 0.1*time)*sin(mpr + om) -
      59*sin(2*d - mpr + 2*f + 2*om) -
      (58 - 0.1*time)*sin(-mpr + om) -
      51*sin(mpr + 2*f + om) +
      48*sin(-2*d + 2*mpr) +
      46*sin(-2*mpr + 2*f + om) -
      38*sin(2*d +2*f + 2*om) -
      31*sin(2*mpr + 2*f + 2*om) +
      29*sin(2*mpr) +
      29*sin(-2*d + mpr + 2*f + 2*om) +
      26*sin(2*f) -
      22*sin(-2*d + 2*f) +
      21*sin(-mpr + 2*f + om) +
      (17 - 0.1*time)*sin(2*m) +
      16*sin(2*d - mpr + om) -
      (16 + 0.1*time)*sin(-2*d + 2*m + 2*f + 2*om) -
      15*sin(m + om) -
      13*sin(-2*d + mpr + om) -
      12*sin(-m + om) +
      11*sin(2*mpr - 2*f) -
      10*sin(2*d - mpr + 2*f + om) -
      8*sin(2*d + mpr + 2*f + 2*om) +
      7*sin(m + 2*f + 2*om) -
      7*sin(-2*d + m + mpr) -
      7*sin(-m + 2*f + 2*om) -
      7*sin(2*d + 2*f + om) +
      6*sin(2*d + mpr) +
      6*sin(-2*d + 2*mpr + 2*f + 2*om) +
      6*sin(-2*d + mpr + 2*f + om) -
      6*sin(2*d - 2*mpr + om) -
      6*sin(2*d + om) +
      5*sin(-m + mpr) -
      5*sin(-2*d - m + 2*f + om) -
      5*sin(-2*d + om) -
      5*sin(2*mpr + 2*f + om) +
      4*sin(-2*d + 2*mpr + om) +
      4*sin(-2*d + m + 2*f + om) +
      4*sin(mpr - 2*f) -
      4*sin(-d + mpr) -
      4*sin(-2*d + m) -
      4*sin(d) +
      3*sin(mpr + 2*f) -
      3*sin(-2*mpr + 2*f + 2*om) -
      3*sin(-d - m + mpr) -
      3*sin(m + mpr) -
      3*sin(-m + mpr + 2*f + 2*om) -
      3*sin(2*d - m - mpr + 2*f + 2*om) -
      3*sin(3*mpr + 2*f + 2*om) -
      3*sin(2*d - m + 2*f + 2*om)
  )
  arcsec_delta <- nutation_terms/10000
  deg_delta <- min2deg(0,0,arcsec_delta)
  return(deg_delta)
}



