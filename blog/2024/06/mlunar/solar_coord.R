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

eliptic.long <- function(time){
  
  l_0 <- (
      175347046*cos(0) +
      3341656*cos(4.6692568 + 6283.075850*time) +
      34894*cos(4.62610 + 12566.1517*time) +
      3497*cos(2.7441 + 5753.3849*time) +
      3418*cos(2.8289 + 3.5231*time) +
      3136*cos(3.6277 + 77713.7715*time) +
      2676*cos(4.4181 + 7860.4194*time) +
      2343*cos(6.1352 + 3930.2097*time) +
      1324*cos(0.7425 + 11506.7698*time) +
      1273*cos(2.0371 + 529.691*time) +
      1199*cos(1.1096 + 1577.3435*time) +
      990*cos(5.233 + 5884.927*time) +
      902*cos(2.045 + 26.298*time) +
      857*cos(3.508 + 398.149*time) +
      780*cos(1.179 + 5223.694*time) +
      753*cos(2.533 + 5507.553*time) +
      505*cos(4.583 + 18849.228*time) +
      492*cos(4.205 + 775.523*time) +
      357*cos(2.920 + 0.067*time) +
      317*cos(5.849 + 11790.629*time) +
      284*cos(1.899 + 796.298*time) +
      271*cos(0.315*10977.079*time) +
      243*cos(0.345 + 5486.778*time) +
      206*cos(4.806 + 2544.314*time) +
      205*cos(1.869 + 5573.143*time) +
      202*cos(2.458 + 6069.777*time) +
      156*cos(0.833 + 213.299*time) +
      132*cos(3.411 + 2942.463*time) +
      126*cos(1.083 + 20.775*time) +
      115*cos(0.645 + 0.980*time) +
      103*cos(0.636 + 4694.003*time) +
      102*cos(0.976 + 15720.839*time) +
      102*cos(4.267 + 7.114*time) +
      99*cos(6.21 + 2146.17*time) +
      98*cos(0.68 + 155.42*time) +
      86*cos(5.98 + 161000.69*time) +
      85*cos(1.30 + 6275.96*time) +
      85*cos(3.67 + 71430.70*time) +
      80*cos(1.81 + 17260.15*time) +
      79*cos(3.04 + 12036.46*time) +
      75*cos(1.76 + 5088.63*time) +
      74*cos(3.50 + 3154.69*time) +
      74*cos(4.68 + 801.82*time) +
      70*cos(0.83 + 9437.76*time) +
      62*cos(3.98 + 8827.39*time) +
      61*cos(1.82 + 7084.90*time) +
      57*cos(2.78 + 6286.60*time) +
      56*cos(4.39 + 14143.50*time) +
      56*cos(3.47 + 6279.55*time) +
      52*cos(0.19 + 12139.55*time) +
      52*cos(1.33 + 1748.02*time) +
      51*cos(0.28 + 5856.48*time) +
      49*cos(0.49 + 1194.45*time) +
      41*cos(5.37 + 8429.24*time) +
      41*cos(2.40 + 19651.05*time) +
      39*cos(6.17 + 10447.39*time) +
      37*cos(6.04 + 10213.29*time) +
      37*cos(2.57 + 1059.38**time) +
      36*cos(1.71 + 2352.87*time) +
      36*cos(1.78 + 6812.77*time) +
      33*cos(0.59 + 17789.85*time) +
      30*cos(0.44 + 83996.85*time) +
      30*cos(2.74 + 1349.87*time) +
      25*cos(3.16 + 4690.48*time)
  )
  l_1 <- (
    628331966747*cos(0) +
    206059*cos(2.678235 + 6283.075850*time) +
    4303*cos(2.6351 + 12566.1517*time) +
    425*cos(1.590 + 3.523*time) +
    119*cos(5.796 + 26.298*tme) +
    109*cos(2.966 + 1577.344*time) +
    93*cos(2.59 + 18849.23*time) +
    72*cos(1.14 + 529.69*time) +
    68.0 * cos(1.87 + 398.15*time) +
    67.0 * cos(4.41 + 5507.55*time) +
    59.0 * cos(2.89 + 5223.69*time) +
    56.0 * cos(2.17 + 155.42*time) +
    45.0 * cos(0.4 + 796.3*time) +
    36.0 * cos(0.47 + 775.52*time) +
    29.0 * cos(2.65 + 7.11*time) +
    21.0 * cos(5.34 + 0.98*time) +
    19.0 * cos(1.85 + 5486.78*time) +
    19.0 * cos(4.97 + 213.3*time) +
    17.0 * cos(2.99 + 6275.96*time) +
    16.0 * cos(0.03 + 2544.31*time) +
    16.0 * cos(1.43 + 2146.17*time) +
    15.0 * cos(1.21 + 10977.08*time) +
    12.0 * cos(2.83 + 1748.02*time) +
    12.0 * cos(3.26 + 5088.63*time) +
    12.0 * cos(5.27 + 1194.45*time) +
    12.0 * cos(2.08 + 4694*time) +
    11.0 * cos(0.77 + 553.57*time) +
    10.0 * cos(1.3 + 6286.6*time) +
    10.0 * cos(4.24 + 1349.87*time) +
    9.0 * cos(2.7 + 242.73*time) +
    9.0 * cos(5.64 + 951.72*time) +
    8.0 * cos(5.3 + 2352.87*time) +
    6.0 * cos(2.65 + 9437.76*time) +
    6.0 * cos(4.67 + 4690.48*time)
  )
  l_2 <- (
    52919.0 * cos(0) +
    8720.0 * cos(1.0721 + 6283.0758*time) +
    309.0 * cos(0.867 + 12566.152*time) +
    27.0 * cos(0.05 + 3.52*time) +
    16.0 * cos(5.19 + 26.3*time) +
    16.0 * cos(3.68 + 155.42*time) +
    10.0 * cos(0.76 + 18849.23*time) +
    9.0 * cos(2.06 + 77713.77*time) +
    7.0 * cos(0.83 + 775.52*time) +
    5.0 * cos(4.66 + 1577.34*time) +
    4.0 * cos(1.03 + 7.11*time) +
    4.0 * cos(3.44 + 5573.14*time) +
    3.0 * cos(5.14 + 796.3*time) +
    3.0 * cos(6.05 + 5507.55*time) +
    3.0 * cos(1.19 + 242.73*time) +
    3.0 * cos(6.12 + 529.69*time) +
    3.0 * cos(0.31 + 398.15*time) +
    3.0 * cos(2.28 + 553.57*time) +
    2.0 * cos(4.38 + 5223.69*time) +
    2.0 * cos(3.75 + 0.98*time)
  )
  l_3 <- (
    289.0 * cos(5.844 + 6283.076*time) +
    35.0 * cos(0) +
    17.0 * cos(5.49 + 12566.15*time) +
    3.0 * cos(5.2 + 155.42*time) +
    1.0 * cos(4.72 + 3.52*time) +
    1.0 * cos(5.3 + 18849.23*time) +
    1.0 * cos(5.97 + 242.73*time)
  )
  l_4 <- (
    114.0 * cos(3.142) +
    8.0 * cos(4.13 + 6283.08*time) +
    1.0 * cos(3.84 + 12566.15*time)
  )
  l_5 <- 1*cos(3.14)
  
  helio_long <- l_0 + l_1 + l_2 + l_3 + l_4 + l_5
  return(helio_long)
}

eliptic.lat <- function(time){
  
  b_0 <- (
    280*cos(3.199 + 84334.662*time) +
      102*cos(5.422 + 5507.553*time) +
      80*cos(3.88 + 5223.69*time) +
      44*cos(3.70 + 2352.87*time) +
      32*cos(4.00 + 1577.34*time)
  )
  b_1 <- (
    9*cos(3.90 + 5507.55*time) +
      6*cos(1.73 + 5223.69*time)
  )
  
  helio_lat <- b_0 + b_1
  return(helio_lat)
}

eliptic.radius <- function(time){
  
  r_0 <- (
    
  )
}
