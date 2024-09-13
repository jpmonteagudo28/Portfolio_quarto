##-------------------------------------##
## General purpose functions
##-------------------------------------##

options(digits = 9)

#> Modulus fraction function
#> @params: k - object of type double
modf <- function(k){
  stopifnot(is.numeric(k))
  kint <- trunc(k)
  kfrac <- abs(k - kint)
  kround <- c(kint,kfrac)
  return(kround)
}

#> Modulus reduce angle function
#>  @params: angle: object of type double 
#>  representing any angle in degrees
modr <- function(angle){
  stopifnot(is.numeric(angle))
  reduced <- angle%%360
  return(reduced)
}

#> Conversion from rad to deg
#> @params: rad - angle in radians
rad2deg <- function(rad){
  stopifnot(is.numeric(rad))
  degrees <- (rad*180)/(pi)
  return(degrees)
}
#> Conversion from rad to deg
#> @params: deg - angle in degrees
deg2rad <- function(deg){
  stopifnot(is.numeric(deg))
  radians <- (deg*pi)/180
  return(radians)
}

#> Convert mins and secs to degrees
min2deg <- function(deg,min,sec){
  #> 1 degree equiv. to 60 min
  degrees <- sum(deg + min/60 + sec/3600)
  return(degrees)
}

#> Convert degrees to mins and secs
deg2min <- function(deg){
  d <- modf(deg)[1]
  m <- modf(deg)[2] * 60
  s <- modf(m)[2] * 60
  
  format_deg <- c(round(d,digits = 0), 
                  round(m, digits = 0),
                  round(s,digits = 3))
  
  names(format_deg) <- c("deg", 
                         "m", 
                         "s")
  return(format_deg)
}

#> Convert to gregorian date
#> @params: date in julian ephemeris format
to.gregorian <- function(jde){
  jde <- jde + 0.5
  Z <- trunc(jde)
  F <- jde - Z
  if (Z < 2299161) {
    A <- Z
  } else {
    alpha <- trunc((Z - 1867216.25) / 36524.25)
    A <- Z + 1 + alpha - trunc(alpha / 4)
  }
  B <- A + 1524
  C <- trunc((B - 122.1) / 365.25)
  D <- trunc(365.25 * C)
  E <- trunc((B - D) / 30.6001)
  
  day <- B - D - trunc(30.6001 * E) + F
  month <- ifelse(E < 14, E - 1, E - 13)
  year <- ifelse(month > 2, C - 4716, C - 4715)
  
  # Extract the fractional day part for time
  hour <- (day - trunc(day)) * 24
  minute <- (hour - trunc(hour)) * 60
  second <- (minute - trunc(minute)) * 60
  
  # Create a POSIXlt object for the Gregorian date and time
  gregorian_date <- as.POSIXlt(sprintf("%04d-%02d-%02d %02d:%02d:%06.3f",
                                       year, month, trunc(day),
                                       trunc(hour), trunc(minute), second),
                               format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
  return(gregorian_date)
}

to.julian <- function(date){
  if (!inherits(date, c("POSIXt", "POSIXct", "POSIXlt", "Date"))) {
    #> Transform to POSIXlt
    POSIX_date <- as.POSIXlt(date)
  }
  Y <- POSIX_date$year + 1900
  M <- POSIX_date$mon + 1
  D <- POSIX_date$mday
  H <- POSIX_date$hour
  mn <- POSIX_date$min
  S <- POSIX_date$sec
  Y_up <- ifelse(M > 2, Y,Y - 1)
  M_up <- ifelse(M > 2,M, M + 12)
  A <- floor(Y_up/100)
  if(Y_up < 1582){ # If looking at Julian calendar
    B = 0
  } else{
    B = 2 - A + floor(A/4)
  }
  D_dec <- (H + mn/60 + S/3600)/24
  jde <- floor(365.25*(Y_up + 4716)) + floor(30.6001*(M_up +1)) + D + D_dec + B - 1524.5
  return(jde)
}

#> Convert gregorian date to julian ephemeris date
#> @params: date character vector containing date and time
time <- function(date){
  (to.julian(date) - 2451545.0)/36525
}