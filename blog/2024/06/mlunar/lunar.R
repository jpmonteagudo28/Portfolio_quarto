##------------------------------------------##
## Lunar Phases
##------------------------------------------##

#> Julian Ephemeris Date for Moon Phases
JDE <- function(k){
  t <- k/1236.85
  jde <- 2451550.09766 + (29.530588861*k) + 
    0.00015437*(t^2) - 0.000000150*(t^3) + 
    0.00000000073*(t^4)
  jde <- round(jde,digits = 6)
  return(jde)
}
#> Calculate E
E <- function(k,t){
  e <- 1 - (0.002516*t) - 0.0000074*(t^2)
  e <- round(e,digits = 6)
  return(e)
}

#> Sun's mean anomaly at time JDE
M <- function(k,t){
  m <- 2.5534 + (29.10535670*k) - 0.0000014*(t^2) - 0.00000011*(t^3)
  m <- modr(round(m, digits = 6))
  m <- deg2rad(m)
  return(m)
}
#> Moon’s mean anomaly at time JDE
M.prime <- function(k,t){
  m_prime <- 201.5643 + (385.81693528*k) + 0.0107582*(t^2) + 
    0.00001238*(t^3) - 0.000000058*(t^4)
  m_prime <- modr(round(m_prime,digits = 6))
  m_prime <- deg2rad(m_prime)
  return(m_prime)
}
#> Moon's argument of latitude
F <- function(k,t){
  f <- 160.7108 + (390.67050284*k) - 0.0016118*(t^2) - 
    0.00000227*(t^3) + 0.000000011*(t^4)
  f <- modr(round(f, digits = 6))
  f <- deg2rad(f)
  return(f)
}

#> Longitude of the ascending node of the lunar orbit
om <- function(k,t){
  o <- 124.7746 - (1.56375588*k) + 0.0020672*(t^2) + 0.00000215*(t^3)
  o <- modr(round(0, digits = 6))
  o <- deg2rad(o)
  return(o)
}

#> Planetary arguments for Lunar Phase Calculation
A1 <- function(k,t){
  a1 <- 299.77 + 0.107408*k - 0.009173*(t^2)
  a1 <- modr(round(a1, digits = 6))
  a1 <- deg2rad(a1)
  return(a1)
}
A2 <- function(k){
  a2 <- 251.88 + 0.016321*k
  a2 <- modr(round(a2,digits = 6))
  a2 <- deg2rad(a2)
  return(a2)
}
A3 <- function(k){
  a3 <- 251.83 + 26.651886*k
  a3 <- modr(round(a3,digits = 6))
  a3 <- deg2rad(a3)
  return(a3)
}

A4 <- function(k){
  a4 <- 349.42 + 36.412478*k
  a4 <- modr(round(a4,digits = 6))
  a4 <- deg2rad(a4)
  return(a4)
}
A5 <- function(k){
  a5 <- 84.66 + 18.206239*k
  a5 <- modr(round(a5, digits = 6))
  a5 <- deg2rad(a5)
  return(a5)
}
A6 <- function(k){
  a6 <- 141.74 + 53.303771*k
  a6 <- modr(round(a6,digits = 6))
  a6 <- deg2rad(a6)
  return(a6)
}
A7 <- function(k){
  a7 <- 207.14 + 2.453732*k
  a7 <- modr(round(a7, digits = 6))
  a7 <- deg2rad(a7)
  return(a7)
}
A8 <- function(k){
  a8 <- 154.84 + 7.306860*k
  a8 <- modr(round(a8,digits = 6))
  a8 <- deg2rad(a8)
  return(a8)
}
A9 <- function(k){
  a9 <- 34.52 + 27.261239*k
  a9 <- modr(round(a9,digits = 6))
  a9 <- deg2rad(a9)
  return(a9)
}
A10 <- function(k){
  a10 <- 207.19 + 0.121824*k
  a10 <- modr(round(a10, digits = 6))
  a10 <- deg2rad(a10)
  return(a10)
}
A11 <- function(k){
  a11 <- 291.34 + 1.844379*k
  a11 <- modr(round(a11,digits = 6))
  a11 <- deg2rad(a11)
  return(a11)
}
A12 <- function(k){
  a12 <- 161.72 + 24.198154*k
  a12 <- modr(round(a12,digits = 6))
  a12 <- deg2rad(a12)
  return(a12)
}
A13 <- function(k){
  a13 <- 239.56 + 25.513099*k
  a13 <- modr(round(a13,digits = 6))
  a13 <- deg2rad(a13)
  return(a13)
}
A14 <- function(k){
  a14 <- 331.55 + 3.592518*k
  a14 <- modr(round(a14, digits = 6))
  a14 <- deg2rad(a14)
  return(a14)
}

dex.date <- function(date){
  if (!inherits(date, c("POSIXt", "POSIXct", "POSIXlt", "Date"))) {
    #> Transform to POSIXlt
    POSIX_date <- as.POSIXlt(date)
    #> if the format is not found by the function, you can provide                                    
    #> your format explicitly.
  }
  year <- POSIX_date$year + 1900 
  #> Pull the year attribute and add 1900, default is to subtract                                   
  #> 1900 to provided year
  day <- as.numeric(format(POSIX_date,"%j")) # day of the year 0 - 365
  dy_yrs <- ifelse((year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0), 366,365.25)
  #> Get the time components
  hour <- POSIX_date$hour
  minute <- POSIX_date$min
  second <- POSIX_date$sec
  #> Calculate the fraction of the day
  fraction_day <- (hour * 3600 + minute * 60 + second) / (86400)
  dex_date <- year + (day -1 + fraction_day)/dy_yrs
  return(dex_date)
}

k <- function(date){
  date <- dex.date(date)
  12.3685 * (date - 2000)
}

calc.moon.phase <- function(k){
  stopifnot(is.numeric(k))
  
  t <- k / 1236.85
  
  # Mean phase JDE
  jde <- JDE(k)
  
  # Anomaly, latitude, and longitude of Sun/Moon
  e <- E(k, t)
  m <- M(k, t)
  mpr <- M.prime(k, t)
  f <- F(k, t)
  om <- om(k, t)
  
  kfrac <- modf(k)[2]
  kint <- modf(k)[1]
  
  planet_args <- (
    0.000325 * sin(A1(k, t)) + 
      0.000165 * sin(A2(k)) + 
      0.000164 * sin(A3(k)) +
      0.000126 * sin(A4(k)) + 
      0.000110 * sin(A5(k)) + 
      0.000062 * sin(A6(k)) +
      0.000060 * sin(A7(k)) + 
      0.000056 * sin(A8(k)) + 
      0.000047 * sin(A9(k)) +
      0.000042 * sin(A10(k)) + 
      0.000040 * sin(A11(k)) + 
      0.000037 * sin(A12(k)) +
      0.000035 * sin(A13(k)) + 
      0.000023 * sin(A14(k))
  )
  
  if (kfrac == 0) {
    adj <- (
      -0.40720 * sin(mpr) + 
        0.17241 * e * sin(m) + 
        0.01608 * sin(2 * mpr) + 
        0.01039 * sin(2 * f) + 
        0.00739 * e * sin(mpr - m) - 
        0.00514 * e * sin(mpr + m) +
        0.00208 * (e^2) * sin(2 * m) - 
        0.00111 * sin(mpr - 2 * f) - 
        0.00057 * sin(mpr + 2 * f) +
        0.00056 * e * sin(2 * mpr + m) - 
        0.00042 * sin(3 * mpr) + 
        0.00042 * e * sin(m + 2 * f) +
        0.00038 * e * sin(m - 2 * f) - 
        0.00024 * e * sin(2 * mpr - m) - 
        0.00017 * sin(om) -
        0.00007 * sin(mpr + 2 * m) + 
        0.00004 * sin(2 * mpr - 2 * f) + 
        0.00004 * sin(3 * m) +
        0.00003 * sin(mpr + m - 2 * f) + 
        0.00003 * sin(2 * mpr + 2 * f) - 
        0.00003 * sin(mpr + m + 2 * f) +
        0.00003 * sin(mpr - m + 2 * f) - 
        0.00002 * sin(mpr - m - 2 * f) - 
        0.00002 * sin(3 * mpr + m) +
        0.00002 * sin(4 * mpr)
    )
    
    true_jde <- (jde + adj) + planet_args
    true_date <- to.gregorian(true_jde)
    return(true_date)
    
  } else if (kfrac == .25 || kfrac == .75) {
    adj2 <- (
      -0.62801 * sin(mpr) + 
        0.17172 * e * sin(m) - 
        0.01183 * e * sin(mpr + m) + 
        0.00862 * sin(2 * mpr) + 
        0.00804 * sin(2 * f) + 
        0.00454 * e * sin(mpr - m) + 
        0.00204 * (e^2) * sin(2 * m) - 
        0.00180 * sin(mpr - 2 * f) - 
        0.00070 * sin(mpr + 2 * f) -
        0.00040 * sin(3 * mpr) - 
        0.00034 * e * sin(2 * mpr - m) + 
        0.00032 * e * sin(m + 2 * f) + 
        0.00032 * e * sin(m - 2 * f) - 
        0.00028 * (e^2) * sin(mpr + 2 * m) + 
        0.00027 * e * sin(2 * mpr + m) - 
        0.00017 * sin(om) - 
        0.00005 * sin(mpr - m - 2 * f) + 
        0.00004 * sin(2 * mpr + 2 * f) - 
        0.00004 * sin(mpr + m + 2 * f) + 
        0.00004 * sin(mpr - 2 * m) + 
        0.00003 * sin(mpr + m - 2 * f) + 
        0.00003 * sin(3 * m) + 
        0.00002 * sin(2 * mpr - 2 * f) + 
        0.00002 * sin(mpr - m + 2 * f) - 
        0.00002 * sin(3 * mpr + m)
    )
    
    w <- (
      0.00306 - 0.00038 * e * cos(m) + 
        0.00026 * cos(mpr) - 
        0.00002 * cos(mpr - m) +
        0.00002 * cos(mpr + m) +
        0.00002 * cos(2 * f)
    )
    
    if (kfrac == .25) {
      if(kint > 0){
        true_jde <- (jde + adj2) + w + planet_args
      } else {  # First Quarter before 2000 AD
        true_jde <- (jde + adj2) - w + planet_args
      }
    } else {  # kfrac == 0.75
      if (kint > 0) {  # Last Quarter after 2000 AD
        true_jde <- (jde + adj2) - w + planet_args
      } else {  # Last Quarter before 2000 AD
        true_jde <- (jde + adj2) + w + planet_args
      }
    }
    true_date <- to.gregorian(true_jde)
    return(true_date)
    
  } else if (kfrac == .5) {
    adj3 <- (
      -0.40614 * sin(mpr) + 
        0.17302 * e * sin(m) + 
        0.01614 * sin(2 * mpr) + 
        0.01043 * sin(2 * f) + 
        0.00734 * e * sin(mpr - m) - 
        0.00514 * e * sin(mpr + m) + 
        0.00209 * (e^2) * sin(2 * m) - 
        0.00111 * sin(mpr - 2 * f) - 
        0.00057 * sin(mpr + 2 * f) + 
        0.00056 * e * sin(2 * mpr + m) - 
        0.00042 * sin(3 * mpr) + 
        0.00042 * e * sin(m + 2 * f) + 
        0.00038 * e * sin(m - 2 * f) - 
        0.00024 * e * sin(2 * mpr - m) - 
        0.00017 * sin(om) - 
        0.00007 * sin(mpr + 2 * m) + 
        0.00004 * sin(2 * mpr - 2 * f) + 
        0.00004 * sin(3 * m) + 
        0.00003 * sin(mpr + m - 2 * f) + 
        0.00003 * sin(2 * mpr + 2 * f) - 
        0.00003 * sin(mpr + m + 2 * f) + 
        0.00003 * sin(mpr - m + 2 * f) -
        0.00002 * sin(mpr - m - 2 * f) - 
        0.00002 * sin(3 * mpr + m) + 
        0.00002 * sin(4 * mpr)
    )
    
    true_jde <- (jde + adj3) + planet_args
    true_date <- to.gregorian(true_jde)
    return(true_date)
    
  } else if (kfrac > 0 && kfrac < .25) {
    return(cat("Moon is in Waxing Crescent Phase. No valid date returned"))
  } else if (kfrac > .25 && kfrac < .5) {
    return(cat("Moon is in Waxing Gibbous Phase. No valid date returned"))
  } else if (kfrac > .5 && kfrac < .75) {
    return(cat("Moon is in Waning Gibbous Phase. No valid date returned"))
  } else {
    return(cat("Moon is in Waning Crescent Phase. No valid date returned"))
  }
}

