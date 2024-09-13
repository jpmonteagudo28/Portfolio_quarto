##--------------------------------------##
## General Purpose functions
##--------------------------------------##
options(digits = 6,scipen = 999,
        rlang_trace_top_env = rlang::current_env(),
        rlang__backtrace_on_error = "none")


#> Calculate grouped mean for freq (percent or counts)
#> params: @x: character vector of categories or ranges
#>         @freq: numeric vector containing category counts or percentages
#>         @freq_format: intended freq format. Use percent if you want freq converted to percent
#>         @na.rm: remove `NA` from calculations
grouped.mean <- function(x,freq,
                       freq_format = "count",
                       na.rm = FALSE){
  stopifnot(is.character(x),
            is.numeric(freq),
            length(x) == length(freq),
            is.character(freq_format))
  
  freq_format<- match.arg(freq_format,c("percent","count"))
  
  midpoint <- function(x){
    bounds <- strsplit(x,"-")[[1]]
    
    lower <- as.numeric(bounds[1])
    upper <- as.numeric(bounds[2])
    
    if (is.na(lower) | is.na(upper)) {
      return(NA)
    } else {
    midpoint <- (lower + upper)/2
    return(midpoint)
    }
  }
  
 freq <- abs(freq)
 if(freq_format == "percent"){
    freq <- (freq/sum(freq)* 100)
  }
  
  x_mid <- sapply(x, midpoint)
  if(na.rm){
    index <- !is.na(x_mid)
    x_mid <- x_mid[index]
    freq <- freq[index]
  }
  
  n <- sum(freq)
  mu <- sum(x_mid*freq)/n
  
  return(mu)
}

#> Calculate grouped median for freq (count/percent)
#> params:
#> 

grouped.median <- function(x,
                           freq, 
                           freq_format = "count",
                           na.rm = FALSE){
  stopifnot(is.character(x),
            is.numeric(freq),
            length(x) == length(freq),
            is.character(freq_format),
            is.logical(na.rm))
  
  freq_format<- match.arg(freq_format,c("percent","count"))
  
  #Dealing with NAs
  na_ind <- is.na(x) | is.na(freq)
  if (any(na_ind)) {
    if (na.rm) {
      x <- x[!na_ind]
      freq <- freq[!na_ind]
    } else {
      return(NA)
    }
  }
  if (length(x) == 0 || length(freq) == 0) {
    return(NA)
  }
  
  # Dealing with percentages
  freq <- abs(freq)
  if (freq_format == "percent") {
    freq <- freq * sum(freq) / 100
  }
  
  # Creating data frame for cumul. freq
  df <- data.frame(x,freq,cf = cumsum(freq))
  half <- sum(freq)/2
  med_class <- df$x[df$cf >= half][1]
  med_class_freq <- df$freq[df$x == med_class]
  index <- which(df$cf >= half)[1]
  
  if(index > 1){
    prev_class_cf <- df$cf[index -1]
  } else{
    prev_class_cf <- 0
  }
  
  # Creating ANON functions for lower limit and sieze of median class
  l <- function(x) as.numeric(strsplit(x,"-")[[1]][1])
  h <- function(x) as.numeric(strsplit(x,"-")[[1]][2]) - 
    as.numeric(strsplit(x,"-")[[1]][1])
  
  lower <- l(med_class)
  size <- h(med_class)
  
  # Calculate grouped median
  median <- lower + ((half  - prev_class_cf)/med_class_freq)*size
  return(median)
}

#> Calculate the geometric median for non-collinear points in 2D+ Euclidean Space
#> params: @x: vector of non-collinear points
#>         @iters: number of iterations
#>         @tol: tolerance value used to check convergence of changes between 
#>               successive guesses.
#> 
#> Using the Weiszfeld algorithm: an iterative method to compute the geometric
#> median of a set of points in Euclidean space. In general, measures of central
#> tendency minimize the sum of the Euclidean distance from the center to each
#> point in the set,this is represented as `arg min`. We try to come up with an 
#> estimate of this center by first using our best guess and then approximating 
#> it by calculating  the distance from each point to a newly chosen point in 
#> the set and updating the center if the distance is smaller than that 
#> calculated using our best, initial guess.
#> You can find an explanation of the algorith here:
#> https://www.sciencedirect.com/science/article/pii/S0898122109004234
#> An implementation using Rccp here: 
#> https://www.geeksforgeeks.org/geometric-median/
#> 
#> NOTE: If you're sample is a 1D set of points use `median` or `grouped.median` instead.
#> The `geom.median` function is used for samples of points in 2D+ space.

geom.median <- function(x,iters = 1000, tol = 1e-8,na.rm = FALSE){
  stopifnot(is.numeric(x),
            is.numeric(iters),
            is.numeric(tol),
            is.logical(na.rm)
            )
  
  if(na.rm){
    x <- x[!is.na(x)]
  }
  x <- as.matrix(x)
  center <- colMeans(x)
  
  for(i in 1:iters){
    dist <- sqrt(rowSums((x - center)^2))
    
    dist[dist == 0] <- tol
    
    weights <- 1/dist
    
    new_center <- colSums(weights * x)/sum(weights)
    
    if(sqrt(sum(new_center - center)^2)< tol){
      return(new_center)
    }
    center <- new_center
  }
  return(center)
}

#> Calculate the median of distances to the geometric mean for a 2D+ space
#> We're calculating the median absolute deviation for matrices and arrays
#> @params x: a numeric vector or matrix containing a set of points in n-dimensions
#> @params iters: number of iterations
#> @params na.rm: option to leave or remove NAs
#> @params ... additional arguments to specify the tolerance of geom.median
geom.mad <- function(x, 
                   iters = 1000,
                   na.rm = FALSE,
                   ...){
  
  stopifnot(is.numeric(x),
            is.numeric(iters),
            is.logical(na.rm)
            )
  
  if(na.rm){
    x <- x[!is.na(x)]
  }
  
  k = ((qnorm(c(.25,.75),0,1))^-1)[2]
  x <- as.matrix(x)
  n <- length(x)
  center <- geom.median(x,iters = iters,...)
  
  dist <- sqrt(rowSums((x - center)^2))
  
  mad <- k * median(dist)
  return(mad)
}
