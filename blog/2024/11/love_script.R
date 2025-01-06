you_are_great <- function(x) {
  if(x %in% c("Jimena","Valentina","Milena"))
    TRUE
  else FALSE
  }

do_i_love_you <- function(x) {
    true_love <- you_are_great(x)
     if(true_love){
        cat("I love you to the moon and back")
      } else {
        cat("Maybe a little bit, but not that much")
      }
}
