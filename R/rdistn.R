#'sumulation d'une loi discrete
#'@export
#'@param x numeric vector
#'@param p numeric vector
#'@param n entier representing numbre of iteration
rdistn <- function(x,p,n) 
 { i=1
  y=c(i : n-1,1)
  for (i in 1:n){
    y[i]=rdist(x,p)
    i=i+1
  }
  return(y)
  }
