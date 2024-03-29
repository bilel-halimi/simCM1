#'sumulation d'une loi discrete
#'@export
#'@param x numeric vector
#'@param p numeric vector 
#'@param n entier representing numbre of iteration

rdist<-function(x,p)
{
  n=length(p)
  r=runif(1)
  b=p[1];
  if((r>=0)&(r<=b))
  {
    y=x[1]
    return(y)
  }
  else
  {a=p[1]
  b=b+p[2]
  for(i in 2:n-1)
  {
    if((r>=a)&(r<=b))
    {
      y=x[i]
      return(y)
    }
    else
    {
      a=b
      b=b+p[i+1]
      i=i+1
    }
  }
  y=x[n]
  return(y)
  }
}
Affn<- function (x,p,n){
  m =rdistn(x,p,n)
  plot(m,col="green", main = "le nuage de point ")

}
AffBox<-function(x,p,n){
  m=rdistn(x,n,p)
  boxplot(m)
}
Moy<- function (x,p,n){
  m= mean(rdistn(x,p,n))
  return(m)
}
Vari <- function(x,p,n){
  m= var(rdistn(x,p,n))
  return(m)
}
