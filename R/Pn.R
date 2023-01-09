#' Pn Function
#'
#' Probability Pn of having n customers in the System
#' @param c number of servers
#' @param lambda arrival rate
#' @param mu service rate
#' @param nmax max number of subjects/objects allowed in the system
#' @param n number of subjects/objects in the system
#'
#' @return Probability
#' @export
#'
Pn <- function(c, lambda, mu, nmax, n){

  # Calucaltion of p0
  u <- lambda/(c*mu)
  summe1 <- sum((c*u)^(0:c)/factorial(0:c))
  summe2 <- sum(u^(((c+1):nmax)-c))
  faktor <- (c*u)^c/factorial(c)
  l <- 1/(summe1+faktor*summe2)

  if (n == 0){
    return(l)
  }
  else if (n >= 1 & n<= c){
    return(((c*u)^n/factorial(n))*l)
  }
  else if (n >= c+1 & n <= nmax){
    ((c*u)^n/(factorial(c)*c^(n-c)))*l
  }
  else if (n >= nmax+1){
    return(0)
  }
  else{
    print("n can't be a negative Number!", quote=FALSE)
  }
}
