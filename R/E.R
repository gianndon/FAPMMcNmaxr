#' Expected Values
#'
#' Returns the expected value depending on the entry for type
#' @param c number of servers
#' @param lambda arrival rate
#' @param mu service rate
#' @param p0 probability of having 0 subjects/objects in the system (has to be calculated before)
#' @param nmax max number of subjects/objects allowed in the system
#' @param type character, type in E_Nq, E_N, E_Wq, E_W for the desired expected value
#'
#' @return Expected Values either of E_Nq, E_N, E_Wq, E_W
#' @export
#'
E <- function(c, lambda, mu, p0, nmax, type=""){

  # Funktion für E_Nq unterschieden zwischen u=1 und u!=1
  E_Nq <- function(c, u, p0, nmax){
    if (u != 1){
      term1 <- p0*(u^(c+1)*c^c)/(factorial(c)*(1-u)^2)
      term2 <- 1-u^(nmax-c+1)-(1-u)*(nmax-c+1)*u^(nmax-c)
      l <- term1*term2
      return(l)
    }
    else if (u == 1){
      l <- p0*((c*u)^c/factorial(c))*((nmax-c)*(nmax-c+1)/2)
      return(l)
    }
  }


  # Berechnung Utilization
  u <- lambda/(c*mu)

  # Berechnung E_Nq
  enq <- E_Nq(c=c, u=u, p0=p0, nmax=nmax)

  # Berechnung pnmax
  pnmax <- ((c*u)^nmax/(factorial(c)*c^(nmax-c)))*p0


  # Rückgabe je nach ausgewähltem Erwartungswert
  if (type == "E_Nq"){
    return(enq)
  }
  else if (type == "E_N"){
    return(enq+(1-pnmax)*lambda/mu)
  }
  else if (type == "E_Wq"){
    return(enq/((1-pnmax)*lambda))
  }
  else if (type == "E_W"){
    ewq <- enq/((1-pnmax)*lambda)
    return(ewq+1/mu)
  }
  else if (type == ""){
    print("Please enter E_Nq, E_N, E_Wq or E_W for type!", quote=FALSE)
  }
  else{
    print("Wrong entry! Please enter E_Nq, E_N, E_Wq or E_W for type!", quote=FALSE)
  }
}
