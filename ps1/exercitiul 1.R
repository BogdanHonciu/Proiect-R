# Calculam constanta de normalizare a unei functii de densitate de probabilitate

calculare_constanta_normalizare <- function(f, lowerBound=-Inf, upperBound=Inf){
  tryCatch(
    expr = {
      # Calculam integrala
      integral <- integrate(f, lower = lowerBound, upper = upperBound)$value
      # Constanta este 1
      constanta_de_normalizare <- 1 / integral
      return(constanta_de_normalizare)
    },
    error = function(e){
      message('Eroare')
      print(e)
      return(0)
    },
    warning = function(w){
      message('Avertisment')
      print(w)
    },
    finally = {
      message('')
    }
  )
}
