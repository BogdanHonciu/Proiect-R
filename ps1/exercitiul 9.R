# Functie pentru integrarea densitatii de probabilitate
integrate_pdf <- function (f, X) {
  tryCatch (
    expr = {
      integrate (f, 0, X)$value
    },
    error = function (e) {
      print(e)
    }
  )
}

# Calucleaza functia inversa
inverse_function <- function (f, u, lowerBound = -10000, upperBound = 10000) {
  tryCatch (
    expr = {
      uniroot((function (x) f(x) - u), lower = lowerBound, upper = upperBound, extendInt = 'yes')
    },
    error = function (e) {
      print(e)
    }
  )
}



random_variable_generator <- function(f, n, lowerBound, upperBound) {
  # Genereaza variabile aleatorii
  values <- runif(n, min = 0, max = 1)
  # Combina argumentele intr-o lista
  r <- c()
  for (v in values) {
    r <- append (r, inverse_function(function (x) (integrate_pdf(f, x)), v, lowerBound, upperBound)[1]$root)
  }
  r
}

