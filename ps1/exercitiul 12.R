library(pracma)

# suma a doua variabile aleatoare continue independente folosind formula de convolutie
convolution_sum <- function(fx,fy) {
  function(z) {
    integrate(function(y) {
      fx(z-y) * fy(y)
    },-Inf,Inf)
  } $ value
}

# diferenta a doua variabile aleatoare continue independente folosind formula de convolutie
convolution_diff <- function(fx,fy) {
  function(z) {
    integrate(function(y) {
      fx(y-z)*fy(y)
    },-Inf,Inf)
  } $ value
}



