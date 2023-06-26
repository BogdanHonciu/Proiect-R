# Calculam momentul central de ordin 0
central_moment <- function(f, o){
  m = medium(f)
  f_new <- function(x){
    ((x - m) ^ o) * f(x)
  }
  res = integrare(f)
  if(typeof(res)=="logical" && res==FALSE){
    print("Momentul nu există!")
    return(0)
  }
  res $ value
}

# CCalculam momentul initial de ordin 0
initial_moment <- function(f, o){
  f_new <- function(x){
    x ^ o * f(x)
  }
  res = integrare(f)
  if(typeof(res)=="logical" && res==FALSE){
    print("Momentul nu există!")
    return(0)
  }
  res $ value
}

# Example:
# f = function (x) {
#   if(x<0 || x>1)
#     return(0)
#   x ^ 3
# }
#
# initial_moment(f, 2)
# central_moment(f, 2)
