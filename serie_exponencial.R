minha_exp <- function(x, n){
  
  exp_x <- 0
  
  for(i in 0:n)
    exp_x <- exp_x + (x^i)/factorial(i)
  
  exp_x
}