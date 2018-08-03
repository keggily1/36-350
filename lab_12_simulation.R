generate_data = function(n,p){
  mat = matrix(rnorm(n*p, mean=0, sd=1), nrow = n, ncol = p)
  vec = vector(length = n)
  for (i in 1:n){
    vec[i]= rnorm(1, mean=0, sd=1)
  }
  return(list(covariates = mat, responses = vec ))
}
generate_data(3,2)