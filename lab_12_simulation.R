generate_data = function(n,p){
  mat = matrix(rnorm(n*p, mean=0, sd=1), nrow = n, ncol = p)
  vec = vector(length = n)
  for (i in 1:n){
    vec[i]= rnorm(1, mean=0, sd=1)
  }
  return(list(covariates = mat, responses = vec ))
}
generate_data(3,2)


model_select = function(covariates, responses, cutoff){
  linreg = lm(responses~covariates)
  sum.linreg.p = summary(linreg)$coefficients[,"Pr(>|t|)"]
  new.covariates = c()
  for (i in 2:(ncol(covariates)+1)){
    if (sum.linreg.p[i]<= cutoff){
      new.covariates = c(new.covariates, (i-1))
    }
  }
  new.linreg = lm(responses~ covariates[, new.covariates])
  sum.new.linreg.p = summary(new.linreg)$coefficients[,"Pr(>|t|)"]
  return(sum.new.linreg.p)
}