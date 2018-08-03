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
  if (length(new.covariates)==0){
    return (c())
  }
  new.linreg = lm(responses~ covariates[, new.covariates])
  sum.new.linreg.p = summary(new.linreg)$coefficients[,"Pr(>|t|)"]
  return(sum.new.linreg.p[-1])
}
# 
# Write a function `run_simulation(n_trials, n, p, cutoff)`
# which uses the previous two functions to run `n_trials`
# simulations which uses data from `generate_data` in
# `model_select`, collects the returned p-values and displays a
# histogram of the p-values. Under the null hypothesis (that the
#                                                       regression coefficients are zero) these p-values should be
# uniformly distributed between 0 and 1; does this seem to be the
# case? Create and save figures for all combinations of `n = c(100,
#                                                              1000, 10000)`, `p = c(10, 20, 50)` and set `n_trials = 1000` and
# `cutoff = 0.05`. Don't include the figures in the commit, only the
# code. *HINT*: Write a `for` loop; alternatively the functions
# `expand.grid` and `m_ply` may prove useful.

run_simulation = function(n_trials, n, p, cutoff){
  for (n in n){
    for(p in p){
      p.values = c()
      for (trial in 1:n_trials){
        data = generate_data(n,p)
        model.p = model_select(data[[1]], data[[2]], cutoff)
        for (p in model.p){
          if (is.numeric(p)){
            p.values = c(p.values, p)
          }
        }
        hist(as.numeric(p.values), freq = TRUE)
      }
    }
  }
}
run_simulation(1000, n= c(100,1000,10000), p= c(10, 20, 50), cutoff = 0.05)