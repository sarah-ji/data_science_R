## parsing command arguments
for (arg in commandArgs(TRUE)) {
  eval(parse(text=arg))
}

## check if a given integer is prime
isPrime = function(n) {
  if (n <= 3) {
    return (TRUE)
  }
  if (any((n %% 2:floor(sqrt(n))) == 0)) {
    return (FALSE)
  }
  return (TRUE)
}

## estimate mean only using observation with prime indices
estMeanPrimes = function (x) {
  n = length(x)
  ind = sapply(1:n, isPrime)
  return (mean(x[ind]))
}

#####
#makenull vectors
#seed (random seed), n (sample size), dist (distribution) and rep
set.seed(seed)

regularmeansq = NULL
primemeansq = NULL


#loop for given dist
for( i in 1:rep){
  if(dist == "gaussian"){
    sample_normdistributions = rnorm(n)
    normalmean = mean(sample_normdistributions)
    normalprimemean = estMeanPrimes(sample_normdistributions)
    regularmeansq[i] = normalmean^2
    primemeansq[i] = normalprimemean^2
  }
  
  else if(dist == "t1"){
    sample_t1distributions = rt(n = n, df =  1)
    t1mean = mean(sample_t1distributions)
    t1primemean = estMeanPrimes(sample_t1distributions)
    regularmeansq[i] = t1mean^2
    primemeansq[i] = t1primemean^2
  }
  
  else if(dist == "t5"){
    sample_t5distributions = rt(n = n, df =  5)
    t5mean = mean(sample_t5distributions)
    t5primemean = estMeanPrimes(sample_t5distributions)
    regularmeansq[i] = t5mean^2
    primemeansq[i] = t5primemean^2
  }
}

# classical sample average
regularMSE = sum(regularmeansq)/rep

# prime-indexed sample average
primeMSE = sum(primemeansq)/rep

MSE = matrix(c(primeMSE, regularMSE), ncol = 1, dimnames = list(c("primeMSE","sampleMSE"), NULL))

MSE