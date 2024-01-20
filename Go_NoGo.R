#Two sample posterior probability on difference
PP<-function(a0, b0, a1, b1, n0, n1, x0, x1, delta, alternative = "go"){
  
  # Prior distribution for control: beta(a0,b0)
  # Prior distribution for experimental: beta(a1,b1)
  # Posterior distribution for control: beta(a0+x0, b0+n0-x0)
  # Posterior distribution for experimental: beta(a1+x1, b1+n1-x1)
  # Number of randomly simulated data: K
  # Sample size in exp: n1, sample size in control: n0
  
  K<-100000
  emp<- rbeta(K, a1 + x1, b1 + n1 - x1) - rbeta(K, a0 + x0, b0 + n0 - x0)
  if(alternative == "go"){
    return(mean(emp >= delta))
  }else if(alternative == "no_go"){
    return(mean(emp < delta))
  }
  
}