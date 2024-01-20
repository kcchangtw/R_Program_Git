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

# Setting initial parameters
a0 = 0.5
b0 = 0.5
a1 = 0.5
b1 = 0.5
n0 = 10
n1 = 20
x0 = 0.2 * n0
delta_go = 0.1
threshold_go = 0.7
delta_nogo = 0
threshold_nogo = 0.9