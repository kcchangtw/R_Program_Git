rm(list = ls())

PP <- function(a0, b0, a1, b1, n0, n1, x0, x1, delta, alternative = "green"){
  
  #Prior distribution for control: beta(a0, b0)
  #Prior distribution for experimental: beta(a1, b1)
  
  #Posterior distribution for control: beta(a0 + x0, b0 + n0 - x0)
  #Posterior distribution for experimental: beta(a1 + x1, b1 + n1 - x1)
  #Number of randomly simulated data: K
  #Sample size in experimental: n1, sample size in control n0
  
  K <- 100000
  
  emp <- rbeta(K, (a1 + x1), (b1 + n1 - x1)) - rbeta(K, (a0 + x0), (b0 + n0 - x0))
  if(alternative =="green"){
    return((sum(emp >= delta) / K))
  }else if(alternative == "red"){
    return((sum(emp < delta) / K))
  }
}

#Setting Parameters
a0 <- 0.5
b0 <- 0.5
a1 <- 0.5
b1 <- 0.5
n0 <- 30
n1 <- 30

delta_green <- 0.1
threshold_green <- 0.7
#method_green <- "green"
delta_red <- 0.05
threshold_red <- 0.8
#method_red <- "red"

matrix_Go_NoGo <- NULL

for(j in 0:n0){
  #j <- 0  
  x0 <- j #setting ORR = 20%, n0 * ORR = 6
  ORR <- paste0(round(x0 / n0 * 100), "%") 
  x1 <- 0:n1
  
  judge_green <- FALSE
  judge_red <- FALSE
  threshold_Go <- NA
  threshold_NoGo <- NA
  
  
  for(i in x1){
    
    if(judge_green == FALSE){
      
      two_sample_posterior_greenmethod = PP(a0 = a0, b0 = b0, a1 = a1, b1 = b1, 
                                            n0 = n0, n1 = n1, x0 = x0, x1 = i, delta = delta_green,
                                            alternative = "green")
      str_temp <- paste0("x1 = ", i, " Two Sample Posterior (green) = ", two_sample_posterior_greenmethod)
      print(str_temp)
    }
    if(judge_red == FALSE){
      
      two_sample_posterior_redmethod = PP(a0 = a0, b0 = b0, a1 = a1, b1 = b1, 
                                          n0 = n0, n1 = n1, x0 = x0, x1 = i, delta = delta_red,
                                          alternative = "red")
      str_temp <- paste0("x1 = ", i, " Two Sample Posterior (red) = ", two_sample_posterior_redmethod)
      print(str_temp)
    }
    if(two_sample_posterior_greenmethod >= threshold_green & judge_green == FALSE){
      threshold_Go <- i
      judge_green <- TRUE
    }
    if(two_sample_posterior_redmethod < threshold_red & judge_red == FALSE){
      threshold_NoGo <- i - 1
      if(threshold_NoGo < 0){
        threshold_NoGo <- NA
      }
      judge_red <- TRUE
    }
    if(i == max(x1) & judge_red == FALSE){
      threshold_NoGo <- max(x1)
    }
    
  }
  
  if(is.na(threshold_NoGo) == FALSE & is.na(threshold_Go) == FALSE){
    
    No_Go_n <- sum(x1 <=  threshold_NoGo)
    Go_n <- sum(x1 >=  threshold_Go)
    Consider_n <- length(x1) - No_Go_n - Go_n
    if(Consider_n >= 0){
      ans <- c(rep("No_Go", No_Go_n),rep("Consider", Consider_n), rep("Go", Go_n))
    }else{
      ans <- c(rep("No_Go", No_Go_n), rep("Go", Go_n))
    }
    ans <- c(rep("No_Go", No_Go_n),rep("Consider", Consider_n), rep("Go", Go_n))
    
  }else if(is.na(threshold_NoGo) == TRUE & is.na(threshold_Go) == FALSE){
    
    Go_n <- sum(x1 >=  threshold_Go)
    Consider_n <- length(x1) - Go_n
    if(Consider_n > 0){
      ans <- c(rep("Consider", Consider_n), rep("Go", Go_n))
    }else{
      ans <- rep("Go", Go_n)
    }
    
  }else if(is.na(threshold_NoGo) == FALSE & is.na(threshold_Go) == TRUE){
    
    No_Go_n <- sum(x1 <=  threshold_NoGo)
    Consider_n <- length(x1) - No_Go_n
    if(Consider_n > 0){
      c(rep("No_Go", No_Go_n),rep("Consider", Consider_n))
    }else{
      ans <- rep("No_Go", No_Go_n)
    }
    
    
  }else{
    Consider_n <- length(x1)
    ans <- rep("Consider", Consider_n)
  }
  
  print("-----")
  print(paste0("ORR = ", ORR, ", x0 = ", x0))
  print(ans)
  print("-----")
  matrix_Go_NoGo <- cbind(matrix_Go_NoGo, ans)
  
}

#matrix to array
mut_preprocess <- as.data.frame(matrix_Go_NoGo)
row_no <- nrow(mut_preprocess)
col_no <- ncol(mut_preprocess)
colnames(mut_preprocess) <- factor(paste0("rc", 0:(col_no - 1)))
mut_preprocess_tidy <- mut_preprocess %>%  gather(key = "rc", value = "value")
mut_preprocess_tidy$rt <- rep(factor(paste0("rt", 0:(row_no - 1))), col_no)

array_Go_NoGo <- data.frame(rc = mut_preprocess_tidy$rc, rt = mut_preprocess_tidy$rt, decision = mut_preprocess_tidy$value)

save(array_Go_NoGo, file = "array_Go_NoGo.RData")

