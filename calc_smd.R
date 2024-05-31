smd <- function(m1, sd1, n1, m2, sd2, n2, paired = F, r = 0.7) {
  
  if(paired == F) {
    
    df <- n1+n2-2
    
    sp <- sqrt( ((n1-1)*sd1^2 + (n2-1)*sd2^2) / df )
    
    MD <- (m2-m1)
    
    rSE <- sp*sqrt((1/n1)+(1/n2))
    
    SMD <- MD/sp
    
    J <- gamma(df/2) / ( sqrt(df/2)*gamma((df-1)/2) )
    
    SE <- sqrt( ((n1+n2)/(n1*n2)) + (SMD^2/(2*(n1+n2))) * J^2 )
    
  }
  
  else {
    
    df <- n1-1
    
    sdiff <- sqrt( sd1^2 + sd2^2 - 2*r*sd1*sd2 )
    
    MD <- (m2-m1)
    
    rSE <- sqrt( (sd1^2 + sd2^2 - 2*r*sd1*sd2)/n1 )
    
    SMD <- (MD/sdiff)*sqrt(2*(1-r))
    
    J <- gamma(df/2) / ( sqrt(df/2)*gamma((df-1)/2) )
    
    SE <- sqrt( (df/(df-2)) * (2*(1-r)/n1) * (1+SMD^2*(n1/(2*(1-r)))) - SMD^2/J^2 )
    
  }
  
  rCI_low <- MD - qt(.95,df)*rSE
  rCI_upp <- MD + qt(.95,df)*rSE
  
  CI_low <- SMD - qt(.95,df)*SE
  CI_upp <- SMD + qt(.95,df)*SE
  
  data.frame( MD = round(MD,3), 
              rCI_low = round(rCI_low,3), rCI_upp = round(rCI_upp,3) ,
              SMD = round(SMD,3), SE = round(SE, 3),
              CI_low = round(CI_low,3), CI_upp = round(CI_upp,3) ,
              sig = ifelse(sign(CI_low)==sign(CI_upp),"*","ns"))
  
}


calc_smd <- function(data, paired = F, r = 0.7,
                    print = T, save_output = F) {
  
  data <- as.data.frame(data)
  
  d <- data.frame(MD = rep(0,nrow(data)),
                  rCI_low = rep(0,nrow(data)),
                  rCI_upper = rep(0,nrow(data)),
                  SMD = rep(0,nrow(data)),
                  SE = rep(0,nrow(data)),
                  CI_low = rep(0,nrow(data)),
                  CI_upper = rep(0,nrow(data)),
                  sig = rep(0,nrow(data)))
  
  
  if(paired == T) {
    
    for (i in 1:nrow(data)) {
      
      es <- smd(data[i,"m1"],data[i,"sd1"],data[i,"n1"],
                data[i,"m2"],data[i,"sd2"],data[i,"n2"], paired = T)
      
      d[i,] <- data.frame(MD = es[[1]], 
                          rCI_low = es[[2]], rCI_upp = es[[3]],,
                          SMD = es[[4]], SE = es[[5]],
                          CI_low = es[[6]], CI_upp = es[[7]],
                          sig = es[[8]])
    }
    
    if(print == T) print(d)
    
    if(save_output == T) {
      
      sink("effsize_within.txt")
      print(d)
      sink() 
      
    }
    
  } else {
    
    for (i in 1:nrow(data)) {
      
      es <- smd(data[i,"m1"],data[i,"sd1"],data[i,"n1"],
                data[i,"m2"],data[i,"sd2"],data[i,"n2"])
      
      d[i,] <- data.frame(MD = es[[1]], 
                          rCI_low = es[[2]], rCI_upp = es[[3]],,
                          SMD = es[[4]], SE = es[[5]],
                          CI_low = es[[6]], CI_upp = es[[7]],
                          sig = es[[8]])
    }
    
    if(print == T) print(d)
    
    if(save_output == T) {
      
      sink("effsize_between.txt")
      print(d)
      sink() 
      
    }
    
  }
  
  
}
