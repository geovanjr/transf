
whos_norm <- function(data) {
  
  require(dplyr, quietly = TRUE)
  
  data <- data %>% as.data.frame()
  
  variable <- NULL
  W <- NULL
  p <- NULL
  
  
    
    for (i in 1:length(data)) {
      
      if (is.numeric(data[,i])) {
        
      var <- data[,i]
      
      name <- colnames(data[i])
      
      sw <- shapiro.test(var)
      
      variable[i] <- name
      W[i] <- round(sw$statistic,3)
      p[i] <- round(sw$p,4)
      
    }
    
    
  }
  
  
  signif <- ifelse(p <= 0.05, '*','normal')
  
  tests <-na.omit(data.frame(variable, W, p, signif))
  
  not_normal <- tests %>% filter(signif == '*')
  
  normal <- tests %>% filter(signif == 'normal')
  
  list(all = tests, normal, not_normal)
  
} 

