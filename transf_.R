
transf <- function(x, trans, data, plot) {
  
  require(tidyverse); require(ggpubr)
  
  data <- data %>% as.data.frame()
  
  var <- data[,deparse(substitute(x))]
  
  if (trans == 'log') {
    
    if (any(var == 0)){
      
      var <- log(var + 0.01) 
      
    } else { var <- log(var) }
    
    sw <- shapiro.test(var)
    
    plt <- var %>% 
      ggqqplot() +
      labs(title = substitute(paste('Shapiro-Wilk = ', s, ', p = ', p), 
                              list(s = round(sw$statistic,3), p = round(sw$p.value,3))))
    
  }
  
  if (trans == 'log2') {
    
    if (any(var == 0)){
      
      var <- log2(var + 0.01) 
      
    } else { var <- log2(var) }
    
    sw <- shapiro.test(var)
    
    plt <- var %>% 
      ggqqplot() +
      labs(title = substitute(paste('Shapiro-Wilk = ', s, ', p = ', p), 
                              list(s = round(sw$statistic,3), p = round(sw$p.value,3))))
    
  }
  
  if (trans == 'log10') {
    
    if (any(var == 0)){
      
      var <- log10(var + 0.01) 
      
    } else { var <- log10(var) }
    
    sw <- shapiro.test(var)
    
    plt <- var %>% 
      ggqqplot() +
      labs(title = substitute(paste('Shapiro-Wilk = ', s, ', p = ', p), 
                              list(s = round(sw$statistic,3), p = round(sw$p.value,3))))
    
  }
  
  if (trans == 'sqrt') {
    
    if (any(var == 0)){
      
      var <- sqrt(var + 0.01) 
      
    } else { var <- sqrt(var) }
    
    sw <- shapiro.test(var)
    
    plt <- var %>% 
      ggqqplot() +
      labs(title = substitute(paste('Shapiro-Wilk = ', s, ', p = ', p), 
                              list(s = round(sw$statistic,3), p = round(sw$p.value,3))))
    
  }
  
  if (trans == 'sq') {
    var <- var^2
    
    sw <- shapiro.test(var)
    
    plt <- var %>% 
      ggqqplot() +
      labs(title = substitute(paste('Shapiro-Wilk = ', s, ', p = ', p), 
                              list(s = round(sw$statistic,3), p = round(sw$p.value,3))))
    
  }
  
  if (trans == 'cuberoot') {
    var <- var^(1/3)
    
    sw <- shapiro.test(var)
    
    plt <- var %>% 
      ggqqplot() +
      labs(title = substitute(paste('Shapiro-Wilk = ', s, ', p = ', p), 
                              list(s = round(sw$statistic,3), p = round(sw$p.value,3))))
    
  }
  
  if (trans == 'inverse') {
    
    if (any(var == 0)){
      
      var <- 1/(var + 0.01) 
      
    } else { var <- 1/var }
    
    sw <- shapiro.test(var)
    
    plt <- var %>% 
      ggqqplot() +
      labs(title = substitute(paste('Shapiro-Wilk = ', s, ', p = ', p), 
                              list(s = round(sw$statistic,3), p = round(sw$p.value,3))))
    
  }
  
  if (trans == 'arcsin') {
    
    if (any(var == 0)){
      
      var <- asin(sqrt(var + 0.01)) 
      
    } else { var <- asin(sqrt(var)) }
    
    sw <- shapiro.test(var)
    
    plt <- var %>% 
      ggqqplot() +
      labs(title = substitute(paste('Shapiro-Wilk = ', s, ', p = ', p), 
                              list(s = round(sw$statistic,3), p = round(sw$p.value,3))))
    
  }
  
  
  if (trans == 'zscore') {
    
    var <- (var - mean(var, na.rm = TRUE)) / sd(var)
    
    sw <- shapiro.test(var)
    
    plt <- var %>% 
      ggqqplot() +
      labs(title = substitute(paste('Shapiro-Wilk = ', s, ', p = ', p), 
                              list(s = round(sw$statistic,3), p = round(sw$p.value,3))))
    
  }
  
  
  if (missing(plot) || plot == TRUE) {
    print(plt)
  }
  
  res <- list(x = var, W = sw$statistic, p = sw$p.value, plot = plt)
  
}
