
transf <- function(x, trans, data) {
  
  require(tidyverse); require(ggpubr)
  
  data <- data %>% as.data.frame()
  
  var <- data[,deparse(substitute(x))]
  
  if (trans == 'log') {
    
    var <- log(var)
    
    sw <- shapiro.test(var)
    
    plot <- var %>% 
      ggqqplot() +
      labs(title = substitute(paste('Shapiro-Wilk = ', s, ', p = ', p), 
                              list(s = round(sw$statistic,3), p = round(sw$p.value,3))))
    print(plot)
    
  }
  
  if (trans == 'log2') {
    
    var <- log2(var)
    
    sw <- shapiro.test(var)
    
    plot <- var %>% 
      ggqqplot() +
      labs(title = substitute(paste('Shapiro-Wilk = ', s, ', p = ', p), 
                              list(s = round(sw$statistic,3), p = round(sw$p.value,3))))
    print(plot)
    
  }
  
  if (trans == 'log10') {
    
    var <- log10(var)
    
    sw <- shapiro.test(var)
    
    plot <- var %>% 
      ggqqplot() +
      labs(title = substitute(paste('Shapiro-Wilk = ', s, ', p = ', p), 
                              list(s = round(sw$statistic,3), p = round(sw$p.value,3))))
    print(plot)
    
  }
  
  if (trans == 'sqrt') {
    var <- sqrt(var)
    
    sw <- shapiro.test(var)
    
    plot <- var %>% 
      ggqqplot() +
      labs(title = substitute(paste('Shapiro-Wilk = ', s, ', p = ', p), 
                              list(s = round(sw$statistic,3), p = round(sw$p.value,3))))
    print(plot)
    
  }
  
  if (trans == 'sq') {
    var <- var^2
    
    sw <- shapiro.test(var)
    
    plot <- var %>% 
      ggqqplot() +
      labs(title = substitute(paste('Shapiro-Wilk = ', s, ', p = ', p), 
                              list(s = round(sw$statistic,3), p = round(sw$p.value,3))))
    print(plot)
    
  }
  
  if (trans == 'cuberoot') {
    var <- var^(1/3)
    
    sw <- shapiro.test(var)
    
    plot <- var %>% 
      ggqqplot() +
      labs(title = substitute(paste('Shapiro-Wilk = ', s, ', p = ', p), 
                              list(s = round(sw$statistic,3), p = round(sw$p.value,3))))
    print(plot)
    
  }
  
  if (trans == 'inverse') {
    var <- 1/var
    
    sw <- shapiro.test(var)
    
    plot <- var %>% 
      ggqqplot() +
      labs(title = substitute(paste('Shapiro-Wilk = ', s, ', p = ', p), 
                              list(s = round(sw$statistic,3), p = round(sw$p.value,3))))
    print(plot)
    
  }
  
  if (trans == 'arcsin') {
    var <- asin(sqrt(var))
    
    sw <- shapiro.test(var)
    
    plot <- var %>% 
      ggqqplot() +
      labs(title = substitute(paste('Shapiro-Wilk = ', s, ', p = ', p), 
                              list(s = round(sw$statistic,3), p = round(sw$p.value,3))))
    print(plot)
    
  }
  
  res <- list(x = var, W = sw$statistic, p = sw$p.value)
}
