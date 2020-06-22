
transf <- function(x, trans, data, plot) {
  
  require(dplyr, quietly = TRUE); require(ggpubr, quietly = TRUE)
  
if (missing(data)) { 
    
    var <- x
    
  } else { 
    
    data <- data %>% as.data.frame()
    
    var <- data[,deparse(substitute(x))] }
  
  var_name <- deparse(substitute(x))
  
  
  if (trans == 'log') {
    
    if (any(var == 0)){
      
      var <- log(var + 0.01) 
      
    } else { var <- log(var) }
    
    sw <- shapiro.test(var)
    
  }
  
  if (trans == 'log2') {
    
    if (any(var == 0)){
      
      var <- log2(var + 0.01) 
      
    } else { var <- log2(var) }
    
    sw <- shapiro.test(var)
    
  }
  
  if (trans == 'log10') {
    
    if (any(var == 0)){
      
      var <- log10(var + 0.01) 
      
    } else { var <- log10(var) }
    
    sw <- shapiro.test(var)
    
  }
  
  if (trans == 'sqrt') {
    
    if (any(var == 0)){
      
      var <- sqrt(var + 0.01) 
      
    } else { var <- sqrt(var) }
    
    sw <- shapiro.test(var)
    
  }
  
  if (trans == 'sq') {
    var <- var^2
    
    sw <- shapiro.test(var)
    
  }
  
  if (trans == 'cuberoot') {
    var <- var^(1/3)
    
    sw <- shapiro.test(var)
    
  }
  
  if (trans == 'inverse') {
    
    if (any(var == 0)){
      
      var <- 1/(var + 0.01) 
      
    } else { var <- 1/var }
    
    sw <- shapiro.test(var)
    
  }
  
  if (trans == 'arcsin') {
    
    if (any(var == 0)){
      
      var <- asin(sqrt(var + 0.01)) 
      
    } else { var <- asin(sqrt(var)) }
    
    sw <- shapiro.test(var)
    
  }
  
  
  if (trans == 'zscore') {
    
    var <- (var - mean(var, na.rm = TRUE)) / sd(var)
    
    sw <- shapiro.test(var)
    
  }
  
  
  if (missing(plot) || plot == TRUE) {
    print(plt)
  }
  
  res <- list(x = var, W = sw$statistic, p = sw$p.value, plot = plt)
  
}
