effsize_plot <- function(x, g, method = 'r', paired = F, 
                         color = 'black', size = 2,
                         width = .05, 
                         line.col = 'grey', lwd = 1, lty = 2, 
                         ggtheme = theme_minimal(),
                         par_r = list(conf = .95, type = 'perc', R = 1000), 
                         par_d = list(pooled = F, hedges.correction = F, conf.level = .95),
                         plot = T, show = F) {
  require(dplyr, warn.conflicts = F)
  require(ggplot2, warn.conflicts = F)
  
  if (method == 'r') {
    
    require(rcompanion, warn.conflicts = F)
    
    if (paired == TRUE) {
      
      variable <- NULL
      V <- NULL
      p <- NULL
      r <- NULL
      lower <- NULL
      upper <- NULL
      
      for (i in 1:length(x)) {
        
        variable[i] <- names(x)[i]
        wt <- wilcox.test(x[,i] ~ g, paired = T)
        
        V[i] <- wt$statistic
        p[i] <- wt$p.value
        
        es <- wilcoxonPairedR(x[,i], g, ci=TRUE, histogram = FALSE, 
                              conf = if (is.null(par_r$conf)) {conf = .95} 
                              else {conf = par_r$conf}, 
                              
                              type = 
                                if (is.null(par_r$type)) {type = 'perc'} 
                              else {type = par_r$type},
                              
                              R = if (is.null(par_r$R)) {R = 1000} 
                              else {R = par_r$R})
        
        r[i] <- es$r
        lower[i] <- es$lower.ci
        upper[i] <- es$upper.ci
        
      }
      
      df <- data.frame(variable, V, p = round(p,4), r, lower, upper)
      
    } else {
      
      variable <- NULL
      W <- NULL
      p <- NULL
      r <- NULL
      lower <- NULL
      upper <- NULL
      
      for (i in 1:length(x)) {
        
        variable[i] <- names(x)[i]
        
        wt <- wilcox.test(x[,i] ~ g, paired = F)
        
        W[i] <- wt$statistic
        p[i] <- wt$p.value
        
        es <- wilcoxonR(x[,i], g, ci=TRUE, histogram = FALSE,
                        conf = if (is.null(par_r$conf)) {conf = .95} 
                        else {conf = par_r$conf}, 
                        
                        type = 
                          if (is.null(par_r$type)) {type = 'perc'} 
                        else {type = par_r$type},
                        
                        R = if (is.null(par_r$R)) {R = 1000} 
                        else {R = par_r$R})
        
        r[i] <- es$r
        lower[i] <- es$lower.ci
        upper[i] <- es$upper.ci
        
      }
      
      df <- data.frame(variable, W, p = round(p,4), r, lower, upper)
      
    }
    
    es_plot <- df %>% 
      ggplot(aes(variable,r)) +
      geom_errorbar(aes(ymin=lower, ymax=upper), width = width, col = color) +
      geom_hline(yintercept = 0, col=line.col, lty=lty, lwd = lwd) +
      geom_point(col = color, size = size) +
      labs(x='') +
      coord_flip() +
      ggtheme
    
  } # r
  
  
  if (method == 'd') {
    
    require(effsize, warn.conflicts = F)
    
    if (paired == TRUE) {
      
      variable <- NULL
      t <- NULL
      p <- NULL
      d <- NULL
      lower <- NULL
      upper <- NULL
      
      for (i in 1:length(x)) {
        
        variable[i] <- names(x)[i]
        
        tt <- t.test(x[,i] ~ g, paired = TRUE)
        
        t[i] <- tt$statistic[[1]]
        p[i] <- tt$p.value
        
        es <- cohen.d(x[,i], g, paired=FALSE,
                      pooled = if (is.null(par_d$pooled)) {pooled = FALSE} 
                      else {pooled = par_d$pooled}, 
                      
                      hedges.correction = 
                        if (is.null(par_d$hedges.correction)) {hedges.correction = FALSE} 
                      else {hedges.correction = par_d$hedges.correction},
                      
                      conf.level = if (is.null(par_d$conf.level)) {conf.level = .95} 
                      else {conf.level = par_d$conf.level})
        
        d[i] <- es$estimate
        lower[i] <- es$conf.int[[1]]
        upper[i] <- es$conf.int[[2]]
        
      }
      
    } else {
      
      variable <- NULL
      t <- NULL
      p <- NULL
      d <- NULL
      lower <- NULL
      upper <- NULL
      
      for (i in 1:length(x)) {
        
        variable[i] <- names(x)[i]
        
        tt <- t.test(x[,i] ~ g, paired = FALSE)
        
        t[i] <- tt$statistic[[1]]
        p[i] <- tt$p.value
        
        es <- cohen.d(x[,i], g, paired=FALSE,
                      pooled = if (is.null(par_d$pooled)) {pooled = FALSE} 
                      else {pooled = par_d$pooled}, 
                      
                      hedges.correction = 
                        if (is.null(par_d$hedges.correction)) {hedges.correction = FALSE} 
                      else {hedges.correction = par_d$hedges.correction},
                      
                      conf.level = if (is.null(par_d$conf.level)) {conf.level = .95} 
                      else {conf.level = par_d$conf.level})
        
        d[i] <- es$estimate
        lower[i] <- es$conf.int[[1]]
        upper[i] <- es$conf.int[[2]]
        
      }
      
    }
    
    df <- data.frame(variable, t, p = round(p,4), d, lower, upper)
    
    es_plot <- df %>% 
      ggplot(aes(variable,d)) +
      geom_errorbar(aes(ymin=lower, ymax=upper), width = width, col = color) +
      geom_hline(yintercept = 0, col=line.col, lty=lty, lwd = lwd) +
      geom_point(col = color, size = size) +
      labs(x='') +
      coord_flip() +
      ggtheme
    
  } # d
  
  
  if (plot == T) {
    print(es_plot)
  }
  
  if (show == T) {
    print(df)
  }
  
  res <- list(es_plot = es_plot, metrics = df) 
  
} # funcao

x <- runif(100,20,40)
x[51:100] <- runif(50,10,38)
y <- runif(100,12,19)
y[51:100] <- runif(50,12,35)

group <- factor(rep(c('Patient','Control'), each = 50))

df <- data.frame(x,y)

es <- effsize_plot(df, g = group, method = 'd', paired = T, plot = T, show = T, 
             color = 'darkgrey')

