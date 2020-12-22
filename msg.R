curve(2*x^2, -2,2, lwd = 3, col = 1, axes = F, 
      ylim = c(-10,10),
      xlim = c(-5,5), 
      xlab = '', ylab = '')
curve(-x^3, 0,4, lwd = 3, col = 1, add = T)
curve(x^3, -4,0, lwd = 3, col = 1, add = T)
points(c(-.5,.5), c(6,6), pch=19, cex=2)
curve(3*x^2+3, -.5,.5, lwd = 3, col = 2, add = T)
curve(-1/2*x^2+7, -.5,.5, lwd = 3, col = 1, add = T)
points(rep(0,length(seq(-3,-8,-2))), seq(-3,-8,-2), pch=8, cex = 1)
curve(-1/2*x^2+10, -2,2, col = 1, add = T, lwd = 3)
curve(-10.7+x-x, -2.3,2.3, col = 1, add = T, lwd = 3)
points(0,4.5,pch=17, cex = 2)
curve(-1/abs(x), -1,1, add = T, lwd = 3)
curve(x*1/3, 0,3, lwd=3, add = T)
curve(-x*1/3, -3,0, lwd=3, add = T)
curve(.2*cos(x*10)+8,-pi/2-1/2,pi/2+1/2, add = T, lwd=3)
title(main='Merry Xmas', sub = 'by Geo')
