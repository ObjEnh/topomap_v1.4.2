## name of script: support_adjustment_of_corner_coordinates.R
cat("version_number= ",v_nr,"\n")
## purpose: plot of the final outline of a single object in small scale
## instruction: use processing mode "demo" in a complete run before applying this script
## author: Joachim Höhle
## GNU General Public License (GPL)

##contents: 
# 1.plot of the final outline of a single object in small scale
################################################################################


## 1.plot of the final outline of a single object in small scale

setwd(home_dir)

if (bnr2 == 18) { #line number must be changed 
  
  #plot of origin, center
  x <- 0
  y <- 0
  #
  plot(x,-y, pch=3, cex=2, col="red", asp=1, xlim=c(1,1887), ylim=c(-2557, -1),
       xlab="x", ylab="y", main=paste("building",bnr2)) #small scale (image #7)
  points(xc,-yc, pch=3, cex=2, col="red", asp=1)
  
  #plot of object
  #loop
  i <- 0
  
  while(i < k1) {
    i <- i+1
    x <- Points_x[i]
    y <- Points_y[i]
    points(x,y, pch=20, cex=1.0, col="red", asp=1)
    lines(b_seri_xy2,  col="blue", asp=1, type="l", lwd=2, lty=1)
  }
  
} #end of ## 1. plot single building in small scale 


# end of supporting software to program 'adjustment of corner coordinates.R'
######################################################################################################
