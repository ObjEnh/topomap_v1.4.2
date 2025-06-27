##name of script: func_det_scale.R
##purpose: determination of scale
##used in: sequence_of_lines.R
##argument(s): none
##author: Joachim Höhle
##GNU General Public License (GPL)

det_scale <- function() {
  #size of window: 2.4*r_max * 2.4*r_max
  mar <- r_max #specified distance from image center
  #coordinates of check points
  x1 <- xc - mar 
  y1 <- yc + mar
  x2 <- xc + mar
  y2 <- yc - mar
  points((x1-orig_x),(y1-orig_y),pch=16,cex=1.5, col="white") #lower check point
  points((x2-orig_x),(y2-orig_y),pch=16,cex=1.5, col="white") #upper check point
  r_max3 <- sqrt((2*r_max)^2+(2*r_max)^2)
  co <- locator(2)
  dx <- co$x[1] - co$x[2]
  dy <- co$y[1] - co$y[2]
  s1 <- sqrt(dx^2 + dy^2)
  k <- r_max3/s1
  cat("scale factor= ",k,"\n") #known distance/measured distance 
} #end of function 'det_scale'

##end of script 'func_det_scale.R'

################################################################################