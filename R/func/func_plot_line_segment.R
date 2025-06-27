##func_plot_line_segment.R
##function 'plot_line_segment'
#name of script: func_plot_line_segment.R
##purpose: plot of one line segment onto orthoimage
##used in: support_line_detection.R
##argument(s): index i
##author: Joachim Höhle
##GNU General Public License (GPL)

plot_line_segment <- function(i) { 
  B5_4_ord[i,]
  cat("PC_nr=", B5_4_ord$lnr[i], "\n")
  y <- round(-y, digits=0) #adapt to math_system 
  cat("y= ", y,"\n")
  #angles
  theta_img <- B5_4_ord$theta_angle[i]
  theta_math <- 180 - theta_img #change to math-system
  cat("theta_math= ", theta_math, "\n")
  theta_math_arc <- theta_math/omega
  a = (-1/tan(theta_math_arc))
  cat("a= ",a,"\n")
  B5_4_ord$ro_pixel[i]
  
  p2 <- round(x*cos(theta_math_arc) + y*sin(theta_math_arc)) #sign may be '+' or '-'
  #p2 <- -p2
  b <- p2/sin(theta_math_arc)
  #orig_y <- (-orig_y) #change to math-system
  orig_y_math <- (-orig_y) #change to math-system
  #calculation by intercept for image extract (math_system)
  b2 <- a*orig_x + b - orig_y_math #original
  
  #change of parameter to image_system
  b2_img <- (-b2)
  a_img <- (-a)
  
  coef2 <- c(b2_img,a_img)
  
  # plot
  if (is.finite(a)) {
    abline(coef2, col="red", lty=1, lwd=2, asp=1)
  }  else {
    ro_l1 <- B5_4_ord$ro_pixel[i]
    ro_l2 <- ro_l1 + ro_1
    ro_l3 <- round(ro_l2 - orig_x)
    lines(c(ro_l3,ro_l3),c(0, (wind_y - orig_y)),col="red")
  } #end if-else
  
} #end of function 'plot_line_segment'

##end of script 'func_plot_line_detection.R'