## name of script: func_det_of_angle.R
# purpose: determination of angle (azimuth) at corner points
# function used in: spObj_sequence_of_lines.R & support_sequence_of_lines.R
# author: Joachim Höhle
# GNU General Public License (GPL)

det_of_angle <- function(corner_x,corner_y) { 
  #browser()
  alph <- (atan2(-(corner_y-yc),(corner_x-xc)))*omega # -dy/dx because alpha 
    #in math-coordinate-system
  cat("dy= ", (corner_y-yc), "\n")
  cat("dx= ", (corner_x-xc), "\n")
  
  #alternative solutions
  #if ((corner_y-yc)<0) {alph=alph+180} 
  #if ((corner_x-xc)<0) {alph=alph-180}
  
  if(alph < 0) {
    alph3 <- alph + 360 
  } else {
    alph3 <- alph
  } #end if-else
  cat("alph= ",alph, "alph3= ",alph3,"\n")
  angle_center_corner <- alph3
  return(angle_center_corner)
} #end of function 'det_of_angle'

#end of script "func_det_of_angle.R"

################################################################################