##name of script: func_ro3_from_xy.R
##purpose: calculation of ro-value from coordinates of one pixel (x,y) 
#          and approximate theta-angle (theta_appr)
##used in: sequence_of_lines.R
##argument(s): none
##author: Joachim Höhle
##GNU General Public License (GPL)

ro3_from_xy <- function() { 
  theta_appr_arc <- theta_appr/omega
  ro3 <- cos(theta_appr_arc) * X + sin(theta_appr_arc) * Y
} #end of function 'ro3_from_xy'

##end of script 'func_ro3_from_xy.R'

################################################################################