##name of script: func_fe.R
##purpose: automatic selection of object-type ("extr_wd","4_long","100_all","100_all+nonortho")
#used in: line_detection.R
##arguments:
#wd2: length of line 
#ces2: number of ortholines at the 8 longest lines
#nonortho: number of nonortholines at the 8 longest lines
#n_pix8: number of pixels at 8th longest line segment (threshold)
##author: Joachim Höhle
##GNU General Public License (GPL)

fe <- function(wd2,ces2,nonortho) { 
  q <- 5 #"nonortho_only"
  ##parameters for selection of object-type
  if (wd2 <= n_pix8 && ces2 >= 4 && nonortho >= 4) {q <- 1} # "extr_wd"
  if (wd2 >  n_pix8 && ces2 >= 3 && nonortho >= 5) {q <- 2} # "4_long"
  if (wd2 >  n_pix8 && ces2 >= 2 && nonortho >= 6) {q <- 3} # "100_all"
  if (wd2 >  n_pix8 && ces2 >= 1 && nonortho >= 7) {q <- 4} # "100_all+nonortho"
  if (wd2 >  n_pix8 && ces2 == 1 && nonortho == 7) {q <- 6} # "nonortho_only_RDP"
  return(q) 
} #end of function 'fe'

#end of script 'func_fe.R

################################################################################