##name of script: func_fe.R
##purpose: automatic selection of object-type ("extr_wd","4_long","100_all","100_all+nonortho")
#used in: line_detection.R
##arguments:
#wd2: length of line 
#ces2: number of ortholines at the 8 longest lines
#nonortho: number of nonortholines at the 8 longest lines 
##author: Joachim Höhle
##GNU General Public License (GPL)

fe <- function(wd2,ces2,nonortho) { 
  q <- 0
  ##parameters for selection of object-type
  if (wd2 <= 80 && ces2 >= 1 && nonortho >= 8) {q <- 1} # "extr_wd"
  if (wd2 > 80 && ces2 >= 3 && nonortho >= 6) {q <- 2} # "4_long"
  if (wd2 > 80 && ces2 >= 2 && nonortho >= 7) {q <- 3} # "100_all"
  if (wd2 > 80 && ces2 >= 1 && nonortho >= 8) {q <- 4} # "100_all+nonortho"
  return(q) 
} #end of function 'fe'

#end of script 'func_fe.R

################################################################################