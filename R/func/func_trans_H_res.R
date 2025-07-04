## name of script: func_trans_H_res.R
## purpose: transformation of the parameters in B (theta_index, ro_index, N) to other units (degrees, pixels)
## used in: script 'detection of lines.R'
## argument(s): 
# B1: ordered matrix of detected lines after Hough transform 
# theta_step1: step of theta-angle in theta-indices
# ro_step1: step in ro in ro-indices
# ro11: first value in ro 
# k1: scale factor for converting the number of pixels into length [pixels] 
# author: Joachim Höhle
# GNU General Public License (GPL)

trans_H_res <- function(B1,theta_step1,ro_step1,ro11,k1) {
  theta_ang <- (B1[i,2]-1)*theta_step1
  ro_pix <- round((B1[i,3]-1)*ro_step1 + ro11)
  n <<- B1[i,4]/k1 
  H_para <- matrix(ncol=1,nrow=6)
  H_para[,] <- c(B1[i,2],B1[i,3],B1[i,4],theta_ang,ro_pix,n)
  return(H_para)
} #end of function 'trans_H_res(B1,theta_step1,ro_step1,ro11,k1)'

#end of script 'func_trans_H_res.R'

################################################################################

