## name of script: func_loadLib_op.R
## purpose: required open source R-packages for 'buildenh'
#author: Joachim Höhle
## GNU General Public License (GPL)

loadLib_op <- function() { #required R-packages
  library("EBImage")
  library("spatstat")
  library("tiff")
  library("rpart")
  library("nlme")
  library("RDP")
} #end of function 'loadLib_op()'

#end of script 'func_loadLib_op.R

################################################################################