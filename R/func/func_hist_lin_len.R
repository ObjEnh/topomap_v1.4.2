##name of script: func_hist_lin_len.R
##purpose: plot of histograms for the 2 main directions
##used in: sequence_of_lines.R
##argument(s): none
##author: Joachim Höhle
##GNU General Public License (GPL)

hist_lin_len <- function() { 
  # in ref direction
  B5_4d_ord
  head(B5_4d_ord)
  max(B5_4d_ord$n_pixel)
  min(B5_4d_ord$n_pixel)
  dif_n_pixel <- max(B5_4d_ord$n_pixel) - min(B5_4d_ord$n_pixel)
  hist(B5_4d_ord$n_pixel,nclass=dif_n_pixel)
  
  # in 'orthogonal to ref' direction
  B5_4dd_ord
  head(B5_4dd_ord)
  max(B5_4dd_ord$n_pixel)
  min(B5_4dd_ord$n_pixel)
  dif_n_pixel <- max(B5_4dd_ord$n_pixel) - min(B5_4dd_ord$n_pixel)
  hist(B5_4dd_ord$n_pixel,nclass=dif_n_pixel)
} #end of function 'hist_lin_len'

##end of script 'func_hist_lin_len.R'

################################################################################