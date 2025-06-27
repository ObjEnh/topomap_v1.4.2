##name of script: func_changeDir.R
#used in 'sequence_of_lines.R
#purpose:changes the direction in the sequence of mipdpoints to 'anti-clock-wise'
#author: Joachim Höhle
#GNU General Public License (GPL)
#
changeDir <- function(sequence) {
  
  n_sequence <- length(sequence)
  seqence_new_dir <- rep(NA,n_sequence)
  j=n_sequence
  i=1
  
  while (i <= n_sequence) {
    seqence_new_dir[i] <- sequence[j] 
    i <- i + 1
    j <- j - 1
  } 
  
  return(seqence_new_dir)

} #end of function 'changeDir(sequence)'

#end of script 'func_changeDir.R'
################################################################################
