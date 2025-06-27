##name of script: func_mean_line.R
#purpose: calculation and plot of midpoint of PointCloud (PC);
#         calculation of angle between center of object to midpoint of PC
#used in: sequence_of_lines.R
#argument: ln_num...number of PC
#author: Joachim Höhle
#GNU General Public License (GPL)

mean_line <- function(ln_num) {
  i <- ln_num 
  x <- mean(all_PC[[i]]$x)
  y <- mean(all_PC[[i]]$y)
  points(x,-y,pch=20,asp=1,cex=2,col="red") #plot
  nr_P <- PC_nr[i]
  angle_center_corner <- det_of_angle(x,y) #determination of angle
  par_midp <<- list(nr_P,angle_center_corner,x,y) 
  return(par_midp)
} #end of function

#end of script 'func_mean_line.R'

################################################################################
