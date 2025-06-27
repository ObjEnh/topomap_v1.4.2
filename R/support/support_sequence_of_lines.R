##name of script: support_sequence_of_lines.R
cat("version_number= ",v_nr,"\n") 
##purpose: correction of center of line & plot of lines
##author: Joachim Höhle
## GNU General Public License (GPL)

##contents:
## 1.digitize center of line 
## 2.plot a pixel cluster (PC) which represents a line-segment
## 3.plot of all pixel clusters (PC) on orthoimage in small scale
## 4.histograms of line-length (n_pixel)
## 5.calculation of ro-value from image coordinates (x,y) and 
#   line orientation (theta_appr) 
## 6.plot of single pixel cluster (PC) representing a line 
#   segment in small or large scale
## 7.determination of scale
## 8.calculation of new center of object
## 9.correction of line-midpoint-position and calculation of angle
## 10.determination of scale factor between image and plot
################################################################################

## 1. digitize and plot center of line 
#display enlarged ortho_image and plot of PC of building outline

#img_uds <- img_ref[orig_x:wind_x,orig_y:wind_y,1:3]
#
if (orig_x < 0) { #solves problems at edges of orthoimage
  orig_x = 0
}

if (orig_y < 0) {
  orig_y = 0 
}

img_uds <- img_ref[orig_x:wind_x,orig_y:wind_y,1:3]

display(img_uds, method = "raster")
points(xc-orig_x,yc-orig_y,pch=3, asp=1, cex=1.3, col="red")
points(as.integer(pc3$col-orig_x), as.integer(pc3$row-orig_y), 
       pch=20, asp=1, cex=0.3, col="green")

##manual measurement of 3 checkpoints (lower, upper, middle) to determine scale
L1 <- trans_ortho() 

## measurement of one new pixel which represents the center of line
#results: x,y in image-system of orthoimage
locator2() #measurement and plotting of one pixel's position, includes calculation of transformation-matrix

#end of script 1.
################################################################################

## 2.plot a pixel cluster (PC) which represents a line segment

PC_number <- readline("type number of PC: ")
PC_number <- as.integer(PC_number)
plot_PC(PC_number) #call of function

#end of script 2.
################################################################################

## 3.plot all pixel clusters (PCs) of one object onto orthoimage in small scale

#data
Img_name
bnr2
lnr_det3

#plot
plot_PC_all() #call of function

#end of script 3.
################################################################################

## 4.histograms of line-length (n_pixel)
#for object-types "extr_wd", "4_long", "100_all" only 

hist_lin_len() #call of function

#end of script 4
################################################################################

## 5.calculation of ro-value (ro3) from image coordinates (X,Y) 
#    and approximate line-orientation (theta_appr)

#data (subject of change)
theta_appr <- 15 # [degrees], adapt to your case
X <- 624 # [pixel] img-system
Y <- 1775 # [pixel] img-system

ro3 <- ro3_from_xy() #call of function
ro3 #[pixel]

#end of script 5
################################################################################


## 6.plot of pixel cluster (PC) representing a line segment 
#    in small or large scale

#data (to be changed)
Img_name
lnr <- 928 #to be adapted

#plot
PC_seg_P_nP <- plot_PC_2scales(lnr) #call of function

#end of script 6
################################################################################

## 7.determination of scale

det_scale() #call of function

#end of script 7
################################################################################

## 8. calculation of new center of object

#data
b13_angle_df

#function call
coo2 <- new_centre_auto()

#end of script 8
################################################################################

## 9. correction of line-midpoint position and calculation of angle
#to be used for object-type "100_all+nonortho" and sequence-method "bdr_follow"
#example b11 of ISPRS1

answ <- readline("Is the position of all midpoints correct? ")

if (answ == "N") {
  midpoints
  n_RepPoint <- 3 #row number (index) of midpoint to be corrected (in b13_angle_df), must be changed
  n_RepPoint <- as.integer(n_RepPoint)
  b13_angle_df$nr_center <- midpoints[,1]
  b13_angle_df
  PC_nr #check index of line in 'PC_nr'
  n_RepPoint2 <- 3
  #index of midpoint to be corrected (in 'all_PC' and 'PC_nr')
  all_PC[[n_RepPoint2]]
  r_dist <- dist_v2(n_RepPoint2, all_PC) #call of function 
  #         function is contained in 'func_loadLib_jh.R'
  r_dist <- round(r_dist) 
  np_r <- max(r_dist) 
  
#plot of course of line
  x <- 100
  y <- 100
  plot(x,y,pch = 3,col="red",cex=0.8,xlim=c(0,np_r),ylim=c(np_r,0),xlab="i",
       ylab="r",axes=T,frame.plot=T,main="course of line")
  
  #loop
  i <- 1
  
  while (i < np2) {
    x <- i
    y <- r_dist[i]
    points(x, y, pch = 20, col="red", cex = 0.8)
    i <- i+1
  }
  
  x <- 100 #x-value, determined from graph (manual operation)
  i <- x
  y <- r_dist[i]
  points(x,y,pch=20,col="blue",cex=1.8)
  np <- nrow(all_PC[[n_RepPoint2]])
  all_PC2 <- all_PC
  all_PC2[[n_RepPoint2]]$dist <- round(r_dist)
  i #determined from graph
  r_dist[i]
  x_centre <- all_PC2[[n_RepPoint2]]$x[i]
  y_centre <- all_PC2[[n_RepPoint2]]$y[i]
  
  #plot
  n_RepPoint
  points(x_centre,-y_centre,pch=20,col="blue",cex=1.5) #change display to graph of object (building)
  b13_angle_df$x_centre[n_RepPoint] <- x_centre
  b13_angle_df$y_centre[n_RepPoint] <- y_centre
  b13_angle_df
}  #end correction of mid_point-position of line segment  
#

#calculation of angle (center of object to new center of segment)

#center of object/building
xc <- plotPar[1]
yc <- plotPar[2]
b13_angle_df2
b13_angle_df3 <- b13_angle_df2
b13_angle_df3 
i2=10 #adapt point (row number in b13_angle_df3) 
x_centre <- b13_angle_df3[i2,3] #to be transferred to spObj_sequence_of_lines_v1.1.R
y_centre <- b13_angle_df3[i2,4] #to be transferred to spObj_sequence_of_lines_v1.1.R

#correction of angle for new midpoint
alpha <- det_of_angle(x_centre,y_centre) #call of function
b13_angle_df3$alpha[i2] <- alpha #correction
b13_angle_df3
#end of script 9. 
###############################################################################

## 10.determination of scale factor between image and plot
#locator-measurements in plot and img
#use of scaling points

#plot (bNA)
sy <- locator(2) #measure 2 checkpoints in y-direction
d_sy <- abs(sy$y[1]-sy$y[2])
#

#img (out_poly)
rx <- locator(2) #measure 2 checkpoints in x-direction
d_rx <- abs(rx$x[1]-rx$x[2])
#

kf4<- d_rx/d_sy
dif <- abs(kf4 - kf3)
dif
if (dif >= 0.0002) {
cat("difference in scale= ",dif,"\n")
kf3 <- kf4
}

#end of 10. scale-checking

################################################################################

# end of supplementing scripts for program 'sequence of lines'


