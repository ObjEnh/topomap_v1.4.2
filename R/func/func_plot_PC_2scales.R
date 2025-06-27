##name of script: func_plot_PC_2scales.R
#purpose: plot of point cloud (PC) of line in large- or small-scale
#function used in: "sequence of lines.R"
##argument(s):
#lnr...number of line
##author: Joachim Höhle
##GNU General Public License (GPL)

plot_PC_2scales <- function(lnr) {
  #input of references
  
  cat("lnr= ",lnr,"\n")
  PC_seg_P_nP <- PC_segment_4(lnr) #call of function
  P <- PC_seg_P_nP[[1]]
  n_P <- PC_seg_P_nP[[2]]
  cat("n_P= ",n_P,"\n")
  #plot point cloud
  #use large scale or small-scale presentation
  setwd(OrgImgPathname)
  
  #small scale:
  img_ref <- readImage(OrgImgFilename)
  display(img_ref, method="raster")
  points(P[,2],P[,3], pch=".", asp=1, cex=1.0, col="green") #switch to 'Plots' to see plot (graphics)
  
  #large scale:
  img_uds <- img_ref[orig_x:wind_x,orig_y:wind_y,1:3]
  display(img_uds, method = "raster")
  points(P[,2]-orig_x,P[,3]-orig_y,pch=".",asp=1,cex=1.0,col="green") #switch to 'Plots' (enlarged ortho)
  #end plot of line segment
  
} #end of function 'plot_PC_2scales'

##end of script 'func_plot_PC_2scales.R'

################################################################################
