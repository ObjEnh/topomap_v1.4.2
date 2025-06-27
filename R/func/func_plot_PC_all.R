##name of script: func_plot_PC_all.R
##purpose: plot of all point clusters (PCs) onto orthoimage
##used in: sequence_of_lines.R
##argument(s): none
##author: Joachim Höhle
##GNU General Public License (GPL)

plot_PC_all <- function() { 
  
  #input of references
  
  if (Img_name == "ISPRS7") {
    OrgImgPathname <- paste(home_dir,"/data",sep = "")
    OrgImgFilename <- "top_mosaic_09cm_area7.tif"
  }
  
  if (Img_name == "ISPRS1") {
    OrgImgPathname <- paste(home_dir,"/data",sep = "")
    OrgImgFilename <- "top_mosaic_09cm_area1.tif"
  }
  
  setwd(OrgImgPathname)
  img_ref <- readImage(OrgImgFilename)
  display(img_ref, method="raster")
  
  for (i in lnr_det3) {
    lnr <- i
    cat("lnr=",lnr,"\n")
    setwd(home_dir)
    fname <- paste("./data/",Img_name,"/b",bnr2,"_",lnr,".txt",sep="")
    pc <- read.table(fname, header=TRUE)
    names(pc) <- c("nr","col","row")
    #plot
    #browser()
    points(pc$col, pc$row, pch=20, asp=1, cex=0.3, col="white")
  } #end for-loop
} #end of function 'plot_PC_all'

##end of script 'func_plot_PC_all.R'

################################################################################