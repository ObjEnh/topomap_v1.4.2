##name of script: spObj_extract_single_building.R
cat("version_number= ",v_nr,"\n")
#purpose: partition of special objects 
#instruction: measure 2 pixels which will separate object (zoom:200%)
#             use 'display(is_bnr)'
#author: Joachim Höhle
#GNU General Public License (GPL)
#####################################################################

if (Img_name == "ISPRS7") {
  
#no corrections

} #end ISPRS7
#################################################################################


if (Img_name == "ISPRS1") {
  
} #end ISPRS1
################################################################################

if (Img_name == "ISPRS4") {
  
  #b1
  if (bnr2 == 1 && p_pos == "cor_sep") {
    cat("start of spObj_extract_single_building.R -first part",sep = "")
    n_bnr2 = 0
    display(is_bnr)
    is_bnr2 <- is_bnr
    x_sep=53;y_sep=158 #manual input
    imageData(is_bnr2)[(x_sep-3):(x_sep+3), (y_sep-3):(y_sep+3)] = FALSE #first cut
    #imageData(is_bnr2)[282:286, 96:100] = FALSE #second cut
    display(is_bnr2)
    is_bnr2_label <- bwlabel(is_bnr2)
    cat("Number of objects= ",max(is_bnr2_label),"\n")
    coor_part<-computeFeatures.moment(is_bnr2_label) #geometric features (moment)
    shap_part<-computeFeatures.shape(is_bnr2_label) #geometric features (shape)
    #
    #first part
    is_label_1 <- is_bnr2_label@.Data == 1 
    imageData(is_label_1)[x_sep, y_sep]
    display(is_label_1)
    #
    #second part
    is_label_2 <- is_bnr2_label@.Data == 2 
    display(is_label_2)
    
    #save original values
    xc_orig <- xc
    yc_orig <- yc
    #
    
    if (proc_mode == "obj_wise" && part == "2parts_1") {
      bnr2 <- (bnr2)*10+1  #change of number 
      bnr2_part <- bnr2 
    }
    
    if (proc_mode == "obj_wise" && part == "2parts_2") {
      bnr2 <- (bnr2)*10+2 #?
      bnr2
    }
    
    #second part 
    if (bnr2 == 12) { 
      cat("start of spObj_extract_single_building.R - second part",sep = "", "\n")
      part <-  "2parts_2"
      bnr2_part <- bnr2 
      is_label_2 <- is_bnr2_label@.Data == 2 #right part
      display(is_label_2)
      is_label_2
      coords <- data.frame(x=as.numeric(row(is_label_2)),y=as.numeric(col(is_label_2)), is_label_2=as.numeric(is_label_2))
      coords <- coords[coords$is_label_2 == 1,] #removal of pixels which do not have the label of the building
      xc <- coor_part[2,1] #generate xc,yc,r_max,alpha and store with bnr2=342
      yc <- coor_part[2,2]
      alpha <- coor_part[2,5]*omega
      r_max <- shap_part[2,6]
      dy_window_plot
      
      #output plot parameter
      plotPar <- c(xc,yc,r_max,alpha,dy_window_plot)
      setwd(home_dir)
      f1 <- paste("./data/",Img_name,"/param_b",bnr2,sep="") 
      save(plotPar, file=f1) #parameter xc, yc, r_max, alpha
      f <- paste("./data/",Img_name,"/b_nr",sep = "")
      save(bnr2,file=f)
      
      #plot of PC and checkpoints (large scale)
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",
           asp=1,xlim=c(xc-r_max2,xc+r_max2),ylim=c(yc+r_max2, 
                                                    yc-r_max2), xlab = NULL, ylab=NULL, ann=T, 
           main=paste("b", bnr2,sep = ""), axes=TRUE)
      points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
      
      #generation of image
      file1 <- paste('./data/',Img_name,'/images/b',bnr2,'_new8.tif',sep = "")
      tiff(file1, width=578, height=578, units="px", bg = "white")
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",asp=1,xlim=c(xc-r_max2,xc+r_max2),
           ylim=c(yc+r_max2, yc-r_max2), xlab = NULL, ylab=NULL, ann=T, main=paste("b", bnr2,
                                                                                   sep = ""), axes=TRUE)
      points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
      dev.off()
      #windows() #if necessary
      cat("end of spObj_extract_single_building.R - second part", sep = "","\n")
    } #end of b42
    n_bnr2 <- n_bnr2 + 1
  } #end b1
 
  #b4
   
  if (bnr2 == 4 && p_pos == "cor_sep") {
    cat("start of spObj_extract_single_building.R -first part",sep = "")
    n_bnr2 = 0
    display(is_bnr)
    is_bnr2 <- is_bnr
    x_sep=473;y_sep=47 #manual input
    imageData(is_bnr2)[(x_sep-2):(x_sep+2), (y_sep-2):(y_sep+2)] = FALSE #first cut
    #imageData(is_bnr2)[282:286, 96:100] = FALSE #second cut
    display(is_bnr2)
    is_bnr2_label <- bwlabel(is_bnr2)
    cat("Number of objects= ",max(is_bnr2_label),"\n")
    coor_part<-computeFeatures.moment(is_bnr2_label) #geometric features (moment)
    shap_part<-computeFeatures.shape(is_bnr2_label) #geometric features (shape)
    #
    #first part
    is_label_1 <- is_bnr2_label@.Data == 1 
    imageData(is_label_1)[x_sep, y_sep]
    display(is_label_1)
    #
    #second part
    is_label_2 <- is_bnr2_label@.Data == 2 
    display(is_label_2)
    
    #save original values
    xc_orig <- xc
    yc_orig <- yc
    #
    
    if (proc_mode == "obj_wise" && part == "2parts_1") {
      bnr2 <- (bnr2)*10+1  #change of number 
      bnr2_part <- bnr2 
    }
    
    if (proc_mode == "obj_wise" && part == "2parts_2") {
      bnr2 <- (bnr2)*10+2 #?
      bnr2
    }
    
    #first part
    if (bnr2 == 41) { 
      is_label_1
      display(is_label_1)
      setwd(home_dir)
      f1 <- paste("./data/",Img_name,"/param_b",bnr2,sep="") 
      save(plotPar, file=f1) #parameter xc, yc, r_max, alpha
      coords <- data.frame(x=as.numeric(row(is_label_1)),y=as.numeric(col(is_label_1)), is_bnr=as.numeric(is_label_1))
      coords <- coords[coords$is_label_1 == 1,] #removal of pixels which do not have the label of the building
      
      #calculation of new centre of object from connected components
      xc <- coor_part[1,1]
      yc <- coor_part[1,2]
      alpha <- coor_part[1,5]*omega
      r_max <- shap_part[1,6]
      
      #storage of plot parameter of separated object
      setwd(home_dir)
      plotPar <- c(xc,yc,r_max,alpha,dy_window_plot)
      f1 <- paste("./data/",Img_name,"/param_b",bnr2,sep="") 
      save(plotPar,file=f1) #parameter xc, yc, r_max, alpha
      
      #plot of PC and checkpoints (large scale)
      dev.set(2)
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",asp=1,xlim=c(xc-r_max2,xc+r_max2),
           ylim=c(yc+r_max2, yc-r_max2), xlab = NULL, ylab=NULL, ann=T, main=paste("b", bnr2,
                                                                                   " - left",sep = ""), axes=TRUE)
      points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
      #
      
      #generation of image
      setwd(home_dir)
      f <- paste("./data/",Img_name,"/b_nr",sep = "")
      save(bnr2,file=f)
      file1 <- paste('./data/',Img_name,'/images/b',bnr2,'_new8.tif',sep = "")
      tiff(file1, width=578, height=578, units="px", bg = "white")
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",asp=1,xlim=c(xc-r_max2,xc+r_max2),ylim=c(yc+r_max2, yc-r_max2), xlab = NULL, ylab=NULL, ann= FALSE, main=paste("b", bnr2), axes=TRUE)
      points(xc, yc, pch = 16, cex=1.5, col = "black", asp=1) #centre of PC
      points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      dev.off()
      cat("end of spObj_extract_single_building.R -first part",sep = "")
    } #end b41 (first part)
    
    #second part 
    if (bnr2 == 42) { 
      cat("start of spObj_extract_single_building.R - second part",sep = "", "\n")
      part <-  "2parts_2"
      bnr2_part <- bnr2 
      is_label_2 <- is_bnr2_label@.Data == 2 #right part
      display(is_label_2)
      is_label_2
      coords <- data.frame(x=as.numeric(row(is_label_2)),y=as.numeric(col(is_label_2)), is_label_2=as.numeric(is_label_2))
      coords <- coords[coords$is_label_2 == 1,] #removal of pixels which do not have the label of the building
      xc <- coor_part[2,1] #generate xc,yc,r_max,alpha and store with bnr2=342
      yc <- coor_part[2,2]
      alpha <- coor_part[2,5]*omega
      r_max <- shap_part[2,6]
      dy_window_plot
      
      #output plot parameter
      plotPar <- c(xc,yc,r_max,alpha,dy_window_plot)
      setwd(home_dir)
      f1 <- paste("./data/",Img_name,"/param_b",bnr2,sep="") 
      save(plotPar, file=f1) #parameter xc, yc, r_max, alpha
      f <- paste("./data/",Img_name,"/b_nr",sep = "")
      save(bnr2,file=f)
      
      #plot of PC and checkpoints (large scale)
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",
           asp=1,xlim=c(xc-r_max2,xc+r_max2),ylim=c(yc+r_max2, 
                                                    yc-r_max2), xlab = NULL, ylab=NULL, ann=T, 
           main=paste("b", bnr2,sep = ""), axes=TRUE)
      points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
      
      #generation of image
      file1 <- paste('./data/',Img_name,'/images/b',bnr2,'_new8.tif',sep = "")
      tiff(file1, width=578, height=578, units="px", bg = "white")
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",asp=1,xlim=c(xc-r_max2,xc+r_max2),
           ylim=c(yc+r_max2, yc-r_max2), xlab = NULL, ylab=NULL, ann=T, main=paste("b", bnr2,
                                                                                   sep = ""), axes=TRUE)
      points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
      dev.off()
      #windows() #if necessary
      cat("end of spObj_extract_single_building.R - second part", sep = "","\n")
    } #end of b42
      n_bnr2 <- n_bnr2 + 1
  } #end b4
  
  ####
  
  #b6
  if (bnr2 == 6 && p_pos == "cor_sep") {
    cat("start of spObj_extract_single_building.R -first part",sep = "")
    n_bnr2 = 1
    display(is_bnr)
    is_bnr2 <- is_bnr
    x_sep=461;y_sep=187 #manual input of coordinates for point of separation
    imageData(is_bnr2)[(x_sep-2):(x_sep+2), (y_sep-2):(y_sep+2)] = FALSE #first cut
    #imageData(is_bnr2)[282:286, 96:100] = FALSE #second cut
    display(is_bnr2)
    is_bnr2_label <- bwlabel(is_bnr2)
    cat("Number of objects= ",max(is_bnr2_label),"\n")
    coor_part<-computeFeatures.moment(is_bnr2_label) #geometric features (moment)
    shap_part<-computeFeatures.shape(is_bnr2_label) #geometric features (shape)
    #
    #first part
    is_label_1 <- is_bnr2_label@.Data == 1 
    #new: close gap
    x_close=208
    y_close=493
    imageData(is_label_1)[(x_close-2):(x_close+2), (y_close-2):(y_close+2)] = 1 #close gap
    display(is_label_1)
    #
    #second part
    is_label_2 <- is_bnr2_label@.Data == 2 
    display(is_label_2)
    
    #save original values
    xc_orig <- xc
    yc_orig <- yc
    
    if (proc_mode == "obj_wise" && part == "2parts_1") {
      bnr2 <- (bnr2)*10+1  #change of number to 61
      bnr2_part <- bnr2
    }
    
    # 
    if (proc_mode == "obj_wise" && part == "2parts_2") {
      bnr2 <- (bnr2)*10+2 #62
      bnr2
    }
    
    #first part
    if (bnr2 == 61) { 
      is_label_1
      display(is_label_1)
      setwd(home_dir)
      f1 <- paste("./data/",Img_name,"/param_b",bnr2,sep="")
      save(plotPar, file=f1) #parameter xc, yc, r_max, alpha
      coords <- data.frame(x=as.numeric(row(is_label_1)),y=as.numeric(col(is_label_1)), is_label_1=as.numeric(is_label_1))
      coords <- coords[coords$is_label_1 == 1,] #removal of pixels which do not have the label of the building
      
      #calculation of new centre of object from connected components
      xc <- coor_part[1,1]
      yc <- coor_part[1,2]
      alpha <- coor_part[1,5]*omega
      r_max <- shap_part[1,6]
      
      #storage of plot parameter of separated object
      setwd(home_dir)
      plotPar <- c(xc,yc,r_max,alpha,dy_window_plot)
      f1 <- paste("./data/",Img_name,"/param_b",bnr2,sep="") #
      save(plotPar,file=f1) #parameter xc, yc, r_max, alpha
      load(f1)
      plotPar
      #plot of PC and checkpoints (large scale)
      dev.set(2)
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",asp=1,xlim=c(xc-r_max2,xc+r_max2),
           ylim=c(yc+r_max2, yc-r_max2), xlab = NULL, ylab=NULL, ann=T, main=paste("b", bnr2,
                                                                                   " - left",sep = ""), axes=TRUE)
      points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
      #
      
      #generation of image
      setwd(home_dir)
      f <- paste("./data/",Img_name,"/b_nr",sep = "")
      save(bnr2,file=f)
      file1 <- paste('./data/',Img_name,'/images/b',bnr2,'_new8.tif',sep = "")
      tiff(file1, width=578, height=578, units="px", bg = "white")
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",asp=1,xlim=c(xc-r_max2,xc+r_max2),ylim=c(yc+r_max2, yc-r_max2), xlab = NULL, ylab=NULL, ann= FALSE, main=paste("b", bnr2), axes=TRUE)
      points(xc, yc, pch = 16, cex=1.5, col = "black", asp=1) #centre of PC
      points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      dev.off()
      cat("end of spObj_extract_single_building.R -first part",sep = "")
    } #end b61 (first part)
    
    #second part 
    if (bnr2 == 62) { 
      cat("start of spObj_extract_single_building.R - second part",sep = "", "\n")
      part <-  "2parts_2"
      bnr2_part <- bnr2 
      is_label_2 <- is_bnr2_label@.Data == 2 #right part
      display(is_label_2)
      #new: close gap
      x_close=463; y_close=188
      imageData(is_label_2)[(x_close-2):(x_close+2),(y_close-2):(y_close+2)] = 2
      display(is_label_2)
      
      #new: separation
      x_sep=539;y_sep=202 #manual input
      imageData(is_label_2)[(x_sep-2):(x_sep+2), (y_sep-2):(y_sep+2)] = FALSE #cut
      display(is_label_2)
      
      #new: close gap
      imageData(is_label_2)[534:539,199:203] = 2
      display(is_label_2)
      
      #
      is_bnr2_label3 <- bwlabel(is_label_2)
      str(is_bnr2_label3)
      cat("Number of objects= ",max(is_bnr2_label3),"\n")
      coor_part<-computeFeatures.moment(is_bnr2_label3) #geometric features (moment)
      shap_part<-computeFeatures.shape(is_bnr2_label3) #geometric features (shape)
      #
      coords <- data.frame(x=as.numeric(row(is_bnr2_label3)),y=as.numeric(col(is_bnr2_label3)), is_bnr2_label3=as.numeric(is_bnr2_label3))
      coords <- coords[coords$is_bnr2_label3 == 1,] #removal of pixels which do not have the label of the building
      length(coords$x)
      xc <- coor_part[1,1] #generate xc,yc,r_max,alpha and store with bnr2=62
      yc <- coor_part[1,2]
      alpha <- coor_part[1,5]*omega
      r_max <- shap_part[1,6]
      dy_window_plot
      
      #output plot parameter
      plotPar <- c(xc,yc,r_max,alpha,dy_window_plot)
      setwd(home_dir)
      f1 <- paste("./data/",Img_name,"/param_b",bnr2,sep="")
      save(plotPar, file=f1) #parameter xc, yc, r_max, alpha
      f <- paste("./data/",Img_name,"/b_nr",sep = "")
      save(bnr2,file=f)
      
      #plot of PC and checkpoints (large scale)
      plotPar
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",
           asp=1,xlim=c(xc-r_max2,xc+r_max2),ylim=c(yc+r_max2, 
                                                    yc-r_max2), xlab = NULL, ylab=NULL, ann=T, 
           main=paste("b", bnr2,sep = ""), axes=TRUE)
      points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
      #
      ##generation of image
      file1 <- paste('./data/',Img_name,'/images/b',bnr2,'_new8.tif',sep = "")
      tiff(file1, width=578, height=578, units="px", bg = "white")
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",asp=1,xlim=c(xc-r_max2,xc+r_max2),
           ylim=c(yc+r_max2,yc-r_max2),xlab=NULL,ylab=NULL,ann=T,main=paste("b",bnr2,sep=""),axes=TRUE)
      
      #checkpoints
      points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
      dev.off()
      #windows() #if necessary
      
      #new: plot with plotPar of b61
      setwd(home_dir)
      bnr2=61
      f1 <- paste("./data/",Img_name,"/param_b",bnr2,sep="")
      load(f1)
      plotPar
      bnr2=62
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",
           asp=1,xlim=c(xc-r_max2,xc+r_max2),ylim=c(yc+r_max2, 
                                                    yc-r_max2), xlab = NULL, ylab=NULL, ann=T, 
           main=paste("b", bnr2," - plotPar of b61",sep = ""), axes=TRUE)
      points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #center of PC
      #
      cat("end of spObj_extract_single_building.R - second part", sep = "","\n")
    } #end of b62
    n_bnr2 <- n_bnr2 + 1
  } #end b6
  
  ####
  
  #b8
  
  if (bnr2 == 8 && p_pos == "cor_sep") {
    cat("start of spObj_extract_single_building.R -first part",sep = "")
    n_bnr2 = 0
    display(is_bnr)
    is_bnr2 <- is_bnr
    x_sep=549;y_sep=345 #manual input
    imageData(is_bnr2)[(x_sep-2):(x_sep+2), (y_sep-2):(y_sep+2)] = FALSE #first cut
    #imageData(is_bnr2)[282:286, 96:100] = FALSE #second cut
    display(is_bnr2)
    is_bnr2_label <- bwlabel(is_bnr2)
    cat("Number of objects= ",max(is_bnr2_label),"\n")
    coor_part<-computeFeatures.moment(is_bnr2_label) #geometric features (moment)
    shap_part<-computeFeatures.shape(is_bnr2_label) #geometric features (shape)
    #
    #first part
    is_label_1 <- is_bnr2_label@.Data == 1 
    imageData(is_label_1)[x_sep, y_sep]
    display(is_label_1)
    #
    #second part
    is_label_2 <- is_bnr2_label@.Data == 2 
    display(is_label_2)
    
    #save original values
    xc_orig <- xc
    yc_orig <- yc
    #
    
    if (proc_mode == "obj_wise" && part == "2parts_1") {
      bnr2 <- (bnr2)*10+1  #change of number 
      bnr2_part <- bnr2 
    }
    
    if (proc_mode == "obj_wise" && part == "2parts_2") {
      bnr2 <- (bnr2)*10+2 #?
      bnr2
    }
    
    #first part
    if (bnr2 == 81) { 
      is_label_1
      display(is_label_1)
      setwd(home_dir)
      f1 <- paste("./data/",Img_name,"/param_b",bnr2,sep="") 
      save(plotPar, file=f1) #parameter xc, yc, r_max, alpha
      coords <- data.frame(x=as.numeric(row(is_label_1)),y=as.numeric(col(is_label_1)), is_label_1=as.numeric(is_label_1))
      coords <- coords[coords$is_label_1 == 1,] #removal of pixels which do not have the label of the building
      
      #calculation of new centre of object from connected components
      xc <- coor_part[1,1]
      yc <- coor_part[1,2]
      alpha <- coor_part[1,5]*omega
      r_max <- shap_part[1,6]
      
      #storage of plot parameter of separated object
      setwd(home_dir)
      plotPar <- c(xc,yc,r_max,alpha,dy_window_plot)
      f1 <- paste("./data/",Img_name,"/param_b",bnr2,sep="") 
      save(plotPar,file=f1) #parameter xc, yc, r_max, alpha
      
      #plot of PC and checkpoints (large scale)
      dev.set(2)
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",asp=1,xlim=c(xc-r_max2,xc+r_max2),
           ylim=c(yc+r_max2, yc-r_max2), xlab = NULL, ylab=NULL, ann=T, main=paste("b", bnr2,
                                                                                   " - left",sep = ""), axes=TRUE)
      points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
      #
      
      #generation of image
      setwd(home_dir)
      f <- paste("./data/",Img_name,"/b_nr",sep = "")
      save(bnr2,file=f)
      file1 <- paste('./data/',Img_name,'/images/b',bnr2,'_new8.tif',sep = "")
      tiff(file1, width=578, height=578, units="px", bg = "white")
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",asp=1,xlim=c(xc-r_max2,xc+r_max2),ylim=c(yc+r_max2, yc-r_max2), xlab = NULL, ylab=NULL, ann= FALSE, main=paste("b", bnr2), axes=TRUE)
      points(xc, yc, pch = 16, cex=1.5, col = "black", asp=1) #centre of PC
      points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      dev.off()
      cat("end of spObj_extract_single_building.R -first part",sep = "")
    } #end b81 (first part)
    
    #second part 
    if (bnr2 == 82) { 
      cat("start of spObj_extract_single_building.R - second part",sep = "", "\n")
      part <-  "2parts_2"
      bnr2_part <- bnr2 
      is_label_2 <- is_bnr2_label@.Data == 2 #right part
      display(is_label_2)
      is_label_2
      coords <- data.frame(x=as.numeric(row(is_label_2)),y=as.numeric(col(is_label_2)), is_label_2=as.numeric(is_label_2))
      coords <- coords[coords$is_label_2 == 1,] #removal of pixels which do not have the label of the building
      xc <- coor_part[2,1] #generate xc,yc,r_max,alpha and store with bnr2=342
      yc <- coor_part[2,2]
      alpha <- coor_part[2,5]*omega
      r_max <- shap_part[2,6]
      dy_window_plot
      
      #output plot parameter
      plotPar <- c(xc,yc,r_max,alpha,dy_window_plot)
      setwd(home_dir)
      f1 <- paste("./data/",Img_name,"/param_b",bnr2,sep="") 
      save(plotPar, file=f1) #parameter xc, yc, r_max, alpha
      f <- paste("./data/",Img_name,"/b_nr",sep = "")
      save(bnr2,file=f)
      
      #plot of PC and checkpoints (large scale)
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",
           asp=1,xlim=c(xc-r_max2,xc+r_max2),ylim=c(yc+r_max2, 
                                                    yc-r_max2), xlab = NULL, ylab=NULL, ann=T, 
           main=paste("b", bnr2,sep = ""), axes=TRUE)
      points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
      
      #generation of image
      file1 <- paste('./data/',Img_name,'/images/b',bnr2,'_new8.tif',sep = "")
      tiff(file1, width=578, height=578, units="px", bg = "white")
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",asp=1,xlim=c(xc-r_max2,xc+r_max2),
           ylim=c(yc+r_max2, yc-r_max2), xlab = NULL, ylab=NULL, ann=T, main=paste("b", bnr2,
                                                                                   sep = ""), axes=TRUE)
      points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
      dev.off()
      #windows() #if necessary
      cat("end of spObj_extract_single_building.R - second part", sep = "","\n")
    } #end of b82
    n_bnr2 <- n_bnr2 + 1
  } #end b8
  
  ####
  
  #b10
  
  if (bnr2 == 10 && p_pos == "cor_sep") {
    cat("start of spObj_extract_single_building.R -first part",sep = "")
    n_bnr2 = 0
    display(is_bnr)
    is_bnr2 <- is_bnr
    x_sep=206;y_sep=497 #first cut
    x_sep2=142;y_sep2=327 #second cut
    x_sep3=80;y_sep3=431 #third cut
    imageData(is_bnr2)[(x_sep-2):(x_sep+2), (y_sep-2):(y_sep+2)] = FALSE #first cut
    imageData(is_bnr2)[(x_sep2-2):(x_sep2+2), (y_sep2-2):(y_sep2+2)] = FALSE #second cut
    imageData(is_bnr2)[(x_sep3-2):(x_sep3+2), (y_sep3-2):(y_sep3+2)] = FALSE #third cut
    display(is_bnr2)
    is_bnr2_label <- bwlabel(is_bnr2)
    cat("Number of objects= ",max(is_bnr2_label),"\n")
    coor_part<-computeFeatures.moment(is_bnr2_label) #geometric features (moment)
    shap_part<-computeFeatures.shape(is_bnr2_label) #geometric features (shape)
    #
    #first part
    is_label_1 <- is_bnr2_label@.Data == 1 
    imageData(is_label_1)[x_sep, y_sep]
    display(is_label_1)
    #
    #second part
    is_label_2 <- is_bnr2_label@.Data == 2 
    display(is_label_2)
    
    #third part
    is_label_3 <- is_bnr2_label@.Data == 3 
    display(is_label_3)
    
    #save original values
    xc_orig <- xc
    yc_orig <- yc
    #
    
    if (proc_mode == "obj_wise" && part == "2parts_1") {
      bnr2 <- (bnr2)*10+1  #change of number 
      bnr2_part <- bnr2 
    }
    
    if (proc_mode == "obj_wise" && part == "2parts_2") {
      bnr2 <- (bnr2)*10+2 
      bnr2
    }
    
    if (proc_mode == "obj_wise" && part == "3parts_3") {
      bnr2 <- (bnr2)*10+3 
      bnr2
    }
    
    #first part
    if (bnr2 == 101) { 
      is_label_1
      display(is_label_1)
      setwd(home_dir)
      f1 <- paste("./data/",Img_name,"/param_b",bnr2,sep="") 
      save(plotPar, file=f1) #parameter xc, yc, r_max, alpha
      coords <- data.frame(x=as.numeric(row(is_label_1)),y=as.numeric(col(is_label_1)), is_label_1=as.numeric(is_label_1))
      coords <- coords[coords$is_label_1 == 1,] #removal of pixels which do not have the label of the building
      
      #calculation of new centre of object from connected components
      xc <- coor_part[1,1]
      yc <- coor_part[1,2]
      alpha <- coor_part[1,5]*omega
      r_max <- shap_part[1,6]
      
      #storage of plot parameter of separated object
      setwd(home_dir)
      plotPar <- c(xc,yc,r_max,alpha,dy_window_plot)
      f1 <- paste("./data/",Img_name,"/param_b",bnr2,sep="") 
      save(plotPar,file=f1) #parameter xc, yc, r_max, alpha
      
      #plot of PC and checkpoints (large scale)
      dev.set(2)
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",asp=1,xlim=c(xc-r_max2,xc+r_max2),
           ylim=c(yc+r_max2, yc-r_max2), xlab = NULL, ylab=NULL, ann=T, main=paste("b", bnr2,
                                                                                   " - left",sep = ""), axes=TRUE)
      points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
      #
      
      #generation of image
      setwd(home_dir)
      f <- paste("./data/",Img_name,"/b_nr",sep = "")
      save(bnr2,file=f)
      file1 <- paste('./data/',Img_name,'/images/b',bnr2,'_new8.tif',sep = "")
      tiff(file1, width=578, height=578, units="px", bg = "white")
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",asp=1,xlim=c(xc-r_max2,xc+r_max2),ylim=c(yc+r_max2, yc-r_max2), xlab = NULL, ylab=NULL, ann= FALSE, main=paste("b", bnr2), axes=TRUE)
      points(xc, yc, pch = 16, cex=1.5, col = "black", asp=1) #centre of PC
      points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      dev.off()
      cat("end of spObj_extract_single_building.R -first part",sep = "")
    } #end b101 (first part)
    
    #second part 
    if (bnr2 == 102) { 
      cat("start of spObj_extract_single_building.R - second part",sep = "", "\n")
      part <-  "2parts_2"
      bnr2_part <- bnr2 
      is_label_2 <- is_bnr2_label@.Data == 2 #right part
      display(is_label_2)
      is_label_2
      coords <- data.frame(x=as.numeric(row(is_label_2)),y=as.numeric(col(is_label_2)), is_bnr=as.numeric(is_label_2))
      coords <- coords[coords$is_bnr == 2,] #removal of pixels which do not have the label of the building part
      xc <- coor_part[2,1] #generate xc,yc,r_max,alpha and store with bnr2=102
      yc <- coor_part[2,2]
      alpha <- coor_part[2,5]*omega
      r_max <- shap_part[2,6]
      dy_window_plot
      
      #output plot parameter
      plotPar <- c(xc,yc,r_max,alpha,dy_window_plot)
      setwd(home_dir)
      f1 <- paste("./data/",Img_name,"/param_b",bnr2,sep="") 
      save(plotPar, file=f1) #parameter xc, yc, r_max, alpha
      f <- paste("./data/",Img_name,"/b_nr",sep = "")
      save(bnr2,file=f)
      
      #plot of PC and checkpoints (large scale)
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",
           asp=1,xlim=c(xc-r_max2,xc+r_max2),ylim=c(yc+r_max2, 
                                                    yc-r_max2), xlab = NULL, ylab=NULL, ann=T, 
           main=paste("b", bnr2,sep = ""), axes=TRUE)
      points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
      
      #generation of image
      file1 <- paste('./data/',Img_name,'/images/b',bnr2,'_new8.tif',sep = "")
      tiff(file1, width=578, height=578, units="px", bg = "white")
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",asp=1,xlim=c(xc-r_max2,xc+r_max2),
           ylim=c(yc+r_max2, yc-r_max2), xlab = NULL, ylab=NULL, ann=T, main=paste("b", bnr2,
                                                                                   sep = ""), axes=TRUE)
      points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
      dev.off()
      #windows() #if necessary
      cat("end of spObj_extract_single_building.R - second part", sep = "","\n")
    } #end of b102
    
    ##third part 
    if (bnr2 == 103) { 
      #browser()
      cat("start of spObj_extract_single_building.R - third part",sep = "", "\n")
      part <-  "3parts_3"
      bnr2_part <- bnr2 
      is_label_3 <- is_bnr2_label@.Data == 3 #third part
      display(is_label_3)
      #display(is_label_3,"raster")
      is_label_3
      coords <- data.frame(x=as.numeric(row(is_label_3)),y=as.numeric(col(is_label_3)), is_bnr=as.numeric(is_label_3))
      coords <- coords[coords$is_bnr == 1,] #removal of pixels which do not have the label of the building part
      xc <- coor_part[3,1] #generate xc,yc,r_max,alpha and store with bnr2=103
      yc <- coor_part[3,2]
      alpha <- coor_part[3,5]*omega
      r_max <- shap_part[3,6]
      dy_window_plot
      
      #output plot parameter
      plotPar <- c(xc,yc,r_max,alpha,dy_window_plot)
      setwd(home_dir)
      f1 <- paste("./data/",Img_name,"/param_b",bnr2,sep="") 
      save(plotPar, file=f1) #parameter xc, yc, r_max, alpha
      f <- paste("./data/",Img_name,"/b_nr",sep = "")
      save(bnr2,file=f)
      
      #plot of PC and checkpoints (large scale)
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",
           asp=1,xlim=c(xc-r_max2,xc+r_max2),ylim=c(yc+r_max2, 
                                                    yc-r_max2), xlab = NULL, ylab=NULL, ann=T, 
           main=paste("b", bnr2,sep = ""), axes=TRUE)
      points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
      
      #generation of image
      file1 <- paste('./data/',Img_name,'/images/b',bnr2,'_new8.tif',sep = "")
      tiff(file1, width=578, height=578, units="px", bg = "white")
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",asp=1,xlim=c(xc-r_max2,xc+r_max2),
           ylim=c(yc+r_max2, yc-r_max2), xlab = NULL, ylab=NULL, ann=T, main=paste("b", bnr2,
                                                                                   sep = ""), axes=TRUE)
      points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
      dev.off()
      #windows() #if necessary
      cat("end of spObj_extract_single_building.R - second part", sep = "","\n")
    } #end of b103
    
    n_bnr2 <- n_bnr2 + 1
  } #end b102
  
  ####
  
  #b15
  
  if (bnr2 == 15 && p_pos == "cor_sep") {
    cat("start of spObj_extract_single_building.R",sep = "")
    n_bnr2 = 0
    display(is_bnr)
    is_bnr2 <- is_bnr
    x_sep=361;y_sep=531 #first cut
    x_sep2=365;y_sep2=539 #second cut
    #x_sep3=80;y_sep3=431 #third cut
    imageData(is_bnr2)[(x_sep-2):(x_sep+2), (y_sep-2):(y_sep+2)] = FALSE #first cut
    imageData(is_bnr2)[(x_sep2-2):(x_sep2+2), (y_sep2-2):(y_sep2+2)] = FALSE #second cut
    display(is_bnr2)
    is_bnr2_label <- bwlabel(is_bnr2)
    cat("Number of objects= ",max(is_bnr2_label),"\n")
    coor_part<-computeFeatures.moment(is_bnr2_label) #geometric features (moment)
    shap_part<-computeFeatures.shape(is_bnr2_label) #geometric features (shape)
    #
    #first part
    is_label_1 <- is_bnr2_label@.Data == 1 
    display(is_label_1)
    #
    #second part
    is_label_2 <- is_bnr2_label@.Data == 2 
    display(is_label_2)
    
    #save original values
    xc_orig <- xc
    yc_orig <- yc
    #
    
    if (proc_mode == "obj_wise" && part == "2parts_1") {
      bnr2 <- (bnr2)*10+1  #change of number 
      bnr2_part <- bnr2 
    }
    
    if (proc_mode == "obj_wise" && part == "2parts_2") {
      bnr2 <- (bnr2)*10+2 
      bnr2
    }
    
    #first part
    if (bnr2 == 151) { 
      is_label_1
      display(is_label_1)
      setwd(home_dir)
      f1 <- paste("./data/",Img_name,"/param_b",bnr2,sep="") 
      save(plotPar, file=f1) #parameter xc, yc, r_max, alpha
      coords <- data.frame(x=as.numeric(row(is_label_1)),y=as.numeric(col(is_label_1)), is_bnr=as.numeric(is_label_1))
      coords <- coords[coords$is_label_1 == 1,] #removal of pixels which do not have the label of the building part
      
      #calculation of new centre of object from connected components
      xc <- coor_part[1,1]
      yc <- coor_part[1,2]
      alpha <- coor_part[1,5]*omega
      r_max <- shap_part[1,6]
      
      #storage of plot parameter of separated object
      setwd(home_dir)
      plotPar <- c(xc,yc,r_max,alpha,dy_window_plot)
      f1 <- paste("./data/",Img_name,"/param_b",bnr2,sep="") 
      save(plotPar,file=f1) #parameter xc, yc, r_max, alpha
      
      #plot of PC and checkpoints (large scale)
      dev.set(2)
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",asp=1,xlim=c(xc-r_max2,xc+r_max2),
           ylim=c(yc+r_max2, yc-r_max2), xlab = NULL, ylab=NULL, ann=T, main=paste("b", bnr2,
                                                                                   " - left",sep = ""), axes=TRUE)
      points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
      #
      
      #generation of image
      setwd(home_dir)
      f <- paste("./data/",Img_name,"/b_nr",sep = "")
      save(bnr2,file=f)
      file1 <- paste('./data/',Img_name,'/images/b',bnr2,'_new8.tif',sep = "")
      tiff(file1, width=578, height=578, units="px", bg = "white")
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",asp=1,xlim=c(xc-r_max2,xc+r_max2),ylim=c(yc+r_max2, yc-r_max2), xlab = NULL, ylab=NULL, ann= FALSE, main=paste("b", bnr2), axes=TRUE)
      points(xc, yc, pch = 16, cex=1.5, col = "black", asp=1) #centre of PC
      points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      dev.off()
      cat("end of spObj_extract_single_building.R -first part",sep = "")
    } #end b101 (first part)
    
    #second part 
    if (bnr2 == 152) { 
      cat("start of spObj_extract_single_building.R - second part",sep = "", "\n")
      part <-  "2parts_2"
      bnr2_part <- bnr2 
      is_label_2 <- is_bnr2_label@.Data == 2 #second part
      display(is_label_2)
      is_label_2
      coords <- data.frame(x=as.numeric(row(is_label_2)),y=as.numeric(col(is_label_2)), is_bnr=as.numeric(is_label_2))
      max(coords$is_bnr) #value is to be used for coords$is_bnr 
      coords <- coords[coords$is_bnr == 1,] #removal of pixels which do not have the label of the building part
      xc <- coor_part[2,1] #generate xc,yc,r_max,alpha and store with bnr2=102
      yc <- coor_part[2,2]
      alpha <- coor_part[2,5]*omega
      r_max <- shap_part[2,6]
      dy_window_plot
      
      #output plot parameter
      plotPar <- c(xc,yc,r_max,alpha,dy_window_plot)
      setwd(home_dir)
      f1 <- paste("./data/",Img_name,"/param_b",bnr2,sep="") 
      save(plotPar, file=f1) #parameter xc, yc, r_max, alpha
      f <- paste("./data/",Img_name,"/b_nr",sep = "")
      save(bnr2,file=f)
      
      #plot of PC and checkpoints (large scale)
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",
           asp=1,xlim=c(xc-r_max2,xc+r_max2),ylim=c(yc+r_max2, 
                                                    yc-r_max2), xlab = NULL, ylab=NULL, ann=T, 
           main=paste("b", bnr2,sep = ""), axes=TRUE)
      points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
      
      #generation of image
      file1 <- paste('./data/',Img_name,'/images/b',bnr2,'_new8.tif',sep = "")
      tiff(file1, width=578, height=578, units="px", bg = "white")
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",asp=1,xlim=c(xc-r_max2,xc+r_max2),
           ylim=c(yc+r_max2, yc-r_max2), xlab = NULL, ylab=NULL, ann=T, main=paste("b", bnr2,
                                                                                   sep = ""), axes=TRUE)
      points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
      dev.off()
      #windows() #if necessary
      cat("end of spObj_extract_single_building.R - second part", sep = "","\n")
    } #end of b152
    n_bnr2 <- n_bnr2 + 1
  } #end b15
  
  ####
  
  #b16
  
  if (bnr2 == 16 && p_pos == "cor_sep") {
    cat("start of spObj_extract_single_building.R",sep = "", "\n")
    #stop("install cuts in object and store coordinates")
    n_bnr2 = 0
    display(is_bnr)
    is_bnr2 <- is_bnr
    x_sep=179;y_sep=567 #first cut
    x_sep2=255;y_sep2=752 #second cut
    x_sep3=226;y_sep3=752 #third cut
    imageData(is_bnr2)[(x_sep-2):(x_sep+2), (y_sep-2):(y_sep+2)] = FALSE #first cut
    imageData(is_bnr2)[(x_sep2-2):(x_sep2+2), (y_sep2-2):(y_sep2+2)] = FALSE #second cut
    imageData(is_bnr2)[(x_sep3-2):(x_sep3+2), (y_sep3-2):(y_sep3+2)] = FALSE #second cut
    display(is_bnr2)
    is_bnr2_label <- bwlabel(is_bnr2)
    cat("Number of objects= ",max(is_bnr2_label),"\n")
    coor_part<-computeFeatures.moment(is_bnr2_label) #geometric features (moment)
    shap_part<-computeFeatures.shape(is_bnr2_label) #geometric features (shape)
    #
    #first part
    is_label_1 <- is_bnr2_label@.Data == 1 
    display(is_label_1)
    #
    #second part
    x_sep1 <- 180
    y_sep1 <- 569 
    imageData(is_bnr2)[(x_sep1-2):(x_sep1+2), (y_sep1-2):(y_sep1+2)] = TRUE #close of cut 1, new
    x_sep2 <- 224
    y_sep2 <- 750
    imageData(is_bnr2)[(x_sep2-2):(x_sep2+2), (y_sep2-2):(y_sep2+2)] = TRUE #close of cut 2, new
    x_sep3 <- 228
    y_sep3 <- 742
    imageData(is_bnr2)[(x_sep3-2):(x_sep3+2), (y_sep3-2):(y_sep3+2)] = FALSE #cut 3, new
    x_close <- 225
    y_close <- 746
    imageData(is_bnr2)[(x_close-2):(x_close+2), (y_close-2):(y_close+2)] = TRUE #close of cut, new
    is_bnr2_label <- bwlabel(is_bnr2) #new
    cat("Number of objects= ",max(is_bnr2_label),"\n")
    is_label_2 <- is_bnr2_label@.Data == 1 
    is_label_2 <- is_bnr2_label@.Data == 2 
    display(is_label_2)
    
    #third part
    is_label_3 <- is_bnr2_label@.Data == 3 
    display(is_label_3)
    
    #save original values
    xc_orig <- xc
    yc_orig <- yc
    
    if (k_part == "1") {
      #set up for first part
      part = "2parts_1"
      bnr2 <- bnr2_orig
    
      if (proc_mode == "obj_wise" && part == "2parts_1") {
        bnr2 <- bnr2*10+1   
        bnr2_part <- bnr2 
        bnr2
      }
    
      #first part
      if (bnr2 == 161) { 
        is_label_1
        display(is_label_1)
        setwd(home_dir)
        f1 <- paste("./data/",Img_name,"/param_b",bnr2,sep="") 
        save(plotPar, file=f1) #parameter xc, yc, r_max, alpha
        coords <- data.frame(x=as.numeric(row(is_label_1)),y=as.numeric(col(is_label_1)), is_bnr=as.numeric(is_label_1))
        max(coords$is_bnr)
        coords <- coords[coords$is_bnr == 1,] #removal of pixels which do not have the label of the building part
        coords[1,]
        
        #calculation of new centre of object from connected components
        xc <- coor_part[1,1]
        yc <- coor_part[1,2]
        alpha <- coor_part[1,5]*omega
        r_max <- shap_part[1,6]
        
        #storage of plot parameter of separated object
        setwd(home_dir)
        plotPar <- c(xc,yc,r_max,alpha,dy_window_plot)
        f1 <- paste("./data/",Img_name,"/param_b",bnr2,sep="") 
        save(plotPar,file=f1) #parameter xc, yc, r_max, alpha
        
        #plot of PC and checkpoints (large scale)
        dev.set(2)
        r_max2 <- round(1.1*r_max)
        plot(coords$x, coords$y, pch=16, cex=0.2,col="black",asp=1,xlim=c(xc-r_max2,xc+r_max2),
             ylim=c(yc+r_max2, yc-r_max2), xlab = NULL, ylab=NULL, ann=T, main=paste("b", bnr2,
             " - left",sep = ""), axes=TRUE)
        points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
        points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
        points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
        points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
        points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
        #
        
        #generation of image
        setwd(home_dir)
        f <- paste("./data/",Img_name,"/b_nr",sep = "")
        save(bnr2,file=f)
        file1 <- paste('./data/',Img_name,'/images/b',bnr2,'_new8.tif',sep = "")
        tiff(file1, width=578, height=578, units="px", bg = "white")
        r_max2 <- round(1.1*r_max)
        plot(coords$x, coords$y, pch=16, cex=0.2,col="black",asp=1,xlim=c(xc-r_max2,xc+r_max2),ylim=c(yc+r_max2, yc-r_max2), xlab = NULL, ylab=NULL, ann= FALSE, main=paste("b", bnr2), axes=TRUE)
        points(xc, yc, pch = 16, cex=1.5, col = "black", asp=1) #centre of PC
        points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
        points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
        points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
        points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
        dev.off()
        cat("end of spObj_extract_single_building.R -first part",sep = "")
      } #end b161 (first part)
    } #end k_part=1
    
    ##second part
    #set up for second part
    
    if (k_part == "2") {
      
      part = "2parts_2"
      bnr2 <- bnr2_orig
      
      if (proc_mode == "obj_wise" && part == "2parts_2") {
        bnr2 <- (bnr2)*10+2 
        bnr2
      }
  
      if (bnr2 == 162) { 
        cat("start of spObj_extract_single_building.R - second part",sep = "", "\n")
        part <-  "2parts_2"
        bnr2_part <- bnr2 
        is_label_2 <- is_bnr2_label@.Data == 2 #second part
        display(is_label_2)
        is_label_2
        coords <- data.frame(x=as.numeric(row(is_label_2)),y=as.numeric(col(is_label_2)), is_bnr=as.numeric(is_label_2))
        max(coords$is_bnr) #value is to be used for coords$is_bnr 
        coords <- coords[coords$is_bnr == 1,] #removal of pixels which do not have the label of the building part
        xc <- coor_part[2,1] #generate xc,yc,r_max,alpha and store with bnr2=102
        yc <- coor_part[2,2]
        alpha <- coor_part[2,5]*omega
        r_max <- shap_part[2,6]
        dy_window_plot
        
        #output plot parameter
        plotPar <- c(xc,yc,r_max,alpha,dy_window_plot)
        setwd(home_dir)
        f1 <- paste("./data/",Img_name,"/param_b",bnr2,sep="") 
        save(plotPar, file=f1) #parameter xc, yc, r_max, alpha
        f <- paste("./data/",Img_name,"/b_nr",sep = "")
        save(bnr2,file=f)
        
        #plot of PC and checkpoints (large scale)
        r_max2 <- round(1.1*r_max)
        plot(coords$x, coords$y, pch=16, cex=0.2,col="black",
             asp=1,xlim=c(xc-r_max2,xc+r_max2),ylim=c(yc+r_max2, 
             yc-r_max2), xlab = NULL, ylab=NULL, ann=T, 
             main=paste("b", bnr2,sep = ""), axes=TRUE)
        points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
        points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
        points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
        points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
        points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
        
        #generation of image
        file1 <- paste('./data/',Img_name,'/images/b',bnr2,'_new8.tif',sep = "")
        tiff(file1, width=578, height=578, units="px", bg = "white")
        r_max2 <- round(1.1*r_max)
        plot(coords$x, coords$y, pch=16, cex=0.2,col="black",asp=1,xlim=c(xc-r_max2,xc+r_max2),
             ylim=c(yc+r_max2, yc-r_max2), xlab = NULL, ylab=NULL, ann=T, main=paste("b", bnr2,
             sep = ""), axes=TRUE)
        points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
        points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
        points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
        points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
        points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
        dev.off()
        #windows() #if necessary
      } #end of b162
      cat("end of spObj_extract_single_building.R - second part", sep = "","\n")
    } #end of k_part=2
    
    ##third part
    
    if (k_part == "3") {
    
      #set up for third part #new
      part = "3parts_3"
      bnr2 <- bnr2_orig
  
      if (proc_mode == "obj_wise" && part == "3parts_3") {
        bnr2 <- (bnr2)*10+3
        bnr2
      }
      
      if (bnr2 == 163) { 
        is_label_3
        display(is_label_3)
        setwd(home_dir)
        f1 <- paste("./data/",Img_name,"/param_b",bnr2,sep="") 
        save(plotPar, file=f1) #parameter xc, yc, r_max, alpha
        coords <- data.frame(x=as.numeric(row(is_label_3)),y=as.numeric(col(is_label_3)), is_bnr=as.numeric(is_label_3))
        coords <- coords[coords$is_bnr == 1,] #removal of pixels which do not have the label of the building part
        
        #calculation of new centre of object from connected components
        
        #new
        display(is_label_3)
        cat("Number of objects= ",max(is_label_3),"\n")
        coor_part<-computeFeatures.moment(is_label_3) #geometric features (moment)
        shap_part<-computeFeatures.shape(is_label_3) #geometric features (shape)
        #coor_part<-computeFeatures.moment(is_bnr2_label) #geometric features (moment)
        #shap_part<-computeFeatures.shape(is_bnr2_label) #geometric features (shape)
        coor_part
        shap_part
        xc <- coor_part[1,1]
        yc <- coor_part[1,2]
        alpha <- coor_part[1,5]*omega
        r_max <- shap_part[1,6]
        #end new
        
        #
        # xc <- coor_part[3,1]
        # yc <- coor_part[3,2]
        # alpha <- coor_part[3,5]*omega
        # r_max <- shap_part[3,6]
        
        #storage of plot parameter of separated object
        setwd(home_dir)
        plotPar <- c(xc,yc,r_max,alpha,dy_window_plot)
        f1 <- paste("./data/",Img_name,"/param_b",bnr2,sep="") 
        save(plotPar,file=f1) #parameter xc, yc, r_max, alpha
        
        #plot of PC and checkpoints (large scale)
        dev.set(2)
        r_max2 <- round(1.1*r_max)
        plot(coords$x, coords$y, pch=16, cex=0.2,col="black",asp=1,xlim=c(xc-r_max2,xc+r_max2),
             ylim=c(yc+r_max2, yc-r_max2), xlab = NULL, ylab=NULL, ann=T, main=paste("b", bnr2,
             " - left",sep = ""), axes=TRUE)
        points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
        points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
        points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
        points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
        points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
        #
        
        #generation of image
        setwd(home_dir)
        f <- paste("./data/",Img_name,"/b_nr",sep = "")
        save(bnr2,file=f)
        file1 <- paste('./data/',Img_name,'/images/b',bnr2,'_new8.tif',sep = "")
        tiff(file1, width=578, height=578, units="px", bg = "white")
        r_max2 <- round(1.1*r_max)
        plot(coords$x, coords$y, pch=16, cex=0.2,col="black",asp=1,xlim=c(xc-r_max2,xc+r_max2),
             ylim=c(yc+r_max2, yc-r_max2), xlab = NULL, ylab=NULL, ann= FALSE, main=paste("b", bnr2), 
             axes=TRUE)
        points(xc, yc, pch = 16, cex=1.5, col = "black", asp=1) #centre of PC
        points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
        points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
        points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
        points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
        dev.off()
        cat("end of spObj_extract_single_building.R - third part",sep = "")
      } #end b163 (third part)
    } #end of k_part=3
    n_bnr2 <- n_bnr2 + 1
  } #end b16
  
  ####
  
} #end ISPRS4 

if (Img_name == "ISPRS4_DLR10") {
  
  ##b3
  if (bnr2 == 3 && p_pos == "cor_sep") {
    cat("start of spObj_extract_single_building.R -first part",sep = "")
    n_bnr2 = 0
    display(is_bnr)
    is_bnr2 <- is_bnr
    x_sep=366;y_sep=84 #manual input
    gap_size=0
    imageData(is_bnr2)[(x_sep-gap_size):(x_sep+gap_size), (y_sep-gap_size):(y_sep+gap_size)] = FALSE #first cut
    #imageData(is_bnr2)[282:286, 96:100] = FALSE #second cut
    display(is_bnr2)
    is_bnr2_label <- bwlabel(is_bnr2)
    cat("Number of objects= ",max(is_bnr2_label),"\n")
    coor_part<-computeFeatures.moment(is_bnr2_label) #geometric features (moment)
    shap_part<-computeFeatures.shape(is_bnr2_label) #geometric features (shape)
    #
    #first part
    is_label_1 <- is_bnr2_label@.Data == 1 
    imageData(is_label_1)[x_sep, y_sep]
    display(is_label_1)
    #
    #second part
    is_label_2 <- is_bnr2_label@.Data == 2 
    display(is_label_2)
    
    #save original values
    xc_orig <- xc
    yc_orig <- yc
    #
    
    if (proc_mode == "obj_wise" && part == "2parts_1") {
      bnr2 <- (bnr2)*10+1  #change of number 
      bnr2_part <- bnr2 
    }
    
    if (proc_mode == "obj_wise" && part == "2parts_2") {
      bnr2 <- (bnr2)*10+2 #change of number
      bnr2
    }
    
    #first part
    if (bnr2 == 31) { 
      is_label_1
      display(is_label_1)
      setwd(home_dir)
      f1 <- paste("./data/",Img_name,"/param_b",bnr2,sep="") 
      save(plotPar, file=f1) #parameter xc, yc, r_max, alpha
      coords <- data.frame(x=as.numeric(row(is_label_1)),y=as.numeric(col(is_label_1)), is_bnr=as.numeric(is_label_1))
      coords <- coords[coords$is_bnr == 1,] #removal of pixels which do not have the label of the building part
      
      #calculation of new centre of object from connected components
      xc <- coor_part[1,1]
      yc <- coor_part[1,2]
      alpha <- coor_part[1,5]*omega
      r_max <- shap_part[1,6]
      
      #storage of plot parameter of separated object
      setwd(home_dir)
      plotPar <- c(xc,yc,r_max,alpha,dy_window_plot)
      f1 <- paste("./data/",Img_name,"/param_b",bnr2,sep="") 
      save(plotPar,file=f1) #parameter xc, yc, r_max, alpha
      
      #plot of PC and checkpoints (large scale)
      dev.set(2)
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",asp=1,xlim=c(xc-r_max2,xc+r_max2),
           ylim=c(yc+r_max2, yc-r_max2), xlab = NULL, ylab=NULL, ann=T, main=paste("b", bnr2,
                " - left",sep = ""), axes=TRUE)
      points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
      #
      
      #generation of image
      setwd(home_dir)
      f <- paste("./data/",Img_name,"/b_nr",sep = "")
      save(bnr2,file=f)
      file1 <- paste('./data/',Img_name,'/images/b',bnr2,'_new8.tif',sep = "")
      tiff(file1, width=578, height=578, units="px", bg = "white")
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",asp=1,xlim=c(xc-r_max2,xc+r_max2),ylim=c(yc+r_max2, yc-r_max2), xlab = NULL, ylab=NULL, ann= FALSE, main=paste("b", bnr2), axes=TRUE)
      points(xc, yc, pch = 16, cex=1.5, col = "black", asp=1) #centre of PC
      points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      dev.off()
      cat("end of spObj_extract_single_building.R -first part",sep = "")
    } #end b31(first part)
    
    #second part 
    if (bnr2 == 32) { 
      #stop("proceed step by step")
      cat("start of spObj_extract_single_building.R - second part",sep = "", "\n")
      part <-  "2parts_2"
      bnr2_part <- bnr2 
      is_label_2 <- is_bnr2_label@.Data == 2 #right part
      display(is_label_2)
      is_label_2
      coords <- data.frame(x=as.numeric(row(is_label_2)),y=as.numeric(col(is_label_2)), is_bnr=as.numeric(is_label_2))
      coords <- coords[coords$is_bnr == 2,] #removal of pixels which do not have the label of the building
      xc <- coor_part[2,1] #generate xc,yc,r_max,alpha and store with bnr2=32
      yc <- coor_part[2,2]
      alpha <- coor_part[2,5]*omega
      r_max <- shap_part[2,6]
      dy_window_plot
      
      #output plot parameter
      plotPar <- c(xc,yc,r_max,alpha,dy_window_plot)
      setwd(home_dir)
      f1 <- paste("./data/",Img_name,"/param_b",bnr2,sep="") 
      save(plotPar, file=f1) #parameter xc, yc, r_max, alpha
      f <- paste("./data/",Img_name,"/b_nr",sep = "")
      save(bnr2,file=f)
      
      #plot of PC and checkpoints (large scale)
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",
           asp=1,xlim=c(xc-r_max2,xc+r_max2),ylim=c(yc+r_max2, 
                                                    yc-r_max2), xlab = NULL, ylab=NULL, ann=T, 
           main=paste("b", bnr2,sep = ""), axes=TRUE)
      points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
      
      #generation of image
      file1 <- paste('./data/',Img_name,'/images/b',bnr2,'_new8.tif',sep = "")
      tiff(file1, width=578, height=578, units="px", bg = "white")
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",asp=1,xlim=c(xc-r_max2,xc+r_max2),
           ylim=c(yc+r_max2, yc-r_max2), xlab = NULL, ylab=NULL, ann=T, main=paste("b", bnr2,
                                                                                   sep = ""), axes=TRUE)
      # points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      # points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      # points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      # points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      # points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
      dev.off()
      #windows() #if necessary
      cat("end of spObj_extract_single_building.R - second part", sep = "","\n")
    } #end of b32
    n_bnr2 <- n_bnr2 + 1
  } #end b3
  
  ##22
  
  if (bnr2 == 22 && p_pos == "cor_sep") {
    #stop("check partition of object")
    cat("start of spObj_extract_single_building.R -first part",sep = "")
    n_bnr2 = 0
    display(is_bnr) 
    is_bnr2 <- is_bnr
    str(is_bnr2)
    x_sep=182;y_sep=560 #manual input
    gap_size=2
    imageData(is_bnr2)[(x_sep-gap_size):(x_sep+gap_size), 
                       (y_sep-gap_size):(y_sep+gap_size)] = FALSE #first cut
    x2_sep=167;y2_sep=580
    imageData(is_bnr2)[(x2_sep-gap_size):(x2_sep+gap_size), 
                       (y2_sep-gap_size):(y2_sep+gap_size)] = FALSE #second cut
    is_bnr2_label <- bwlabel(is_bnr2)
    cat("Number of objects= ",max(is_bnr2_label),"\n")
    coor_part<-computeFeatures.moment(is_bnr2_label) #geometric features (moment)
    shap_part<-computeFeatures.shape(is_bnr2_label) #geometric features (shape)
    #
    #first part
    is_label_1 <- is_bnr2_label@.Data == 1 
    display(is_label_1)
    #
    #second part
    is_label_2 <- is_bnr2_label@.Data == 2 
    display(is_label_2)
    
    #save original values
    xc_orig <- xc
    yc_orig <- yc
    #
    
    if (proc_mode == "obj_wise" && part == "2parts_1") {
      bnr2 <- (bnr2)*10+1  #change of number 
      bnr2_part <- bnr2 
    }
    
    if (proc_mode == "obj_wise" && part == "2parts_2") {
      bnr2 <- (bnr2)*10+2 #change of number
      bnr2
    }
    
    #first part
    if (bnr2 == 221) { 
      
      #close gap 
      display(is_label_1)
      is_bnr2 <- is_label_1
      display(is_bnr2,"raster")
      #
      x1 <- x_sep-gap_size
      y1 <- y_sep-gap_size
      x2 <- x2_sep-gap_size
      y2 <- y2_sep+gap_size
      
      # Number of points you want along the line
      n_points <- 100
      
      # Generate equally spaced x values between x1 and x2
      x_vals <- seq(x1, x2, length.out = n_points)
      
      # Compute corresponding y values using linear interpolation
      y_vals <- y1 + (y2 - y1) * (x_vals - x1) / (x2 - x1)
      
      # Combine into a data frame
      line_points <- data.frame(x = x_vals, y = y_vals)
      
      # View the result
      head(line_points)
      close_size=2
      line_points_pixel <- round(line_points)
      line_points_pixel 
      points(line_points_pixel$x,line_points_pixel$y,col="red", pch=".", cex=1.5, asp=1)
      #fill gap by a line
      vec = 1 : 100
      
      for(i in vec) {
      imageData(is_bnr2)[line_points_pixel$x[i]-1,line_points_pixel$y[i]] = 1  
      imageData(is_bnr2)[line_points_pixel$x[i],line_points_pixel$y[i]] = 1
      imageData(is_bnr2)[line_points_pixel$x[i]+1,line_points_pixel$y[i]] = 1
      }
      
      display(is_bnr2)
      #
      
      # imageData(is_bnr2)[(x_sep+close_size):(x2_sep-close_size),
      #                    (y_sep-close_size):(y2_sep+close_size)] = T #this fills gap
      is_bnr2_label <- bwlabel(is_bnr2)
      is_label_1 <- is_bnr2_label@.Data
      #display(is_label_1)
      display(is_label_1,"raster")
      
      setwd(home_dir)
      f1 <- paste("./data/",Img_name,"/param_b",bnr2,sep="") 
      save(plotPar, file=f1) #parameter xc, yc, r_max, alpha
      coords <- data.frame(x=as.numeric(row(is_label_1)),
                y=as.numeric(col(is_label_1)), is_bnr=as.numeric(is_label_1))
      coords <- coords[coords$is_bnr == 1,] #removal of pixels which do not have the label of the building part
      
      #calculation of new centre of object from connected components
      xc <- coor_part[1,1]
      yc <- coor_part[1,2]
      alpha <- coor_part[1,5]*omega
      r_max <- shap_part[1,6]
      
      #storage of plot parameter of separated object
      setwd(home_dir)
      plotPar <- c(xc,yc,r_max,alpha,dy_window_plot)
      f1 <- paste("./data/",Img_name,"/param_b",bnr2,sep="") 
      save(plotPar,file=f1) #parameter xc, yc, r_max, alpha
      
      #plot of PC and checkpoints (large scale)
      dev.set(2)
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",asp=1,xlim=c(xc-r_max2,xc+r_max2),
           ylim=c(yc+r_max2, yc-r_max2), xlab = NULL, ylab=NULL, ann=T, main=paste("b", bnr2,
                                                                                   " - left",sep = ""), axes=F)
      # points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      # points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      # points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      # points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      # points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
      #
      
      #generation of image
      setwd(home_dir)
      f <- paste("./data/",Img_name,"/b_nr",sep = "")
      save(bnr2,file=f)
      file1 <- paste('./data/',Img_name,'/images/b',bnr2,'_new8.tif',sep = "")
      tiff(file1, width=578, height=578, units="px", bg = "white")
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",asp=1,
           xlim=c(xc-r_max2,xc+r_max2),ylim=c(yc+r_max2, yc-r_max2), 
           xlab = NULL, ylab=NULL, ann= FALSE, main=paste("b", bnr2), axes=F)
      # points(xc, yc, pch = 16, cex=1.5, col = "black", asp=1) #centre of PC
      # points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      # points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      # points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      # points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      dev.off()
      cat("end of spObj_extract_single_building.R -first part",sep = "")
    } #end b221(first part)
    
    #second part 
    if (bnr2 == 222) { 
      cat("start of spObj_extract_single_building.R - second part",sep = "", "\n")
      part <-  "2parts_2"
      bnr2_part <- bnr2 
      is_label_2 <- is_bnr2_label@.Data == 2 #right part
      display(is_label_2)
      is_label_2
      coords <- data.frame(x=as.numeric(row(is_label_2)),y=as.numeric(col(is_label_2)), is_bnr=as.numeric(is_label_2))
      coords <- coords[coords$is_bnr == 1,] #removal of pixels which do not have the label of the building
      xc <- coor_part[2,1] #generate xc,yc,r_max,alpha and store with bnr2=32
      yc <- coor_part[2,2]
      alpha <- coor_part[2,5]*omega
      r_max <- shap_part[2,6]
      dy_window_plot
      
      #output plot parameter
      plotPar <- c(xc,yc,r_max,alpha,dy_window_plot)
      setwd(home_dir)
      f1 <- paste("./data/",Img_name,"/param_b",bnr2,sep="") 
      save(plotPar, file=f1) #parameter xc, yc, r_max, alpha
      f <- paste("./data/",Img_name,"/b_nr",sep = "")
      save(bnr2,file=f)
      
      #plot of PC and checkpoints (large scale)
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",
           asp=1,xlim=c(xc-r_max2,xc+r_max2),ylim=c(yc+r_max2, 
                                                    yc-r_max2), xlab = NULL, ylab=NULL, ann=T, 
           main=paste("b", bnr2,sep = ""), axes=TRUE)
      # points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      # points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      # points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      # points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      # points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
      
      #generation of image
      file1 <- paste('./data/',Img_name,'/images/b',bnr2,'_new8.tif',sep = "")
      tiff(file1, width=578, height=578, units="px", bg = "white")
      r_max2 <- round(1.1*r_max)
      plot(coords$x, coords$y, pch=16, cex=0.2,col="black",asp=1,xlim=c(xc-r_max2,xc+r_max2),
           ylim=c(yc+r_max2, yc-r_max2), xlab = NULL, ylab=NULL, ann=T, main=paste("b", bnr2,
           sep = ""), axes=TRUE)
      # points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      # points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      # points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      # points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
      # points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
      dev.off()
      #windows() #if necessary
      cat("end of spObj_extract_single_building.R - second part", sep = "","\n")
    } #end of b222
    n_bnr2 <- n_bnr2 + 1
  } #end b22
  
  
} #end ISPRS4_DLR10 

cat("end of program 'spObj_extract_single_building.R' ",sep = "","\n")
################################################################################
      

