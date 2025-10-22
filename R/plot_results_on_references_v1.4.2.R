##plot of results on references (orthoimage,Ground Truth)
##plot of results on graph
##name of script: plot_results_on_references.R
cat("version_number= ",v_nr,"\n")
##examples: ISPRS data: image ISPRS7/LCM1, ISPRS1/LCM2, ISPRS4/SVL_5, ISPRS4/DLR10
##instruction: use supplementing scripts if necessary 
##author: Joachim Höhle
#GNU General Public License (GPL)
cat("####################################################################","\n")
cat("start of program 'plot_results_on_references.R'","\n")

#input of table with line-pair, vertex-number and final coordinates (x,y)
##plotting of results onto orthoimage 
#large scale
#cases=1,2,3,4,5,6

if (cas == "extr_wd" || cas == "4_long" || cas == "100_all" 
    || cas == "100_all+nonortho") { 
  setwd(home_dir)
  f5 <- paste("./results/",Img_name,"/b",bnr2,"_intsec_linepair_vertex_coord.txt",sep="")
  intsec_linepair_vertex_coord2 <- read.table(f5)
  names(intsec_linepair_vertex_coord2) <- c("line_pair","vertex_nr","x","y")
  cat("table with line-pairs,vertex/corner-number,coordinates(x,y)","\n")
  print(intsec_linepair_vertex_coord2)
  
  #input of table with line-pair, vertex-number and final coordinates (x,y)
  setwd(home_dir)
  f5 <- paste("./results/",Img_name,"/b",bnr2,"_intsec_linepair_vertex_coord.txt",sep="")
  intsec_linepair_vertex_coord2 <- read.table(f5)
  names(intsec_linepair_vertex_coord2) <- c("line_pair","vertex_nr","x","y")
  cat("table with line-pairs,vertex/corner-number,coordinates(x,y)","\n")
  print(intsec_linepair_vertex_coord2)
  #
  f6 <- paste("./results/",Img_name,"/b",bnr2,"_coord_adj_plot.txt",sep="")
  xy_5pts <- read.table(f6)
  names(xy_5pts) <- c("x","y")
  xy_5pts
  
  ##plot of outline with vertices & line-numbers onto enlarged orthoimage
  n_x1 <- length(b13_angle_df2$nr_center)
  centers_PC <- matrix(nrow=n_x1,ncol=3) #new
  vec <- 1:n_x1
  for (i in vec) { 
    centers_PC[i,1] <- b13_angle_df2$nr_center[i]
    centers_PC[i,2] <- b13_angle_df2$x_centre[i]
    centers_PC[i,3] <- (-b13_angle_df2$y_centre[i])
  }
  centers_PC
  
  display(img_uds,method = "raster")
  n_x <- length(PC_nr)
  vec_y <- 1 : (n_x + 1)
  #browser()
  points(as.integer(pc3$col-orig_x), as.integer(pc3$row-orig_y), 
           pch=20, asp=1, cex=0.3, col="green")
  points(xc-orig_x,yc-orig_y,pch=3, asp=1, cex=1.3, col="red")
  points(intsec_linepair_vertex_coord2[,3]-orig_x,
           (-intsec_linepair_vertex_coord2[,4] - orig_y),
           pch=20, asp=1, cex=1.5, col="red")
  lines(xy_5pts[,2]-orig_x, (-xy_5pts[,3]-orig_y),  col="blue", asp=1, type="l", lwd=2, lty=1)
    
  #loop
  i=1
  
  while(i <= n_x) {
    text(centers_PC[i,2]-orig_x,(-centers_PC[i,3]-orig_y), labels=centers_PC[(i),1],
         pos=2, offset = 0.5, cex = 1, col = "red") 
    i=i+1
  } #end while-loop

  for (i in vec_y) {
    cat("i=",i,"\n") 
    text(intsec_linepair_vertex_coord2[i,3]-orig_x,(-intsec_linepair_vertex_coord2[i,4]-orig_y), 
           labels = intsec_linepair_vertex_coord2[i,2], 
           pos=2, offset = 0.7, cex = 1, col = "white")
  } #end for-loop
  
  cat("table with line-pairs,vertex/corner-number,coordinates(x,y)","\n")
  print(intsec_linepair_vertex_coord2)
  
  #end of plot of outline with vertexes & line-numbers onto enlarged orthoimage
  
  ##plot of coordinates and connecting lines of object onto orthoimage
  setwd(home_dir)
  fname12 <- paste("./results/",Img_name,"/b",bnr2,"_coord_adj_plot.txt",sep="")
  b <- read.table(fname12)
  cat("plot of orthoimage", "\n")
  k1 <- nrow(b)
  names(b) <- c("Points_nr","Points_x","Points_y")
  b$Points_y <- (-b$Points_y) #change to img-system
  #
  cat("plot of building outline on top of orthoimage","\n")
  display(img_uds,method = "raster")
  n_x <- length(PC_nr)
  vec_y <- 1 : n_x
  orig_y <- (-orig_y_math) #change to img-system
  orig_y
  points((xc-orig_x),(yc-orig_y),pch=3, asp=1, cex=1.3, col="red")
  points(pc3$col-orig_x,pc3$row-orig_y,pch=20,asp=1,cex=0.3,col="green")
  lines((b$Points_x-orig_x),(b$Points_y-orig_y),col="red",asp=1,type="l",lwd=2,lty=1)
  
  #plot of lines one by one
  display(img_uds,method = "raster")
  points((xc-orig_x),(yc-orig_y),pch=3, asp=1, cex=1.3, col="red")
  points(pc3$col-orig_x,pc3$row-orig_y,pch=20,asp=1,cex=0.3,col="green")
  
  #loop
  for (i in vec_y) {
    cat("i=",i,"\n")
    b$Points_x_red[i] <- b$Points_x[i]-orig_x
    b$Points_x_red[i+1] <- b$Points_x[i+1]-orig_x
    b$Points_y_red[i] <- b$Points_y[i]-orig_y
    b$Points_y_red[i+1] <- b$Points_y[i+1]-orig_y
    lines(b$Points_x_red[i:(i+1)],b$Points_y_red[i:(i+1)],
          col="blue",asp=1,type="l",lwd=2,lty=1)
  } #end for-loop
  
  #end of plot at large scale 
  
  cat("does the result agree with the orthoimage (large scale) ?","\n")
  
  if (proc_mode == "demo") {
    cat("if demo - type Y ","\n")
  }
  
  answ <- readline ("type Y or N: ")
  
  if (answ == "N") {
    cat ("start again with this object and select other values for 'cas' and/or 'sek' ","\n")
    setwd(home_dir2)
    source(paste("extract_single_building_v",v_nr,".R",sep = ""))  
  }

  ##plot of coordinates and connecting lines of object onto orthoimage
  #small scale 
  setwd(home_dir)
  fname12 <- paste("./results/",Img_name,"/b",bnr2,"_coord_adj_plot.txt",sep="")
  b <- read.table(fname12,header=T)
  cat("plot of orthoimage", "\n")
  k1 <- nrow(b)
  names(b) <- c("Points_nr","Points_x","Points_y")
  b$Points_y <- (-b$Points_y) #change to img-system
  b2 <- b[,2:3]

  #plot of final results onto orthoimage (small scale)
  setwd(OrgImgPathname)
  img_ref <- readImage(OrgImgFilename)
  display(img_ref, method = "raster")
  
  i <- 0
  while(i < k1) {
    i <- i+1
    lines(b2, col="white", asp=1, type="l", lwd=2, lty=1)
  } #end while

  cat("does the result agree with the orthoimage (small scale) ?","\n")
  
  if (proc_mode == "demo") { 
    answ = "Y" #automated
  } else {
    answ <- readline ("type Y or N: ") #interaction required
  }
  
  if (answ == "N") {
    cat ("start again with this object and select other values for 'cas' and/or 'sek' ","\n")
    setwd(home_dir2)
    source(paste("extract_single_building_v",v_nr,".R",sep = ""))  
  }
  
  if (answ == "Y") {
    
    #plot onto Ground Truth (small scale)
    setwd(OrgGtsPathname)
    img_GTS <- readImage(OrgGtsFilename)
    display(img_GTS, method="raster")
    display(img_GTS)
    i <- 0
    while(i < k1) {
      i <- i+1
      lines(b2, col="red", asp=1, type="l", lwd=2, lty=1)
    } #end while
    
    #plot onto Ground Truth (large scale)
    GTS_uds <- img_GTS[orig_x:wind_x, orig_y:wind_y, 1:3]
    display(GTS_uds, method="raster")
    
    for (i in vec_y) {
      cat("i=",i,"\n")
      b$Points_x_red[i] <- b$Points_x[i]-orig_x
      b$Points_x_red[i+1] <- b$Points_x[i+1]-orig_x
      b$Points_y_red[i] <- b$Points_y[i]-orig_y
      b$Points_y_red[i+1] <- b$Points_y[i+1]-orig_y
      lines(b$Points_x_red[i:(i+1)],b$Points_y_red[i:(i+1)],
            col="red",asp=1,type="l",lwd=2,lty=1)
    } #end for-loop i
  
  } #end if answ="Y")
  
  cat("Test on agreement with the Ground Truth","\n")
  
  if (proc_mode == "demo") {
    cat("if demo - type Y ","\n")
  }

  answ <- readline("does the result agree with the Ground Truth? type Y or N: ")
  

  if (answ == "Y") { 
  
    if (Img_name == "ISPRS7") { 
    
    #plot object onto map (Ground Truth, map_ISPRS7)
    par(mai = c(1.02,0.82,0.82,0.42)) #setup of margins/plot region [inches]
    x=0
    y=0
    plot(x,-y, pch=3, cex=1.5,  cex.axis = 1.2, cex.lab=1.5, col="red", asp=1, xlim=c(1,1887), ylim=c(-2557,-1), 
         axes = TRUE, ann = T, frame.plot = TRUE, main = paste("building #", bnr2," of image '",Img_name,"'",sep = ""))
    
    } #end image "ISPRS7"
  
    if (Img_name == "ISPRS1") { 
      #plot object onto map (Ground Truth, map_ISPRS7)
      par(mai = c(1.02,0.82,0.82,0.42)) #setup of margins/plot region [inches]
      x=0
      y=0
      plot(x,-y, pch=3, cex=1.5,  cex.axis = 1.2, cex.lab=1.5, col="red", asp=1, xlim=c(1,1919), ylim=c(-2569,-1), 
           axes = TRUE, ann = T, frame.plot = TRUE, main = paste("building #", bnr2," of image '",Img_name,"'",sep = ""))
    } #end plot on image ISPRS1
  
    fname12 <- paste("./results/",Img_name,"/b",bnr2,"_coord_adj_plot.txt",sep="")
    setwd(home_dir)
    b <- read.table(fname12,header=T)
    k1 <- nrow(b)
    names(b) <- c("Points_nr","Points_x","Points_y")
    b3 <- b[,2:3]
    print(b3)
    cat("plot of building-outline","\n")

    #loop
    i <- 0
    
    while(i < k1) {
      i <- i + 1
      lines(b3, col="black", asp=1, type="l", lwd=1, lty=1)
    } #end loop while
    
  } else { #poor agreement
    cat ("start again with this object and select other values for 'cas' and/or 'sek' ","\n")
    setwd(home_dir2)
    source(paste("extract_single_building_v",v_nr,".R",sep = ""))  
  } #end if-else
  
  #store bnr2 in a file containing all processed buildings
  setwd(home_dir)
  fname15 <- paste("./results/",Img_name,"/b_all.txt",sep="")
  write.table(bnr2, file= fname15, row.names = F, col.names = F, append=TRUE)
  
  ##processing of other objects (buildings)

  answ2 <- readline("other buildings to process? type Y or N: ")
  
  if (answ2 == "Y" && proc_mode == "auto") {
    
    k_y_auto <- k_y_auto + 1 #next building
    
    if (k_y_auto < n_y_auto) {
      setwd(home_dir2)
      source(paste("extract_single_building_v",v_nr,".R",sep = ""))
    } #end if (k_y_auto < n_y_auto)
      
    if (k_y_auto >= n_y_auto) {
      cat(paste("end of processing object ",bnr2, sep = ""),"\n")
      cat("end of processing mode 'auto' ","\n")
      proc_mode <- "NA"
    } #end if (k_y_auto > n_y_auto)
      
  } #end if answ2 = "Y" && proc_mode = "auto"  

  if (answ2 == "Y" && proc_mode != "auto") { 
    setwd(home_dir2)
    source(paste("extract_single_building_v",v_nr,".R",sep = ""))
  }  #end (answ2 = "Y" && proc_mode = "obj_wise")

  if (answ2 == "N") {
    
    #planning a new processing
    answ5 <- readline("do you want to start a complete new processing? type Y or N: ")
    
    if (answ5 == "Y") { 
      setwd(home_dir)
      fname15 <- paste("./results/",Img_name,"/",sep="")
      setwd(fname15)
      file.remove("b_all.txt") #removal of files with numbers of processed objects (buildings)
      #cat("end of program 'plot_results_on_references.R'","\n")
    } else {
      cat("end of program 'plot_results_on_references.R'","\n")
    } #end if-else
    
  } #end if answ2 = "N"
  
} #end of cases=1,2,3,4

if (cas == "nonortho_only") {
  setwd(home_dir)
  f5 <- paste("./results/",Img_name,"/man","/b",bnr2,"_intsec_linepair_vertex_coord2.txt",sep="")
  intsec_linepair_vertex_coord2 <- read.table(f5)
  names(intsec_linepair_vertex_coord2) <- c("line_pair","vertex_nr","x","y")
  cat("table with line-pairs,vertex/corner-number,coordinates(x,y)","\n")
  print(intsec_linepair_vertex_coord2)
  intsec_linepair_vertex_coord2
  
  #plot graph (small scale)
  par(mai = c(1.02,0.82,0.82,0.42)) #setup of margins/plot region [inches]
  x=0
  y=0
  plot(x,y, pch=3, cex=1.5,  cex.axis = 1.2, cex.lab=1.5, col="red", asp=1, xlim=c(1,600), ylim=c(-813,-1),
       axes = TRUE, ann = T, frame.plot = TRUE, main = paste("building #", bnr2," of image '",Img_name,"'",sep = ""))
  points(intsec_linepair_vertex_coord2$x,intsec_linepair_vertex_coord2$y,type="p", col="red", pch=20, cex=1)
  points(intsec_linepair_vertex_coord2$x,intsec_linepair_vertex_coord2$y, type="l", col="green", lty=1, lwd=2)
  #

  #plot graph (large) scale)
  dev.list()
  dev.set(2)
  par(mai = c(1.02,0.82,0.82,0.42)) #setup of margins/plot region [inches]
  x <- xc
  y <- yc
  r_max2 <- 1.1 * r_max
  #
  plot(x,-y, pch=3, cex=2, col="red", asp=1, xlim=c(xc - r_max2,xc + r_max2),
       ylim=c(-(yc + r_max2),-(yc - r_max2)), ann = TRUE, axes = TRUE,
       main=paste("b ",bnr2, sep=("")))
  points(intsec_linepair_vertex_coord2$x,intsec_linepair_vertex_coord2$y,
         type="l", col="blue", lty=1, lwd=2, asp=1)
  #
  
  #plot onto orthoimage (small) scale)
  f5 <- paste("./results/",Img_name,"/man/b",bnr2,"_intsec_linepair_vertex_coord2.txt",sep="")
  intsec_linepair_vertex_coord2 <- read.table(f5)
  names(intsec_linepair_vertex_coord2) <- c("line_pair","vertex_nr","x","y")
  cat("table with line-pairs,vertex/corner-number,coordinates(x,y)","\n")
  print(intsec_linepair_vertex_coord2)
  setwd(OrgImgPathname)
  img_ref <- readImage(OrgImgFilename)
  display(img_ref, method = "raster")
  lines(intsec_linepair_vertex_coord2[,3], 
        (-intsec_linepair_vertex_coord2[,4]),col="white",asp=1,type="l",lwd=2,lty=1)
  
  #plot onto orthoimage (large scale)
  display(img_uds,method = "raster")
  lines(intsec_linepair_vertex_coord2[,3]-orig_x, 
        (-intsec_linepair_vertex_coord2[,4]-orig_y),col="white",asp=1,type="l",lwd=2,lty=1)
  
  # #plot vertex numbers
  # n_x <- length(intsec_linepair_vertex_coord$x)
  # intsec_linepair_vertex_coord[n_x,2] <- 1
  # 
  # vec_y <- 1 : n_x
  # 
  # for (i in vec_y) {
  #   #browser()
  #   cat("i=",i,"\n")
  #   text(intsec_linepair_vertex_coord[i,3]-orig_x,(intsec_linepair_vertex_coord[i,4]-orig_y),
  #        labels = intsec_linepair_vertex_coord[i,2],
  #        pos=2, offset = 0.7, cex = 1, col = "white")
  # } #end for-loop
  
  #end of plot of outline with vertex-numbers onto enlarged orthoimage
  
  
  cat("does the result agree with the orthoimage (large scale)?","\n")
  
  if (proc_mode == "demo") {
    cat("if demo - type Y ","\n")
  }
  
  answ <- readline ("type Y or N: ")
  
  if (answ == "N") {
    cat ("start again with this object and select other values for 'cas' and/or 'epsilon' ","\n")
    setwd(home_dir2)
    source(paste("extract_single_building_v",v_nr,".R",sep = ""))  
  }
  
  if (answ == "Y") {
    
    #plot onto Ground Truth (GT)
    setwd(OrgGtsPathname)
    img_GTS <- readImage(OrgGtsFilename)
    display(img_GTS, method="raster")
    lines(intsec_linepair_vertex_coord2[,3], intsec_linepair_vertex_coord2[,4],col="red",asp=1,type="l",lwd=2,lty=1)
    GTS_uds <- img_GTS[orig_x:wind_x, orig_y:wind_y, 1:3]
    display(GTS_uds, method="raster")
    lines(intsec_linepair_vertex_coord2[,3]-orig_x,-intsec_linepair_vertex_coord2[,4]-orig_y,col="red",asp=1,type="l",lwd=2,lty=1)
  } #end if answ="Y")
  
  cat("Test on agreement with the Ground Truth","\n")
  
  if (proc_mode == "demo") {
    cat("if demo - type Y ","\n")
  }
  
  answ <- readline("does the result agree with the Ground Truth? type Y or N: ")
  
  if (answ == "N") { 
    
    cat ("start again with this object and select other values for 'cas' and/or 'epsilon' ","\n")
    setwd(home_dir2)
    source(paste("extract_single_building_v",v_nr,".R",sep = ""))  
  } #end if
  #end of plotting of results with cas=nonortho_only
  
  
  ##processing of other objects (buildings)
  answ2 <- readline("other buildings to process? type Y or N: ")
  
  if (answ2 == "Y" && proc_mode == "auto") {
    
    k_y_auto <- k_y_auto + 1 #next object
    
    if (k_y_auto < n_y_auto) {
      setwd(home_dir2)
      source(paste("extract_single_building_v",v_nr,".R",sep = ""))
    } #end if (k_y_auto < n_y_auto)
    
    if (k_y_auto >= n_y_auto) {
      cat(paste("end of processing object ",bnr2, sep = ""),"\n")
      cat("end of processing mode 'auto' ","\n")
      proc_mode <- "NA"
    } #end if (k_y_auto > n_y_auto)
    
  } #end if answ2 = "Y" && proc_mode = "auto"  
  
  if (answ2 == "Y" && proc_mode != "auto") {
    dev.list()
    #dev.off(3)
    setwd(home_dir2)
    source(paste("extract_single_building_v",v_nr,".R",sep = ""))
  }  #end (answ2 = "Y" && proc_mode = "obj_wise")
  
  if (answ2 == "N") {
    
    #planning a new processing
    answ5 <- readline("do you want to start a complete new processing? type Y or N: ")
    
    if (answ5 == "Y") { 
      setwd(home_dir)
      fname15 <- paste("./results/",Img_name,"/",sep="")
      setwd(fname15)
      file.remove("b_all.txt") #removal of files with numbers of processed objects (buildings)
      cat("end of program 'plot_results_on_references.R'","\n")
    } else {
      dev.list()
      #dev.off(3)
      cat("end of program 'plot_results_on_references.R'","\n")
      #cat("end of software package 'topomap'","\n")
    } #end if-else
    
  } #end if answ2 = "N"
  
} #end plot of results of cas=nonortho_only

################################################################################
  
if (cas == "nonortho_only_RDP") { 
  dev.list()
  dev.set(2)
  #input of table with line-pair, vertex-number and final coordinates (x,y)
  setwd(home_dir)
  f5 <- paste("./results/",Img_name,"/RDP/b",bnr2,"_intsec_linepair_vertex_coord3.txt",sep="")
  intsec_linepair_vertex_coord2 <- read.table(f5)
  names(intsec_linepair_vertex_coord2) <- c("line_pair","vertex_nr","x","y")
  cat("table with line-pairs,vertex/corner-number,coordinates(x,y)","\n")
  print(intsec_linepair_vertex_coord2) #math-system
  
  #plot graph (small scale)
  par(mai = c(1.02,0.82,0.82,0.42)) #setup of margins/plot region [inches]
  x=0
  y=0
  plot(x,y, pch=3, cex=1.5,  cex.axis = 1.2, cex.lab=1.5, col="red", asp=1, xlim=c(1,600), ylim=c(-813,-1),
       axes = TRUE, ann = T, frame.plot = TRUE, main = paste("building #", bnr2," of image '",Img_name,"'",sep = ""))
  points(intsec_linepair_vertex_coord2$x,intsec_linepair_vertex_coord2$y,type="p", col="red", pch=20, cex=1)
  points(intsec_linepair_vertex_coord2$x,intsec_linepair_vertex_coord2$y, type="l", col="green", lty=1, lwd=2)
  
  #plot graph (large) scale)
  dev.list()
  dev.set(2)
  par(mai = c(1.02,0.82,0.82,0.42)) #setup of margins/plot region [inches]
  x <- xc
  y <- yc
  r_max2 <- 1.1 * r_max
  #
  plot(x,-y, pch=3, cex=2, col="red", asp=1, xlim=c(xc - r_max2,xc + r_max2),
       ylim=c(-(yc + r_max2),-(yc - r_max2)), ann = TRUE, axes = TRUE,
       main=paste("b ",bnr2, sep=("")))
  points(intsec_linepair_vertex_coord2$x,intsec_linepair_vertex_coord2$y,type="l", col="blue", lty=1, lwd=2, asp=1)

  #plot onto orthoimage (small) scale)
  cat("table with line-pairs,vertex/corner-number,coordinates(x,y)","\n")
  print(intsec_linepair_vertex_coord2) #img-system
  setwd(OrgImgPathname)
  img_ref <- readImage(OrgImgFilename)
  display(img_ref, method = "raster")
  lines(intsec_linepair_vertex_coord2[,3], 
        (-intsec_linepair_vertex_coord2[,4]),col="white",asp=1,type="l",lwd=2,lty=1)
  
  #plot onto orthoimage (large scale)
  display(img_uds,method = "raster")
  lines(intsec_linepair_vertex_coord2[,3]-orig_x, 
        (-intsec_linepair_vertex_coord2[,4]-orig_y),col="white",asp=1,type="l",lwd=2,lty=1)
  
  # ##plot vertex numbers
  # n_x <- length(intsec_linepair_vertex_coord2$x)
  # intsec_linepair_vertex_coord2[n_x,2] <- 1
  # 
  # vec_y <- 1 : n_x
  # 
  # for (i in vec_y) {
  #   #browser()
  #   cat("i=",i,"\n")
  #   text(intsec_linepair_vertex_coord2[i,3]-orig_x,(-intsec_linepair_vertex_coord2[i,4]-orig_y),
  #        labels = intsec_linepair_vertex_coord2[i,2],
  #        pos=2, offset = 0.7, cex = 1, col = "white")
  # } #end for-loop
  # #end of plot of outline with vertexes-numbers onto enlarged orthoimage

  
  cat("does the result agree with the orthoimage (large scale)?","\n")
  
  if (proc_mode == "demo") {
    cat("if demo - type Y ","\n")
  }
  
  answ <- readline ("type Y or N: ")
  
  if (answ == "N") {
    cat ("start again with this object and select other values for 'cas' and/or 'epsilon' ","\n")
    setwd(home_dir2)
    source(paste("extract_single_building_v",v_nr,".R",sep = ""))  
  }
  
  if (answ == "Y") {
    
    #plot onto Ground Truth (GT)
    setwd(OrgGtsPathname)
    img_GTS <- readImage(OrgGtsFilename)
    display(img_GTS, method="raster")
    lines(intsec_linepair_vertex_coord2[,3], -intsec_linepair_vertex_coord2[,4],col="red",asp=1,type="l",lwd=2,lty=1)
    #large scale
    GTS_uds <- img_GTS[orig_x:wind_x, orig_y:wind_y, 1:3]
    display(GTS_uds, method="raster")
    lines(intsec_linepair_vertex_coord2[,3]-orig_x,-intsec_linepair_vertex_coord2[,4]-orig_y,col="red",asp=1,type="l",lwd=2,lty=1)
  } #end if answ="Y")
  
  cat("Test on agreement with the Ground Truth","\n")
  
  if (proc_mode == "demo") {
    cat("if demo - type Y ","\n")
  }
  
  answ <- readline("does the result agree with the Ground Truth? type Y or N: ")
  
  if (answ == "N") { 
      
    cat ("start again with this object and select other values for 'cas' and/or 'epsilon' ","\n")
    setwd(home_dir2)
    source(paste("extract_single_building_v",v_nr,".R",sep = ""))  
  } #end if
  
  #store building label (bnr2) in a file containing all processed buildings
  setwd(home_dir)
  fname15 <- paste("./results/",Img_name,"/RDP/b_all.txt",sep="")
  write.table(bnr2, file= fname15, row.names = F, col.names = F, append=TRUE)
  #
  
  
  ##processing of other objects (buildings)
  answ2 <- readline("other buildings to process? type Y or N: ")
  
  if (answ2 == "Y" && proc_mode == "auto") {
    
    k_y_auto <- k_y_auto + 1 #next building
    
    if (k_y_auto < n_y_auto) {
      setwd(home_dir2)
      source(paste("extract_single_building_v",v_nr,".R",sep = ""))
    } #end if (k_y_auto < n_y_auto)
    
    if (k_y_auto >= n_y_auto) {
      cat(paste("end of processing object ",bnr2, sep = ""),"\n")
      cat("end of processing mode 'auto' ","\n")
      proc_mode <- "NA"
    } #end if (k_y_auto > n_y_auto)
    
  } #end if answ2 = "Y" && proc_mode = "auto"  
  
  if (answ2 == "Y" && proc_mode != "auto") {
    dev.list()
    #dev.off(3)
    setwd(home_dir2)
    source(paste("extract_single_building_v",v_nr,".R",sep = ""))
  }  #end (answ2 = "Y" && proc_mode = "obj_wise")
  
  if (answ2 == "N") {
    
    #planning a new processing
    answ5 <- readline("do you want to start a complete new processing? type Y or N: ")
    
    if (answ5 == "Y") { 
      setwd(home_dir)
      fname15 <- paste("./results/",Img_name,"/",sep="")
      setwd(fname15)
      file.remove("b_all.txt") #removal of files with numbers of processed objects (buildings)
      cat("end of program 'plot_results_on_references.R'","\n")
    } else {
      dev.list()
      #dev.off(3)
      cat("end of program 'plot_results_on_references.R'","\n")
    } #end if-else
    
  } #end if answ2 = "N"
  
} #end of case="nonortho_only_RDP" 

cat("end of software package 'topomap'\n")
invokeRestart("abort")
###############################################################################



