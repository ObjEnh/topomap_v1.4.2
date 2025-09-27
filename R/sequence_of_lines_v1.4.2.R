#name of script: sequence_of_lines.R
cat("version_number= ",v_nr,"\n") 
#description: program derives the sequence of line segments in polygons
#input: result of the Hough-transform
#output: file with segments of the outline in proper order
#instruction:try another sek-method in case of failing
#special objects may require correction which can be carried out by additional scripts
#use additional script: 'spObj_sequence_of_lines_v1.4.0.R'
#parameter 'p_pos' indicates the type of correction ("cor_pos" or "cor_sek")
#author: Joachim Höhle
#GNU General Public License (GPL)
###############################################################################
## start of program 'sequence_of_lines.R'
cat("start of program 'sequence_of_lines.R'","\n")
setwd(home_dir)

##input

#input of Hough-trans result
H_param <- load(paste("./data/",Img_name,"/H_par",sep = ""))
H_param

#input of decision tree (derived from ISPRS orthoimage #26)
load(paste("./data/tt_prior_sek.RData",sep = ""))
accSample3_new2 <- accSample3_train
accSample3_new3 <- accSample3_new2[1,]

#input of plot-parameter
setwd(home_dir)
f<-paste("./data/",Img_name,"/param_b",bnr2, sep="")
load(f)
plotPar
xc <- plotPar[1]
yc <- plotPar[2]
r_max <- plotPar[3]
#

#input of pixel cloud (PC) of object
fname <- paste("./data/",Img_name,"/idxy_LCM_b",bnr2,".csv",sep="")
pc3 <- read.table(fname, header=TRUE)
names(pc3)[1:2] <- c("col","row")
#end of input

cat ("preparation of line-sequence","\n")

##sequence of lines (sek)
#selecting the proper method for determining the sequence of line segments (PCs)
#selection of object type with respect to the sequence of lines
cat("has object a complex structure?","\n") 
cat("activate parameter 'soph' in some methods (sek) - if required","\n")
cat("complex structure: soph=1, simple structure: soph=0")
#soph=0

#parameters for the estimation of line-sequence
cat("min_pixel= ",min_pixel,"\n")
cat("bn_PC= ",bn_PC,"\n")
cat("with_northo= ",with_northo,"\n")
cat("soph= ",soph,"\n")

#update of accSample3_new3 with actual object parameter
accSample3_new3$min_pix <- min_pixel
accSample3_new3$bn_PC <- bn_PC
accSample3_new3$with_nonorth <- with_northo
accSample3_new3$soph <- soph
accSample3_new3$building_nr <- bnr2
accSample3_new3$class3 <- "NA"
accSample3_new3$prior <- accSample3_new2$prior[bnr2] 
accSample3_new3

#prediction of sequence by means of decision tree
pred3 <- predict(tt.prior_sek, newdata=accSample3_new3,type="class")
accSample3_new3$class3 <- pred3
accSample3_new3$class3
accSample3_new3
pred4 <- as.numeric(pred3) 

if (n_nonortholines2 == n_total) {
  pred4=3
}

cat("suggested (predicted) method (meth) for determination of line-sequence_DT= ", pred4,"\n")
cat("1: Mpts(midpoints+angle), 2: Mpts+dist(midpoints+distance), 3: bdr_follow (boundry following)","\n")

if (Img_name == "ISPRS1" && proc_mode == "demo" ||
    Img_name == "ISPRS7" && proc_mode == "demo") { 
      meth <- 3
} 

if (Img_name == "ISPRS4" && proc_mode == "demo") {
   meth <- 1
}

if (Img_name == "ISPRS4_DLR10" && proc_mode == "demo") {
  meth <- 1
}


if (proc_mode != "demo") {
  meth <- readline("type number for method for determination of line-sequence: ") #manual input of method
  meth <- as.numeric(meth)
}

sek <- switch(meth,"Mpts","Mpts+dist","bdr_follow") 

#set up of matrix 'b13_angle_df' with corner point number,angle and coordinates

#change to common type
PC_nr <- B5_6$lnr 
n_x <- length(PC_nr)
b13_angle <- matrix(nrow=n_x,ncol=4)
b13_angle[,] <- 0
b13_angle_df <- data.frame(b13_angle)
names(b13_angle_df) <- c("nr_center","alpha","x_centre","y_centre")
#

b13_angle_df_seq <- b13_angle_df #set up of matrix for correct sequence
n_PC <- n_x
vec_y <- 1 : n_PC
nr <- matrix(ncol=1, nrow=n_PC)

for (i in vec_y) {
  nr[i,1] <- b13_angle_df_seq$nr_center[i]
}

nr #vector of point labels (characters)
b13_angle_df_seq
################################################################################

##separation of processing into 3 routes of code ("Mpts","Mpts+dist","bdr_follow") 

##determination of sequence of lines by angle from object-center to midpoints of line

if (sek == "Mpts") { 
  n_PC #number of PCs forming the outline of the building
  b13_angle_df$alpha <- 0
  b13_angle_df$x_centre <- 0
  b13_angle_df$y_centre <- 0
  xc <- plotPar[1]
  yc <- plotPar[2]

  #plot of PCs at large scale
  par(mai = c(1.02,0.82,0.82,0.42)) #setup of margins/plot region [inches]
  x <- xc
  y <- yc
  r_max2 <- 1.1*r_max
  plot(x,-y, pch=3, cex=2, col="red", asp=1, xlim=c(xc-r_max2,xc+r_max2), ylim=c(-(yc+r_max2),-(yc-r_max2)), 
       frame.plot=TRUE, main=paste("b ",bnr2, sep=(""))) #large scale
  points(pc3$col, -pc3$row, pch=20, asp=1, cex=0.5, col="orange")

  #generation of line-midpoints 
  b13_angle_df
  b13_angle_df2 <- b13_angle_df
  n_x <- length(PC_nr)
  vec_y <- 1 : n_x
  for(i2 in vec_y) {
    lnum <- i2
    mean_line(lnum)
    par_midp
    b13_angle_df2[lnum,1] <- par_midp[[1]]
    b13_angle_df2[lnum,2] <- par_midp[[2]]
    b13_angle_df2[lnum,3] <- par_midp[[3]]
    b13_angle_df2[lnum,4] <- par_midp[[4]]
  } #end for-loop
  
  print(b13_angle_df2) #check of angles

  ##plot of midpoints in graph
  r_max2 <- 1.1*r_max
  labels <- b13_angle_df2$nr_center
  plot(xc,-yc, pch=3, cex=2, col="red", asp=1, xlim=c(xc - r_max2,xc + r_max2), ylim=c(-(yc + r_max2),-(yc - r_max2)),
       main=paste("b ",bnr2, sep=(""),collapse=NULL)) #large scale
  points(pc3$col, -pc3$row, pch=20, asp=1, cex=0.3, col="cyan") # original pointcloud for building
  #
  n_b13_angle_df2 <- length(b13_angle_df2$nr_center)
  vec4 <- 1 : n_b13_angle_df2
  nr_dpx <<- round(r_max2/5) #displacement of number in x-direction
  
  for (i in vec4) {
    browser() # if display point by point
    cat("nr_center= ",b13_angle_df2$nr_center[i],"\n")
    points(b13_angle_df2$x_centre[i],-b13_angle_df2$y_centre[i], asp=1, pch=20,col="black", cex=1.5)
    text((b13_angle_df2$x_centre[i]+nr_dpx),(-b13_angle_df2$y_centre[i]),labels[i], cex=1,col="red")
  }
  
  #correction of midpoints which represent a line-segment
  answ <- readline("are the position of all midpoints correct? type Y or N: ") #interaction required
  
  if (substr(bnr2,3,3) == "1") { 
    part <- "2parts_1"
    bnr2_part <- bnr2
  }
  
  if (substr(bnr2,3,3) == "2") {
    part <- "2parts_2"
    bnr2_part <- bnr2
  }
  
  if (substr(bnr2,3,3) == "3") {
    part <- "3parts_3"
    bnr2_part <- bnr2
  }
  
  if (answ == "N" && proc_mode == "obj_wise" || answ == "N" && proc_mode == "demo") {
    cat("bnr2= ", bnr2,"\n")
    p_pos <- "cor_pos" #correction of position in sek="Mpts",
    b13_angle_df2
    setwd(home_dir2)
    source(paste("./spObj/spObj_sequence_of_lines_v",v_nr,".R",sep="")) #correction of position
    b13_angle_df3
  } else { #no correction
    b13_angle_df3 <- b13_angle_df2
  } #end if-else  
  
  b13_angle_df3
  labels <- b13_angle_df3$nr_center
  #
  
  ##plot of line-centers
  r_max2 <- 1.1*r_max
  plot(xc,-yc, pch=3, cex=2, col="red", asp=1, xlim=c(xc - r_max2,xc + r_max2), ylim=c(-(yc + r_max2),-(yc - r_max2)),
       main=paste("b ",bnr2, sep=(""),collapse=NULL)) #large scale
  points(pc3$col, -pc3$row, pch=20, asp=1, cex=0.3, col="cyan") #original pointcloud for building
  points(b13_angle_df3$x_centre,-b13_angle_df3$y_centre, asp=1, pch=20,col="black", cex=1.5)
  text((b13_angle_df3$x_centre+2),(-b13_angle_df3$y_centre+2),labels, cex=1,col="red")
  #
  
  ##ordering of the angles which represent line segments
  b13_angle_df2_seq <- b13_angle_df3[order(b13_angle_df3$alpha, decreasing = FALSE),]
  row.names(b13_angle_df2_seq) <- 1 : nrow(b13_angle_df2_seq)
  b13_angle_df2_seq

  #plot of line-centers at large scale
  r_max2 <- 1.1*r_max
  plot(xc,-yc, pch=3, cex=2, col="red", asp=1, xlim=c(xc - r_max2,xc + r_max2), ylim=c(-(yc + r_max2),-(yc - r_max2)),
    main=paste("b ",bnr2, sep=(""),collapse=NULL)) #large scale
  points(pc3$col, -pc3$row, pch=20, asp=1, cex=0.3, col="cyan") # original pointcloud for building
  points(b13_angle_df2_seq$x_centre,-b13_angle_df2_seq$y_centre, asp=1, pch=20,col="blue", cex=1.5)
} #end of route "Mpts"

################################################################################

##"Mpts+dist"
#use of distances

if (sek == "Mpts+dist") {
  n_PC <- length(PC_nr) #number of PCs forming the outline of the building
  names(b13_angle_df) <- c("nr_center","alpha","x_centre","y_centre")
  b13_angle_df
  b13_angle_df$alpha <- 0
  b13_angle_df2 <- b13_angle_df[,1:4]
  xc <- plotPar[1]
  yc <- plotPar[2]
  r_max <- plotPar[3]

  ##plot of final line-centers at large scale
  r_max2 <- 1.1*r_max
  plot(xc,-yc, pch=3, cex=2, col="red", asp=1, xlim=c(xc - r_max2,xc + r_max2), ylim=c(-(yc + r_max2),-(yc - r_max2)),
    main=paste("b ",bnr2, sep=(""),collapse=NULL)) #large scale
  points(pc3$col, -pc3$row, pch=20, asp=1, cex=0.3, col="cyan") # original pixel cloud for building
  
  #generation of line-centers 
  n_x <- length(PC_nr)
  vec_y <- 1 : n_x
  
  for(i2 in vec_y) {
    lnum <- i2
    mean_line(lnum) #call of function 'mean_line()'
    par_midp
    b13_angle_df2[lnum,1] <- par_midp[[1]]
    b13_angle_df2[lnum,2] <- par_midp[[2]]
    b13_angle_df2[lnum,3] <- par_midp[[3]]
    b13_angle_df2[lnum,4] <- par_midp[[4]]
  } #end for-loop
  b13_angle_df2
  #
  
  #correction of midpoint positions
  cat("check of positions of all midpoints","\n")
  
  if (proc_mode == "demo") {
    cat("if demo: type N (ISPRS7) or Y (ISPRS1) or Y (ISPRS4)","\n")
  }
  
  answ <- readline("Is the position of all midpoints correct? - type Y or N: ")
  
  if (substr(bnr2,3,3) == "1") { 
    part <- "2parts_1"
    bnr2_part <- bnr2
  }
  
  if (substr(bnr2,3,3) == "2") {
    part <- "2parts_2"
    bnr2_part <- bnr2
  }
  
  if (answ == "N") {
    
    if (part == "2parts_1") { #first part of object
      p_pos <- "cor_pos" #correction of position
    }
    
    if (part == "2parts_2") { #second part of object
      p_pos <- "cor_pos" 
    }
    
    if (proc_mode == "obj_wise") {
      p_pos <- "cor_pos"
    }
  
  b13_angle_df2
  setwd(home_dir2)
  source(paste("./spObj/spObj_sequence_of_lines_v",v_nr,".R",sep="")) #corrections for "Mpts+dist"
  b13_angle_df2
} #end if (sek = "Mpts+dist")

b13_angle_df2  

##plot of graph
  r_max2 <- 1.1*r_max
  plot(xc,-yc, pch=3, cex=2, col="red", asp=1, xlim=c(xc - r_max2,xc + r_max2), ylim=c(-(yc + r_max2),-(yc - r_max2)),
    main=paste("b ",bnr2, sep=(""),collapse=NULL)) #large scale
  points(pc3$col, -pc3$row, pch=20, asp=1, cex=0.3, col="cyan") #original pointcloud for building
  points(b13_angle_df2$x_centre,-b13_angle_df2$y_centre, asp=1, pch=20,col="black", cex=1.5)
  #
  
  ##correction of midpoints which represent line segments
  b13_angle_df2
  
  #preparation
  b13_outlines <- b13_angle_df2 
  b_angle_df2_seq <- b13_outlines
  b_angle_df2_seq 
  names_of_lines <- b_angle_df2_seq$nr_center[1:n_x]
  
  #print labels of lines
  for (i in names_of_lines) {
    cat("name of line = ", i, "\n")
  }
  b_angle_df_seq <- b_angle_df2_seq
  
  ##determination of sequence
  
  #add 'theta_appr' to df 'b_angle_df_seq'
  i<-1
  
  while (i <= n_x) {
    j <- 1
    while (j <= n_x) {
      
      if (b_angle_df_seq$nr_center[i] == B5_6$lnr[j]) {
      b_angle_df_seq$theta_appr[i] <- B5_6$theta_angle[j]
      } #end if
      
      j<-j+1
    } #end loop j
    i=i+1
  } #end loop i
  #
 
  b_angle_df_seq

  #add of vectors
  b_angle_df_seq[6:(7+n_x-2)] <- 5000
  x <- 1:n_x
  na_col <- rep(NA,n_x)
  
  for (i in x) {
    na_col[i] <- paste0("r",i)
  }
  
  names(b_angle_df_seq)[6:(6+n_x-1)] <- na_col
  b_angle_df_seq

  #matrix with sequence of line centers
  pt_nr_m <- matrix(nrow=n_x, ncol=2)
  pt_nr_m[,1] <- rep(0,n_x)
  pt_nr_m[1,2] <- b_angle_df_seq$nr_center[1]
  pt_nr_m[1,1] <- 1
  pt_nr_df <- as.data.frame(pt_nr_m)
  names(pt_nr_df) <- c("index","pt_nr")
  pt_nr_df
  head(b_angle_df_seq)
  b_angle_df_seq[,2:4] <- as.numeric(c(b_angle_df_seq$alpha,b_angle_df_seq$x_centre, b_angle_df_seq$y_centre))
  options(digits = 5)

  #distance vector
  refj <- 1
  b_angle_df_seq$r1 <- dist_v(n_x, b_angle_df_seq, refj) #call of function
  b_angle_df_seq$r1[refj] <- 6000
  xn <- 1 : n_x
  b_angle_df_seq[,6]
  pt_nr_df[2:n_x,] <- 0
  pt_nr_df

  #find next point
  n=1 
  
  #loop n
  for (n in xn) {
    j <- 1
    k <- 1
    cat("refj=",refj,"\n")
    cat("n=",n,"\n")
    
    #loop
    while (j <= n_x) { 
      n3 <- n + 1
      cat("n3=",n3,"\n")
      numb <- paste ("b_angle_df_seq$r",n,"[",j,"]",sep = "", collapse = NULL)
      numb3 <- eval(parse(text=numb))
      numb4 <- as.integer(numb3)
      numb <- numb4
      numb2 <- paste ("min(b_angle_df_seq$r",n,")",sep = "", collapse = NULL)
      numb3 <- eval(parse(text=numb2))
      numb2 <- as.integer(numb3)
      
      if (numb == numb2) {
        numb_index <- j
        cat("point_nr=", b_angle_df_seq$nr_center[j],"\n")
        point_nr <- b_angle_df_seq$nr_center[j]
        refj <- numb_index
        cat("k=",k,"\n")
        k3 <- n + 6 #error in table without influence
        cat("k3=",k3,"\n")
        b_angle_df_seq[,k3] <- dist_v(n_x,b_angle_df_seq,refj) #call of function
        k <- k+1
        pt_nr_df[n3,1] <- numb_index
        pt_nr_df[n3,2] <- point_nr

        #all centers found will obtain high distance (=6000)
        for (n2 in pt_nr_df$index[1:n3]) {
           b <- eval(parse(text=paste("b_angle_df_seq$r",n3,"[",n2,"] <- 6000",sep = "", collapse = NULL)))
           cat("b=",b,"\n")
        } #end loop n2
      } #end if
    j<-j+1
    } #end loop while
  
    cat("numb_index=",numb_index,"\n")
  } #end loop n
  # end r-calculation
  
  b_angle_df_seq

  ## sequence of line segments
  lnr_det3 <- pt_nr_df[1:n_x,2]
  nr2 <- lnr_det3
  nr2 # sequence of line segments
  b_angle_df_seq[,1:(5+n_x)]
  pt_nr_df[1:n_x,]

  ##test: the derived sequence is correct?
  b_angle_df_seq_test <- b_angle_df_seq
  b_angle_df_seq_test <- b_angle_df_seq_test[,1:7]
  b_angle_df_seq_test$nr_center <- nr2
  b_angle_df_seq_test[,2:7] <- 0
  colnames(b_angle_df_seq_test) <- c("nr_center","alpha","x_centre","y_centre","theta_appr","ro_appr2","d_ro2")
  b_angle_df_seq_test

  #change table to new sequence
  i=1
  
  while (i <= n_x) {
    j = 1
    while(j <= n_x) {
      if (b_angle_df_seq_test$nr_center[i] == b_angle_df_seq$nr_center[j]) {
        b_angle_df_seq_test[i,2:5] <- b_angle_df_seq[j,2:5]
      } #end if
      j <- j + 1
    } #end loop j
    i <- i + 1
  } #end loop i
  #
  
  b_angle_df_seq_test
  theta_appr2 <- b_angle_df_seq_test$theta_appr[1]
  theta_appr2 #correction?
  theta_appr_arc <- theta_appr2/omega
  vec_x <- 1 : n_x
  
  #loop
  for (i in vec_x) {
    X <- b_angle_df_seq_test$x_centre[i]
    Y <- b_angle_df_seq_test$y_centre[i]
    ro <- cos(theta_appr_arc) * X + sin(theta_appr_arc) * Y
    b_angle_df_seq_test$ro_appr2[i] <- round(ro)
  } #end of loop i

  b_angle_df_seq_test[n_x+1,] <- b_angle_df_seq_test[1,]
  b_angle_df_seq_test
  vec_y <- 1 : n_x
  
  for (i in vec_y) {
    b_angle_df_seq_test$d_ro2[i] <- b_angle_df_seq_test$ro_appr2[i+1] - b_angle_df_seq_test$ro_appr2[i]
  } #end for-loop
  
  b_angle_df_seq_test[n_x+1,7] <- NA
  b_angle_df_seq_test
  s_d_ro2 <- abs(sum(b_angle_df_seq_test$ro2,na.rm =TRUE))
  cat("abs_sum of d_ro2= ", s_d_ro2, "\n")
  tol_round_err <- 3 #tolerated error due to rounding
  
  if (s_d_ro2 > tol_round_err) { #test
    cat("error in sequence", "\n")
  } else { 
    cat("test of sequence may be OK","\n")
  } #end if-else
  
  #end of test

  #general solution for list
  all_lines <- list()
  vec_x
  
  for (i in vec_x) {
    all_lines[[i]] <- "PC"
  }
  
  all_lines
  #

  ##solution for x PCs
  for (i in vec_x) {
    all_lines[i] <- paste("P",i,sep="")
  }
  all_lines

  #plot of graph at large scale
  x <- xc
  y <- yc
  plot(x,-y, pch=3, cex=1.5, col="red", asp=1, xlim=c(xc - r_max2,xc + r_max2), ylim=c(-(yc + r_max2),-(yc - r_max2)), main=paste("b ",bnr2, sep=(""))) #large scale
  points(pc3$col, -pc3$row, pch=20, asp=1, cex=0.5, col="green")
  PC_nr <- nr2
  cat("ordered line numbers:",PC_nr,"\n")
  #
  #plot of extreme user coordinates (y)
  x=par("usr")[2]; y=par("usr")[3]
  points(x, y, pch=3, asp=1, cex=3, col="red")
  x=par("usr")[2]; y=par("usr")[4]
  points(x, y, pch=3, asp=1, cex=3, col="red")
  
  #loop for reading all pixel clusters (PCs)
  k=1
  for (i in PC_nr) {
    #browser()
    lnr=i
    setwd(home_dir)
    fname=paste("./data/",Img_name,"/b",bnr2,"_",lnr,".txt", sep="")
    P0 <- read.table(fname, col.names=c("idx","x","y"))
    nrow <- nrow(P0)
    P0_red <- reduce_pointset(P0) #correction for gaps using histogram analysis
    nrow <- length(P0_red$idx)
    cat("nrow = ",nrow,"\n")
    all_lines[[k]] <- P0_red
    #all_lines[[k]] <- P0
    k <- k + 1
    cat("lnr=",lnr,"\n")
    points(P0_red[,2],-P0_red[,3], pch='.', asp=1, cex=2, col="blue") #corrected PC
    #points(P0[,2],-P0[,3], pch='.', asp=1, cex=2, col="blue") #original PC
  } #end loop
  all_lines

  #convert 'all_lines' (matrix) to 'all_PC' (list)
  all_PC <- all_lines
  all_PC

  #convert 'all_lines' (matrix) to 'all_PC' (list)
  names_PC <- list()
  vec_x <- 1:n_PC
  
  for (i in vec_x) {
    names_PC[[i]] <- "PCN"
  }

  #loop
  k <- 1
  for (i in PC_nr) {
    na_PC <- paste("PC_",PC_nr[k],sep="")
    name_PC <- as.name(na_PC)
    names_PC[[k]] <- name_PC
    k <- k + 1
  } #end of for-loop
  names(all_PC) <- names_PC

  #plot image detail (large scale)
  display(img_uds,method = "raster")
  n_x <- length(PC_nr)
  vec_y <- 1 : n_x
  
  #check of PCs
  for (i in vec_y) {
    x1 <- (all_PC[[i]]$x - orig_x)
    y1 <- (all_PC[[i]]$y - orig_y)
    points(x1, y1, pch='.', asp=1, cex=2, col="green")
  } #end loop
  # end of check-plot

  #check by plot of identified point clouds PC_nr
  n_x <- length(PC_nr)
  vec_y <- 1 : n_x
  vec_y
  
  for (i in vec_y) {
    points(all_PC[[i]]$x-orig_x,all_PC[[i]]$y-orig_y, pch='.', asp=1, cex=2, col="green")
  } #end loop
  
  #end of check plot
  
  all_PC
  length(all_PC)

  n_PC <- length(all_PC)

  #output of files of individual PCs
  vec_y <- 1 : n_PC
  setwd(home_dir)
  all_PC
  
  #loop
  for (i in vec_y) { 
    fname8 <- paste("./data/",Img_name,"/all_PC$PC_nr",PC_nr[i],".txt",sep="")
    write.table(all_PC[[i]], fname8)
  } #end loop output of list 'all_PC'

  #output of sequence (sek = "Mpts+dist")
  setwd(home_dir)
  fname9 <- paste("./data/", Img_name,"/b",bnr2,"_case.txt", sep="")
  write.table(sek,fname9,row.names=FALSE, col.names=FALSE)
} 

#end of route "Mpts+dist"
#####################################################################

##sequence of lines by "boundary following" 
#stop("checking step by step - manual operation")

if (sek == "bdr_follow") { 
  setwd(home_dir)
  
  if (part != "no_part") {
    #bnr2_orig <- substr(bnr2,1,2) #bnr2 < 10
    bnr2_orig <- substr(bnr2,1,3) #bnr2 >= 10
    bnr2_orig  <- as.integer(bnr2_orig)
    bnr2 <- bnr2_orig
  }
  #stop("continue step by step")
  b_new <- readImage(paste("./data/",Img_name,"/images/b",bnr2,"_new8.tif",sep = "")) 
  colorMode(b_new) <- Grayscale
  plot(b_new[,,1])
  b_bin <- b_new[,,1]
  print(b_bin)

  display(b_bin, "raster")
  #display(b_bin, "browser")
  plot(b_bin)
  
  # if (Img_name == "ISPRS4" || Img_name == "ISPRS4_DLR10") { #low-resolution image
  #   kern=makeBrush(5,shape="diamond")
  #   b_bin_erode <- erode(b_bin, kern)
  #   b_bin <- b_bin_erode
  #   str(b_bin)
  # }
  
  b <- b_bin@.Data
  str(b)
  b_im <- as.im(b) #class image with other indexing
  plot(b_im)
  
  Z18 <- connected(b_im, background = 0, method="C") #connect=8 (default)
  nc <- length(levels(Z18)) #nc=number of levels
  cat("nc= ", nc, "\n")
  W <- tiles(tess(image=Z18)) #separation of components
  
  if (nc == 2) { #stop("select data manually at script-line 647 in script 'sequence_of_line.R'")}
    plot(W$'2', col="white")  #black building
    w = W$'2'
    plot(w)
  } else {
    p_pos = "cor_img"
    setwd(home_dir2)
    source(paste("./spObj/spObj_sequence_of_lines_v",v_nr,".R",sep="")) #selection of W$x
    plot(w)
  } #end if-else
  
  out_poly <- as.polygonal(w) #conversion to polygons
  # out_poly2 <- simplify.owin(w,0.5) #changes number of line segments
  # out_poly <- out_poly2
  plot(out_poly)
  out_poly_df <- as.data.frame(out_poly)
  n_pt <- length(out_poly_df$x)
  y3 <- 1 : n_pt
  x_v <- round(out_poly_df$x)
  y_v <- round(out_poly_df$y)
  
  for (i in y3) {
    points(x_v[i], y_v[i], pch=16, col="red", cex=0.2) #points from edge
  }
  
  x_v
  y_v
  
  ##transfer of midpoints into system of image 'out_poly'
  #automatic determination of scale factor my3
  par(mai = c(1.02,0.82,0.82,0.42))
  pixel_size <- par("mai")[3]/60 #60 pixel
  #determined by manual measurement in browser of top margin (0.82") 
  #using function 'locator(1)'
  pixel_size #pixel size of screen, unit is inch, corresponds to 73.17073 pixels/inch,
  #value depends on used screen. A value of many screens is: 72 pixels/inch.
  1/pixel_size #number of pixels per inch
  
  #determination of a scale factor between plot and image
  margin_size_dy <- (par("mai")[1]+par("mai")[3])/pixel_size #image
  margin_size_dy
  window_size_y <- 578 # specified value of square window (578 x 578 pixels)
  size_plotting_region_y <- (window_size_y - margin_size_dy)
  size_plotting_region_y #y_dimension of the plotting region in derived image (img)
  #
  setwd(home_dir)
  
  #plot of PC and checkpoints
  r_max2 <- round(1.1*r_max)
  plot(coords$x,coords$y,pch=16,cex=0.2,col="black",asp=1,xlim=c(xc-r_max2,xc+r_max2),ylim=c(yc+r_max2,yc-r_max2),xlab=NULL,ylab=NULL,ann=T,main=paste("b",bnr2),axes=TRUE)
  #plot(coords$x,coords$y,pch=16,cex=0.2,col="black",asp=1,xlim=c(1,1887),ylim=c(2557,1),xlab=NULL,ylab=NULL,ann=FALSE,main=paste("b",bnr2),axes=TRUE) #small scale
  points(xc+r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
  points(xc-r_max, yc+r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
  points(xc-r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
  points(xc+r_max, yc-r_max, pch=16, cex=1.5, col="black", asp=1) #point for scaling
  points(xc, yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
  #
  par("usr")
  dy_window_plot <- abs(par("usr")[3] - par("usr")[4]) 
  dy_window_plot #y-dimension of the plotting region in graph
  # f1<-paste("./data/",Img_name,"/param_b",bnr2,sep="") #change
  # load(f1)
  # plotPar #original(non-partitioned) object
  xc <- plotPar[1]
  yc <- plotPar[2]
  r_max <- plotPar[3]
  
  #check
  #dy_window_plot <- plotPar[5]
  dy_window_plot
  kf3 <- size_plotting_region_y/dy_window_plot #scale factor between image (img) and plot
  #kf3 may be checked in 'support_sequence_of_lines.R', script #10
  1/kf3
  window_size_x <- 578 #specified value
  margin_size_x <- (par("mai")[2]+par("mai")[4])/pixel_size #unit pixel
  x0_img <- (par("mai")[2]/pixel_size) + (window_size_x-margin_size_x)/2
  y0_img <- par("mai")[3]/pixel_size + size_plotting_region_y/2

  #derivation of origin (left top corner) of window for a building
  x0 <- round(xc - x0_img/kf3) #x-coordinate of window origin [pixel]
  y0 <- round(yc - y0_img/kf3) #y-coordinate of window origin [pixel]

  ##2D-transformation
  a0_bdr <- x0
  b0_bdr <- y0
  D_bdr <- matrix(nrow=2, ncol=2)
  alpha = (-90) #degrees (angle of rotation of coordinate systems 
  #(original image, out_poly) eventually subject of change)
  alpha_arc <- alpha/omega #radiant of rotation angle
  a1_bdr <- cos(alpha_arc)
  b1_bdr <- sin(alpha_arc)
  D_bdr[1,] <- c(a1_bdr, -b1_bdr)
  D_bdr[2,] <- c(b1_bdr, a1_bdr)
  D_bdr_inv <- solve(D_bdr)
  D_bdr_inv
  #
  
  n_PC #number of PCs forming the outline of the building
  b13_angle_df
  names(b13_angle_df) <- c("nr_center","alpha","x_centre","y_centre")
  b13_angle_df$alpha <- 0
  b13_angle_df$x_centre <- 0
  b13_angle_df$y_centre <- 0
  #
  
  #generation of point-centers 
  #displacement of center to determine correct sequence of lines?
  n_x <- length(PC_nr)
  vec_y <- 1 : n_x
  b13_angle_df2 <- b13_angle_df 
  b13_angle_df2[,] <- 0
  all_PC <- all_lines   #correct names
  #
  for(i2 in vec_y) {
    lnum <- i2
    mean_line(lnum)
    par_midp
    b13_angle_df2[lnum,1] <- par_midp[[1]]
    b13_angle_df2[lnum,2] <- par_midp[[2]]
    b13_angle_df2[lnum,3] <- par_midp[[3]]
    b13_angle_df2[lnum,4] <- par_midp[[4]]
  } #end for-loop
  
  b13_angle_df2
  b13_angle_df2 <- b13_angle_df2[1: n_x,] #new

  #plot of midpoints
  plot(xc,-yc, pch=3, cex=2, col="blue", asp=1, xlim=c(xc - r_max2,xc + r_max2), ylim=c(-(yc + r_max2),-(yc - r_max2)),
    main=paste("b ",bnr2, sep=(""),collapse=NULL)) #large scale
  points(pc3$col, -pc3$row, pch=20, asp=1, cex=0.2, col="cyan") # original pointcloud for building

  for (i in vec_y) {
    browser()
    cat("nr_center= ",b13_angle_df2$nr_center[i],"\n")
    points(b13_angle_df2$x_centre[i],-b13_angle_df2$y_centre[i], asp=1, pch=20,col="blue", cex=1.5)
  }
  
  ##correction of midpoints which represent line segments
  #check of the positions of all midpoints
  
  if (Img_name == "ISPRS1" && proc_mode == "demo" ||
      Img_name == "ISPRS7" && proc_mode == "demo" ||
      Img_name == "ISPRS4" && proc_mode == "demo" ||
      Img_name == "ISPRS4_DLR10" && proc_mode == "demo") {
      answ = "Y" 
  } else {
    
    if (Img_name == "ISPRS1" && proc_mode != "demo" || Img_name == "ISPRS7" && proc_mode != "demo" ||
        Img_name == "ISPRS4" && proc_mode != "demo" || Img_name == "ISPRS4_DLR10" && proc_mode != "demo" ) { 
      
        answ <- readline("are the positions of all midpoints correct? type Y or N:  ") #manual input
        
    } 
    
  } #end if-else
  
  if (substr(bnr2,2,2) == "1") { #point number <10
  #if (substr(bnr2,3,3) == "1") { #point number >=10 
    part <- "2parts_1"
    bnr2_part <- bnr2
  }

  if (substr(bnr2,2,2) == "2") { #point number <10
  #if (substr(bnr2,3,3) == "2") { #point number >=10
    part <- "2parts_2"
     bnr2_part <- bnr2
  }
  
  if (substr(bnr2,2,2) == "3") { #point number < 10
  #if (substr(bnr2,3,3) == "3") { #point number >= 10
    part <- "3parts_3"
    bnr2_part <- bnr2
  }

  if (answ == "N") {
    
    if (part == "2parts_1") {
      bnr2_part <- bnr2
      p_pos <- "cor_pos" #correction of position
      b13_angle_df2
      setwd(home_dir2)
      source(paste("./spObj/spObj_sequence_of_lines_v",v_nr,".R",sep="")) #sek="bdr_follow"
      b13_angle_df3
      sequence_seg2 <- b13_angle_df3$nr_center
      sequence_seg2
    } 
    
    if (part == "2parts_2") {
      bnr2_part <- bnr2
      p_pos <- "cor_pos" #correction of position
      setwd(home_dir2)
      source(paste("./spObj/spObj_sequence_of_lines_v",v_nr,".R",sep="")) 
      b13_angle_df3
      sequence_seg2 <- b13_angle_df3$nr_center #new
      sequence_seg2 #new
    }
    
    if (part == "3parts_3") {
      bnr2_part <- bnr2
      p_pos <- "cor_pos" #correction of position
      b13_angle_df2
      setwd(home_dir2)
      source(paste("./spObj/spObj_sequence_of_lines_v",v_nr,".R",sep="")) 
      b13_angle_df2
      sequence_seg2 <- b13_angle_df3$nr_center 
      sequence_seg2 
    }
    
  } else {
    
      if (answ == "Y") { 
        #b13_angle_df
        #b13_angle_df2 <- b13_angle_df
        b13_angle_df3 <- b13_angle_df2
      }
    
  } #end if-else

  proc_mode
  answ
  bnr2
  #b13_angle_df3
  
  if (answ == "N" && proc_mode == "obj_wise" && part == "no_part" || 
      answ == "N" && proc_mode == "demo" && part == "no_part") {
      p_pos <- "cor_pos" #correction of position
      setwd(home_dir2)
      b13_angle_df2
      source(paste("./spObj/spObj_sequence_of_lines_v",v_nr,".R",sep="")) #sek="bdr_follow"
      b13_angle_df3
  } #end if 
  
  #b13_angle_df3
  sequence_seg2 <- b13_angle_df3$nr_center 
  
  #check plot
  #plot of PCs at large scale
  par(mai = c(1.02,0.82,0.82,0.42)) #setup of margins/plot region [inches]
  par("usr")
  r_max
  r_max2 <- round(1.1*r_max)
  plot(xc,-yc, pch=3, cex=2, col="blue", asp=1, xlim=c(xc - r_max2,xc + r_max2), ylim=c(-(yc + r_max2),-(yc - r_max2)),
  main=paste("b ",bnr2, sep=(""))) #large scale
  points(pc3$col, -pc3$row, pch=20, asp=1, cex=0.3, col="black") # original pixel cloud for building
  points(b13_angle_df3$x_centre,-b13_angle_df3$y_centre, asp=1, pch=20,col="red", cex=1.5)
  #plot of PCs and checkpoints
  #plot(coords$x,coords$y,pch=16,cex=0.2,col="brown",asp=1,xlim=c(xc-r_max2,xc+r_max2),ylim=c(yc+r_max2,yc-r_max2),xlab=NULL,ylab=NULL,ann=T,main=paste("b",bnr2),axes=TRUE)
  #plot(coords$x,coords$y,pch=16,cex=0.2,col="black",asp=1,xlim=c(1,1887),ylim=c(2557,1),xlab=NULL,ylab=NULL,ann=FALSE,main=paste("b",bnr2),axes=TRUE) #small scale
  points(xc+r_max, -(yc+r_max), pch=16, cex=1.5, col="black", asp=1) #point for scaling
  points(xc-r_max, -(yc+r_max), pch=16, cex=1.5, col="black", asp=1) #point for scaling
  points(xc-r_max, -(yc-r_max), pch=16, cex=1.5, col="black", asp=1) #point for scaling
  points(xc+r_max, -(yc-r_max), pch=16, cex=1.5, col="black", asp=1) #point for scaling
  points(xc, -yc, pch = 3, cex=1.5, col = "red", asp=1) #centre of PC
  
  n2_midpts <- length(b13_angle_df3$nr_center)
  
  #check of positions
  vec <- 1 : n2_midpts
  
  for (i in vec) { 
    readline("press 'enter' to display next midpoint ")
    cat("index=", i, "point_number= ", b13_angle_df3$nr_center[i],"\n")
    points(b13_angle_df3$x_centre[i],-b13_angle_df3$y_centre[i], asp=1, pch=20,col="red", cex=1.5)  
  }
  
  
  #transformation to img-system
  b13_angle_df3
  b_angle_df_seq_red <- b13_angle_df3
  n_x <- nrow(b_angle_df_seq_red) 
  #
  midpts_trans <- matrix(nrow=n_x,ncol=3)
  midpts_trans
  ynp <- 1 : n_x
  a0_bdr
  b0_bdr
  
  for (i in ynp) {
    x <- b_angle_df_seq_red$x_centre[i]
    y <- b_angle_df_seq_red$y_centre[i]
    v <- c((x-a0_bdr), -(y-b0_bdr))
    V <- D_bdr_inv  %*%  v
    V1 <- V*kf3
    midpts_trans[i,1] <- b_angle_df_seq_red$nr_center[i]
    midpts_trans[i,2] <- round(V1[1,1])
    midpts_trans[i,3] <- round(V1[2,1])
    as.integer(midpts_trans[i,2])
    as.integer(midpts_trans[i,3])
  } #end for-loop
  
  midpts_trans

  ##output of transformed line-midpoints
  setwd(home_dir)
  fname8 <- paste("./data/",Img_name,"/b",bnr2,"_midpts_trans2.txt",sep="")
  write.table(midpts_trans,fname8)

  ##find sequence of line segments
  midpts_trans
  midpoints <- midpts_trans
  midpoints 
  midpoints[,2:3] <- as.integer(midpoints[,2:3])
  midpoints
  #
  
  n_midpts <- length(midpoints[,1])
  y4 <- 1 : n_midpts
  
  #plot midpoints in 'out_poly'
  plot(out_poly)
  
  for (i in y4) {
    #browser()
    cat("midpoint_nr= ",midpoints[i,1],"\n")
    points(midpoints[i,2], midpoints[i,3], pch=16, col="red", cex=1.0, asp=1)
  }

  #search for line-centers in PC
  nr_midpts <- rep(0,500) # vector of labels for detected midpoints
  k_max=8 #pattern around midpoints: xi-k_max...xi...xi+k_max; yi-k_max...yi...yi+k_max, alternativ: 8 
  pat_size <- (2*(k_max) + 1)^2 #number of pixels in pattern

  #loop
  n <- 1
  
  #plot of PC
  for (i in y3) {
    #browser()
    points(x_v[i], y_v[i], pch=20, col="green", cex=0.1, asp=1) #points from edge
  }
  
  for (i in y3) {
    points(x_v[i], y_v[i], pch=".", col="blue", cex=0.1, asp=1) #points from edge
    for (j in y4) { #loop j
      X <- midpoints[j,2]
      Y <- midpoints[j,3]
      X <- as.integer(X)
      Y <- as.integer(Y)
      vec_x1 <- X + c(-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8)
      vec_y1 <- Y + c(-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8)
      
      for (ky in vec_y1) {
        #cat("ky= ",ky,"\n")
        
        for (mx in vec_x1) {
          if (x_v[i] == mx && y_v[i] == ky) {
            nr_midpts[n] <- midpoints[j,1]
            n <- n+1
          } #end if

        } #end loop mx
      } #end loop ky
    } #end loop j
  } #end loop i
  
  nr_midpts #numbers of PCs representing line segments
  k12 <- length(nr_midpts)
  
  #sequence of line segments
  sequence_seg <- rep(NA,n_midpts)
  i <- 1
  j <- 1
  
  while(i <= (k12-1)) { 
    if (nr_midpts[i] != nr_midpts[i+1] & nr_midpts[i] > 0) {
      sequence_seg[j] <- nr_midpts[i]
      j=j+1 
    } #end if
    i=i+1    
  } #end loop i
  
  sequence_seg[j] <- nr_midpts[k12]
  sequence_seg <- sequence_seg[!is.na(sequence_seg)]
  sequence_seg <- sequence_seg[sequence_seg > 0]
  n_seq <- length(sequence_seg)
  sequence_seg #vector of identified midpoints 
  n_midpts 
  n_seq
  #end of detection of midpts
  
  ##check of the identified points 
  if (n_seq != n_midpts) {
    cat("sequence is not correctly detected -> correct sequence manually","\n") #use 'spObj_sequence_of_line'
    p_pos <- "cor_sek" 
    setwd(home_dir2)
    sequence_seg
    source(paste("./spObj/spObj_sequence_of_lines_v",v_nr,".R",sep="")) #manual correction of sequence
    sequence_seg2
    sequence_seg <- sequence_seg2 #new
  } #end if 
  
  cat ("sequence = ",sequence_seg, "\n")   
  
  ##re-arrange matrix 'b13_angle_df4' to new sequence
  b13_angle_df3
  b13_angle_df4 <- b13_angle_df3
  b13_angle_df4[,] <- 0
  sequence_seg
  vec2 <- 1 : length(sequence_seg) 
  b13_angle_df4$nr_center <- sequence_seg
  b13_angle_df4
  vec2
  b13_angle_df4[,5] <- "new"
  names(b13_angle_df4)[5] <- "status"
  n_10 <- length(b13_angle_df4$nr_center)
  b13_angle_df4
  
  for (n1 in vec2) {
    i=1
    
    while (i <= n_10) { 
      if (b13_angle_df4[n1,1] == b13_angle_df3[i,1] && b13_angle_df4[i,5] != "done") { #point 1 2x
        b13_angle_df4[n1,2:4] <- b13_angle_df3[i,2:4]
        b13_angle_df4[i,5] <- "done"
        cat("n1= ",n1,"i= ",i, "\n")
        break
      } 
      i <- i+1
    }
  } #end of for-loop
  b13_angle_df4
  
  #plot of two points for the determination of direction of sequence
  plot(xc,-yc, pch=3, cex=2, col="blue", asp=1, xlim=c(xc - r_max2,xc + r_max2), ylim=c(-(yc + r_max2),-(yc - r_max2)),
       main=paste("b ",bnr2, sep=(""))) #large scale
  points(pc3$col, -pc3$row, pch=20, asp=1, cex=0.3, col="black") # original pixel cloud for building
  points(b13_angle_df4$x_centre[1],-b13_angle_df4$y_centre[1], asp=1, pch=20,col="red", cex=1.5)
  points(b13_angle_df4$x_centre[2],-b13_angle_df4$y_centre[2], asp=1, pch=20,col="green", cex=1.5)
  #
  
  #plot of all points 
  for (n1 in vec2) {
    readline("press 'enter' to display next point ")
    print(b13_angle_df4$nr_center[n1])
    points(b13_angle_df4$x_centre[n1],-b13_angle_df4$y_centre[n1], asp=1, pch=20,col="green", cex=1.5)
  }
  
  #change of direction in sequence
  sequence_seg_rev <- rep(NA,n_seq)
  b13_angle_df4
  dx_seq <- (b13_angle_df4$x_centre[1]-b13_angle_df4$x_centre[2])

  if (dx_seq < 0) {
    sequence_seg_rev <- changeDir(sequence_seg) #function call for change of direction in sequence
    sequence_seg_rev #reversed sequence
    sequence_seg <- sequence_seg_rev
  }
  
  sequence_seg
  
  ##re-arrange matrix 'b13_angle_df3' to new sequence
  b13_angle_df4
  b13_angle_df5 <- b13_angle_df4
  b13_angle_df5[,] <- 0
  sequence_seg
  vec2 <- 1 : length(sequence_seg)
  b13_angle_df5$nr_center <- sequence_seg
  b13_angle_df5
  b13_angle_df4[,5] <- "new"
  names(b13_angle_df4)[5] <- "status"
  n_10 <- length(b13_angle_df4$nr_center)
  b13_angle_df4

  for (n1 in vec2) {
    i=1

    while (i <= n_10) {
      if (b13_angle_df5[n1,1] == b13_angle_df4[i,1] && b13_angle_df4[i,5] != "done") {
         b13_angle_df5[n1,2:4] <- b13_angle_df4[i,2:4]
         b13_angle_df4[i,5] <- "done"
         cat("n1= ",n1,"i= ",i, "\n")
         break
      }
      i <- i+1
    } #end loop i
  } #end of for-loop
  
  b13_angle_df5
  
  #plot of midpts
  b13_angle_df5
  plot(xc,-yc, pch=3, cex=2, col="blue", asp=1, xlim=c(xc - r_max2,xc + r_max2), ylim=c(-(yc + r_max2),-(yc - r_max2)),
       main=paste("b ",bnr2, sep=(""))) #large scale
  points(pc3$col, -pc3$row, pch=20, asp=1, cex=0.3, col="black") # original pixel cloud for building
  points(b13_angle_df5$x_centre,-b13_angle_df5$y_centre, asp=1, pch=20,col="green", cex=1.5)
} #end sek = "bdr_follow" 

#end of route 'bdr-follow'
################################################################################

##end of sequence of line segments
##output of sequence of lines

if (sek == "Mpts") { 
  PC_nr <- b13_angle_df2_seq$nr_center
} #end sek = "Mpts"

if (sek == "Mpts+dist") {
  PC_nr <- nr2 
} #end if

if (sek == "bdr_follow") {
  PC_nr <- b13_angle_df5$nr_center
} #end if

PC_nr #solution for sequence of lines (PC)

##output
f4 <- paste("./data/",Img_name,"/b",bnr2,"_PC_nr.txt",sep="")
setwd(home_dir)
write.table(PC_nr,file=f4)

##preparation
x3 <- length(PC_nr)
all_PC <- list() #generation of a list
vec2 <- 1 : x3

for (i in vec2) {
  all_PC[[i]] <- "PC"
}

all_PC
cat("end of 'sequence of lines.R' - continue with 'adjustment_of_line.R' ","\n")
cat("####################################################################","\n")
setwd(home_dir2)
source(paste("adjustment_of_line_v",v_nr,".R",sep=""))

