##name of script: 'line_detection.R'
cat("version_number= ",v_nr,"\n") 
#description: separation of pixel clusters (PC) representing line segments 
#detecting of lines representing the selected object
#examples: ISPRS dataset 'Vaihingen', orthoimages #1, #7, results of ISPRS labeling benchmark (#4_SVL_5, #4_DRL10)
#instruction: check the ro-range and the main direction of object
#instruction: select 'fix ro-range: ro_rg=3' to process faster (see line 254 of script)
#the minimum lengths of the lines are defined by a default value (n_pix) 
#the default value (n_pix) may be adapted to the existing objects of the orthoimage
#use 'Zoom' for evaluation of position
#change eventually the default value for each object type ("extr_wd", "4_long", "100_all", "100_all+nonortho", "nonortho_only", "nonortho_only_RDP")
#if lines of other orientation than the main orientation are part of the object then specify their labels
#use scripts #8 and #9 of 'support_line_detection.R' 
#author: Joachim Höhle
#GNU General Public License (GPL)
cat("#########################################################################","\n")

cat("start of program 'line_detection.R'","\n")
setwd(home_dir)
##parameter
omega=180/pi #factor to convert from radiant to degree
#k=1.64 #approximate scale factor for length of line segment (empirically determined for ISPRS1 and ISPRS7)
k=1.5 #approximate scale factor for length of line segment (empirically determined for ISPRS4, ISPRS4_DLR10)
options(digits=8)

##input of point cluster (PC) of line segment
setwd(home_dir)
bnr2
f1=paste("./data/",Img_name,"/idxy_LCM_b",bnr2,".csv",sep="")
pc2<-read.table(f1, header=TRUE) #land cover map
head(pc2)
names(pc2)[1]<-"col"
names(pc2)[2]<-"row"
length(pc2$row)
nrow<-length(pc2$col)

##input plot-parameter
f1<-paste0("./data/",Img_name,"/param_b", bnr2, sep = "", collapse = NULL) 
load(f1)
xc <- plotPar[1]
yc <- plotPar[2]
r_max <- plotPar[3]
alpha <- plotPar[4] #approximate image angle of ellipse' main axis
alpha_math <- (-alpha) #change to math-system

##extreme coordinates
max(pc2$col)
max(pc2$row)
min(pc2$col)
min(pc2$row)

##plot of origin, center of coordinates system and 
#point cluster of extracted building (small scale, complete area)
#plot in small scale (orthoimages #7,#1,#4)

x = 0; y = 0 #origo

if (Img_name == "ISPRS7") {
  plot(x,-y, pch=3, cex=2, col="black", asp=1, xlim=c(0,1887), 
  ylim=c(-2557, 1000), main=paste("b",bnr2))
} 

if (Img_name == "ISPRS1") {
  plot(x,-y, pch=3, cex=2, col="black", asp=1, xlim=c(0,1919), 
  ylim=c(-2569, 1000))
}  

if (Img_name == "ISPRS4") {
  plot(x,-y, pch=3, cex=2, col="black", asp=1, xlim=c(0,600), 
       ylim=c(-813, 318))
}  

if (Img_name == "ISPRS4_DLR10") {
  plot(x,-y, pch=3, cex=2, col="black", asp=1, xlim=c(0,600), 
       ylim=c(-813, 318))
}  

points(xc,-yc, pch=3, cex=1.5, col="red", asp=1) #centre of PC
points(pc2$col,-pc2$row, pch=".", cex=1.5, col="blue", asp=1) #PC
# end plot in small scale

#plot of graph in large scale
par(mai = c(1.02,0.82,0.82,0.42)) #setup of margins/plot region [inches]
r_max2 <- 1.1*r_max
plot(xc,-yc, pch=3, cex=3, col="red", asp=1, xlim=c(xc-r_max2,xc+r_max2),
    ylim=c(-yc-r_max2,-yc+r_max2), xlab="col", ylab="row", main=paste("b",bnr2))
points(xc,-yc, pch=3, cex=1.5, col="red", asp=1) #center of PC
points(pc2$col,-pc2$row, pch=".", cex=1.5, col="blue", asp=1) #PC
#end of plot

## preparation for the Hough-transformation

#resolution of Hough-matrix:
theta_step <- 5 #step at angle [degrees]
cat("step at angle (theta)=",theta_step,"degrees","\n")
ro_step <- 5 #step in distance [pixel]
cat("step at distance (ro)= ",ro_step,"pixel","\n")

#extreme coordinates
(X_min=min(pc2$col))
(Y_min=min(pc2$row))
(X_max=max(pc2$col))
(Y_max=max(pc2$row))

##range of ro (Dis_min, Dis_max)
Dis_min <- sqrt(X_min^2+Y_min^2)
Dis_min <- as.integer(Dis_min)
Dis_max <- sqrt(X_max^2+Y_max^2)
Dis_max <- as.integer(Dis_max)

#range of angle 
theta <- seq(0,175, by=theta_step) #theta = theta_img
n_theta <- length(theta)
#

#cat("manual input is required","\n")

##select a ro_range (ro_rg)

#manual input into console
cat("ro_rg=0: range with Dis_min...Dis_max
ro_rg=1: range with 0...Dis_max (default for ISPRS7)
ro_rg=2: range calculated with angle between main axis of ellipse and x-axis (math. definition) 
ro_rg=3: range with -Dis_max...Dis_max (default ISPRS1) ")

#setting of ro_range
if (Img_name == "ISPRS1" && proc_mode == "demo") {
  ro_rg = 3
} #else {
#   if (Img_name == "ISPRS1") {
#     ro_rg <- readline("select ro_range: ") #type 1 or 2 or 3
#     ro_rg <- as.integer(ro_rg)
#   }
#}

if (Img_name == "ISPRS7" && proc_mode == "demo") {
  ro_rg = 1
} #else {
#   if (Img_name == "ISPRS7") {
#     ro_rg <- readline("select ro_range: ") #type 1 or 2 or 3
#     ro_rg <- as.integer(ro_rg)
#   }  
# }

if (Img_name == "ISPRS4" && proc_mode == "demo") {
  ro_rg = 3
} #else {
#   if (Img_name == "ISPRS4") {
#     ro_rg <- readline("select ro_range: ") #type 1 or 2 or 3
#     ro_rg <- as.integer(ro_rg)
#   }  
# }

if (Img_name == "ISPRS4_DLR10" && proc_mode == "demo") {
  ro_rg = 3
}

ro_rg = 3 #option: fix selection

cat("selected ro-range =", ro_rg,"\n")

#
if(ro_rg == 0) {
  ro <- seq(Dis_min,Dis_max,by=ro_step) #first solution of theta_ref
  n_ro <- length(ro)
  ro_1 <- ro[1]
  ro[n_ro]
  setwd(home_dir)
  save(theta_step, ro_step, ro, ro_1, n_theta, n_ro, ro_rg, file="H_par") #storage 
} #end ro_rg=0
#

if (ro_rg == 1) { # 1 
  ro <- seq(0, Dis_max, by=ro_step) 
  n_ro <- length(ro)
  ro_1 <- ro[1]
  ro[n_ro]
  
  #storage of Hough parameters
  setwd(home_dir)
  save(theta_step, ro_step, ro, ro_1, n_theta, n_ro, ro_rg, 
       file = paste("./data/",Img_name,"/H_par", sep="", collapse=NULL)) 
} #end of ro_rg = 1
#

if(ro_rg == 2) {
  #procedure with rotation of coordinate system with angle of fitted ellipse (building should have a longer side)
  #solution fails when building is a squared area
  
  if(alpha_math < 0) {
    alpha_math <- 180 + alpha_math
  }
  
  theta_appr <- alpha_math - 90
  d_safety = 0 #safety value in [pel] #default=50
  theta1 <- theta_appr
  theta1_arc <- theta1/omega
  theta2 <- theta1 + 90 
  theta2_arc <- theta2/omega
  alpha_rad <- alpha/omega
  max(pc2$col)
  max(pc2$row)
  min(pc2$col)
  min(pc2$row)
  X <- max(pc2$col) 
  Y <- (-max(pc2$row)) #change to math-system
  ro1_max <- cos(theta1_arc) * X + sin(theta1_arc) * Y
  ro2_max <- cos(theta2_arc) * X + sin(theta2_arc) * Y
  ro2_max <- ro2_max
  X <- min(pc2$col)
  Y <- (-min(pc2$row)) #change to math-system
  ro1_min <- cos(theta1_arc) * X + sin(theta1_arc) * Y
  ro2_min <- cos(theta2_arc) * X + sin(theta2_arc) * Y
  ro2_min <- ro2_min
  ro_range <- c(ro1_max, ro2_max, ro1_min, ro2_min) 
  ro_range
  max_ro <- as.integer(max(ro_range))
  min_ro <- as.integer(min(ro_range))
  min_ro2 <- as.integer(min_ro - d_safety)
  max_ro2 <- as.integer(max_ro + d_safety)
  ro <- seq(min_ro2,max_ro2,by=ro_step)
  ro
  n_ro <- length(ro)
  ro_1 <- ro[1]
  
  #plot graph
  alpha <- alpha_math 
  alpha_arc <- alpha/omega
  a = tan(alpha_arc)
  theta_ang <- alpha + 90
  theta_arc <- theta_ang/omega
  ro_0 <- cos(theta_arc) * xc + sin(theta_arc) * (-yc)
  b <- ro_0/(cos(alpha_arc))
  coef <- c(b,a)
  
  if(is.finite(b)){
    abline(coef, col="red", lty=1, lwd=2, asp=1)
  } else {
     ro_l1 <- ro_0
     lines(c(ro_l1,ro_l1),c(0,-2500))
  } #end if-else
  
  save(theta_step, ro_step, ro, ro_1, n_theta, n_ro, ro_rg, 
       file=paste("./data/",Img_name,"/H_par", sep="", collapse=NULL)) #storage of Hough parameters

} #end ro_rg=2 incl. plot

#
if (ro_rg == 3) { #default value
  Dis_min2 <- 2*(-Dis_max)
  Dis_max2 <- 2*Dis_max
  ro <- seq(Dis_min2, Dis_max2, by=ro_step) 
  n_ro <- length(ro)
  ro_1 <- ro[1]
  ro[n_ro]
  
  #storage of Hough-parameters
  setwd(home_dir)
  save(theta_step, ro_step, ro, ro_1, n_theta, n_ro, ro_rg, 
       file = paste("./data/",Img_name,"/H_par", sep="", collapse=NULL)) 
} #end of ro_rg = 3

#end of range selection

#is alpha realistic?
#if not - find solution in 'support_line_detection.R'

##start of Hough-transform
theta_rad <- theta/omega
H <- array(dim=c(n_theta, n_ro)) # init Hough-matrix
dim(H)

## extracting separated point clusters (P)
# setting H to zero
i <- 0

while (i < n_theta) {
  i <- i + 1
  H[,] <- 0
}

##display of parameter space
ratio <- (n_ro - 1)/(ro[n_ro] - ro[1])
X <- pc2$col
Y <- pc2$row
ro2 <- rep(0,n_ro)
ro_index <- rep(0,n_ro) #?
n_X <- length(pc2$col)
P <- matrix(nrow=n_X, ncol=3)
ro_ind_end <- round(ratio*(ro[n_ro]-ro_1)) +  1

##loop for all points
lnr = 1 #line number
P[,] <- 0
k1 <- 0
k3 = 1

while (k1 < n_X){
  k1 <- k1+1
  i <- 0
  while (i < n_theta) { 
    i <- i+1
    ro2[i] <- cos(theta_rad[i])*X[k1] + sin(theta_rad[i])*Y[k1]
    ro_index[i] <- round(ratio*(ro2[i] - ro_1) + 1) #calculation of ro_index
    if (ro_index[i] >= 1 && ro_index[i] <= ro_ind_end ) { 
      k2 <- ro_index[i]
      H[i,k2] <- H[i,k2] + 1 #storage of points belonging to a line
    } #end ro index
  } #end loop i
} #end loop k1

n_P <- (k3 - 1)

##Generation of point cloud
antal <- n_theta * n_ro
pointcloud <- matrix(nrow=antal,ncol=3)
i <- 0

while (i < n_theta) {
  i <- i+1
  j <- 0
  
  while (j < n_ro) {
    j <- j+1
    n1 <- n_ro*(i - 1) + j
    pointcloud[n1,1] <- i
    pointcloud[n1,2] <- j 
    pointcloud[n1,3] <- H[i,j]
  } #end loop j
  
} #end loop i
#end generation of pixel cluster (PC)

A <- pointcloud
B <- A[order(A[,3],decreasing = TRUE),] #ordered point cloud

##end of Hough-trans

##storage of matrix B (10 longest lines)
B[1:10,] 
f1 <- paste("./data/",Img_name,"/parameter_space_b",bnr2,".txt",sep="")
write.table(B,f1)

##test of main direction
#does first (longest) line have an orthogonal line 
#(theta_ind + 90 or theta_ind -90) ?
B1 <- B
cat("detected line segments (theta_index, ro_index, N), ordered with respect to 
    length of line (N):","\n")
#k ~ 1.64 (determined empirically) 
#B2 <- subset(B1,B1[,3] >= 80) # 56*k 
#B2 <- subset(B1,B1[,3] >= 57) # 35*k 
#B2 <- subset(B1,B1[,3] >= 41) # 25*k  
#B2 <- subset(B1,B1[,3] >= 20) # 12*k 
#B2 <- subset(B1,B1[,3] >= 10) # 6*k 
#B2 <- subset(B1,B1[,3] >= 5) # 3*k 

#k ~ 2.31) (determined empirically) 
#B2 <- subset(B1,B1[,3] >= 130) # 56*k 
#B2 <- subset(B1,B1[,3] >= 81) # 35*k 
#B2 <- subset(B1,B1[,3] >= 58) # 25*k  
#B2 <- subset(B1,B1[,3] >= 28) # 12*k 
#B2 <- subset(B1,B1[,3] >= 23) # 10*k 
#B2 <- subset(B1,B1[,3] >= 14) # 6*k 
#B2 <- subset(B1,B1[,3] >= 7) # 3*k 

#thr_line_seg <- 15 #alternatives: 10,20 [units] ca. 15/k=9.1 pixel (ISPRS1, ISPRS7)
thr_line_seg <- 15 #alternatives: 10,20 [units] ca. 15/k=6.5 pixel = 1.84m (ISPRS4, ISPRS_DLR10) 
B2 <- subset(B1,B1[,3] >= thr_line_seg)  
nrow(B2)
head(B2)
ind_max <- max(B2[,1],na.rm = FALSE) #theta_index
ind_min <- min(B2[,1],na.rm = FALSE) #theta_index
dim(B2)
cat("dimB2= ",dim(B2),"\n")

##loop for check plot of extracted pixel clusters (optional)
#instruction: change to 'Plots' and '<-'

#plot of longest point cluster (PC) representing a line
plot(xc,-yc, pch=3, cex=3, col="red", asp=1, 
     xlim=c(xc-r_max2,xc+r_max2), ylim=c(-yc-r_max2,-yc+r_max2), 
     xlab="col", ylab="row",main=paste("b",bnr2))
points(pc2$col,-pc2$row, pch=".", cex=1.5, col="blue", asp=1) #PC

lnr <- 1 #longest PC
PC_seg_P_nP <- PC_segment_4(lnr)
P <- PC_seg_P_nP[[1]] 
n_P <- PC_seg_P_nP[[2]]
setwd(home_dir)
f<-paste("./data/",Img_name,"/b",bnr2,"_",lnr,".txt",sep="")
P <- P[1:n_P,]
write.table(P,file=f, sep="   ")
#

## plotting onto graph (large scale)
r_max2 <- round(1.1*r_max)
plot(xc,-yc, pch=3, cex=3, col="green", asp=1, 
     xlim=c(xc-r_max2,xc+r_max2), ylim=c(-yc-r_max2,-yc+r_max2), 
     xlab="col", ylab="row",main=paste("b",bnr2))
points(pc2$col,-pc2$row, pch=".", cex=1.5, col="blue", asp=1) #PC
points(P[,2],-P[,3], pch=".", asp=1, cex=3.0, col="green") #switch to 'Plots' to see plot
#end plot of PC and ref-line

head(P)

##plot of center and PC of outline at large scale (graph)
pc3 <- pc2
r_max2 <- 1.1 * r_max
plot(xc,-yc, pch=3, cex=1.5, col="red", asp=1, xlim=c(xc-r_max2,xc+r_max2), ylim=c(-(yc+r_max2),-(yc-r_max2)),main=paste("b ",bnr2, sep=(""))) #large scale
points(pc3$col, -pc3$row, pch=20, asp=1, cex=0.5, col="green")

#plot of reference line (if building's form is long)

##transformation of B-parameters into angles and pixels
#selected parameters in Hough-transform

##calculation of scale factor kf (antal pixel in PC->length of line 1)
lnr <- 1 # number of a long and continuous line in Hough-transform (default: line 1)
fname <- paste("./data/",Img_name,"/b",bnr2,"_",lnr,".txt", sep="")
P <- read.table(fname) #point cloud of first line
head(P)
names(P) <- c("idx","x","y")
nrow1 <- nrow(P)
points(P[,2],-P[,3], pch=20, asp=1, cex=0.5, col="red") #point cloud of first line (reference line)
#

##plot of corrected point cluster of line #1
lnr
P_red <- reduce_pointset(P)
points(P_red[,2],-P_red[,3], pch='.', asp=3, cex=3, col="blue")
x_max=max(P_red[,2]) #change to first and last point
x_min=min(P_red[,2])
y_max=max(P_red[,3])
y_min=min(P_red[,3])
d_line <- sqrt((x_max-x_min)^2+(y_max-y_min)^2) #length of line [pixel]
#

setwd(home_dir)
fname6 <- paste("./data/",Img_name,"/parameter_space_b",bnr2,".txt", sep="")
B0 <- read.table(fname6, row.names=NULL) #result of Hough transform
options(digits = 8)
names(B0) <- c("lnr","theta_index","ro_index","N")
B0[1:10,]
kf <- B0[lnr,4]/d_line #scale factor to convert into number of pixel

if (kf < 1) {
  kf = k
}
#end of calculation of the scale factor 'kf'

#stop("check of H_para")
wd <- 10 #width of building [pixels] (alternative: 15) #changed from 10 to n_pix
#B22 <- subset(B0, round(B0[,4]/kf) >= wd) # minimum length of side (15 pixel= 1.4m) for ISPRS1,ISPRS7
B22 <- subset(B0, round(B0[,4]/kf) >= wd) # minimum length of side (10 pixel = 2.8m) for ISPRS4,ISPRS_DLR10
n1 <- length(B22[,1])
B3 <- matrix(nrow=n1,ncol=7)
B3[,1:4] <- 0
head(B3)

##completion of matrix B3

#loop
ro_1
vec <- 1 : n1

#transformation
for(i in vec) {
  H_para <- trans_H_res(B22,theta_step,ro_step,ro_1,kf) 
  theta_index <- H_para[1,] #is theta_ind?
  ro_index <- H_para[2,] #is ro_ind?
  N <- H_para[3,]
  theta_angle <- H_para[4,]
  ro_pixel <- H_para[5,]
  n <- H_para[6,]
  B3[i,] <- c(i,theta_index, ro_index, N, theta_angle,ro_pixel,round(n))
} #end loop for(i in vec) 

head(B3)

##generation of data frame
B4 <- data.frame(B3)
names(B4) <- c("lnr", "theta_index", "ro_index", "n", "theta_angle","ro_pixel","n_pixel")
head(B4)
print(B4[1:10,])
#

#derivation of point clouds for first 10 lines
#stop("check device")

vec <- 1 : 10

for (n1 in vec) {
  PC_seg_P_nP <- PC_segment_4(n1)
  P <- PC_seg_P_nP[[1]] 
  n_P <- PC_seg_P_nP[[2]]
  setwd(home_dir)
  f<-paste("./data/",Img_name,"/b",bnr2,"_",n1,".txt",sep="")
  P <- P[1:n_P,]
  write.table(P,file=f, sep="   ")
} #end for(i in vec)
#
pc3 <- pc2
r_max2 <- 1.1 * r_max
plot(xc,-yc,pch=3,cex=1.5,col="red",asp=1,xlim=c(xc-r_max2,xc+r_max2),ylim=c(-(yc+r_max2),-(yc-r_max2)),main=paste("b ",bnr2,sep=(""))) #large scale
points(pc3$col, -pc3$row, pch=20, asp=1, cex=0.5, col="green")

##plot of first point clouds (n_long_PCs) of Hough-transform
cat("choose a reference line from the selected number of longest lines","\n") 
vec <- 1 : n_long_PCs #can be changed

for (n1 in vec) {
  readline("press 'enter' to display next line ")
  points(pc3$col, -pc3$row, pch=20, asp=1, cex=0.5, col="green")
  cat("lnr= ",n1 ,", theta_angle= ",B4$theta_angle[n1],"degrees", "\n")
  fname <- paste("./data/",Img_name,"/b",bnr2,"_",n1,".txt", sep="")
  P <- read.table(fname) #point cloud of first line
  head(P)
  names(P) <- c("idx","x","y")
  nrow1 <- nrow(P)
  n_PC2 <- round(nrow1/kf) 
  cat("number of pixels=",n_PC2,"\n")
  points(P[,2],-P[,3], pch=20, asp=1, cex=0.5, col="blue") #point cloud of first line (reference line)
} # end for-loop

## selection of line as reference line (ref-line for orientation/angle)
# consider manual determination of ref-line (see: support_line_detection.R)

if (Img_name == "ISPRS7" & proc_mode == "demo") {
  lnr=1
} else {
  if (Img_name == "ISPRS7" && proc_mode != "demo") {
    lnr <- readline("type reference line number: ") 
    lnr <- as.integer(lnr)
  }  
}

if (Img_name == "ISPRS1" && proc_mode == "demo") { 
  lnr=2
} else {
  if (Img_name == "ISPRS1" && proc_mode != "demo") { 
    lnr <- readline("type reference line number: ") 
    lnr <- as.integer(lnr)
  }  
}

if (Img_name == "ISPRS4" && proc_mode == "demo") {
  lnr=1
} else {
  if (Img_name == "ISPRS4" && proc_mode != "demo") {
    lnr <- readline("type reference line number: ") 
    lnr <- as.integer(lnr)
  }  
}

if (Img_name == "ISPRS4_DLR10" && proc_mode == "demo") {
  lnr=1
} else {
  if (Img_name == "ISPRS4_DLR10" && proc_mode != "demo") {
    lnr <- readline("type reference line number: ") 
    lnr <- as.integer(lnr)
  }  
}

lnr_ref <- lnr
theta_ref_ind <- B4[lnr,2] 

##test for existence of main ortho-lines

#histogram
b_v <- as.numeric(B4[,2])
vbr <- 0.5 : 36.5
hn <- hist(b_v,breaks=vbr,plot=TRUE) #plot of histogram b_v

if (theta_ref_ind <= 18) { 
  alph_ref_ind <- theta_ref_ind + 18 
} else { #alph_ref_ind = angle of orthogonal line
  alph_ref_ind <- theta_ref_ind - 18
} #end if-else

hn$counts[theta_ref_ind]
cat("number of counts at theta_ref_ind= ",hn$counts[(theta_ref_ind)],"\n")
hn$counts[alph_ref_ind]
cat("number of counts at alph_ref_ind= ",hn$counts[alph_ref_ind],"\n")

if (hn$counts[(alph_ref_ind)] < 2 || hn$counts[(theta_ref_ind)] < 2) {
  cat("warning: two ortho_lines of length >= 15 pixel (~1.4m for ISPRS data) do not exist!","\n")
  #stop("stop -> select new ref-line from B4[,2]")
}

##plot of selected reference line (lnr_ref)
L_new  <- PC_segment_4(lnr_ref)  
P <- L_new[[1]]
n_P <- L_new[[2]]
P <- P[1:n_P,]
P <- as.data.frame(P)
names(P) <- c("idx","x","y")
P_red <- reduce_pointset(P) #new
head(P_red)
x_m <- mean(P_red[,2])
y_m <- mean(-P_red[,3]) #change to math-system
plot(xc,-yc, pch=3, cex=3, col="red", asp=1, xlim=c(xc-r_max2,xc+r_max2),
     ylim=c(-yc-r_max2,-yc+r_max2), xlab="col", ylab="row", main=paste("b",bnr2))
points(pc3$col, -pc3$row, pch=20, asp=1, cex=0.5, col="green")
points(P[,2],(-P[,3]), pch=".", asp=1, cex=2.0, col="blue") #see 'Plots' (plot))
points(P_red[,2],(-P_red[,3]), pch=".", asp=1, cex=2.0, col="red") #see 'Plots' (plot)
points(x_m, y_m, pch=16, asp=1, cex=1.0, col="blue")

#plot of ref-line of Hough-trans results onto graph (math-system)
theta_math <- 180 - B4$theta_angle[lnr_ref]
#theta_math <- 360 - B4$theta_angle[lnr_ref]

if (theta_math >= 180) {
  theta_math <- theta_math - 180
}

cat("theta_math= ", theta_math,"\n")
a <- -1/tan(theta_math/omega)
cat("a=",a,"\n")
x <- x_m
y <- y_m 
p2 <- round(x*cos(theta_math/omega) + y*sin(theta_math/omega))
b <- round(p2/sin(theta_math/omega))
cat("b= ", b, "\n")
coef = c(b,a)

#plot onto graph
if(is.finite(a)) {
  abline(coef, col="red", lty=1, lwd=2, asp=1) #ref line 
} else {
  ro_l1 <- B4$ro_pixel[lnr] #changed
  lines(c(ro_l1,ro_l1),c(0,-2569),col="red",lty=1,lwd=2,asp=1) #(ISPRS1), ref/th1
  lines(c(ro_l1,ro_l1),c(0,1919),col="red",lty=1,lwd=2,asp=1) #(ISPRS1), orth/th2
} # end of plotting reference-line

##analyzing the Hough-matrix (B4)

#search for parallel and orthogonal lines based on reference line (ref) 
#select ref line, usually the first (longest) line
#joint search for parallel and orthogonal lines with 'theta_angle' and 'theta_angle-90' degrees
#minimum length of line segment (wd=lol): 10 pixels
#stop("manual test")

k13 <- nrow(B4)
theta_ref <- B4$theta_angle[lnr_ref] #reference line
ro_ref <- B4$ro_pixel[lnr_ref]
theta_ref_ind <- B4$theta_index[lnr_ref] #changed
cat("theta_ref=",theta_ref,"degrees","\n")
setwd(home_dir)
f1 <- paste("./data/",Img_name,"/th_ref_",bnr2,sep="")
save(theta_ref, file=f1) #main direction
alph_ref <- theta_ref - 90

if (alph_ref < 0) { #alph_ref has to be positive
  alph_ref <- alph_ref + 180
}

cat("alph_ref=", alph_ref,"degrees","\n")
alph_ref_ind <- alph_ref/theta_step + 1
alph_ref_arc <- alph_ref/omega
theta_ref_arc <- theta_ref/omega
r_max <- plotPar[3]
lol <- wd # minimum length of line segment [pixels]
i=0
B5_2 <- matrix(nrow=k13,ncol=7)
B5_2[,1:7] <- 0
k1=1

#loop
while (i < k13) {
  i <- i + 1
  if (B4$theta_angle[i] == theta_ref && B4$n_pixel[i] >= lol || 
      B4$theta_angle[i] == alph_ref && B4$n_pixel[i] >= lol) {
    B5_2[k1,] <- c(B4$lnr[i],B4$theta_index[i], B4$ro_index[i], B4$n[i], 
      B4$theta_angle[i],B4$ro_pixel[i],B4$n_pixel[i])
    k1 <- k1 + 1
  }
} #end of loop while

## generalizing, reduction of matrix, conversion to data frame
head(B5_2)
B5_3 <- subset(B5_2,B5_2[,7] >= lol) # length of lines (lol) >= 10 pixels (=0.9 m) (n_pixel)
B5_3
B5_4 <- data.frame(B5_3)
names(B5_4) <- c("lnr", "theta_index", "ro_index", "n", "theta_angle","ro_pixel","n_pixel")
head(B5_4)
B5_4
B5_4_ord <- B5_4[order(B5_4$ro_pixel,decreasing = F),]
k16 <- nrow(B5_4_ord)
row.names(B5_4_ord) <- 1 : k16
B5_4_ord

## Search of lines

## check of building outline by means of orthoimage
setwd(OrgImgPathname)
img_ref <- readImage(OrgImgFilename)
img_x_max <- dim(img_ref)[1]
img_y_max <- dim(img_ref)[2]
display(img_ref,method = "raster")
points(pc3$col, pc3$row, pch=20, asp=1, cex=0.2, col="white")
points(xc,yc,pch=3, asp=1, cex=1.0, col="red")

## display of PC and orthoimage in large scale
orig_x<-as.integer(xc - 1.2 * r_max) #r_max is of variable size
orig_y<-as.integer(yc - 1.2 * r_max)

if (orig_x < 0) { #solves problems at edges of orthoimage
  orig_x = 0
}

if (orig_y < 0) {
  orig_y = 0 
}

orig_x
orig_y
wind_x <- as.integer(orig_x + 2.4 * r_max)
wind_y <- as.integer(orig_y + 2.4 * r_max)

##test for extension of maximum image format

if (wind_x > img_x_max) {
  wind_x <- img_x_max
}

if (wind_y > img_y_max) {
  wind_y <- img_y_max
}

## display enlarged ortho-image and PC of building-outline
img_uds <- img_ref[orig_x:wind_x, orig_y:wind_y, 1:3]
display(img_uds,method = "raster")
#display(img_uds,method = "browser") #enables zooming
points(xc-orig_x,yc-orig_y,pch=3, asp=1, cex=1.3, col="red")
points(as.integer(pc3$col-orig_x), as.integer(pc3$row-orig_y), pch=20, asp=1, cex=0.2, col="green")

##plot of pixel cloud (PC) in enlarged orthoimage (image-system)
n_lnr <- lnr_ref 

#loop
lnr <- lnr_ref

while (lnr <= n_lnr) { #plot of lnr_ref
  cat("lnr= ", lnr, "\n")
  L_new  <- PC_segment_4(lnr) #change to 'PC_segment_4(i)' if several lines must be plotted
  P <- L_new[[1]]
  n_P <- L_new[[2]]
  P <- P[1:n_P,]
  P <- as.data.frame(P)
  names(P) <- c("idx","x","y")
  P_red <- reduce_pointset(P) #new
  head(P_red)
  x_m <- mean(P_red[,2])
  y_m <- mean(P_red[,3]) 
  points((P_red[,2]-orig_x),(P_red[,3]-orig_y), pch=".", asp=1, cex=1.0, col="blue") #see 'Plots' (plot)
  points(x_m-orig_x, y_m-orig_y, pch=16, asp=1, cex=1.0, col="red")
  lnr <- lnr + 1
} #end loop while

##plotting of approximate ref-line in orthoimage (small scale)

# check of building outline by means of orthoimage (small scale)
setwd(OrgImgPathname)
img_ref <- readImage(OrgImgFilename)
img_x_max <- dim(img_ref)[1]
img_y_max <- dim(img_ref)[2]
display(img_ref,method = "raster")
points(pc3$col, pc3$row, pch=20, asp=1, cex=0.2, col="white")
points(xc,yc,pch=3, asp=1, cex=1.5, col="red")

#window
fr <- matrix(nrow=5, ncol=2)
orig_y <- abs(orig_y)
fr[1,1] <- orig_x
fr[1,2] <- orig_y
fr[2,1] <- wind_x
fr[2,2] <- orig_y
fr[3,1] <- wind_x
fr[3,2] <- wind_y
fr[4,1] <- orig_x
fr[4,2] <- wind_y
fr[5,1] <- orig_x
fr[5,2] <- orig_y
lines(fr, type="l", asp=1, lwd=2, lty=1, col="yellow")

#plot of ref line onto orthoimage (small scale)
theta_math <- 180 - B4$theta_angle[lnr_ref]
theta_math #theta_ref_math
cat("theta_math= ", theta_math,"\n")
ro_ref <- B4$ro_pixel[lnr_ref] 
p <- ro_ref
x <- x_m
y <- -y_m #y in math_system
p2 <- round(x*cos(theta_math/omega) + y*sin(theta_math/omega))
alpha <- theta_math - 90
a <- tan(alpha/omega)
gamma <- 90 - alpha 
b <- round (p2/sin(gamma/omega)) 
cat("b= ", b, "\n")

#change to image-system
a_img <- (-a)
b_img <- (-b)
coef=c(b_img,a_img)
#

if (is.finite(b)) {
  abline(coef, col="green", lty=1, lwd=2, asp=1) #plot in small scale
} else {
  ro_l1 <- (-p2) #change to img-system
  ro_l2 <- ro_l1 
  lines(c(ro_l2,ro_l2),c(0,img_y_max),col="blue",lty=1,lwd=1,asp=1) 
}

#large scale (checking of special lines)
n_lnr <- lnr_ref

#loop
lnr=lnr_ref

while (lnr <= n_lnr) { #plot of lnr_ref
  display(img_uds, "raster")
  points(pc3$col-orig_x, pc3$row-orig_y, pch=20, asp=1, cex=0.2, col="white")
  points(xc-orig_x,yc-orig_y,pch=3, asp=1, cex=1.5, col="red")
  cat("lnr= ", lnr,"\n")
  L_new  <- PC_segment_4(lnr) #change to 'PC_segment_4(i)' if several lines must be plotted
  P <- L_new[[1]]
  n_P <- L_new[[2]]
  P <- P[1:n_P,]
  #
  P <- as.data.frame(P)
  names(P) <- c("idx","x","y")
  #stop("manual action")
  P_red <- reduce_pointset(P) 
  #head(P)
  head(P_red)
  #x_m <- mean(P[,2])
  x_m <- mean(P_red[,2])
  cat("x_m=", x_m,"\n")
  #y_m <- mean(P[,3])
  y_m <- mean(P_red[,3])
  cat("y_m=", y_m,"\n")
  points((P_red[,2]-orig_x),(P_red[,3]-orig_y), pch=".", asp=1, cex=1.0, col="red") #see 'Plots' (plot)
  #points((P[,2]-orig_x),(P[,3]-orig_y), pch=".", asp=1, cex=1.0, col="blue") #see 'Plots' (plot)
  points(x_m-orig_x, y_m-orig_y, pch=16, asp=1, cex=1.0, col="red")
  
  #plotting of line
  theta_math <- 180 - B4$theta_angle[lnr]
  theta_math #theta_ref_math
  p <- B4$ro_pixel[lnr] #+ ro_1
  x <- x_m
  y <- -y_m #y in math_system
  p2 <- round(x*cos(theta_math/omega) + y*sin(theta_math/omega))
  alpha <- theta_math - 90
  a <- tan(alpha/omega)
  gamma <- 90 - alpha 
  b <- round (p2/sin(gamma/omega)) 
  cat("b= ", b, "\n")
  b_math <- b 
  
  #calculation of intercept (b2) at image extract
  orig_y_math <- (-orig_y) #change to math-system
  b2_math <- b_math - orig_y_math + a*orig_x
  cat("b2_math=", b2_math, "\n")
  
  #change to image-system
  b2_img <- round(-b2_math)
  a_img <- (-a)
  coef2 <- c(b2_img,a_img)
  
  if (is.finite(a) && is.finite(b)) {
    abline(coef2, col="green", lty=1, lwd=2, asp=1)
  } else {
    ro_l1 <- B4$ro_pixel[lnr]
    ro_l3 <- round(ro_l1 - orig_x)
    lines(c(ro_l3,ro_l3),c(0,(wind_y-orig_y)),col="red",lty=1, lwd=2, asp=1)
    lines(c(ro_l3,ro_l3),c(0,(wind_x-orig_x)),col="red",lty=1, lwd=2, asp=1)
  } #end if-else
  
  lnr <- lnr + 1
} #end loop while

#parameters of longest detected lines
n_longest_lines <- 8 #number of longest lines after Hough trans (default)
x1 <- B4$theta_index[1 : n_longest_lines]
ce <- matrix(nrow=10, ncol=2)
ce_df <- data.frame(ce)
names(ce_df) <- c("lnr","counts")
ce_df$counts <- 0
#

y1 <- B4$theta_index[1:n_longest_lines]
n1 <- length(y1)
y2 <- rep(0,n1)
y1 <- y1[order(y1,decreasing = F)]
y2[1] <- y1[1]
k <- 2
i <- 2

#loop
while (i <= n1)  {
  
  if (y1[i] != y2[k-1]) {
    y2[k] <- y1[i]
    k <- k + 1
  } #end if
  
  i <- i + 1
} #end while-loop

y2 <- subset(y2,y2>0)
y2
n_y2 <- length(y2)
ce_df <- ce_df[1 : n_y2,]
ce_df$counts
k=1

#loop
for (j in y2) {
  for (i in x1) {
    if (j == i) {
      ce_df$counts[k] <- ce_df$counts[k] + 1
      ce_df$lnr[k] <- j
    } #end if
  } #end loop i
  k <- k+1
} #end loop j
ce_df
ce_df$lnr

#derivation of parameter (ces) for number of orthogonal lines
ces <- 0 #number of orthogonal lines in the first 8 lines at B4 
i <- 1

#loop
while (i <= length(ce_df$lnr)) {
  if (ce_df$lnr[i] == theta_ref_ind || ce_df$lnr[i] == alph_ref_ind) {
    ces <- ces + ce_df$counts[i]
  } #end if
  i <- i+1
} #end while-loop

n_ortholines_1 <- ces
n_nonortholines <- n_longest_lines - n_ortholines_1 #number of non-ortholines at first 8 lines
max_pix <- B4$n_pixel[n_longest_lines] #size of 8th PC

##parameters for estimating of object-type:
cat("max_pix= ", max_pix,"\n") 
cat("n_ortholines_1= ",n_ortholines_1,"\n")
cat("n_nonortholines= ", n_nonortholines,"\n") #number of non-ortholines 
#

##determination of object-type
ty <- fe(max_pix,n_ortholines_1,n_nonortholines) #ty-value is an estimate
cat("suggestion for object type=",ty, "\n")
cat("object types (cas): 1 (extr_wd), 2 (4_long), 3 (100_all), 4 (100_all+nonortho), 5 (nonortho_only), 6 (nonortho_only_RDP)","\n")

if (ty == 0) {
  cat("suggestion is 4", "\n") 
}

if (proc_mode == "demo" && Img_name == "ISPRS4") { 
  ty=2
}  

if (proc_mode == "demo" && Img_name == "ISPRS4_DLR10") { 
  ty=1
}   

if (proc_mode == "demo" && Img_name == "ISPRS1" || 
    proc_mode == "demo" && Img_name == "ISPRS7") { 
  ty=4
} 

if (proc_mode != "demo") {
  ty <- readline("type object type= ") #manual input
  ty <- as.integer(ty)
}

cas <- switch(ty,"extr_wd","4_long","100_all",
        "100_all+nonortho","nonortho_only","nonortho_only_RDP")
cat("case= ", cas, "\n")
###########################################################################################################

## Search of orthogonal lines -> specified length (wd)

if (cas == "extr_wd") {
  k14 <- length(B5_4$lnr)
  n_pix <- 5 #alternative: 15 [pixel] - new: select minimum length of line segment n_pixel = n_meter/pixel size
  wd <- n_pix #length of shortest line segment [pixel] or 15*0.09 = 1.4m (search for small lines)
  B5_4b <- B5_4
  B5_4b[,1:7] <- 0
  B5_4b[1,] <- B5_4[1,]
  i=1
  k1=2
  
  #loop
  while (i < k14) {
    i=i+1
    
    if (B5_4$theta_angle[i] == theta_ref && B5_4$n_pixel[i] >= wd ||
        B5_4$theta_angle[i] == alph_ref && B5_4$n_pixel[i] >= wd) {
      B5_4b[k1,] <- B5_4[i,]
      k1 <- k1+1
    } #end if
    
  } #end loop
  
  B5_4b
  B5_4c <- subset(B5_4b,B5_4b$lnr > 0)
  B5_4c # matrix with lines longer than 'wd'
  k15 <- length(B5_4c$lnr)
  
  ## Search of lines with theta_ref
  vec <- 1:k15
  B5_4d <- B5_4c
  B5_4d[,1:7] <- 0
  
  #loop
  j <- 1
  for (i in vec){
    
    if (B5_4c$theta_angle[i] == theta_ref) {
      B5_4d[j,] <- B5_4c[i,]
      j <- j + 1
    } #end if
    
  } # end for-loop
  
  B5_4d <- subset(B5_4d,B5_4d$n_pixel >= wd)
  B5_long_lines <- B5_4d[1:2,]
  B5_4d_ord <- B5_4d[order(B5_4d[,6],decreasing=FALSE),]
  B5_4d_ord2 <- subset(B5_4d_ord,B5_4d_ord$n_pixel >= wd)
  B5_4d_ord2
  
  ## Search of lines with alph_ref
  vec <- 1 : k15
  B5_4dd <- B5_4c
  B5_4dd[,1:7] <- 0
  j = 1
  
  #loop
  for (i in vec) {
    
    if (B5_4c$theta_angle[i] == alph_ref) {
      B5_4dd[j,] <- B5_4c[i,]
      j = j + 1
    } #end if
    
  } #end for-loop

  B5_4dd
  B5_4dd <- subset(B5_4dd, B5_4dd$n_pixel >= wd)
  B5_long_lines[3:4,] <- B5_4dd[1:2,] # matrix for weighted mean of angle
  B5_long_lines
  min(B5_long_lines$n_pixel)
  B5_4dd_ord <- B5_4dd[order(B5_4dd[,6],decreasing=FALSE),]
  B5_4dd_ord
  
  ## Find extreme values in ro_pixel
  ro_ma <- max(B5_4d_ord$ro_pixel)
  ro_mi <- min(B5_4d_ord$ro_pixel)
  B5_4e <- B5_4d
  B5_4e[,1:7] <- 0
  k5 <- length(B5_4d$lnr)
  vec2 <- 1:k5
  j=1
  
  #loop
  for (i in vec2) {
    
    if (B5_4d$ro_pixel[i] == ro_ma || B5_4d$ro_pixel[i] == ro_mi) {
      B5_4e[j,] <- B5_4d[i,]
      j = j + 1
    } #end if
    
  } #end for-loop
  
  B5_4e <- subset(B5_4e,B5_4e$lnr > 0)
  B5_4e
  #
  
  ## Find extreme values for orthogonal lines
  vec <- 1 : k15
  B5_4f <- B5_4c
  B5_4f[,1:7] <- 0
  j=1
  
  #loop
  for (i in vec) {
    
    if (B5_4$theta_angle[i] == alph_ref) {
      B5_4f[j,] <- B5_4c[i,]
      j = j + 1
    } #end if
    
  } #end for-loop

  B5_4f <- subset(B5_4f,B5_4f$n_pixel >= wd)
  B5_4f
  #
  
  ## find extreme values
  B5_4e2 <- B5_4c
  B5_4e2[,1:7] <- 0
  ro_ma2 <- max(B5_4f$ro_pixel)
  ro_mi2 <- min(B5_4f$ro_pixel)
  k5 <- length(B5_4f$lnr)
  vec2 <- 1:k5
  j = 1
  
  #loop
  for (i in vec2) {
    
    if (B5_4f$ro_pixel[i] == ro_ma2 || B5_4f$ro_pixel[i] == ro_mi2) {
      B5_4e2[j,] <- B5_4f[i,]
      j = j + 1
    } #end if
    
  } # end for-loop
  
  B5_4e2 <- subset(B5_4e2,B5_4e2$n_pixel >= wd)
  B5_4e2
  B5_4e
  B5_4e3 <- B5_4e
  B5_4e3[3:4,] <- B5_4e2
  B5_4e3_4extr <- B5_4e3 # file with extreme line segments
  B5_4e3_4extr
  lnr_det3 <- B5_4e3_4extr$lnr
  lnr_det3 # solution of 4 extreme lines (at threshold=wd)
  
  #parameter for automation
  bn_PC <- nrow(B5_4e3_4extr)
  bn_pixel <- min(B5_4e3_4extr$n_pixel)
  min_pixel <- bn_pixel
  with_northo <- 1 #it is assumed that all angles are theta_ref or alph_ref
  soph <- 0 #default (determination of sequence is easy)
  n_nonortholines=0
  n_nonortholines2=0
  n_total=4
  
} #end case="extr_wd"
################################################################################

## use of longer extreme lines (case="4_long")
uds <- img_ref[orig_x : wind_x,orig_y:wind_y,1:3]
display(img_uds, method = "raster")
#display(img_uds,method = "browser") #enables zooming
points(xc-orig_x,yc-orig_y,pch=3, asp=1, cex=1.3, col="red")
points(as.integer(pc3$col-orig_x), as.integer(pc3$row-orig_y), pch=20, asp=1, cex=0.2, col="green")

if (cas == "4_long") {  
  B5_4[1:8,]
  
  #n_pix must be changed according to available PCs in B5_4$n_pixel
  #n_pix must be longer than in 'extr_wd'
  #n_pix <- 35 #length of segment (3.2m) default value 
  #n_pix <- 56 #length of segment (5.0) alternative 
  #n_pix <- 25  #length of segment (2.3m) alternative
  #n_pix <- 78 #length of segment (7.0m) alternative
  #n_pix <- 94 #length of segment (8.5m) alternative
  
  cat("define minimum size of line segment: 35 pixel (recommended) 
      or 11, 18, 25, 56, 78, 94 (alternatives)","\n")
  n_pix <- readline("type minimum size of line - if demo - type 35: ") #manual input (n_pix = n_pixel[m]/pixelsize[m])
  n_pix <- as.integer(n_pix)
  n_meter <-round(n_pix * pixel_size2,1)
  cat("n_pix=",n_pix,"pixels","or", n_meter, "meter","\n")
  wd <- n_pix
  thr <- wd #threshold
  k14 <- length(B5_4$lnr)
  B5_4b <- B5_4
  B5_4b[,1:7] <- 0
  B5_4b[1,] <- B5_4[1,]
  i=1
  k1=2
  
  #loop
  while (i < k14) {
    i=i+1
    
    if (B5_4$theta_angle[i] == theta_ref && B5_4$n_pixel[i] >= wd ||
        B5_4$theta_angle[i] == alph_ref && B5_4$n_pixel[i] >= wd) {
      B5_4b[k1,] <- B5_4[i,]
      k1 <- k1+1
    } #end if
    
  } #end while-loop
  
  B5_4b
  B5_4c <- subset(B5_4b,B5_4b$lnr > 0)
  B5_4c #matrix with lines longer than 'wd'
  k15 <- length(B5_4c$lnr)
  
  if (k15 < 4) { #test for number of lines
    #stop("not enough lines - use case = 'extr_wd' !")  
  } #end if
  
  ## Search of lines with theta_ref
  vec <- 1:k15
  B5_4d <- B5_4c
  B5_4d[,1:7] <- 0
  j=1
  
  #loop
  for (i in vec) {
    
    if (B5_4c$theta_angle[i] == theta_ref) {
      B5_4d[j,] <- B5_4c[i,]
      j=j+1
    } #end if
    
  } # end for-loop
  
  B5_4d <- subset(B5_4d,B5_4d$n_pixel >= wd)
  B5_long_lines <- B5_4d[1:2,]
  B5_4d_ord <- B5_4d[order(B5_4d[,6],decreasing=FALSE),]
  B5_4d_ord 
  
  #main direction
  B5_4d_ord
  n_r <- length(B5_4d_ord$lnr)
  vec_1 <- 1 : n_r
  row.names(B5_4d_ord) <- 1 : n_r
  B5_4d_ord
  ro_mi <- min(B5_4d_ord$ro_pixel) 
  ro_ma <- max(B5_4d_ord$ro_pixel)
  B5_4d_ord_red <- subset(B5_4d_ord, B5_4d_ord$n_pixel >= n_pix)  
  n_r2 <- nrow(B5_4d_ord_red)
  vec_2 <- 1 : n_r2
  rownames(B5_4d_ord_red) <- 1 : n_r2
  B5_4d_ord_red
  ro_mi_long2 <- min(B5_4d_ord_red$ro_pixel) 
  ro_ma_long2 <- max(B5_4d_ord_red$ro_pixel)
  B5_4e <- B5_4d_ord_red[1:4,]  #matrix with 4 lines
  B5_4e[1:4,] <-  0
  B5_4e 
  B5_4d_ord_red
  
  if (nrow(B5_4d_ord_red) == 2) {  
    for (i in vec_2) {
      
      if (B5_4d_ord_red$ro_pixel[i] == ro_mi_long2) {
        B5_4e[1,] <- B5_4d_ord_red[i,]  
      } 
      
      if (B5_4d_ord_red$ro_pixel[i] == ro_ma_long2) {
        B5_4e[2,] <- B5_4d_ord_red[i,]  
      }
      
    } #end i-loop 
    
  } else {
    
    for (i in vec_1) {
      if (B5_4d_ord$ro_pixel[i] == ro_mi) { #when only 1 or 0 line with 35 pixels exist
        B5_4e[1,] <- B5_4d_ord[i,]  
      }
      
      if (B5_4d_ord$ro_pixel[i] == ro_ma) { 
        B5_4e[2,] <- B5_4d_ord[i,]  
      }
      
    } #end i-loop 
  } #end if-else
  B5_4e 
  
  ##second main direction (alph_ref)
  B5_4dd <- B5_4
  B5_4dd[,1:7] <- 0
  vec <- 1 : k14
  j = 1
  
  #loop
  for (i in vec) {
    
    if (B5_4$theta_angle[i] == alph_ref) {
      B5_4dd[j,] <- B5_4[i,]
      j = j + 1
    } #end if
    
  } #end for-loop
  
  B5_4dd
  B5_4dd <- subset(B5_4dd, B5_4dd$n_pixel >= wd)
  B5_4dd_ord <- B5_4dd[order(B5_4dd[,6],decreasing=FALSE),]
  n_r <- nrow(B5_4dd_ord)
  
  if (n_r > 0) { 
    vec_1 <- 1 : n_r
    ro_mi <- min(B5_4dd_ord$ro_pixel) 
    ro_ma <- max(B5_4dd_ord$ro_pixel)
    #
    B5_4dd_ord_red <- subset(B5_4dd_ord, B5_4dd_ord$n_pixel >= n_pix)
    B5_4dd_ord_red
    n_r2 <- nrow(B5_4dd_ord_red)
    vec_2 <- 1 : n_r2
    ro_mi_long2 <- min(B5_4dd_ord_red$ro_pixel) 
    ro_ma_long2 <- max(B5_4dd_ord_red$ro_pixel)
    B5_4dd_ord_red
    
    if (nrow(B5_4dd_ord_red) == 2) {  
      
      for (i in vec_2) {
        if (B5_4dd_ord_red$ro_pixel[i] == ro_mi_long2) {
          B5_4e[3,] <- B5_4dd_ord_red[i,]  
        } 
        if (B5_4dd_ord_red$ro_pixel[i] == ro_ma_long2) {
          B5_4e[4,] <- B5_4dd_ord_red[i,]  
        }
      } #end i-loop 
      
    } else {
      i=1
      for (i in vec_1) {
        
        if (B5_4dd_ord$ro_pixel[i] == ro_mi) {
          B5_4e[3,] <- B5_4dd_ord[i,]  
        } 
        
        if (B5_4dd_ord$ro_pixel[i] == ro_ma) {
          B5_4e[4,] <- B5_4dd_ord[i,]  
        }
        
      } #end i-loop 
    } #end if-else2
    
  } else {
    #browser("manual action required")
  } #end if-else 1
  
  B5_4e 
  B5_4e_4long <- B5_4e
  B5_4e_4long
  
  #loop
  vec_4 <- 1 : 4
  
  for (i in vec_4) {
    
    if (B5_4e_4long$n_pixel[i] == max(B5_4e_4long$n_pixel)) {
      lnr_ref2 <- B5_4e_4long$lnr[i]
      theta_ref2 <- B5_4e_4long$theta_angle[i] #new reference line
      alph_ref2 <- theta_ref
    } #end if
    
  } #end for-loop
  
  B5_4e_4long
  n_B5_4e_4_long <- length(B5_4e_4long$lnr)
  vec <- 1 : n_B5_4e_4_long
  row.names(B5_4e_4long) <- vec
  vec2 <- 1 : (n_B5_4e_4_long - 1)
  
  for (i in vec2) {
    
    if (B5_4e_4long$lnr[i] == B5_4e_4long$lnr[i+1]) {     
      stop("double line number - use case = 'extr_wd' !")
    }
    
  } #changed
  print(B5_4e_4long)
  
  cat("correct lines?","\n") 
  answ3 <- readline("are four long lines of proper ro-values detected? type Y or N: ")
  
  if (answ3 =="N") {
    B5_4e_4long
    setwd(home_dir2)
    p_pos <- "cor_det" #correction of detected lines for "case="4_long"
    source(paste("./spObj/spObj_line_detection_v",v_nr,".R",sep=""))
    B5_4e_4long2
  } else {
    B5_4e_4long2 <- B5_4e_4long  
  } #end if-else
  
  B5_4e_4long2
  B5_long_lines <- B5_4e_4long2
  B5_long_lines 
  
  #parameter for sequence-determination
  min_pixel <- min(B5_4e_4long$n_pixel)
  bn_PC <- nrow(B5_4e_4long) 
  with_northo <- 1
  soph=0 #meaning: determination of sequence is easy

} #end cas="4_long"
#########################################################

##more than 4 orthogonal line segments at the object (case="100_all")
#automated solution of outline with more than 4 line segments
#with removal of "shorter_line"

if (cas == "100_all") {
  print(B5_4)
  cat("minimum length of line: 15 pixel (recommended), 10, 35 (alternative)")   #n_pix <- n_meter/pixel_size[m]
  n_pix <- readline("type minimum length of line [pixel]= ") #manual input 15 (recommended) or 35 (alternativ)
  n_pix <- as.integer(n_pix)
  wd <- n_pix
  thr <- 11 #threshold at search of line (default value: n_pix) new
  cat("n_pix=",n_pix,"pixels","\n")
  cat("thr=",thr,"pixels","\n")
  B5_6 <- rectang_lines() #call of function 'rectang_lines()'
  print(B5_6)
  
  #plot of centers and PC of outline at large scale
  r_max2 <- 1.1 * r_max
  plot(xc,-yc, pch=3, cex=2, col="red", asp=1, xlim=c(xc-r_max2,xc+r_max2), ylim=c(-(yc+r_max2),-(yc-r_max2)),main=paste("b ",bnr2, sep=(""))) #large scale
  points(pc3$col, -pc3$row, pch=20, asp=1, cex=0.5, col="green")
  
  cat("parameter of line segments (unsorted):","\n")
  lnr_det3 <- B5_6$lnr
  lnr_det3
  n_lnr3 <- length(lnr_det3)
  vec3 <- 1 : n_lnr3
  centers_PC <- matrix(nrow=n_lnr3, ncol=4)
  centers_PC[,] <- 0
  centers_PC
  
  #loop
  for (n in vec3) {
    lnr <- lnr_det3[n]
    cat("lnr= ",lnr,"\n")
    PC_seg_P_nP <- PC_segment_4(lnr) #call of function
    P <- PC_seg_P_nP[[1]]
    n_P <- PC_seg_P_nP[[2]]
    x_m <- mean(P[,2])
    y_m <- mean(-P[,3])
    points(x_m,y_m, pch=20, asp=1, cex=1.5, col="blue")
    centers_PC[n,1] <- lnr
    centers_PC[n,2] <- x_m
    centers_PC[n,3] <- y_m
    centers_PC[n,4] <- round(n_P/kf)
  } #end of for-loop
  
  centers_PC
  
  ## plot image detail
  display(img_uds, method = "raster")
  
  #loop
  for (i in vec3) {
    cat("i=",i,"\n")
    points((pc3$col - orig_x),(pc3$row - orig_y), pch='.', asp=1, cex=2, col = "green")
    points(xc-orig_x,yc-orig_y,pch=3, asp=1, cex=1.3, col="red")
    x <- centers_PC[i,2]
    y <- centers_PC[i,3]
    points(x-orig_x,-(y-orig_y_math),pch=18, asp=1, cex=1.3, col="red")
  } #end for-loop
  # end of check plot

  #loop for approximate lines
  for (n1 in vec3) {
    browser() #observation of lines
    cat("PC_nr=", B5_6$lnr[n1], "\n")
    theta_angle <- B5_6$theta_angle[n1]
    theta_math <- (180 - theta_angle) #theta of oriented line
    x <- centers_PC[n1,2]
    y <- centers_PC[n1,3]
    p2<- round(x * cos(theta_math/omega) + y * sin(theta_math/omega))
    a <- -1/tan(theta_math/omega)
    b <- round(p2/sin(theta_math/omega))
    
    #calculation of intercept for image extract (math_system)
    y1_math <- a * orig_x + b
    y1_math <- round(y1_math) #change to math-system
    orig_y_math <- (-orig_y) #change to math_system
    b2 <- y1_math - orig_y_math
    a_img <- -a #change to img-system
    b2_img <- (-b2)
    
    # plotting of lines
    coef2 <- c(b2_img,a_img)
    
    if (is.finite(a_img)) {
      abline(coef2, col="blue", lty=1, lwd=2, asp=1)
    }  else {
      ro_l1 <- B4$ro_pixel[lnr]
      ro_l2 <- ro_l1 + ro_1
      ro_l3 <- round(ro_l2 - orig_x)
      lines(c(ro_l3,ro_l3),c(0, (wind_y - orig_y)),col="blue")
    } #end if-else

    cat("#","\n")
  } #end for-loop (large scale)
  
  cat("detected lines are correct?" , "\n") #case: "100_all"
  answ <- readline("type Y or N: ") ###################### action required
  
  B5_6
  
  if (answ == "N") {
    #stop("correction of detected lines")
    p_pos <- "cor_det" #correction of detected lines for cas = "100_all" 
    setwd(home_dir2) 
    source(paste("./spObj/spObj_line_detection_v",v_nr,".R",sep = "")) #special object: correction of detected lines
    B5_6R4 
  } else {
    B5_6R4 <- B5_6
  } #end if-else

  lnr_det5 <- B5_6R4$lnr
  
  lnr_det5
  
  ## derivation of PCs and plotting of detected line segments
  theta <- seq(0,175, by=theta_step)
  theta_rad <- theta/omega
  n_pts <- nrow(B5_6R4)
  row.names(B5_6R4) <- 1 : n_pts
  
  ##test for even number
  if(n_pts%%2 == 1) {
    cat("error - even number of points is required","\n")
  }
  
  B5_6R4 
  lnr_det5
  
  #parameter for the determination of sequence of lines
  min_pixel <- min(B5_6R4$n_pixel)
  bn_PC <- nrow(B5_6R4)
  with_northo <- sum(B5_6R4$ortho)/length(B5_6R4$ortho)
  n_ortholines2 <- length(lnr_det5)
  bn_PC <- n_ortholines2
  soph=1 #sequence is difficult to determine
  n_nonortholines=0
  n_nonortholines2=0
  n_total=bn_PC
} #end case="100_all"
###########################################################################

##cas: 100_all + addition of non-orthogonal lines 

if (cas == "100_all+nonortho") { #solution for lines parallel to ref line
  #stop("proceed step by step")
  cat("define minimum size of line segment: 15 pixel (recommended) or 35 (alternativ)","\n")
  n_pix <- readline("type minimm size of line - if demo - type 35: ") #manual input (n_pix=n_pix[m]/pixelsize[m])
  n_pix <- as.integer(n_pix)
  wd <- n_pix
  thr <- 10 #default value for difference in ro
  cat("n_pix=",n_pix,"pixels","\n")
  cat("thr=",thr,"pixels","\n")
  B5_6 <- line_reduce() #call of function
  B5_6
  
  ##plot of centers and PC of outline at large scale
  r_max2 <- 1.1*r_max
  plot(xc,-yc, pch=3, cex=2, col="red", asp=1, xlim=c(xc-r_max2,xc+r_max2), ylim=c(-(yc+r_max2),-(yc-r_max2)),main=paste("b ",bnr2, sep=(""))) #large scale
  points(pc3$col, -pc3$row, pch=20, asp=1, cex=0.5, col="green")
  #
  
  cat("parameter of line segments (unsorted):","\n")
  print(B5_6) #parameter of line segments (unsorted)
  lnr_det3 <- B5_6$lnr
  lnr_det3
  
  ##derivation of PCs and plotting of detected line segments
  theta <- seq(0,175, by=theta_step)
  theta_rad<-theta/omega
  n_seg <- length(lnr_det3)
  n_seg <- as.integer(n_seg)
  cat("number of segments in the outline=", n_seg,"\n")
  
  ##check for detected line segments
  #number is 'odd'?
  n_pts <- nrow(B5_6)
  
  ##test for even number
  if(n_pts%%2 == 1) {
    cat("warning - even number of points is required","\n")
  }
  
  n_ortholines2 <- length(lnr_det3)
  n_ortholines2 <- as.integer(n_ortholines2)
  # end intro
  
  lnr_det3
  
  cat("are lines of other orientation part of the object unknown? ", "\n")
  cat("use scripts #8 and 9# at 'support_line_detection' ", "\n")
  
  if (proc_mode == "demo" || 
      proc_mode == "auto" ||
      proc_mode == "obj_wise") {
    
    vec <- 1 : 26 #max 25 additional lines
    n_nonortholines2 <- 0

    for (i in vec) {
      #additional lines by manual input
      #n_nonortholines2 are the lines which are non-orthogonal 
      #manual input at the console - if more than one line: repeat, 
      #if 0 type 0, see "demo"
      
      if (Img_name == "ISPRS7") { 
        cat("if demo -> type 3 RETURN, 4 RETURN, 0 RETURN", "\n")
      } 
      
      # if (Img_name == "ISPRS4") {
      #   cat("if demo -> type 1 RETURN, 24 RETURN, 0 RETURN", "\n") #update
      # }
      
      # if (Img_name == "ISPRS4_DLR10") {
      #    cat("if auto -> type x RETURN, type 0 RETURN ", "\n") #update
      # }
      

      add_nr <- readline("type the label of an additional non-orthogonal line= ") #manual input at the console
      add_nr <- as.integer(add_nr)
      add_nr
      
      if (add_nr > 0) {
         n_nonortholines2 <-  n_nonortholines2 + 1
         lnr_det3[n_ortholines2 + n_nonortholines2] <- add_nr
      } else { 
         break
      } #end if-else
    } #end i-loop
    
    
    lnr_det3
    n_total <- length(lnr_det3)
    n_ortholines2
    n_nonortholines2
    vec_nr <- lnr_det3[(n_ortholines2+1):(n_ortholines2+n_nonortholines2)]
    vec_nr #numbers of additional non-ortholines
    
    ## plot image detail
    display(img_uds, method = "raster")
    n_lnr3 <- length(lnr_det3)
    vec3 <- 1 : n_lnr3
    #
    centers_PC <- matrix(nrow=n_lnr3, ncol=4)
    centers_PC[,] <- 0
    centers_PC
    
    vec <- 1 : n_nonortholines2
    for (n1 in vec) {
      B5_6[n_ortholines2+n1,] <- c(0,0,0,0,0,0,0)
    }
    
    B5_6$lnr[(n_ortholines2+1):(n_ortholines2+n_nonortholines2)] <- vec_nr
    
    #test
    B4[vec_nr,2:7]
    B5_6[(n_ortholines2+1):(n_ortholines2+n_nonortholines2),] <- B4[vec_nr,1:7]
    B5_6
    
    
    #loop
    
    for (n in vec3) {
      lnr <- lnr_det3[n]
      cat("lnr= ",lnr,"\n")
      PC_seg_P_nP <- PC_segment_4(lnr) #call of function
      P <- PC_seg_P_nP[[1]]
      n_P <- PC_seg_P_nP[[2]]
      x_m <- mean(P[,2])
      y_m <- mean(-P[,3])
      centers_PC[n,1] <- lnr
      centers_PC[n,2] <- x_m
      centers_PC[n,3] <- y_m
      centers_PC[n,4] <- round(n_P/kf)
    } #end of for-loop
    
    centers_PC
    
    ## plot centers of line at image extract
    
    #loop
    for (i in vec3) {
      cat("i=",i,"\n")
      points((pc3$col - orig_x),(pc3$row - orig_y), pch='.', asp=1, cex=2, col = "green")
      points(xc-orig_x,yc-orig_y,pch=3, asp=1, cex=1.3, col="red")
      x <- centers_PC[i,2]
      y <- centers_PC[i,3]
      points(x-orig_x,-(y-orig_y_math),pch=18, asp=1, cex=1.3, col="red")
    } #end for-loop
    
    #end of check-plot
    
    #loop for plotting approximate lines
    
    for (n1 in vec3) {
      browser()
      display(img_uds, method = "raster")
      points((pc3$col - orig_x),(pc3$row - orig_y), pch='.', asp=1, cex=2, col = "green")
      cat("PC_nr=", B5_6$lnr[n1], "\n")
      theta_angle <- B5_6$theta_angle[n1]
      theta_math <- (180 - theta_angle) #theta of oriented line
      x <- centers_PC[n1,2]
      y <- centers_PC[n1,3]
      p2<- round(x * cos(theta_math/omega) + y * sin(theta_math/omega))
      a <- -1/tan(theta_math/omega)
      b <- round(p2/sin(theta_math/omega))
      
      #calculation of intercept for image extract (math_system)
      y1_math <- a * orig_x + b
      y1_math <- round(y1_math) #change to math-system
      orig_y_math <- (-orig_y) #change to math_system
      b2 <- y1_math - orig_y_math
      a_img <- -a #change to img-system
      b2_img <- (-b2)
      
      # plotting of lines
      coef2 <- c(b2_img,a_img)
      
      if (is.finite(a_img)) {
        abline(coef2, col="red", lty=1, lwd=2, asp=1)
      }  else {
        ro_l1 <- B4$ro_pixel[lnr]
        ro_l2 <- ro_l1 + ro_1
        ro_l3 <- round(ro_l2 - orig_x)
        lines(c(ro_l3,ro_l3),c(orig_y, (wind_y - orig_y)),col="blue")
      } #end if-else
      
      cat("#","\n")
    } #end for-loop (large scale)
    
    cat("are the lines correctly identified?","\n") #case="100_all+nonortho"
    cat("if 'auto' -> type: N","\n")
    answ <- readline("type: Y or N: ")
    #stop("stop")
    
    if (answ == "N") {
        lnr_det3 <- B5_6$lnr
        B5_6
        p_pos <- "cor_det"
        setwd(home_dir2)
        source(paste("./spObj/spObj_line_detection_v",v_nr,".R",sep = "")) #case: 100_all + addition of non-orthogonal lines 
        B5_6R4
        lnr_det5 <- B5_6R4$lnr
    } #end if
    
    #parameter for the determination of sequence of lines
    min_pixel <- min(B5_6R4$n_pixel)
    bn_PC <- nrow(B5_6R4)
    with_northo <- sum(B5_6R4$ortho)/length(B5_6R4$ortho)
    n_ortholines2 <- length(lnr_det5)
    bn_PC <- n_ortholines2
    soph=0 #sequence is difficult to determine
    
  
} #end case= "100_all + nonortho"
################################################################################
  
##case: non-orthogonal lines only 

if (cas == "nonortho_only") { 
  cat("define minimum size of line segment: 15 pixel (recommended) or 35 (alternativ)","\n")
  n_pix <- readline("type minimm size of line - if demo - type 35: ") #manual input (n_pix=n_pix[m]/pixelsize[m])
  n_pix <- as.integer(n_pix)
  wd <- n_pix
  thr <- 10 #default value for difference in ro
  cat("n_pix=",n_pix,"pixels","\n")
  cat("thr=",thr,"pixels","\n")
  
  #determination of line segments manually
  lnr_det3 <- rep(0,26) #maximal number of line segments - subject of change
  cat("determine nonorthogonal lines ", "\n")
  cat("use scripts #8 and 9# at 'support_line_detection' ", "\n")
  cat("input_mode= ", input_mode,"\n")
  
  if (proc_mode == "demo" && input_mode == "single" || 
      proc_mode == "auto" && input_mode == "single"||
      proc_mode == "obj_wise" && input_mode == "single") { 
    
    vec <- 1 : 26 #max 25 additional lines (subject of change)
    n_nonortholines2 <- 0
    
    #loop
    for (i in vec) {
      #input of identifiers of non-orthogonal line segments
      #include selected reference line
      #sequence of line-segments, for example anti-clockwise, must be maintained
      #manual input at the console 
      #end of input: type 0
      
      # if (Img_name == "ISPRS4_DLR10") {
      #    cat("if auto -> type x RETURN, type 0 RETURN ", "\n") #update x (object with non-ortholines)
      # }
      
      
      add_nr <- readline("type the label of a non-orthogonal line= ") #manual input at the console
      add_nr <- as.integer(add_nr)
      
      if (add_nr > 0) {
        n_nonortholines2 <-  n_nonortholines2 + 1
        lnr_det3[n_nonortholines2] <- add_nr
      } else { 
        break
      } #end if-else
    } #end i-loop
    
    lnr_det3 <- subset(lnr_det3,lnr_det3 > 0)
    n_total <- length(lnr_det3)
    n_nonortholines2
    #
  } #end input_mode = "single"   
    
  ##determination of line segments manually, input_mode = "vector"
    
  if (proc_mode == "demo" && input_mode == "vector" || 
      proc_mode == "auto" && input_mode == "vector" ||
      proc_mode == "obj_wise" && input_mode == "vector") {
      
      lnr_det3 <- rep(0,26) #maximal number of line segments - subject of change
      cat("determine nonorthogonal lines ", "\n")
      cat("use scripts #15 & #16 at 'support_line_detection' ", "\n")
      cat("input_mode= ", input_mode,"\n")
      
      setwd(home_dir)
      f6 <- "./results/ISPRS4_DLR10/man/b26_vertex_labels_all.txt" #example
      lnr_det8 <- read.table(f6)
      lnr_det3 <- lnr_det8$x
      lnr_det3
      n_total <- length(lnr_det3)
      n_nonortholines2 <- n_total
    } #end of input_mode = "vector"
    
    ##display image detail (large scale)
    display(img_uds, method = "raster")
    n_lnr3 <- length(lnr_det3)
    vec3 <- 1 : n_lnr3
    #
    centers_PC <- matrix(nrow=n_lnr3, ncol=4)
    centers_PC[,] <- 0
    centers_PC
    
    B5_6 <<- matrix(nrow = n_lnr3, ncol = 7) #make B5_6 to global variable
    B5_6 <- data.frame(B5_6) 
    names(B5_6) <- c("lnr","theta_index","ro_index","n","theta_angle","ro_pixel","n_pixel")
    B5_6[,] <- 0
    lnr_det3
    vec_nr <- lnr_det3
    B5_6$lnr[1:n_nonortholines2] <- lnr_det3
    B5_6
    lnr_det5 <- lnr_det3
    k14 <- max(lnr_det5)
    i=1

    #loop
    for (n1 in lnr_det5) {
      j = 1
      while (j <= k14) {
        #cat("j= ",j,"\n")
        if (n1 == B4$lnr[j]) {
          B5_6[i,] <- B4[j,]
          i = i+1
          break
        }  #end if
        j = j+1
      } #end loop j
    } #end of loop n1

    B5_6
    
    #loop
    for (n in vec3) {
      lnr <- lnr_det3[n]
      cat("lnr= ",lnr,"\n")
      PC_seg_P_nP <- PC_segment_4(lnr) #call of function
      P <- PC_seg_P_nP[[1]]
      n_P <- PC_seg_P_nP[[2]]
      x_m <- mean(P[,2])
      y_m <- mean(-P[,3])
      centers_PC[n,1] <- lnr
      centers_PC[n,2] <- x_m
      centers_PC[n,3] <- y_m
      centers_PC[n,4] <- round(n_P/kf)
    } #end of loop n
    
    centers_PC
    
    ## plot centers of line at image extract
      
    #loop
    for (i in vec3) {
      cat("i=",i,"\n")
      points((pc3$col - orig_x),(pc3$row - orig_y), pch='.', asp=1, cex=2, col = "green")
      points(xc-orig_x,yc-orig_y,pch=3, asp=1, cex=1.3, col="red")
      x <- centers_PC[i,2]
      y <- centers_PC[i,3]
      points(x-orig_x,-(y-orig_y_math),pch=18, asp=1, cex=1.3, col="red")
    } #end i-loop
    #end of check-plot
    
    #loop for plotting approximate lines (large scale)
    for (n1 in vec3) {
      browser()
      display(img_uds, method = "raster")
      points((pc3$col - orig_x),(pc3$row - orig_y), pch='.', asp=1, cex=2, col = "green")
      cat("PC_nr=", B5_6$lnr[n1],"theta_angle=", B5_6$theta_angle[n1], "\n")
      theta_angle <- as.integer(B5_6$theta_angle[n1])
      theta_math <- (180 - theta_angle) #theta of oriented line
      x <- centers_PC[n1,2]
      y <- centers_PC[n1,3]
      p2<- round(x * cos(theta_math/omega) + y * sin(theta_math/omega))
      a <- -1/tan(theta_math/omega)
      b <- round(p2/sin(theta_math/omega))
      #calculation of intercept for image extract (math_system)
      y1_math <- a * orig_x + b
      y1_math <- round(y1_math) #change to math-system
      orig_y_math <- (-orig_y) #change to math_system
      b2 <- y1_math - orig_y_math
      a_img <- -a #change to img-system
      b2_img <- (-b2)
      # plotting of line segments
      coef2 <- c(b2_img,a_img)
      
      if (is.finite(a_img)) {
        abline(coef2, col="red", lty=1, lwd=2, asp=1)
      }  else {
        ro_l1 <- B4$ro_pixel[lnr]
        ro_l2 <- ro_l1 + ro_1
        ro_l3 <- round(ro_l2 - orig_x)
        lines(c(ro_l3,ro_l3),c(orig_y, (wind_y - orig_y)),col="blue")
      } #end if-else
      
      cat("#","\n")
    } #end loop n1
    
    cat("are the lines correctly identified?","\n") #case="nonortho_only"
    #cat("if 'auto' -> type: N","\n")
    answ <- readline("type: Y or N: ")
    
    if (answ == "N") {
      lnr_det3 <- B5_6$lnr
      B5_6
      p_pos <- "cor_det"
      setwd(home_dir2)
      source(paste("./spObj/spObj_line_detection_v",v_nr,".R",sep = "")) #case: nonortho_only
      B5_6R4
      lnr_det5 <- B5_6R4$lnr
      B5_6 <- B5_6R4
    } #end if
      
  } #end proc modes: "demo", "auto", "obj-wise" and "vector
    
  B5_6
  lnr_det5
  n_total2 <- length(lnr_det5)
  n_nonortholines2
  
  #check-plot (large scale)
  par(mai = c(1.02,0.82,0.82,0.42)) 
  x <- xc
  y <- yc
  r_max2 <- 1.1 * r_max
  plot(x,-y, pch=3, cex=2, col="red", asp=1, 
       xlim=c(xc - r_max2,xc + r_max2), 
       ylim=c(-(yc + r_max2),-(yc - r_max2)), 
       ann=TRUE, axes=TRUE, main=paste("b ",bnr2, sep=("")))
  points(pc3$col, -pc3$row, pch=20, asp=1, cex=0.5, col="red")
  lnr_det6 <- lnr_det5
  B5_6[,8] <- 0 #is used for special objects
  names(B5_6)[8] <- "ortho"
  len <- length(B5_6$lnr)
  vec <- 1 : len
  B5_6[,8] <- 0 #manual correction
  row.names(B5_6) <- 1 : nrow(B5_6)
  
  #parameter for determination of line-sequence
  min_pixel <- min(B5_6$n_pixel)
  bn_PC <- nrow(B5_6)
  with_northo <- sum(B5_6$ortho)/length(B5_6$ortho)
  soph <- 1 #sequence is difficult to determine
  
  #output of case="nonortho_only"
  setwd(home_dir)
  fname9 <- paste("./data/",Img_name,"/b",bnr2,"_case.txt", sep="")
  write.table(cas,fname9,row.names = FALSE, col.names = FALSE)

} #end case = "nonortho_only"
################################################################################

##case=non-orthogonal lines only_RDP 

if (cas == "nonortho_only_RDP") { 
  
  setwd(home_dir)
  
  if (part != "no_part") {
    #bnr2_orig <- substr(bnr2,1,2) #bnr2 < 10
    bnr2_orig <- substr(bnr2,1,3) #bnr2 >= 10
    bnr2_orig  <- as.integer(bnr2_orig)
    bnr2 <- bnr2_orig
  }
  
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
  
  if (nc == 1) { #stop("select data manually at script-line 647 in script 'sequence_of_line.R'")}
    plot(W$'1', col="white",asp=1)  #black building
    w = W$'1'
    plot(w,asp=1)
    #p_pos = "cor_img"
  } else {
    p_pos = "cor_img"
    setwd(home_dir2)
    source(paste("./spObj/spObj_line_detection_v",v_nr,".R",sep="")) #selection of W$x
    #plot(w)
    simplified_lines_cor
  } #end if-else

  lines(simplified_lines_cor, col = "red", lty = 1, lwd=2,asp=1)
  simplified_lines <- simplified_lines_cor
  simplified_lines
  #i=52 #i=index in simplified lines of first scale-point b221
  #j=33 ##i=index in simplified lines of second scale-point b221
  points(simplified_lines$x[i],simplified_lines$y[i],
         pch=20,col=("brown"),cex=1,asp=1)
  points(simplified_lines$x[j],simplified_lines$y[j],
         pch=20,col=("black"),cex=1,asp=1)
} #end case = "nonortho_only_RDP"
################################################################################

##expression for automatic selection of type (ty) of figure

if (cas == "extr_wd") {
  B5_6 <- B5_4e3_4extr
}

if (cas == "4_long") {
  B5_6 <- B5_4e_4long2
}

if (cas == "100_all") {
  n_B5_6R4 <- length(B5_6R4$lnr)
  B5_6 <- B5_6R4
  B5_6[1:n_B5_6R4,1:8] <- B5_6R4[1:n_B5_6R4,]
  B5_6
}

if (cas == "100_all+nonortho") {
  B5_6 <- B5_6R4 
}
  
if (cas == "nonortho_only") {
  B5_6
  #stop("new code")
  phi_deg <- B5_6$theta_angle
  phi <- (phi_deg/180)*pi
  m <- length(phi_deg) #number of lines in object
  #matrix elements for m lines
  A <- matrix(nrow=2*m, ncol=m) #matrix
  A[,] <- 0
  A <- design_mat(m,phi)
  x0 <- B5_6$ro_pixel
  b0 <- A %*% x0 #calculation of intersections
  b0 #approximate coordinates
  #
  xk <- rep(0,m)
  yk <- rep(0,m)
  vec <- seq(from=1, to=(2*m), by=2)
  
  k=1
  for (i in vec) {
    cat("i=", i, "\n")
    xk[k] <- b0[i,1]
    yk[k] <- b0[i+1,1]
    k=k+1
  }
  
  head(b0)
  xk
  yk
  #check-plot (large scale)
  par(mai = c(1.02,0.82,0.82,0.42)) 
  x <- xc
  y <- yc
  r_max2 <- 1.1 * r_max
  plot(x,-y, pch=3, cex=2, col="red", asp=1, 
       xlim=c(xc - r_max2,xc + r_max2), 
       ylim=c(-(yc + r_max2),-(yc - r_max2)), 
       ann=TRUE, axes=TRUE, main=paste("b ",bnr2, sep=("")))
  points(pc3$col, -pc3$row, pch=20, asp=1, cex=0.5, col="red")
  
  for (i in vec) { 
    points(xk[i],-yk[i], type="p", col="blue", pch=3, cex=1, asp=1)
  }
  
  xkk <- xk
  xkk[m+1] <- xk[1]
  xkk
  ykk <- yk
  ykk[m+1] <- yk[1]
  ykk
  points(xkk,-ykk,type="l", lwd=2, lty=1,col="black",asp=1)

  # set up of file 'intsec_linepair_vertex_coord'
  intsec_linepair_vertex_coord <- matrix(nrow=(m), ncol=4)
  intsec_linepair_vertex_coord[,1]  <- NA
  intsec_linepair_vertex_coord[,2]  <- B5_6$lnr
  intsec_linepair_vertex_coord[,3] <- xk
  intsec_linepair_vertex_coord[,4] <- -yk
  intsec_linepair_vertex_coord2 <- rbind(intsec_linepair_vertex_coord,intsec_linepair_vertex_coord[1,])
  intsec_linepair_vertex_coord2
  setwd(home_dir)
  f5 <- paste("./results/",Img_name,"/man","/b",bnr2,"_intsec_linepair_vertex_coord2.txt",sep="")
  write.table(intsec_linepair_vertex_coord2,f5)
  
  ##continue by 'plot_results_on_references_v1.4.2'
  setwd(home_dir2)
  source(paste("plot_results_on_references_v",v_nr,".R",sep=""))
} #end case=nonortho_only

if (cas == "nonortho_only_RDP") {
  #stop("continue step by step")
  #transformation
  simplified_lines
  dev.set(2)
  plot(simplified_lines, col = "red", pch=3, cex=1, main=paste("b ",bnr2,"- RDP-result", sep=("")), asp=1)
  points(simplified_lines$x,simplified_lines$y, type="l", col="blue", lwd=2, lty=1,asp=1)
  
  ##plot of pc3 in extern window
  
  #dev.list()
  windows()  # Opens external graphics window
  #dev.set(4)
  par(mai = c(1.02,0.82,0.82,0.42)) #setup of margins/plot region [inches]
  x <- xc
  y <- yc
  r_max2 <- 1.1 * r_max
  #large system centered at xc,yc and r_max2
  plot(x,-y, pch=3, cex=2, col="red", asp=1, xlim=c(xc - r_max2,xc + r_max2),
       ylim=c(-(yc + r_max2),-(yc - r_max2)), ann = TRUE, axes = TRUE,
       main=paste("b ",bnr2, sep=("")))
  points(pc3$col, -pc3$row, pch=20, asp=1, cex=0.5, col="red")
  
  #measurement of ref-line in external window using standard locator()
  #this operation has been necessary because 
  #digitizing by means of 'locator()' works in 
  #external window correctly
  #place external window out side RStudio panes
  cat("manual measurement of two pixels with extreme y-coordinates","\n")
  dev.list()
  dev.set(4)
  #
  xy <- locator(2)
  print(xy)
  x_11 <- xy[[1]][1]
  x_11
  y_11 <- xy[[2]][1]
  y_11
  points(x_11,y_11, pch = 19, col = "brown")
  x_22 <- xy[[1]][2]
  y_22 <- xy[[2]][2]
  points(x_22,y_22, pch = 19, col = "blue")
  dev.set(2)
  plot(simplified_lines, col = "red", pch=3, cex=1, main=paste("b ",bnr2,"- RDP-result", sep=("")), asp=1)
  points(simplified_lines$x,simplified_lines$y, type="l", col="blue", lwd=2, lty=1,asp=1)
  
  #plot scale points
  points(simplified_lines$x[i],simplified_lines$y[i],
         pch=20,col=("brown"),cex=1.5,asp=1)
  points(simplified_lines$x[j],simplified_lines$y[j],
         pch=20,col=("blue"),cex=1.5,asp=1)
  
  ##calculalation of transformation parameters
  x1=simplified_lines$x[i] # simplified_lines coordinates vertex i
  y1=simplified_lines$y[i] #  
  x2=simplified_lines$x[j] # simplified_lines coordinates vertex j
  y2=simplified_lines$y[j] #
  
  points(x1,y1,pch=20,col=("brown"),cex=1.5,asp=1)
  points(x2,y2,pch=20,col=("blue"),cex=1.5,asp=1)
  
  trans_param <- function() { 
    dX <- (x_22 - x_11)
    dY <- (y_22 - y_11)
    dx <- (x2 - x1)
    dy <- (y2 - y1)
    N <- dx^2 + dy^2
    #
    a1 <- (dx*dX + dy*dY)/N
    b1 <- (dx*dY - dy*dX)/N
    a0 <- x_11 - a1*x1 + b1*y1
    b0 <- y_11 - b1*x1 - a1*y1
    #
    tr_lat <- c(a0,b0)
    D <- matrix(nrow=2, ncol=2)
    D[1,1] <- a1
    D[1,2] <- -b1
    D[2,1] <- b1
    D[2,2] <- a1
    kf3 <- sqrt(a1^2+b1^2)
    L1 <- list(D,tr_lat,kf3)
    return(L1)
  } #end of function 'trans_param()'
  
  L1 <- trans_param()
  D <- L1[[1]]
  tr_lat <- L1[[2]]
  kf3<- L1[[3]]
  
  #transformation of the two vertices of ref-line (check of transformation)
  transform2 <- function() {
    #transformation parameters
    D
    tr_lat
    kf3
    print(tr_lat)
    print(D)
    loc <- c(x1,y1) #vertex 13
    pts9 <- tr_lat + D%*%loc #transformation to image-system
    return(pts9)
  } #end of transform2 
  
  pts9 <- transform2()
  x <- pts9[1,1]
  y <- pts9[2,1]
  x
  y
  y <- -y #sign change for plotting in external window
  dev.set(4) #activats of external window
  points(x,y,pch=20, cex=2,asp=1, col="green") #plot of two scale points
  
  #transformation of all vertices in matrix 'simplified_lines'
  simplified_lines
  simplified_lines_complete <- simplified_lines
  simplified_lines_complete_trans <- simplified_lines_complete
  vec <- 1 : length(simplified_lines_complete$x)
  #loop
  for (i in vec) {
    x <- simplified_lines_complete$x[i]
    y <- simplified_lines_complete$y[i]
    loc <- c(x,y) #vertex 
    pts9 <- tr_lat + D%*%loc #transformation to image-system
    x9 <- pts9[1,1]
    y9 <- pts9[2,1]
    x_trans <- x9
    y_trans <- y9
    points(x_trans,y_trans,pch=20, asp =1, cex=1,asp=1, col="green")
    simplified_lines_complete_trans$x[i] <- x_trans
    simplified_lines_complete_trans$y[i] <- y_trans
  } #end of loop
  
  simplified_lines_complete_trans
  dev.list()
  #dev.set(4)
  
  ##plot all simplified lines in xy-system
  points(simplified_lines_complete_trans$x,simplified_lines_complete_trans$y,type ="p",pch=20,cex=1,col="green",asp=1)
  points(simplified_lines_complete_trans$x,simplified_lines_complete_trans$y,type ="l",lty=1,lwd=2,col="blue",asp=1)
  dev.off(4)
  
  #storage
  simplified_lines_complete_trans
  n_siml2 <- length(simplified_lines_complete_trans$x)
  
  intsec_linepair_vertex_coord <- matrix(nrow=n_siml2, ncol=4)
  intsec_linepair_vertex_coord[,2]  <- 1 : (n_siml2)
  intsec_linepair_vertex_coord[,3] <- simplified_lines_complete_trans[,1]
  intsec_linepair_vertex_coord[,4] <- -simplified_lines_complete_trans[,2]
  intsec_linepair_vertex_coord2 <- rbind(intsec_linepair_vertex_coord,intsec_linepair_vertex_coord[1,])
  setwd(home_dir)
  f5 <- paste("./results/",Img_name,"/RDP/","b",bnr2,"_intsec_linepair_vertex_coord2.txt",sep="")
  write.table(intsec_linepair_vertex_coord2,f5)
  cat("table with line-pairs,vertex/corner-number,coordinates(x,y)","\n")
  print(intsec_linepair_vertex_coord2)
  
  #coordinates of building in img_uds
  display(img_uds,"raster")
  points(intsec_linepair_vertex_coord2[,3]-orig_x,
         intsec_linepair_vertex_coord2[,4]-orig_y,type ="l",lty=1,lwd=2,col="white")
  display(img_ref,"raster")
  #dev.set(4)
  points(intsec_linepair_vertex_coord2[,3],intsec_linepair_vertex_coord2[,4],type ="l",lty=1,lwd=2,col="white")
  dev.list()

  #continue by 'plot_results_on_references_v1.4.2'
  setwd(home_dir2)
  source(paste("plot_results_on_references_v",v_nr,".R",sep=""))
} #end of case = "nonortho_only_RDP"  

if (cas == "extr_wd" || cas == "4_long" || cas == "100_all" ||
  cas == "100_all+nonortho") {
  row.names(B5_6) <- 1 : length(B5_6$lnr)
  lnr_det7 <- B5_6$lnr
  lnr_det7
  
  ##derivation of PCs and plotting of detected line segments
  theta<-seq(0,175, by=theta_step)
  theta_rad <- theta/omega
  
  ##plot of selected lines
  par(mai = c(1.02,0.82,0.82,0.42)) #setup of margins/plot region [inches]
  par('usr')
  x <- xc
  y <- yc
  r_max2 <- 1.1 * r_max
  mar <- 30 #to be changed with r_max2
  
  plot(x,-y, pch=3, cex=2, col="red", asp=1, xlim=c(xc - r_max2,xc + r_max2),
       ylim=c(-(yc + r_max2),-(yc - r_max2)), ann = TRUE, axes = TRUE,
       main=paste("b ",bnr2, sep=("")))
  points(pc3$col, -pc3$row, pch=20, asp=1, cex=0.5, col="red")
  points(xc-mar,-(yc+mar), pch=3, asp=1, cex=1, col="green")
  points(xc+mar,-(yc+mar), pch=3, asp=1, cex=1, col="green")
  points(xc-mar,-(yc-mar), pch=3, asp=1, cex=1, col="green")
  points(xc+mar,-(yc-mar), pch=3, asp=1, cex=1, col="green")
  
  #loop PC plotting
  lnr_det7
  n_lnr7 <- length(lnr_det7)
  centers_PC <- matrix(nrow=n_lnr7, ncol=4)
  centers_PC[,] <- 0
  centers_PC
  vec <- 1 : n_lnr7
  
  #loop
  for (n in vec) {
    #browser()
    lnr <- lnr_det7[n]
    cat("lnr= ",lnr,"\n")
    PC_seg_P_nP <- PC_segment_4(lnr) #call of function
    P <- PC_seg_P_nP[[1]]
    n_P <- PC_seg_P_nP[[2]]
    P <- as.data.frame(P)
    names(P) <- c("idx","x","y")
    P_red <- reduce_pointset(P) #new
    head(P_red)
    x_m <- mean(P_red[,2])
    y_m <- mean(-P_red[,3])
    points(x_m, y_m, pch=20, asp=1, cex=1.5, col="green")
    centers_PC[n,1] <- lnr
    centers_PC[n,2] <- x_m
    centers_PC[n,3] <- y_m
    centers_PC[n,4] <- n_P
  } #end of for-loop
  
  centers_PC
  
  #plot of approximate lines onto graph
  n_9 <- length(B5_6$lnr)
  len <- 1 : n_9
  B5_6
  row.names(B5_6) <- 1 : n_9
  B5_6$ortho <- 0
  
  #loop for plotting onto graph
  for (n1 in len) {
    browser() #observation of lines
    cat("PC_nr=", B5_6$lnr[n1], "\n")
    theta_angle <- B5_6$theta_angle[n1]
    theta_math <- (180 - theta_angle) #theta of oriented line
    x <- centers_PC[n1,2]
    y <- centers_PC[n1,3]
    p2<- round(x * cos(theta_math/omega) + y * sin(theta_math/omega))
    cat("p2= ",p2,"\n")
    a <- -1/tan(theta_math/omega)
    b <- round(p2/sin(theta_math/omega))
    coef = c(b,a)
  
    if (is.finite(a)) {
      abline(coef, col="red", lty=1, lwd=2, asp=1)
    }
  
  } # end loop n1
  
  ##plot approximate lines onto orthoimage (large scale)
  B5_6
  par(mai = c(1.02,0.82,0.82,0.42)) #setup of margins/plot region [inches]
  display(img_uds, method = "raster")
  
  #display(img_uds, method = "browser")
  points(xc-orig_x,yc-orig_y,pch=3, asp=1, cex=1.3, col="blue")
  points(as.integer(pc3$col-orig_x), as.integer(pc3$row-orig_y), pch=20, asp=1, cex=0.3, col="green")
  
  #plot approximate lines onto orthoimage (small and large scale)
  len
  orig_x
  orig_y
  
  #display orthoimage at small scale
  display(img_ref,method = "raster")
  #display(img_ref,method = "browser")
  points(pc3$col, pc3$row, pch=20, asp=1, cex=0.5, col="green")
  points(xc, yc, pch=3, asp=1, cex=1.5, col="red")
  
  #window
  fr <- matrix(nrow=5, ncol=2)
  orig_y <- abs(orig_y)
  fr[1,1] <- orig_x
  fr[1,2] <- orig_y
  fr[2,1] <- wind_x
  fr[2,2] <- orig_y
  fr[3,1] <- wind_x
  fr[3,2] <- wind_y
  fr[4,1] <- orig_x
  fr[4,2] <- wind_y
  fr[5,1] <- orig_x
  fr[5,2] <- orig_y
  #
  lines(fr, type="l", asp=1, lwd=2, lty=1, col="yellow")
  
  #loop
  len
  for (n1 in len) {
    cat("PC_nr=", B5_6$lnr[n1], "\n")
    theta_angle <- B5_6$theta_angle[n1]
    theta_math <- (180 - theta_angle) #theta of oriented line
    x <- centers_PC[n1,2]
    y <- centers_PC[n1,3]
    p2<- round(x * cos(theta_math/omega) + y * sin(theta_math/omega))
    a <- -1/tan(theta_math/omega)
    b <- round(p2/sin(theta_math/omega))
    #change to image-system
    a <- (-a)
    b <- (-b)
    #plot
    coef <- c(b,a)
  
    if (is.finite(a)) {
      abline(coef, col="blue", lty=1, lwd=2, asp=1)
    }
  
  } #end for-loop (small scale)
  
  ##plot approximate lines onto orthoimage (large scale)  
  display(img_uds, method = "raster")
  #display(img_uds, method = "browser")
  points(xc-orig_x,yc-orig_y,pch=3, asp=1, cex=1.3, col="red")
  points(as.integer(pc3$col-orig_x), as.integer(pc3$row-orig_y), pch=20, asp=1, cex=0.3, col="green")
  
  #instruction: for selecting lines -> activate browser-mode 
  
  # loop
  for (n1 in len) {
    #browser()
    cat("PC_nr=", B5_6$lnr[n1], "\n")
    theta_angle <- B5_6$theta_angle[n1]
    theta_math <- (180 - theta_angle) #theta of oriented line
    x <- centers_PC[n1,2]
    y <- centers_PC[n1,3]
    points(x-orig_x,-(y-orig_y_math),pch=18, asp=1, cex=1.3, col="red")
    
    #calculation of p2 in math-system with oriented line
    p2 <- round(x*cos(theta_math/omega) + y*sin(theta_math/omega)) #math-system
    
    a <- -1/tan(theta_math/omega)
    b <- round(p2/sin(theta_math/omega))
    
    #calculation of intercept for image extract (math_system)
    y1_math <- a * orig_x + b
    y1_math <- round(y1_math) #change to math-system
    orig_y_math <- (-orig_y) #change to math_system
    b2 <- y1_math - orig_y_math
    
    #change to img-system
    a_img <- (-a)
    b2_img <- (-b2)
    
    # plot
    coef2 <- c(b2_img,a_img)
    
    if (is.finite(a)) {
      abline(coef2, col="red", lty=1, lwd=2, asp=1)
    }  else {
      ro_l1 <- B4$ro_pixel[lnr]
      ro_l2 <- ro_l1 + ro_1
      ro_l3 <- round(ro_l2 - orig_x)
      lines(c(ro_l3,ro_l3),c(0, (wind_y - orig_y)),col="white")
    } #end if-else
  
  } #end of loop n1
  #end of plot (large scale)
  
  #output of unsorted lines
  bnr2
  setwd(home_dir)
  f3 <- paste("./data/",Img_name,"/unsorted_lines_b",bnr2,".txt",sep="")
  B5_6
  write.table(B5_6,f3)
  
  #storage in a list (all_lines)
  n_lnr <- nrow(B5_6)
  PC_nr <- B5_6$lnr[1:n_lnr]
  n_PC <- n_lnr
  vec_x <- 1 : n_PC
  
  ##general solution for list
  all_lines <- list()
  
  for (i in vec_x) {
    all_lines[[i]] <- "PC"
  }
  
  ##solution for x PCs
  
  for (i in vec_x) {
    all_lines[i] <- paste("P",i,sep="")
  }
  
  ##loop for reading all point clusters (PCs)
  par(mai = c(1.02,0.82,0.82,0.42)) #setup of margins/plot region [inches]
  x <- xc
  y <- yc
  r_max2 <- 1.1*r_max
  plot(x,-y, pch=3, cex=2, col="red", asp=1, 
       xlim=c(xc-r_max2,xc+r_max2), ylim=c(-(yc+r_max2),-(yc-r_max2)), 
       main=paste("b ",bnr2, sep=("")), axes=TRUE) #large scale
  points(pc3$col, -pc3$row, pch=20, asp=1, cex=0.5, col="orange")
  cat("line numbers (not in correct sequence):","\n")
  print(PC_nr)
  palette2 <- c("brown","red","gray","darkgreen","blue","magenta","black","cyan",
                "brown","red","gray","darkgreen","blue", "magenta", "black", "cyan","brown","red")
  setwd(home_dir)
  k=1
  
  #loop
  for (i in PC_nr){
    lnr <- i
    cat("lnr=",lnr,"\n")
    #browser() #to be removed
    fname=paste("./data/",Img_name,"/b",bnr2,"_",lnr,".txt", sep="")
    P0 <- read.table(fname, col.names=c("idx","x","y"))
    nrow <- nrow(P0)
    cat("nrow=",nrow,"\n")
    P0_red <- reduce_pointset(P0) #correction for gaps using histogram analysis
    nrow <- length(P0_red$idx)
    all_lines[[k]] <- P0_red
    points(P0_red[,2],-P0_red[,3], pch='.', asp=1, cex=2, col=palette2[k])
    #points(P0_red[,2],-P0_red[,3], pch='.', asp=1, cex=2, col="green")
    k=k+1
  } #end loop
  
  ## convert 'all_lines' (matrix) to 'all_PC' (list)
  all_PC <- all_lines
  names_PC <- list()
  n_PC <- length(PC_nr)
  vec_x <- 1 : n_PC
  
  for (i in vec_x) {
    names_PC[[i]] <- "PCN"
  }
  
  #loop
  k=1
  
  for (i in PC_nr) {
    na_PC<-paste("PC_",PC_nr[k],sep="")
    name_PC <- as.name(na_PC)
    names_PC[[k]] <- name_PC
    k <- k+1
  } #end of for-loop
  
  names_PC
  names(all_PC) <- names_PC
  
  ##plot image detail
  display(img_uds, method = "raster")
  n_x <- length(PC_nr)
  vec_y <- 1 : n_x
  
  #loop
  for (i in vec_y) {
    cat("i=",i,"\n")
    points((all_PC[[i]]$x - orig_x),(all_PC[[i]]$y - orig_y), 
           pch='.', asp=1, cex=2, col = "green")
    points(xc-orig_x,yc-orig_y,pch=3, asp=1, cex=1.3, col="red")
    x <- centers_PC[i,2]
    y <- centers_PC[i,3]
    points(x-orig_x,-(y-orig_y_math),pch=18, asp=1, cex=1.3, col="red")
  } #end for-loop
  
  # end of check plot
  
  ##output of files of individual PCs
  
  #loop
  setwd(home_dir)
  
  for (i in vec_y) {
    fname8 <- paste("./data/",Img_name,"/all_PC$PC_nr",PC_nr[i],".txt",sep="")
    write.table(all_PC[[i]], fname8)
  } #end loop output of list PC_all
  
  all_PC
  
  cat("end of script 'line-detection.R' - continue with 'sequence_of_lines.R' ","\n")
  cat("####################################################################","\n")
  setwd(home_dir2)
  source(paste("sequence_of_lines_v",v_nr,".R",sep=""))
} #end all cases except 'nonortho_only' and 'nonortho_only_RDP'   

################################################################################

