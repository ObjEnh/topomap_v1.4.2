##name of script: adjustment of corner coordinates.R
cat("version_number= ",v_nr,"\n")
## description: calculation of final coordinates of polygon-corners (vertices)
# method 1: use of condition for orthogonal lines
# method 2: weighted average of the main direction (theta_av) 
# ro-values are calculated by least-squares adjustment (all line types)
# quality control by standard deviation of residuals
## instructions: use script 'support_adjustment_of_corner_coordinates.R' in case of problems
## author: Joachim Höhle
## GNU General Public License (GPL)
cat("##################################################################","\n")

cat("start of program 'adjustment_of_corner_coordinates.R'","\n")

##inputs

#input of plot-parameter
n_pts # number of corners/lines
xc <- plotPar[1]
yc <- plotPar[2]
r_max <- plotPar[3]

#input of adjusted angle (theta, weighted average)
setwd(home_dir)
fname10 <- paste("./data/",Img_name,"/theta_av_b",bnr2,".txt", sep="")

if (cas == "100_all+nonortho") {
  theta_av <- as.numeric(read.table(fname10))
  cat("theta_adj_av (weighted average) = ", theta_av, "[degrees]", "\n")  
}

# input of table with adjusted parameters (theta_adj, ro_adj)
f2 <- paste("./data/",Img_name,"/param_adj_b",bnr2,".txt",sep="")
B8 <- read.table(f2, header=T)
options(digits = 5)
cat("adjusted parameter of line segments (theta_adj, ro_adj) and length of lines (np):","\n")
print(B8)
n_B8 <- nrow(B8)
z <- 1 : n_B8
B8[,8] <- 0
names(B8)[8] <- "ortho"
 
for (i in z) {
  
  if (B8$theta_ang[i] == theta_ref || B8$theta_ang[i] == alph_ref) {
    B8$ortho[i] <- 1
  } else {
    B8$ortho[i] <- 0
  } #end if-else

} #end for-loop 

B8

#setup of new data frame with adjusted values 
B8S <- B8 
n_ortholines2 <- sum(B8S$ortho)

#introduction of adjusted angle (theta_av)
if (cas == "100_all+nonortho") {
  
  for (i in z) {
    
    if (B8S$ortho[i] == 1 && B8S$theta_ang[i] < 90) { #buildings with orthogonal lines
        B8S$theta_adj[i] <- theta_av 
    } 
    
    if (B8S$ortho[i] == 1 && B8S$theta_ang[i] >= 90) { #buildings with orthogonal lines
      B8S$theta_adj[i] <- theta_av + 90
    } 
  
  } # end for-loop

} #end if (case "100_all+nonortho")
 
B8S

n_pts <- nrow(B8S)

if (sum(B8S$ortho) < n_pts) { 
  cas <- "100_all+nonortho" #case for objects with non-orthogonal lines
  n_nonortholines2 <- n_pts - sum(B8S$ortho)
}

B8S

if (cas != "100_all+nonortho") {
  n_nonortholines2 <- 0
}

n_parallel_nonortholines2 = 0 # number of parallel nonortholines (to be determined!)

if (cas == "100_all+nonortho") { #solution for orthogonal lines
  
  B8S
  
  ##input of theta_av2 
  if (n_nonortholines2 != 0 && n_nonortholines2 <= 3 && n_parallel_nonortholines2 != 0) { 
    setwd(home_dir)
    fname11 <- paste("./data/",Img_name,"/theta_av2_b", bnr2,".txt",sep="")
    theta_av2 <- as.numeric(read.table(fname11))
    cat("theta angle2 (weighted average) = ", theta_av2, "[degrees]", "\n")
    
    z1 <- 1 : nrow(B8S)
    for (i in z1) {
      
      if (B8S$ortho[i] == 0 && n_parallel_nonortholines2 != 0) {
        
        B8S$theta_adj[i] <- theta_av2
      }
      
    } # end for-loop
    
    for (i in z1) {
  
      if (B8S$ortho[i] == 0 && B8S$theta_ang[i] > 90) { 
        B8S$theta_adj[i] <- B8S$theta_adj[i] + 90
      }
    }
  } # end if
} #end if cas == "100_all+nonortho"

B8S
n_pts <- nrow(B8S)

if (sum(B8S$ortho) < n_pts) {
  cas <- "100_all+nonortho" #case for objects with non-orthogonal lines
}

B8S
n_ortholines2
n_nonortholines2
n_parallel_nonortholines2

if (n_nonortholines2 > 1 && n_ortholines2 != 1 && n_parallel_nonortholines2 != 0) { #used at special object
  p_pos <- "cor_adj_coco"
  setwd(home_dir2)
  source(paste("./spObj/spObj_adjustment_of_corner_coordinates_v",v_nr,".R",sep = "")) 
} #end if

B8S

phi_deg <- B8S$theta_adj
phi_deg <- (-phi_deg) #change to math-system
options(digits=6)
cat("sequence of angles [degrees]:","\n")
print(phi_deg)
phi <- (phi_deg*pi/180)
m <- length(phi) # number of lines in object
A <- matrix(nrow=2*m, ncol=m) #matrix for building with m corners
A[,] <- 0
A <- design_mat(m,phi) #call of function
A

if (cas == "extr_wd" || cas == "4_long" || cas == "100_all") { #orthogonal objects
  
  #input of approximate coordinates
  setwd(home_dir)
  fname12 <- paste("./data/",Img_name,"/b",bnr2,"_coord_appr.txt",sep = "")
  b01 <- read.table(fname12,col.names="xy")
  b01 #approximate corner coordinates
  m1 <- length(b01$xy)
  m2=m1/2
  X <- rep(0,m2)
  Y <- rep(0,m2)
  
  vec <- 1 : m2
  
  for (i in vec) {
    X[i] <- b01$xy[2*i - 1]
    Y[i] <- b01$xy[2*i]
  }
  
  X
  Y
  n <- m2
  
  #calculation of adjustment with orthogonality condition
  res <- adj_orthog_corcoo(n,X,Y) #function call
  res
  XY_adj_ncorners_1 <- res[[1]] #adjusted coordinates of orthogonal objects
  std_dev_coo_1 <- res[[2]] #standard deviation of coordinates
} #end if cas == "extr_wd" || cas == "4_long" || cas == "100_all"

if (cas == "100_all+nonortho") {
  #input of approximate coordinates
  setwd(home_dir)
  fname12 <- paste("./data/",Img_name,"/b",bnr2,"_coord_appr.txt",sep = "")
  b01 <- read.table(fname12,col.names="xy")
  b01 #approximate corner coordinates
  b <- rep(0,m)
  b <- b01$xy
  n_b <-length(b01$xy)
  b_mat <- matrix(nrow=n_b,ncol=2)
  b_mat <- as.data.frame(b_mat)
  b_mat[1:n_b,1] <- b
  b_mat[1:n_b,2] <- 0
  names(b_mat)[1:2] <- c("coo","ortho")
  n_B8S <- nrow(B8S)
  
  i <- 1
  j <- 1
  #loop i
  
  while (i <= n_B8S) {
      b_mat[j,2] <- b_xy_vertex$ortho[i]
      b_mat[(j+1),2] <- b_xy_vertex$ortho[i]
      i <- i + 1
      j <- 2*i - 1
   } #end of loop i
  
  b_mat
  b <- b_mat$coo
  
  ##adjusted coordinates x,y
  p <- adjust_coord(A,b) #function call
  p
  m1 <- length(p[,1])
  m2 <- m1/2
  
  ##checks by plotting the results
  
  #plot of adjusted corner coordinates
  Points_x <- rep(0,(m2+1))
  Points_x[m2+1] <- p[1,1] #repeat first point (x-coordinate)
  
  k <- 1
  i <- 1
  #loop i
  while(i <= (2*m2 - 1)) {
    Points_x[k] <- p[i,1]
    k <- k + 1
    i <- i + 2
  } #end loop i
  Points_x
  
  Points_y <- rep(0,m2+1)
  Points_y[m2+1] <- p[2,1] #repeat first point (y-coordinated)
  k <- 1
  i <- 2
  
  #loop i
  while(i <= 2*m2){
    Points_y[k] <- p[i,1]
    k <- k + 1
    i <- i + 2
  } #end loop i
  
  Points_y
  b_seri_xy2 <- matrix(nrow=m2+1, ncol=3)
  b_seri_xy2[,1] <- 1 : (m2+1)
  b_seri_xy2[,2] <- Points_x
  b_seri_xy2[,3] <- Points_y
  b_seri_xy2
  colnames(b_seri_xy2) <- c("nr","x","-y")
  b_seri_xy2
  k1 <- m2
  
  #plot
  i <- 0
  
  #loop i
  while(i < k1) {
    i <- i + 1
    x <- b_seri_xy2[i,2]
    y <- b_seri_xy2[i,3]
    xy <- b_seri_xy2[,2:3]
    points(x,y, pch=20, cex=1.5, col="blue", asp=1)
  }
  
  xy <- b_seri_xy2[,2:3]
  lines(xy,  col="green", asp=1, type="l", lwd=2, lty=1)
  
  cat("adjusted coordinates:","\n")
  print(b_seri_xy2)
  
  ## output of final coordinates 
  
  #output of final coordinates for plotting
  setwd(home_dir)
  fname14 <- paste("./results/",Img_name,"/b",bnr2,"_coord_adj_plot.txt",sep="")
  write.table(b_seri_xy2,fname14)
  
  #output of point_numbers (vertices) with intersecting line-pairs
  f4 <- paste("./data/",Img_name,"/b",bnr2,"_PC_nr.txt",sep="")
  setwd(home_dir)
  line_nrs <- read.table(f4, col.names = "lnr")
  line_nrs2 <- rep(0,m2)
  
  i=1
  while (i < m2) { 
    line_nrs2[i] <- line_nrs$lnr[i+1]
    i <- i + 1
  }
  
  line_nrs2[m2] <- line_nrs$lnr[1]
  intsec_linepair_vertex_coord <- matrix(nrow=m2, ncol=4)
  intsec_linepair_vertex_coord [,1] <- paste(line_nrs$lnr,"_",line_nrs2,sep="")
  intsec_linepair_vertex_coord [,2] <- 1 : m2
  intsec_linepair_vertex_coord [,3:4] <- b_seri_xy2[1:m2,2:3]
  #
  print(intsec_linepair_vertex_coord)
  f5 <- paste("./results/",Img_name,"/b",bnr2,"_intsec_linepair_vertex_coord.txt",sep="")
  write.table (intsec_linepair_vertex_coord, f5)
  #
  setwd(home_dir)
  fname13=paste("./results/",Img_name,"/b",bnr2,"_coord_adj.txt",sep="")
  write.table(b_seri_xy2[1:m2,],fname13)
  #
} #end if cas = "100_all+nonortho"

if (cas == "extr_wd" || cas == "4_long" || cas == "100_all") { #solving orthogonal buildings
  XY_adj_ncorners_1
  n <- m2
  b_seri_xy2 <- matrix(nrow=n+1, ncol=3)
  b_seri_xy2[1:n,] <- XY_adj_ncorners_1[1:n,]
  b_seri_xy2[(n+1),] <- XY_adj_ncorners_1[1,]
  dimnames(b_seri_xy2)[[2]] <- list("nr","x","-y")
  b_seri_xy2
  row.names(b_seri_xy2) <- 1 : (m2+1)
  b_seri_xy2
  k1 <- m2
  
  #plot
  i <- 0
  
  #loop i
  while(i < k1) {
    i <- i + 1
    x <- X[i]
    y <- Y[i]
    xy <- b_seri_xy2[,2:3]
    points(x,y, pch=20, cex=1.5, col="black", asp=1)
  }
  
  xy <- b_seri_xy2[,2:3]
  lines(xy,  col="black", asp=1, type="l", lwd=2, lty=1)
  
  cat("adjusted coordinates:","\n")
  print(b_seri_xy2)
  
  ## output of final coordinates 
  
  #for plotting
  setwd(home_dir)
  fname14 <- paste("./results/",Img_name,"/b",bnr2,"_coord_adj_plot.txt",sep="")
  write.table(b_seri_xy2,fname14)
  
  #output of point_numbers (vertices) with intersecting line-pairs
  f4 <- paste("./data/",Img_name,"/b",bnr2,"_PC_nr.txt",sep="")
  setwd(home_dir)
  line_nrs <- read.table(f4, col.names = "lnr")
  line_nrs2 <- rep(0,m2)
  
  i=1
  while (i < m2) { 
    line_nrs2[i] <- line_nrs$lnr[i+1]
    i <- i + 1
  }
  
  line_nrs2[m2] <- line_nrs$lnr[1]
  intsec_linepair_vertex_coord <- matrix(nrow=m2, ncol=4)
  intsec_linepair_vertex_coord [,1] <- paste(line_nrs$lnr,"_",line_nrs2,sep="")
  intsec_linepair_vertex_coord [,2] <- 1 : m2
  intsec_linepair_vertex_coord [,3:4] <- b_seri_xy2[1:m2,2:3]
  #
  print(intsec_linepair_vertex_coord)
  f5 <- paste("./results/",Img_name,"/b",bnr2,"_intsec_linepair_vertex_coord.txt",sep="")
  write.table (intsec_linepair_vertex_coord, f5)
  #
  
  #output of point_numbers (vertices) with intersecting line-pairs
  f4 <- paste("./data/",Img_name,"/b",bnr2,"_PC_nr.txt",sep="")
  setwd(home_dir)
  line_nrs <- read.table(f4, col.names = "lnr")
  line_nrs2 <- rep(0,m2)
  
  i=1
  while (i < m2) { 
    line_nrs2[i] <- line_nrs$lnr[i+1]
    i <- i + 1
  }
  
  line_nrs2[m2] <- line_nrs$lnr[1]
  intsec_linepair_vertex_coord <- matrix(nrow=m2, ncol=4)
  intsec_linepair_vertex_coord [,1] <- paste(line_nrs$lnr,"_",line_nrs2,sep="")
  intsec_linepair_vertex_coord [,2] <- 1 : m2
  intsec_linepair_vertex_coord [,3:4] <- b_seri_xy2[1:m2,2:3]
  print(intsec_linepair_vertex_coord)
} #end cas == "extr_wd" || cas == "4_long" || cas == "100_all" (all orthogonal objects)


#output of adjusted coordinates

if (cas == "100_all" || cas == "extr_wd" || cas == "4_long") { 
  setwd(home_dir)
  fname13=paste("./results/",Img_name,"/b",bnr2,"_coord_adj.txt",sep="")
  write.table(XY_adj_ncorners_1,file=fname13)
}

cat("end of 'adjustment_of_corner_coordinates.R'-
continue with 'plot results_on_references.R'","\n")
setwd(home_dir2)
source(paste("plot_results_on_references_v",v_nr,".R",sep=""))
################################################################################



