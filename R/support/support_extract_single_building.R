## name of script: support_extract_single_building.R
cat("version_number= ",v_nr,"\n")
## purpose: preparations for package 'buildenh'
## author: Joachim Höhle
## GNU General Public License (GPL)

##contents:

## 1. plot numbers (labels) of all buildings (before and after applying a threshold)
## 2. plot number of one building (before and after applying a threshold)
## 3. find number 'bnr2' by number 'bnr' and vice versa
## 4. generation of table with bnr/bnr2 columns
## 5. find bnr2 by bnr and vice versa by using the generated table 
## 6. separation of object
###########################################################################################

## 1. plot numbers (labels) of buildings (before and after applying a threshold)
#numbering without area thresholding: object label is 'bnr'
#use proper image under 'Plots'  (apply arrows: <- or -> )

#input of LCM with all outlines of objects
setwd(home_dir)
LCM_enh_b=readImage(paste("./data/",Img_name,"/images/LCM_cart_enh_b3_scaled_2.jpg",sep="")) #classification by JH, scaled affine

#display of enhanced image
display(LCM_enh_b, method="raster")

y1 <- 1 : nrow(coor)

for (i in y1) {
  x <- coor[i,1]
  y <- coor[i,2]
  text(x,y,i,cex=1.2,col="red")
} #end for loop

#plot numbers (labels) of objects (buildings) after applying the threshold 
#    for minimum size of area (ISPRS1,ISPRS7: 3086 pixels, ISPRS4: 312 pixels)

#input of enhanced image
LCM_enh_b <- readImage(paste("./data/",Img_name,"/images/LCM_cart_enh_b3_scaled_2.jpg",sep="")) #classification by JH, scaled affine
display(LCM_enh_b, method="raster")
#

y1 <- 1 : nrow(shap2_A_red3)

for (i in y1) {
  nr <- i 
  xc <- shap2_A_red3[i,8]
  yc <- shap2_A_red3[i,9]
  text(xc,yc,nr,cex=1.2,col="red")
} #end for loop

#end of #1: plot numbers (labels) of buildings (before and after applying a threshold) 

################################################################################

## 2. plot number of one building (bnr2)
#numbering with threshold for area of buildings >= 3086 pixels
#use proper image under 'Plots' (use arrows: <- or -> )

display(LCM_enh_b, method="raster")
names(shap2_A_red3) <- c("bnr2","area","perimeter","radius.mean","radius.sd","radius.min","radius.max","cx","cy","alpha_arc")
y1 <- 1 : nrow(shap2_A_red3)
i <- 18 #select number (bnr2)
xc <- shap2_A_red3[i,8]
yc <- shap2_A_red3[i,9]
text(xc,yc,i,cex=1.2,col="red")
#

#end of ## 2. plot the number of one object (building) 
###############################################################################################################

## 3. find number 'bnr2' by number 'bnr' and vice versa
bnr=18 #type building number, here b18
cat("bnr=", bnr, "\n")

y1 <- 1 : nrow(shap2_A_red3)

for (n in y1) {
  
  if (shap2_A_red3$bnr[n] ==  bnr) {
    bnr2 <- n
    cat("bnr2=",bnr2,"\n")
  } #end if
  
} #end for-loop

#end of ## 3. find number 'bnr2' by number 'bnr' and vice versa
##############################################################################################################

## 4. generation of table with bnr/bnr2 columns

names(shap2_A_red3) <- c("bnr2","area","perimeter","radius.mean","radius.sd","radius.min",
                       "radius.max","cx","cy","alpha_arc")
nrow(shap2_A_red3)
y1 <- 1 : nrow(shap2_A_red3)
obj_nrs <- matrix(nrow=nrow(shap2_A_red3), ncol=2)
colnames(obj_nrs) <- c("bnr","bnr2")
names(shap1_A) <- c("bnr","area", "perimeter", "radius.mean", "radius.sd", "radius.min",
                    "radius.max","cx","cy","alpha_arc")
vec <- 1 : nrow(shap1_A)

for (i in y1) {
  for (k in vec) { 
    
    if (shap1_A$area[k] == shap2_A_red3$area[i] ) {
      obj_nrs[i,1] <- shap1_A$bnr[k]
      obj_nrs[i,2] <- shap2_A_red3$bnr2[i]
    } #end if
    
  } #end for k
} #end for i

obj_nrs #table bnr/bnr2
#

#end of ## 4: generation of table with bnr/bnr2 columns
###############################################################################

## 5. find bnr2 by bnr and vice versa using the table derived in #4.

answ <- readline("type bnr: ")
bnr <- as.integer(answ)
cat("bnr= ",bnr,"\n")
y1 <- 1 : nrow(shap2_A_red3)

for (i in y1) {
  
  if (obj_nrs[i,1] == bnr) {
    bnr2 <- obj_nrs[i,2]
  } #end if
  
} #end for-loop

cat("bnr2= ", bnr2,"\n")
#

##find 'bnr' by 'bnr2'
answ <- readline("type bnr2: ")
bnr2 <- as.integer(answ)
cat("bnr2= ",bnr2,"\n")

for (i in y1) {
  
  if (obj_nrs[i,2] == bnr2) {
    bnr <- obj_nrs[i,1]
  } #end if
  
} #end for-loop

cat("bnr= ", bnr,"\n")
#

#end of ## 5. find bnr2 by bnr and vice versa using a table

## 6: separation of object
#determination of pixels for separation


#display enlarged ortho_image and PC of building outline

if (orig_x < 0) { #solves problems at edges of orthoimage
   orig_x = 0
}

if (orig_y < 0) {
   orig_y = 0
}

img_uds <- img_ref[orig_x : wind_x, abs(orig_y) : wind_y,1:3]
display(img_uds, method = "raster")
#display(img_uds,method = "browser") #display enables zooming
points(xc-orig_x,yc-orig_y,pch=3, asp=1, cex=1.3, col="red")
points(as.integer(pc3$col-orig_x), as.integer(pc3$row-orig_y), 
       pch=20, asp=1, cex=0.3, col="green")

#determination of transformation parameter by means of orthoimage (large scale)
#measure two control 2 points (left lower, right upper) and one checkpoint (middle)
L1 <- trans_ortho() #

# measurement of new points (results: x,y)
#orig_y <- (-orig_y) #change to img-system (when 2. and more lines have to be determined)
locator2() #measurement and marking of one pixel's position, includes transformation-matrix
#end of ## 5.
#########################################################################################################

##end of script 'support_extract_single_building.R'

