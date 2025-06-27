##name of script: enhance_image.R
cat("version_number= ",v_nr,"\n")
#description: cartographic enhancement of extracted class "building"
#author: Joachim Höhle
#instruction: scale of image has to be checked and eventually to be corrected
#instruction: define pixel size2 on ground (at lines 17/18)
#instruction: define area_threshold (at lines 23/24)
#instruction: define structuring element (size, shape))
#instruction: define parameters of adaptive thresholding (w, h, offset)
#GNU General Public License (GPL)
##############################################################################

cat("start of program 'enhance_image.R' ", "\n")
cat("Image name: ",Img_name,"\n")

##parameters:size of brush [pixels]
#pixel_size2=0.09 # pixel size on the ground in [m] #orthoimage #1, #7
pixel_size2=0.283 # pixel size on the ground in [m] #orthoimage #4
size <- 5 #used in makeBrush
shape <- 'diamond' #used in makeBrush
w <- 2; h <- 2 #half width and height of the moving rectangular window (used in func 'thresh')
offset <- 0.01 #thresholding offset from the average value (used in thresh)
#area_threshold <- 3086 #orthoimages #1, #7, removes buildings of area < 5mx5m (3086 pixels)
area_threshold = 312 #orthoimage #4, removes buildings of area < 5mx5m (312 pixels)
#instruction: calculation of area_threshold 
#area_threshold[qm]=area_threshold[pixel]*GSD^2
#area_threshold[pixel]=area_threshold[qm]/GSD^2

##input of orthoimage with extracted buildings
setwd(OrgClassResPathname)
LCM_b <-readImage(OrgClassResFilename)
#LCM_img <-readImage(OrgClassResFilename) #new
display(LCM_b) #display 
display(LCM_b, "raster") #display 
str(LCM_b) #new
LCM_b@.Data
#display(LCM_b) #display by "Viewer"
##input of other classes
##instruction:change OrgClassResFilename
#succeed in a similar way as for buildings

##Generation of enhanced image 'LCM_b' (class building)
kern=makeBrush(size,shape) #filtering
LCMdilate=dilate(LCM_b,kern) #morphological operator dilate
LCMdilaterode=erode(LCMdilate,kern) #morphological operator 'erode' (='closing')
print(LCMdilaterode)
LCMdilaterode_gray <- channel(LCMdilaterode,'gray') #convert to gray image
max(imageData(LCMdilaterode)) 
min(imageData(LCMdilaterode)) 
LCMdilaterode_t <- thresh(LCMdilaterode_gray,w,h,offset) #thresholding -> white oulines
LCMdilaterode_t_f <- fillHull(LCMdilaterode_t) #fill with pixels of intensity 1 (white)
LCMdilaterode_t_f_lab <-  bwlabel(LCMdilaterode_t_f) #segmentation by labeling
cat('Number of buildings=',max(LCMdilaterode_t_f_lab),'\n')
sh=computeFeatures.shape(LCMdilaterode_t_f_lab) #compute of area and radius
area_threshold #threshold for size of area, in pixels
sh_area<-subset(sh,sh[,1] < area_threshold) #calculate objects smaller than s.area
nrow(sh_area)
n_rem <- as.integer(row.names(sh_area))
LCM_cart_enh_building <- rmObjects(LCMdilaterode_t_f_lab,n_rem)
reenumerate(LCM_cart_enh_building)
display(LCM_cart_enh_building)

#calculation of parameters of fitting ellipse
sh2 <- computeFeatures.shape(LCM_cart_enh_building) 
sh2

#output of refined image (buildings > area-threshold)
setwd(home_dir)
writeImage(LCM_cart_enh_building,paste("./data/",Img_name,"/images/LCM_cart_enh_b3.jpg", sep = "")) 

##test for correct removal of small objects
LCM_comb_b_red=rgbImage(red=LCM_cart_enh_building)
#display(LCM_comb_b_red)

#test with input-orthoimage (check of enhancement)
LCM_building_gray <- channel(LCM_b, 'gray')
display(LCM_building_gray)
display(LCM_cart_enh_building)

#checking of remaining objects
LCM_bb=rgbImage(blue=LCM_building_gray, red=LCM_cart_enh_building) 
display(LCM_bb)
#
LCM_b <- LCM_cart_enh_building
display(LCM_b, "raster")
display(LCM_b, "browser")
#end of generation of refined class "building"

##scaling of image
#cat("Is scaling necessary? type Y or N","\n") 
#answ <- readline("Is scaling necessary? ") #activate if required
answ = "Y" #do not use for other projects than "ISPRS1", "ISPRS7", "ISPRS4", "ISPRS4_DLR10" 

if (answ == "Y") {
  setwd(home_dir2)
  source(paste("./spObj/spObj_enhance_image_v",v_nr,".R",sep="")) #solution in 'spObj_enhance_image.R' 
}

#display(LCM_b_2,"raster")
display(LCM_b_2)
#

cat("end of 'enhance_image.R' - continue with 'extract_single_building.R' ","\n")
setwd(home_dir2)
#stop("stop")
source(paste("extract_single_building_v",v_nr,".R",sep=""))
#end of script 'enhance_image'
################################################################################



