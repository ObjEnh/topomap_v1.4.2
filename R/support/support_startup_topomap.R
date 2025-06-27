## name of script: support_startup_topomap.R
cat("version_number= ",v_nr,"\n")
## purpose: extraction of one class of classification result
## author: Joachim Höhle
## GNU General Public License (GPL)

##contents:

## 1. extraction of one class by defined colour 

################################################################################

## 1. extract one class by defined colour 
##input: Result of classification (RGB image) 
##results of participants in ISPRS Labelling Benchmark (SVL5,DLR10)

#start of script
#install.packages("magick")
library(magick)
#home_dir <- "C:/Users/Joachim/R_programs/topomap/topomap_v1.4.1"
#home_dir2 <- "C:/Users/Joachim/R_programs/topomap/topomap_v1.4.1/R"
home_dir <- "C:/Users/Joachim/R_programs/buildenh_jh/v1.4/buildenh_v1.4.0_new/buildenh_v1.4.0"
home_dir2 <- "C:/Users/Joachim/R_programs/buildenh_jh/v1.4/buildenh_v1.4.0_new/buildenh_v1.4.0/R"
OrgClassResFilename <- "ISPRS_result_Gerke.tif" #ISPRS_labelling_contest_results_Gerke__top_mosaic_09cm_area4_class.tif_resized.jpg
#OrgClassResFilename <- "ISPRS_result_Gerke.tif" #ISPRS_labelling_contest_results_Gerke__top_mosaic_09cm_area4_class.tif_resized.tif
#OrgClassResFilename <- "Resultat_G4_neg_ISPRS_Benchmark"
#OrgClassResFilename <- "extracted_object_new4.tif"
OrgClassResPathname <- paste(home_dir,"/data/ISPRS4/Data_ISPRS Working Group III_4 - Participant_ M. Gerke (SVL_full_chessboard_seg_noCRF)_files_area 4/",sep = "")
#OrgClassResPathname <- paste(home_dir,"/data/",sep = "")
OrgClassResPathname
#OrgClassResFilename <- "top_mosaic_09cm_area4_class_DLR10.tif_resized.jpg"
setwd(OrgClassResPathname)
LCM_img <- readImage(OrgClassResFilename) #new
str(LCM_img)
print(LCM_img)
print(LCM_img[,,3])

#display the image
display(LCM_img)

#separate the RGB channels
red_channel <- LCM_img[,,1]
green_channel <- LCM_img[,,2]
blue_channel <- LCM_img[,,3]

#get the dimensions of the original LCM_img
LCM_img_dimensions <- dim(LCM_img)
LCM_img_dimensions


image_dimensions <- dim(LCM_img)
height <- image_dimensions[1]
width <- image_dimensions[2]
#
display(red_channel,method ="raster",bg="red")
display(green_channel, method= "raster",bg="green")
display(blue_channel,method="raster",bg="blue")
red_channel_neg <- 1 - red_channel
green_channel_neg <- 1 - green_channel
blue_channel_neg <- 1 - blue_channel
display(red_channel_neg,"raster")
display(green_channel_neg,"raster")
display(blue_channel_neg,"raster")

#convert the channels to vectors
red_vector <- as.vector(red_channel)
green_vector <- as.vector(green_channel)
blue_vector <- as.vector(blue_channel)

#blue_vector
str(blue_vector)
x_blue <- ifelse(blue_vector >= 0.9,1,0) #x_blue has intensity > 0.9
x_blue
blue_matrix <- matrix(x_blue,nrow=height, ncol=width)
display(blue_matrix)

#green_vector
green_vector
x_green <- ifelse(green_vector >= 0.9,1,0) #x_green has intensity > 0.9
x_green
green_matrix <- matrix(x_green,nrow=height, ncol=width)
display(green_matrix)

#red_vector
red_vector
str(red_vector)
x_red <- ifelse(red_vector >= 0.9,1,0) #x_red has intensity > 0.9
x_red
red_matrix <- matrix(x_red,nrow=height, ncol=width)
display(red_matrix,"raster",bg="red")   #red
display(green_matrix,"raster",bg="green") #green
display(blue_matrix,"raster",bg="blue")  #green

#combine the matrices into a 3D array
combined_array <- array(c(red_matrix, green_matrix, blue_matrix), dim = c(height, width, 3))
str(combined_array)
print(combined_array)

#verify by displaying the combined image
combined_image <- Image(combined_array)
display(combined_image)
combined_image[,,1]
combined_image[,,2]
combined_image[,,3]
str(combined_image)
max(combined_image@.Data)
#
matrix_rgb <- cbind(x_red,x_green,x_blue)
str(matrix_rgb)
matrix_rgb[,3]
vector_extr <- rep(0,length(x_red))
vec <- 1 : length(x_red)
x_red
x_green
x_blue

for (i in vec) {
  if (x_red[i] == 0 && x_green[i] == 0 && x_blue[i] == 1) {
    vector_extr[i] <- 1
  } else {
    vector_extr[i] <- 0 #new
  }
}
vector_extr
matrix_extr <- matrix(vector_extr,nrow=height, ncol=width)
display(matrix_extr)
Res <- Image(matrix_extr)

#display(vector_extr)
array_extr <- array(vector_extr,dim=c(600,813,3))
str(array_extr)
array_extr[,,2]
display(array_extr[,,2])
array_extr[,,1] <- 1
array_extr[,,2] <- 1
extr_c <- rgbImage(red=array_extr[,,1],green=array_extr[,,2],blue=array_extr[,,3])
display(extr_c[,,1])
display(extr_c[,,2])
display(extr_c[,,3])
display(extr_c)
colorMode(extr_c) <- Grayscale
display(extr_c[,,3])
Res <- extr_c[,,3]
display(Res,"raster")
Res_neg <- 1- Res
display(Res_neg)
str(Res)
setwd(OrgClassResPathname)
#save(Res,file = "Resultat_DLR10_ISPRS_Benchmark" )
#load(file="Resultat_DLR10_ISPRS_Benchmark")
display(Res)
#writeImage(Res,"Resultat_G4_ISPRS_Benchmark.tif")
#writeImage(Res,"Resultat_DLR10_ISPRS_Benchmark.tif") #white buildings
getwd()
f=OrgClassResFilename
Res_fromFile <- readImage(f)
display(Res_fromFile)
Res_extr_building_neg <- 1 - Res_fromFile
display(Res_extr_building_neg)
#
extr_c[,,3] <- 1 - extr_c[,,3]
colorMode(extr_c) <- Grayscale
str(extr_c[,,3])
display(extr_c[,,3])
Res_neg <- extr_c[,,3]
display(Res_neg)
#writeImage(Res_neg,files="Resultat_G4_neg_ISPRS_Benchmark2.tif") #black buildings
#writeImage(Res_neg,files="Resultat_DLR10_neg_ISPRS_Benchmark.tif") #black buildings
#

##end of script 1 (extract one class by defined colour.R)
