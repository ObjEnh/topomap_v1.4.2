cat("start of software package 'topomap' ","\n")
v_nr = "1.4.2" #version number of the program package
cat("name of the script:",paste("startup_topomap_v",v_nr,".R", sep=""),"\n")
#description: script starts the package 'buildenh'
#examples: extracted buildings from land cover maps derived by classification programs
#data: ISPRS test "Vaihingen": orthoimage of areas #1, #7, #4 (ISPRS Semantic Labeling Contest-results)
#author: Joachim Höhle
#publication: Automated mapping of buildings through classification of DSM-based
#ortho-images and cartographic enhancement, International Journal of Applied Earth
#Observations and Geoinformation 95(2021) 102237; https://doi.org/10.1016/j.jag.2020.102237
#instructions: use given examples for getting acquainted with the programs/scripts
#instructions: change directories for input 
#instructions: set parameter for automatic processing (line #126)
#instructions: input project title 
#instructions: save your home directory
#instructions: select orthoimage (line #68)
#instructions: select the OrgClassResFilename,OrgImgPathname,OrgImgFilename,OrgGtsPathname,OrgGtsFilename   
#instructions: type 'Ctrl+A'(select all) and 'Source'
#instructions: display all 4 panes
#instructions: new users may start by examples (processing mode = demo)
#instructions: The parameters (bnr2, p_m_md, part, ref_line, cas, n_pix, meth/sek, thr_line_seg) 
#must be selected in course of the 9 main scripts. The used parameters in the example can be 
#found in a table stored at './data'.
#instructions: find supporting software at './R/support'
#instructions: extract a class from the classification result (see support_startup_topomap.R)
#depends: R 4.2.1; BiocManager 1.30.18 (EBImage); spatstat 2.1-3; tiff 01-11; rpart 4.1.19; nlme 3.1-164;
#depends: RDP 0.3.0;
#Copyright(C) 2022 Joachim Höhle
#GNU General Public License (GPL)
##############################################################################################
#save your home directory
old_dir <- setwd("./")
getwd()
#
#home_dir <- "C:/Users/Joachim/R_programs/buildenh_jh/v1.4/buildenh_v1.4.0_new/buildenh_v1.4.0"
#home_dir2 <- "C:/Users/Joachim/R_programs/buildenh_jh/v1.4/buildenh_v1.4.0_new/buildenh_v1.4.0/R"
home_dir <- "C:/Users/Joachim/R_programs/topomap/topomap_v1.4.2"
home_dir2 <- "C:/Users/Joachim/R_programs/topomap/topomap_v1.4.2/R"
###############################################################################################

## title of project (manual input of characteristics)

#prj_title <- "ISPRS1_LCM2" #example#2
#orthoimage ISPRS#1 
#classification method: DT/LCM2 by 5 attributes
#training by orthoimage #26
#enhancement of buildings
#
#prj_title <- "ISPRS7_LCM1" #(example1)
#orthoimage: ISPRS#7
#classification method: DT/LCM1 by 17 attributes
#training by orthoimage #7
#enhancement of buildings
#
#prj_title <- "ISPRS Semantic Labeling benchmark_result" 
#orthoimage  ISPRS#4
#classification method: SVL_5, supervised
#enhancement of buildings
#pixel size on ground=0.283m

prj_title <- "ISPRS Semantic Labeling benchmark_result" 
#orthoimage  ISPRS#4
#classification method: DLR10, supervised
#enhancement of buildings
#pixel size on ground=0.283m


##########################################

cat("project title is = ", prj_title,"\n")
setwd(home_dir)

#select orthoimage (activate manually)
#Img_name <- readline("type name of orthoimage: ") #line can be avoided when Img_name is selected
#Img_name <- "ISPRS1" #name of orthoimage to be processed (example2)
#Img_name <- "ISPRS7" #name of orthoimage to be processed (example1)
#Img_name <- "ISPRS4" #name of orthoimage to be processed (example3)
Img_name <- "ISPRS4_DLR10" #name of orthoimage to be processed (example4)

if (Img_name == "ISPRS1") { #example2
  ##setting of path- & file-name for original data:
  setwd(home_dir)
  OrgClassResPathname <- paste(home_dir,"/data",sep = "")
  OrgClassResFilename <- "ISPRS_#1_b.tiff" #extracted buildings
  #generate file in 'support_startup_topomap' script #1.
  OrgImgPathname <- paste(home_dir,"/data",sep = "")
  OrgImgFilename <- "top_mosaic_09cm_area1.tif" #pixel size=0.09m
  OrgGtsPathname <- paste(home_dir,"/data",sep = "") 
  OrgGtsFilename <- "gts_top_mosaic_09cm_area1.tif" #pixel size on ground=0.09m
} #end of image1

if (Img_name == "ISPRS7") { #example1
  ##setting of path- & file-name for original data:
  OrgClassResFilename <- "ISPRS_#7_b.tiff" #extracted buildings,generate file in 'support_startup_topomap' script #1.
  OrgClassResPathname <- paste(home_dir,"/data",sep = "")
  OrgImgPathname <- paste(home_dir,"/data",sep = "")
  OrgImgFilename <- "top_mosaic_09cm_area7.tif"  #pixel size=0.09m
  OrgGtsPathname <- paste(home_dir,"/data",sep = "")
  OrgGtsFilename <- "gts_top_mosaic_09cm_area7.tif" #pixel size on ground=0.09m
} #end of image7

if (Img_name == "ISPRS4") { #classification: result SVL_5
  ##setting of path- & file-name for original data:
  OrgClassResFilename <- "Resultat_G4_neg_ISPRS_Benchmark_building.tif" #extracted class 'building', gray, #pixel size=0.28305m 
  #generate file in 'support_startup_topomap' script #1.
  OrgClassResPathname <- paste(home_dir,"/data/",sep = "") #pixel size=0.283m
  OrgImgPathname <- paste(home_dir,"/data/",sep = "")
  OrgImgFilename <- "ortho_top_mosaic_09cm_area4.tif_resized.jpg"  #pixel size=0.28305m
  OrgGtsPathname <- paste(home_dir,"/data",sep = "")
  OrgGtsFilename <- "GTS_top_mosaic_09cm_area4_600x813.tif" #pixel size on ground=0.28305m
} #end of orthoimage "ISPRS4"

if (Img_name == "ISPRS4_DLR10") { #classification result: DLR10
  ##setting of path- & file-name for original data:
  OrgClassResPathname <- paste(home_dir,"/data/",sep = "") 
  OrgClassResFilename <- "Resultat_DLR10_neg_ISPRS_Benchmark.tif" #extracted class 'building', gray, #pixel size=0.28305m
  #generate file in 'support_startup_topomap' script #1.
  OrgImgPathname <- paste(home_dir,"/data/",sep = "")
  OrgImgFilename <- "ortho_top_mosaic_09cm_area4.tif_resized.jpg"  #pixel size=0.28305m
  OrgGtsPathname <- paste(home_dir,"/data",sep = "")
  OrgGtsFilename <- "GTS_top_mosaic_09cm_area4_600x813.tif" #pixel size on ground=0.28305m
} #end of orthoimage "ISPRS4"

##setting of parameters
proc_mode <- "NA" #mode of processing
n_long_PCs <- 1 #number of pixel clouds (PCs) 
input_mode <- "vector" #input of nonortholines ("single" or "vector")
n_pix8=80 #number of pixels of the 8th longest line segment in B8 (parameter 
#for determination of object type)
thr_d_alph_obj=2 #[degrees], threshold for deviation of rotation angle in 
#transformation of img-system to man-system

## install packages if required
# install.packages('EBImage')
# if (!require("BiocManager", quietly = TRUE))
# install.packages("BiocManager")
# BiocManager::install("EBImage")
# install.packages('spatstat')
# install.packages('tiff')
# install.packages('rpart')
# install.packages('nlme')
# install.packages("RDP")

##loading of libraries 
setwd(home_dir2)
source("./func/func_loadLib_op.R") #load of other R-packages
source("./func/func_loadLib_jh.R") #load of functions for the R-package 'buildenh'
#
loadLib_op() #call of function
loadLib_jh() #call of function

#other functions
display = function(...) if (interactive()) EBImage::display(...)

#setup for processing mode "auto"

if (Img_name == "ISPRS1") {
  y_auto <- c(4,5,7) #objects for automatic processing (orthoimage #1)  
}

if (Img_name == "ISPRS7") {
  y_auto <- c(20,22,23) #objects for automatic processing (orthoimage #7)  
}

if (Img_name == "ISPRS4") {
  y_auto <- c(3,4,6) #objects for automatic processing (orthoimage #4)  
}

if (Img_name == "ISPRS4_DLR10") {
  y_auto <- c(4,5) #objects for automatic processing (orthoimage #4)  
}
n_y_auto <- length(y_auto)
k_y_auto <- 1

#setup of parameter/variables
par(mai = c(1.02,0.82,0.82,0.42)) #setup of margins/plot region [inches]

bnr2_part <- "NA" #partition of object

cat("end of 'startup_buildenh.R' - continue with 'enhance_image.R' ", "\n")
#end of 'startup_topomap.R'

##start the next script ("enhance_image.R")
setwd(home_dir2)
source(paste("enhance_image_v",v_nr,".R",sep=""))
#

