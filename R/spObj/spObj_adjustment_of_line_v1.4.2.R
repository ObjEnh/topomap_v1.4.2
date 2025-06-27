##name of script: spObj_adjustment_of_line.R
cat("version_number= ",v_nr,"\n")
#author: Joachim Höhle
#GNU General Public License (GPL)


cat("start of spObj_adjustment of line ","\n")


##buildings of orthoimage ISPRS_#7

if (Img_name == "ISPRS7") {  
  #no correction
} #end of ISPRS7

###

##buildings of orthoimage ISPRS1

if (Img_name == "ISPRS1") {
 #no corrections 
} #end of ISPRS1

###

##buildings of orthoimage ISPRS4

#b9

if (Img_name == "ISPRS4") {
  
  if(bnr2 == 81 && p_pos == "cor_adj_line") {
    B6
    B6[7,]<- c(140,125,-35,40,125,-35,2)
    B6[10,]<- c(515,35,585,22,35,585,2)
  } 
  
  if(bnr2 == 9 && p_pos == "cor_adj_line") {
    B6
    B6[15,]<- c(467,125,64,15,125,64,1)
  } 
} #end of ISPRS4

if (Img_name == "ISPRS4_DLR10") {
  
  #b17
  
  if(bnr2 == 17 && p_pos == "cor_adj_line") {
    #stop("correction of adjustment")
    B6
    B6[15,]<- c(923,125,255,17,125,255,1)
  } #end b17
  
} #end of ISPRS4_DLR10

##end of script 'spObj_adjustment_of_line_v1.4.0.R' 
