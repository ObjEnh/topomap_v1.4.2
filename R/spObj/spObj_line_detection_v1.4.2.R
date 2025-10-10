##name of script: spObj_line_detection.R
##purpose: dealing with special objects in script 'line detection.R' 
##instructions: change of default values may be required
#default values: ref_line=1
#default values: n_pix=15 ("extr_wd"),25 ("4_long"),35 ("100_all"),15("100_all+nonortho")
#small rectangular lines can be detected by:
#changing the default value (n_pix=35) in 'line detection.R' or by
#pointing to one pixel using script 'support_line_detection.R'
##author: Joachim Höhle
##GNU General Public License (GPL)

cat(paste("name of script: spObj_line_detection.R_v",v_nr,sep = ""),"\n")

##orthoimage "ISPRS7"

if (Img_name == "ISPRS7") { 
  
  ##b18
  #cas="100_all+nonortho"
  
  if (bnr2 == 18 && p_pos == "cor_det") {
    lnr_det4 <- lnr_det3[-c(2,4,7,10)] 
    lnr_det4[10] <- c(5)
    lnr_det5 <- lnr_det4
  } #end b18
  
  ##b23
  #cas = "100_all"
  
  if (bnr2 == 23 && p_pos == "cor_det") {
     n2 <- nrow(B5_6)
     B5_6[n2+1,] <- B5_4[10,]
     B5_6 <- B5_6[-c(2:3,7:9,11:15),1:8]
     n_B5_6 <- length(B5_6$lnr)
     row.names(B5_6) <- 1 : n_B5_6
     B5_6$ortho <- 1
     B5_6R4 <- B5_6
  } #end b23
    
} #end of orthoimage "ISPRS7"

###############################################################################

##ISPRS1

if (Img_name == "ISPRS1") { 
  
  ##b11
  #demo
  #cas="100_all+nonortho"
  
  if (bnr2 == 11 && p_pos == "cor_det") {
    B5_6
    B5_6[5,] <- B4[173,] #correction of line
    B5_6R4 <- B5_6
    lnr_det5 <- B5_6R4$lnr
  } #end b11
  
} #end of orthoimage "ISPRS1"


##ISPRS4

if (Img_name == "ISPRS4") { 
  
  #b12 (b1-part2)
  #cas='100_all'
  
  if (bnr2 == 12 && p_pos == "cor_det") { 
    B5_6
    B5_6[14,] <- B5_4_ord[46,]
    B5_6[15,] <- B5_4_ord[41,]
    B5_6[16,] <- B5_4_ord[54,]
    B5_6[17,] <- B5_4_ord[9,]
    B5_6[18,] <- B5_4_ord[12,]
    B5_6[19,] <- B5_4_ord[15,]
    B5_6[20,] <- B5_4_ord[5,]
    B5_6[21,] <- B5_4_ord[57,]
    B5_6[22,] <- B5_4_ord[23,]
    B5_6[1:22,8] <- 1
    row.names(B5_6) <- 1 : length(B5_6$lnr)
    B5_6 <- B5_6[-c(9,11),]
    row.names(B5_6) <- 1 : length(B5_6$lnr)
    B5_6
    B5_6R4 <- B5_6
    B5_6R4
  } #end b12 (b1-part2)
  
  ####
  
  #b61
  #cas ="100_all"
  
  if (bnr2 == 61 && p_pos == "cor_det") { 
    B5_6
    B5_6[10,] <- B5_4_ord[7,]
    B5_6[1:10,8] <- 1
    B5_6 <- B5_6[-3,]
    B5_6R4 <- B5_6
    row.names(B5_6R4) <- 1 : length(B5_6R4$lnr)
    B5_6R4
  } #end b61
  
  #b62
  #case='100_all'
  
  if (bnr2 == 62 && p_pos == "cor_det") { 
    B5_6
    B5_7 <- B5_6
    B5_6R4 <- B5_7[-6,] #417
    row.names(B5_6R4) <- 1 : nrow(B5_6R4)
    B5_6R4
    B5_6R4[7,] <- B5_4_ord[8,] #4
    B5_6R4[8,] <- B5_4_ord[16,] #126
    B5_6R4[7:8,8] <- 1
    row.names(B5_6R4) <- 1 : length(B5_6R4$lnr)
    B5_6R4
  } #end b62
  
  ####
  
  #b81
  #cas = "100_all"
  
  if (bnr2 == 81 && p_pos == "cor_det") { 
    B5_6
    B5_6 <- B5_6[-c(7:10),]
    B5_6
    B5_6R4 <- B5_6
    row.names(B5_6R4) <- 1 : length(B5_6R4$lnr)
    B5_6R4
    B5_6R4[10,] <- B5_4_ord[10,]
    B5_6R4[10,8] <- 1
    B5_6R4
  } #end b81
  
  #82
  #cas = "100_all"
  
  if (bnr2 == 82 && p_pos == "cor_det") { 
    B5_6
    B5_6 <- B5_6[-c(8,13),]
    B5_6
    B5_6R4 <- B5_6
    row.names(B5_6R4) <- 1 : length(B5_6R4$lnr)
    B5_6R4
    B5_6R4[13,] <- B5_4_ord[41,] #164
    B5_6R4[14,] <- B5_4_ord[24,] #289
    B5_6R4[15,] <- B5_4_ord[6,] #2
    B5_6R4[16,] <- B5_4_ord[14,] #1
    B5_6R4[17,] <- B5_4_ord[14,] #1
    B5_6R4[18,] <- B5_4_ord[14,] #1
    row.names(B5_6R4) <- 1 : length(B5_6R4$lnr)
    B5_6R4[1:18,8] <- 1
    B5_6R4
  } #end b82
  
  ####
  
  ##b9
  #cas = "100_all"
  
  if (bnr2 == 9 && p_pos == "cor_det") { 
    B5_6
    B5_6 <- B5_6[-c(6,7),]
    B5_6
    B5_6R4 <- B5_6
    row.names(B5_6R4) <- 1 : length(B5_6R4$lnr)
    B5_6R4[8,] <- B5_4_ord[35,]
    B5_6R4[8,8] <- 1
    row.names(B5_6R4) <- 1 : length(B5_6R4$lnr)
    B5_6R4[7,] <- B5_4_ord[37,]
    B5_6R4[7,8] <- 1
    B5_6R4
  } #end b9
  
  ####
  
  ##b101
  #cas = "100_all"
  
  if (bnr2 == 101 && p_pos == "cor_det") { 
    B5_6
    B5_6 <- B5_6[-c(1,3,10,11,12),] #75,21,188,941,580
    B5_6
    B5_6R4 <- B5_6
    B5_6R4
    row.names(B5_6R4) <- 1 : length(B5_6R4$lnr)
    B5_6R4[9,] <- B5_4_ord[5,] #296
    B5_6R4[10,] <- B5_4_ord[27,] #26
    B5_6R4[11,] <- B5_4_ord[45,] #178
    B5_6R4[12,] <- B5_4_ord[47,] #104
    B5_6R4[13,] <- B5_4_ord[48,] #179
    B5_6R4[14,] <- B5_4_ord[55,] #288
    B5_6R4[1:14,8] <- 1
    row.names(B5_6R4) <- 1 : length(B5_6R4$lnr)
    B5_6R4
  } #end b101
  
  if (bnr2 == 102 && p_pos == "cor_det") { 
    B5_6
    B5_6 <- B5_6[-c(2,3,6),] #707,#449,#639
    B5_6
    B5_6R4 <- B5_6
    row.names(B5_6R4) <- 1 : length(B5_6R4$lnr)
    B5_6R4[1:4,8] <- 1
    row.names(B5_6R4) <- 1 : length(B5_6R4$lnr)
    B5_6R4
  } #end b102
  
  if (bnr2 == 103 && p_pos == "cor_det") { 
    B5_6
    B5_6 <- B5_6[-c(8,11,12),] #429,#700,#934
    B5_6
    B5_6R4 <- B5_6
    row.names(B5_6R4) <- 1 : length(B5_6R4$lnr)
    B5_6R4
    B5_6R4[11,] <- B5_4_ord[47,] #1b
    B5_6R4[12,] <- B5_4_ord[33,] #255
    row.names(B5_6R4) <- 1 : length(B5_6R4$lnr)
    B5_6R4[1:12,8] <- 1
    B5_6R4
  } #end b103
  
  ####
  
  #b161
  #cas = "100_all+nonortho"
  
  if (bnr2 == 161 && p_pos == "cor_det") { 
    B5_6
    B5_6R4 <- B5_6[-c(3,4,5,8,9,10,11,12),] #105,71,83,133,823,1118,342,247
    row.names(B5_6R4) <- 1 : length(B5_6R4$lnr)
    B5_6R4
  } #end b161
  
  if (bnr2 == 162 && p_pos == "cor_det") { 
    B5_6
    B5_6R4 <- B5_6[-c(2:18),] #133,320,282,26,212,137,109,122,259,98,1221,10,241,872,805
    row.names(B5_6R4) <- 1 : length(B5_6R4$lnr)
    B5_6R4
  } #end b162
  
  if (bnr2 == 163 && p_pos == "cor_det") { 
    B5_6
    B5_6R4 <- B5_6[-c(7),] #
    row.names(B5_6R4) <- 1 : length(B5_6R4$lnr)
    B5_6R4[10,] <- c(24,28,410,121,135,309,32)
    B5_6R4
  } #end b163
  
} #end of orthoimage "ISPRS4"

################################################################################
##ISPRS4_DLR10

if (Img_name == "ISPRS4_DLR10") { 
  
##b32 (b3-part2)
#cas='100_all'

  if (bnr2 == 32 && p_pos == "cor_det") { 
    B5_6
    B5_6 <- B5_6[-c(7,12),] #925,#878
    B5_6
    B5_6R4 <- B5_6
    B5_6R4
    row.names(B5_6R4) <- 1 : length(B5_6R4$lnr)
    B5_6R4
    B5_6R4[12,] <- B5_4_ord[43,] #285
    B5_6R4[13,] <- B5_4_ord[8,] #312
    B5_6R4[14,] <- B5_4_ord[7,] #138
    B5_6R4[15,] <- B5_4_ord[15,] #14
    B5_6R4[16,] <- B5_4_ord[2,] #4
    B5_6R4[1:16,8] <- 1
    row.names(B5_6R4) <- 1 : length(B5_6R4$lnr)
    B5_6R4
  } #end b32
##
  if (bnr2 == 8 && p_pos == "cor_det") { 
    B5_6
    B5_6 <- B5_6[-c(11),] #538
    B5_6R4 <- B5_6
    B5_6R4
    n_z <- length(B5_6R4$lnr)
    row.names(B5_6R4) <- 1 : n_z
    B5_6R4[14,] <- B5_4_ord[34,] #872
    B5_6R4[15,] <- B5_4_ord[5,] #427
    B5_6R4[16,] <- B5_4_ord[38,] #636
    B5_6R4[17,] <- B5_4_ord[7,] #106
    B5_6R4[18,] <- B5_4_ord[6,] #101
    B5_6R4[1:18,8] <- 1
    B5_6R4
  } #end b8 
  
  if (bnr2 == 10 && p_pos == "cor_det") { 
    B5_6
    B5_6 <- B5_6[-c(7,8),] #977, #903
    B5_6R4 <- B5_6
    B5_6R4
    n_z <- length(B5_6R4$lnr)
    row.names(B5_6R4) <- 1 : n_z
    B5_6R4[11,] <- B5_4_ord[3,] #29
    B5_6R4[12,] <- B5_4_ord[52,] #1087
    B5_6R4[13,] <- B5_4_ord[15,] #8
    B5_6R4[14,] <- B5_4_ord[5,] #11
    n_z2 <- length(B5_6R4$lnr)
    B5_6R4[1:n_z2,8] <- 1 
    B5_6R4
  } #end b10  
  
  #
  if (bnr2 == 12 && p_pos == "cor_det") { 
    B5_6
    B5_6R4 <- B5_6
    B5_6R4
    n_z <- length(B5_6R4$lnr)
    row.names(B5_6R4) <- 1 : n_z
    B5_6R4[8,] <- B5_4_ord[3,] #87
    B5_6R4[9,] <- B5_4_ord[13,] #546
    B5_6R4[10,] <- B5_4_ord[38,] #91
    n_z2 <- length(B5_6R4$lnr)
    B5_6R4[1:n_z2,8] <- 1 
    row.names(B5_6R4) <- 1 : n_z2
    B5_6R4
  } #end b12 
  
##b14
#cas='100_all+nonortho'  
  if (bnr2 == 14 && cas == "100_all+nonortho" && p_pos == "cor_det") {
    cat("p_pos= ", p_pos, "\n")
    #stop("continue step by step")
    B5_6
    B5_6R4 <- B5_6
    B5_6R4 <- B5_6R4[-c(1:15),]
    n_z <- nrow(B5_6R4)
    row.names(B5_6R4) <- 1 : n_z
    B5_6R4[1:n_z,8] <- 0
    colnames(B5_6R4)[8]="ortho"
    n_nonortholines2 <- length(B5_6R4[,1])
    n_ortholines2=0
    n_total = n_nonortholines2 
  } #end b14 - "cor_det"
  # end bnr2=14, cas ="100_all+nonortho", p_pos="cor_det" 
  
#cas='nonortho_only'
  
  if (bnr2 == 14 && cas == "nonortho_only" && p_pos == "cor_det") {
    cat("p_pos= ", p_pos, "\n")
    B5_6
    B5_6R4 <- B5_6
    n_z <- nrow(B5_6R4)
    B5_6R4[1:n_z,8] <- 0
    colnames(B5_6R4)[8]="ortho"
    n_nonortholines2 <- length(B5_6R4[,1])
    n_ortholines2=0
    n_total = n_nonortholines2 
  } #end b14 - "cor_det"
# end bnr2=14, cas ="nonortho_only", p_pos="cor_det" 

  ##b15
  #cas='100_all'
  
  if (bnr2 == 15 && p_pos == "cor_det") { 
    B5_6
    B5_6R4 <- B5_6
    n_z <- length(B5_6R4$lnr)
    row.names(B5_6R4) <- 1 : n_z
    B5_6R4
    B5_6R4[11,] <- B5_4_ord[10,] #91
    B5_6R4[12,] <- B5_4_ord[7,] #262
    B5_6R4[13,] <- B5_4_ord[27,] #239
    B5_6R4[14,] <- B5_4_ord[21,] #196
    B5_6R4[15,] <- B5_4_ord[1,] #1
    B5_6R4[16,] <- B5_4_ord[10,] #91
    n_z2 <- length(B5_6R4$lnr)
    B5_6R4[1:n_z2,8] <- 1
    row.names(B5_6R4) <- 1 : n_z2
    colnames(B5_6R4)[8]="ortho"
    B5_6R4
  } #end b15  
  
  ##b17
  #cas='100_all'
  
  if (bnr2 == 17 && p_pos == "cor_det") { 
    B5_6
    B5_6R4 <- B5_6
    B5_6R4 <- B5_6R4[-c(8,11,12),] 
    n_z <- length(B5_6R4$lnr)
    row.names(B5_6R4) <- 1 : n_z
    B5_6R4
    B5_6R4[11,] <- B5_4_ord[26,] #270
    B5_6R4[12,] <- B5_4_ord[47,] #10
    B5_6R4[13,] <- B5_4_ord[33,] #923
    B5_6R4[14,] <- B5_4_ord[46,] #292
    B5_6R4[15,] <- B5_4_ord[30,] #90
    B5_6R4[16,] <- B5_4_ord[51,] #101
    n_z2 <- length(B5_6R4$lnr)
    B5_6R4[1:n_z2,8] <- 1
    row.names(B5_6R4) <- 1 : n_z2
    colnames(B5_6R4)[8]="ortho"
    B5_6R4
  } #end b17
  
  ##b21
  #cas="100_all"
  
  if (bnr2 == 21 && p_pos == "cor_det") { 
    B5_6
    B5_6R4 <- B5_6
    n_z <- length(B5_6R4$lnr)
    row.names(B5_6R4) <- 1 : n_z
    B5_6R4
    B5_6R4[9,] <- B5_4_ord[44,] #424
    B5_6R4[10,] <- B5_4_ord[12,] #393
    B5_6R4[11,] <- B5_4_ord[15,] #90
    B5_6R4[12,] <- B5_4_ord[15,] #90
    B5_6R4[13,] <- B5_4_ord[35,] #10
    B5_6R4[14,] <- B5_4_ord[50,] #1
    n_z2 <- length(B5_6R4$lnr)
    B5_6R4[1:n_z2,8] <- 1
    row.names(B5_6R4) <- 1 : n_z2
    colnames(B5_6R4)[8]="ortho"
    B5_6R4
  } #end b21
  
  ##b221
  #cas="100_all+"
  
  if (bnr2 == 221 && p_pos == "cor_det") { 
    B5_6
    B5_6R4 <- B5_6
    B5_6R4 <- B5_6R4[-c(2:14),] 
    n_z <- length(B5_6R4$lnr)
    row.names(B5_6R4) <- 1 : n_z
    B5_6R4[1,] <- c(1,25,356,286,120,369,118)
    B5_6R4
    n_z2 <- length(B5_6R4$lnr)
    B5_6R4[1:n_z2,8] <- 0
    row.names(B5_6R4) <- 1 : n_z2
    colnames(B5_6R4)[8]="ortho"
    B5_6R4
    n_nonortholines2 <- n_z2
    n_ortholines2 <- 0
    n_total <- n_nonortholines2 + n_ortholines2
  } #end b221
  
  ##b25
  if (bnr2 == 25 && p_pos == "cor_det") { 
    B5_6
    B5_6R4 <- B5_6
    n_z <- length(B5_6R4$lnr)
    row.names(B5_6R4) <- 1 : n_z
    B5_6R4
    B5_6R4[8,] <- B5_4_ord[10,] #135
    n_z2 <- length(B5_6R4$lnr)
    B5_6R4[1:n_z2,8] <- 1
    row.names(B5_6R4) <- 1 : n_z2
    colnames(B5_6R4)[8]="ortho"
    B5_6R4
  } #end of b25
  
  ##b26
  
  #cas='100_all+nonortho'
  
  if (bnr2 == 26 && p_pos == "cor_det" && cas == "100_all+nonortho") { 
    #stop("continue step by step")
    B5_6
    B5_6R4 <- B5_6
    B5_6R4 <- B5_6R4[-c(1:17),] 
    n_z <- length(B5_6R4$lnr)
    row.names(B5_6R4) <- 1 : n_z
    B5_6R4
    n_z2 <- length(B5_6R4$lnr)
    B5_6R4[1:n_z2,8] <- 0 #changed 
    row.names(B5_6R4) <- 1 : n_z2
    colnames(B5_6R4)[8]="ortho"
    B5_6R4
  } #end of b26
  
  
  ##cas='nonortho_only_RDP'
  
  # if (bnr2 == 26 && p_pos == "cor_img") { 
  #   #stop("continue step by step")
  #   cat("p_pos= ", p_pos, "\n")
  #   plot(W$'2', col="white")  #black building
  #   w = W$'2'
  #   plot(w)
  #   out_poly <- as.polygonal(w) #conversion to polygons
  #   plot(out_poly)
  #   out_poly_df <- as.data.frame(out_poly)
  #   n_pt <- length(out_poly_df$x)
  #   y3 <- 1 : n_pt
  #   x_v <- round(out_poly_df$x)
  #   y_v <- round(out_poly_df$y)
  #   plot(x_v,y_v,type = "l", asp=1)
  #   lines(x_v,y_v,col="red", lty=2, asp=1)
  #   simplified_lines <- RamerDouglasPeucker(x_v, y_v, epsilon = 5) #call of RDP-function
  #   simplified_lines
  #   lines(simplified_lines, col = "green", lty = 1, lwd=2,asp=1)
  #   simplified_lines_cor <- simplified_lines
  #   n_simpl_lines_cor <- length(simplified_lines_cor$x)
  #   row.names(simplified_lines_cor) <- 1 : n_simpl_lines_cor
  #   simplified_lines_cor
  #   lines(simplified_lines_cor, col = "blue", lty = 1, lwd=2,asp=1)
  # } #end b26 - cas='nonortho_only_RDP'
  # 
} #end of orthoimage "ISPRS4_DLR10"

##end of script 'spObj_line_detection.R'


