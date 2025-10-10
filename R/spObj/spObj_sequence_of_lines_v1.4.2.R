##name of script: spObj_sequence_of_lines.R
cat("version_number= ",v_nr,"\n")
#purpose: dealing with special objects 
#correction of position of line-centers, angle of line and sequence of lines 
#instruction: if corrections in the positions are required -> use interactive 
#             detection of lines in 'support_line_detection.R' 
#data: ISPRS orthoimages #1, #7, #4
#author: Joachim Höhle
#GNU General Public License (GPL)


##orthoimage ISPRS7

if (Img_name == "ISPRS7") { 
   
  #b18 
  #demo
  
  if (bnr2 == 18 && p_pos == "cor_img") {
    plot(W$'3', col="white")  
    w = W$'3'
  } #end b18 - "cor_img"
  
  if (bnr2 == 18 && p_pos == "cor_pos") {  
    b13_angle_df[10,1:4] <- c(5,NA,789.8,1144.4) #line 5b
    b13_angle_df[8,1:4] <- c(3,NA,1017,1084)
    b13_angle_df[9,1:4] <- c(4,NA,1131.6,1044.1)
    b13_angle_df[6,1:4] <- c(5,NA,953.2,921.5) #line 5a
    b13_angle_df2 <- b13_angle_df
  } #end of b18
  
  #end b18

    
  #b23
  #auto
  
  if (bnr2 == 23 && p_pos == "cor_det") { 
    lnr_det3[6] <- c(36) 
    lnr_det5 <- lnr_det3
    n_PCR <- length(lnr_det5)
    B5_6R <- B5_6
    B5_6R <- B5_6R[1:n_PCR,]
    B5_6R$lnr[1:n_PCR] <- lnr_det5 
    row.names(B5_6R) <- 1:n_PCR
    B5_6R
    B5_4
    n_B5_4 <- length(B5_4$lnr)
    
    # loop
    i <- 1
    
    while (i <= n_B5_4) {
      
      if (B5_4$lnr[i] == lnr_det3[6]) { 
        B5_6R[6,2:7] <- B5_4[i,2:7]
        B5_6R[6,8] <- 1
      } #end if 
      
      i <- i + 1 
    } #end of loop
    
    B5_6R4 <- B5_6R
  } #end of b23

} #end of ISPRS7
################################################################################

##orthoimage ISPRS1

if (Img_name == "ISPRS1") {
  #b11
  #demo
  
  if (bnr2 == 11 && p_pos == "cor_img") {
    plot(W$'3', col="white")  
    w = W$'3'
  } #end b18 - "cor_img"
  
  if (bnr2 == 11 && p_pos == "cor_pos") {  
    #determine new position by 'support_line_detection.R', #6
    #or with angle by 'support_sequence_of_lines.R', #9
    b13_angle_df
    b13_angle_df[8,3:4] <- c(1066,589) #lnr=24, midpoint is manually derived
    b13_angle_df2 <- b13_angle_df
  } #end b11

} #end of ISPRS1


##orthoimage ISPRS4
#Gerke_SVL5

if (Img_name == "ISPRS4") {
  
  #b12 (b1,"2parts_2")
  
  if (bnr2 == 12 && p_pos == "cor_pos") {  
    #determine new position by 'support_line_detection.R', #6
    #or with angle by 'support_sequence_of_lines.R', #9
    b13_angle_df
    b13_angle_df[9,1:4] <- c(432,NA,162,157) #lnr=432,midpoint is manually determined
    b13_angle_df[10,1:4] <- c(110,NA,101,126) #lnr=110,midpoint is manually determined
    b13_angle_df[13,1:4] <- c(26,NA,244,151) #lnr=26,midpoint is manually determined
    b13_angle_df[14,1:4] <- c(37,NA,157,235) #lnr=37,midpoint is manually determined
    b13_angle_df[15,1:4] <- c(429,NA,172,98) #lnr=429,midpoint is manually determined
    b13_angle_df[16,1:4] <- c(569,NA,212,174) #lnr=569,midpoint is manually determined
    b13_angle_df[17,1:4] <- c(872,NA,224,168) #lnr=872,midpoint is manually determined
    b13_angle_df[18,1:4] <- c(296,NA,184,101) #lnr=344,midpoint is manually determined
    b13_angle_df[19,1:4] <- c(311,NA,129,186) #lnr=311,midpoint is manually determined
    b13_angle_df[20,1:4] <- c(70,NA,224,187) #lnr=70,midpoint is manually determined
    b13_angle_df2 <- b13_angle_df
    b13_angle_df2 
  } #end b61 "cor_pos"
  
  
  if (bnr2 == 12 && p_pos == "cor_img") {
    plot(W$'3', col="white")  #black building
    w = W$'3'
  } #end b62 - "cor_img"
  
  ###
  
  ##b61
  if (bnr2 == 61 && p_pos == "cor_pos") {  
    #determine new position by 'support_line_detection.R', #6
    #or with angle by 'support_sequence_of_lines.R', #9
    b13_angle_df
    b13_angle_df[2,1:4] <- c(19,NA,382,108) #lnr=19,midpoint is manually changed
    b13_angle_df[10,1:4] <- c(2,NA,440,190) #lnr=2,midpoint is manually derived
    b13_angle_df2 <- b13_angle_df
    b13_angle_df2 
  } #end b61 "cor_pos"
  
  if (bnr2 == 61 && p_pos == "cor_img") {
    plot(W$'2', col="white")  #black building
    w = W$'2'
  } #end b62 - "cor_img"
  
  #b62
  
  if (bnr2 == 62 && p_pos == "cor_pos") {  
    #determine new position by 'support_line_detection.R', #6 or ('support_sequence_of_lines.R, #1)
    #or for angle by 'support_sequence_of_lines.R', #9
    b13_angle_df2[3,1:4] <- c(145,NA,467,212) #midpoint coordinates are manually derived
    b13_angle_df2[7,1:4] <- c(4,NA,477,180) #lnr=4a,midpoint coordinates are manually derived
    b13_angle_df2[2,1:4] <- c(4,NA,509,200) #lnr=4b,midpoint coordinates are manually derived
    b13_angle_df2[8,1:4] <- c(126,NA,463,188) #midpoint coordinates are manually derived
    b13_angle_df2
  } #end b62 - "cor_pos"
  
  if (bnr2 == 62 && p_pos == "cor_img") {
    plot(W$'3', col="white")  #black building
    w = W$'3'
  } #end b62 - "cor_img"
  
  ##end b62
  
  ####
  
  ##b81
  if (bnr2 == 81 && p_pos == "cor_pos") {  
    #determine new position by 'support_line_detection.R', #6
    #or with angle by 'support_sequence_of_lines.R', #9
    b13_angle_df2
    b13_angle_df2[2,1:4] <- c(15,337.9,545,314) #lnr=,midpoint is manually changed
    b13_angle_df2[7,1:4] <- c(515,345.2,521,286) #lnr=,midpoint is manually changed
    b13_angle_df3 <- b13_angle_df2 
    b13_angle_df3 
  } #end b9 "cor_pos"
  
  ##82
  
  if (bnr2 == 82 && p_pos == "cor_img") {
    plot(W$'3', col="white")  #black building
    w = W$'3'
  } #end b82 - "cor_img"
  
  if (bnr2 == 82 && p_pos == "cor_pos") {  
    #determine new position by 'support_line_detection.R', #6
    #or with angle by 'support_sequence_of_lines.R', #9
    b13_angle_df2
    b13_angle_df2[1,1:4] <- c(2,NA,421,299) #lnr=2a,midpoint is manually changed
    b13_angle_df2[15,1:4] <- c(2,NA,487,346) #lnr=2b,midpoint is manually changed
    b13_angle_df2[3,1:4] <- c(1,NA,363,307) #lnr=1a,midpoint is manually changed
    b13_angle_df2[16,1:4] <- c(1,NA,438,359) #lnr=1b,midpoint is manually changed
    b13_angle_df2[17,1:4] <- c(1,NA,463,375) #lnr=1c,midpoint is manually changed
    b13_angle_df2[18,1:4] <- c(1,NA,499,401) #lnr=1d,midpoint is manually changed
    b13_angle_df2[2,1:4] <- c(148,NA,377,300) #lnr=148,midpoint is manually changed
    b13_angle_df2[7,1:4] <- c(655,NA,382,311) #lnr=655,midpoint is manually changed
    b13_angle_df3 <- b13_angle_df2 
    b13_angle_df3 
  } #end 82 + "cor_pos"
  
  ###
  
  ##b9
  if (bnr2 == 9 && p_pos == "cor_pos") {  
    #determine new position by 'support_line_detection.R', #6
    #or with angle by 'support_sequence_of_lines.R', #9
    b13_angle_df2
    b13_angle_df2[8,1:4] <- c(12,291.7,285,291) #lnr=12,midpoint is manually changed
    b13_angle_df3 <- b13_angle_df2 
    b13_angle_df3 
  } #end b9 "cor_pos"
  
  ##b102
  
  if (bnr2 == 102 && p_pos == "cor_img") { #filled area is not the object
    plot(W$'3', col="white")  #black building
    w = W$'3'
  } #end b102 - "cor_img"

  ##b103
  
  if (bnr2 == 103 && p_pos == "cor_img") { #filled area is not the object
    plot(W$'3', col="white")  #black building
    w = W$'3'
  } #end b102 - "cor_img"
  
  ##b103
  
  if (bnr2 == 103 && p_pos == "cor_pos") {  
    b13_angle_df2
    b13_angle_df3 <- b13_angle_df2
    b13_angle_df3[11,1:4] <- c(255,NA,254,486) #lnr=255,midpoint is manually changed
    b13_angle_df3[12,1:4] <- c(1,NA,216,490) #lnr=1b,midpoint is manually changed
    b13_angle_df3[2,1:4] <- c(83,NA,277,469) #lnr=83,midpoint is manually changed
    b13_angle_df3
  } #end b103 - "cor_pos"
  
  ###
  
  ##b161
  
  if (bnr2 == 161 && p_pos == "cor_img") { #filled area is not the object
    plot(W$'3', col="white")  #black building
    w = W$'3'
  } #end b161 - "cor_img"
  
  if (bnr2 == 161 && p_pos == "cor_pos") {  
    #stop("manual operation - add position")
    #determine position by 'support_sequence_of_lines.R' #1
    b13_angle_df2
    b13_angle_df3 <- b13_angle_df2
    b13_angle_df3[2,1:4] <- c(79,NA,85,552) #lnr=79,midpoint is manually changed
    b13_angle_df3[7,1:4] <- c(4,NA,43,587) #lnr=4,midpoint is manually changed
    b13_angle_df3[9,1:4] <- c(128,NA,141,557) #lnr=128,midpoint is manually changed
    b13_angle_df3[10,1:4] <- c(928,NA,125,669) #lnr=928,midpoint is manually changed
    b13_angle_df3[11,1:4] <- c(303,NA,121,645) #lnr=303,midpoint is manually changed
    b13_angle_df3[12,1:4] <- c(137,NA,67,578) #lnr=137,midpoint is manually changed
    b13_angle_df3[13,1:4] <- c(433,NA,97,587) #lnr=433,midpoint is manually changed
    b13_angle_df3
  } #end b161 - "cor_pos"
  
  ##162
  
  if (bnr2 == 162 && p_pos == "cor_img") { #filled area is not the object
    plot(W$'3', col="white")  #black building
    w = W$'3'
  } #end b162 - "cor_img"
  
  if (bnr2 == 162 && p_pos == "cor_pos") {  
    #stop("manual operation - add position")
    #determine position by 'support_sequence_of_lines.R' #1
    b13_angle_df2
    b13_angle_df3 <- b13_angle_df2
    b13_angle_df3[10,1:4] <- c(9,NA,187,756) #lnr=9,midpoint is manually changed
    b13_angle_df3[13,1:4] <- c(22,NA,209,786) #lnr=22,midpoint is manually changed
    b13_angle_df3[16,1:4] <- c(573,NA,227,731) #lnr=573,midpoint is manually changed
    b13_angle_df3[3,1:4] <- c(418,NA,172,619) #lnr=418,midpoint is manually changed
    b13_angle_df3[11,1:4] <- c(141,NA,186,772) #lnr=141,midpoint is manually changed
    b13_angle_df3[15,1:4] <- c(152,NA,234,773) #lnr=152,midpoint is manually changed
    b13_angle_df3
  } #end b162 - "cor_pos"
  
  if (bnr2 == 162 && p_pos == "cor_sek") { 
    sequence_seg
    sequence_seg <- c(22, 963, 141, 9, 13, 95, 209, 82, 107, 535, 418, 38, 1, 4, 436, 573, 152, 801)
    sequence_seg2 <-sequence_seg
  } #end b162 - "cor_sek"
  
  ##163
  
  if (bnr2 == 163 && p_pos == "cor_pos") {  
    #stop("manual operation - add position")
    #determine position by 'support_sequence_of_lines.R' #1
    b13_angle_df2
    b13_angle_df3 <- b13_angle_df2
    b13_angle_df3[10,1:4] <- c(24,301,318,756) #lnr=24,midpoint is manually determined
    b13_angle_df3
  } #end b162 - "cor_pos"
  
  ##b17
  
  if (bnr2 == 17 && p_pos == "cor_img") { #filled area is not the object
    
    plot(W$'2', col="white")  #black building
    w = W$'2'
    
  } #end b17 - "cor_img"
  
  if (bnr2 == 17 && p_pos == "cor_pos") {  
    #stop("manual operation - add position")
    #determine position by 'support_sequence_of_lines.R' #1
    b13_angle_df2
    b13_angle_df3 <- b13_angle_df2
    b13_angle_df3[3,1:4] <- c(194,NA,319,528) #lnr=194,midpoint is manually determined
    b13_angle_df3
  } #end b162 - "cor_pos"
  ###
  
} #end of ISPRS4

####

##orthoimage ISPRS4
#classification DLR10

if (Img_name == "ISPRS4_DLR10") {
  
  ##b32 (b3,"2parts_2")
  
  if (bnr2 == 32 && p_pos == "cor_img") {
    plot(W$'3', col="white")  #black building
    w = W$'3'
  } #end b32 - "cor_img"
  
  if (bnr2 == 32 && p_pos == "cor_pos") {  
    #determine new position by 'support_line_detection.R', #6
    #or with angle by 'support_sequence_of_lines.R', #9
    b13_angle_df2
    b13_angle_df2[2,1:4] <- c(26,NA,375,106) #lnr=26,midpoint is manually determined
    b13_angle_df2[13,1:4] <- c(312,NA,474,173) #lnr=312,midpoint is manually determined
    b13_angle_df2[14,1:4] <- c(138,NA,432,147) #lnr=138,midpoint is manually determined
    b13_angle_df3 <- b13_angle_df2 
  } #end b32 "cor_pos"
  
  ##b8 
  
  if (bnr2 == 8 && p_pos == "cor_img") {
    plot(W$'3', col="white")  #black building
    w = W$'3'
  } #end b32 - "cor_img"
  
  if (bnr2 == 8 && p_pos == "cor_pos") {  
    #determine new position by 'support_line_detection.R', #6
    #or with angle by 'support_sequence_of_lines.R', #9
    b13_angle_df2
    b13_angle_df2[10,1:4] <- c(110,NA,102,124) #lnr=110,midpoint is manually determined
    b13_angle_df2[11,1:4] <- c(308,NA,129,189) #lnr=308,midpoint is manually determined
    b13_angle_df2[15,1:4] <- c(427,NA,225,172) #lnr=427,midpoint is manually determined
    b13_angle_df2[16,1:4] <- c(636,NA,217,174) #lnr=636,midpoint is manually determined
    b13_angle_df2[17,1:4] <- c(106,NA,222,181) #lnr=106,midpoint is manually determined
    b13_angle_df3 <- b13_angle_df2
  } #end b8 - "cor_pos"
  
  if (bnr2 == 8 && p_pos == "cor_sek") { 
    sequence_seg
    sequence_seg <- sequence_seg[-c(10,11)]
    sequence_seg2 <-sequence_seg
  } #end b8 - "cor_sek")
  
  ##b10
  
  if (bnr2 == 10 && p_pos == "cor_pos") {  
    #determine new position by 'support_line_detection.R', #6 & #11
    #or with angle by 'support_sequence_of_lines.R', #9
    #stop("determine angle")
    b13_angle_df2
    #b13_angle_df2[10,1:4] <- c(110,NA,102,124) #lnr=110,midpoint is manually determined
    b13_angle_df2[11,1:4] <- c(29,NA,500,260) #lnr=29,midpoint is manually determined
    b13_angle_df2[12,1:4] <- c(1087,NA,522,285) #lnr=1087,midpoint is manually determined
    b13_angle_df2[13,1:4] <- c(8,NA,362,237) #lnr=8,midpoint is manually determined
    b13_angle_df2[14,1:4] <- c(11,NA,459,244) #lnr=11,midpoint is manually determined
    b13_angle_df3 <- b13_angle_df2
  } #end b10 - "cor_pos"
  
  ##b12
  
  if (bnr2 == 12 && p_pos == "cor_img") {
    plot(W$'3', col="white")  #black building
    w = W$'3'
  } #end b32 - "cor_img"
  
  if (bnr2 == 12 && p_pos == "cor_pos") {
    #determine new position by 'support_line_detection.R', #6
    #or with angle by 'support_sequence_of_lines.R', #9
    #stop("continue step by step")
    b13_angle_df2
    b13_angle_df2[2,1:4] <- c(123,NA,295,249) #lnr=123,midpoint is manually determined
    b13_angle_df2[6,1:4] <- c(220,NA,307,248) #lnr=220,midpoint is manually determined
    b13_angle_df2[8,1:4] <- c(87,NA,319,243) #lnr=87,midpoint is manually determined
    b13_angle_df2[9,1:4] <- c(546,NA,299,288) #lnr=546,midpoint is manually determined
    b13_angle_df2[10,1:4] <- c(91,NA,287,294) #lnr=91,midpoint is manually determined
    b13_angle_df3 <- b13_angle_df2
  } #end b12 - "cor_pos"
  
  if (bnr2 == 12 && p_pos == "cor_sek") {
    sequence_seg2 = c(87,21,546,91,1,6,8,63,123,220)
  } #end b12 - "cor_pos"
  
  ##b14
  if (bnr2 == 14 && p_pos == "cor_img") {
    #plot(W$'3', col="white")  #black building
    #w = W$'3'
    plot(W$'4', col="white")  #black building
    w = W$'4'
  } #end b32 - "cor_img"
  
  if (bnr2 == 14 && p_pos == "cor_pos") {
    #determine new position by 'support_line_detection.R':#6
    #or with angle by 'support_sequence_of_lines.R', #9
    #stop("manual action required")
    b13_angle_df2
    b13_angle_df2[9,1:4] <- c(30,NA,232,323)  #lnr=30,midpoint is manually determined
    b13_angle_df2[8,1:4] <- c(420,NA,221,348) #lnr=420,midpoint is manually determined
    b13_angle_df2[6,1:4] <- c(336,NA,186,384) #lnr=336,midpoint is manually determined
    b13_angle_df2[5,1:4] <- c(466,NA,162,389) #lnr=466,midpoint is manually determined
    b13_angle_df2[4,1:4] <- c(275,NA,176,425) #lnr=275,midpoint is manually determined
    b13_angle_df2[3,1:4] <- c(19,NA,190,427) #lnr=19,midpoint is manually determined
    b13_angle_df2[14,1:4] <- c(8,NA,274,327)  #lnr=8,midpoint is manually determined
    b13_angle_df3 <- b13_angle_df2
    b13_angle_df3
  } #end b14 - "cor_pos"
  
  ##15
  
  if (bnr2 == 15 && p_pos == "cor_img") {
    plot(W$'3', col="white")  #black building
    w = W$'3'
  } #end b15 - "cor_img"
  
  if (bnr2 == 15 && p_pos == "cor_pos") {
    #determine new position by 'support_line_detection.R':#6
    #or with angle by 'support_sequence_of_lines.R', #9
    b13_angle_df2
    b13_angle_df2[2,1:4] <- c(55,NA,365,303) #lnr=55,midpoint is manually determined
    b13_angle_df2[11,1:4] <- c(91,NA,459,374) #lnr=91a,midpoint is manually determined
    b13_angle_df2[12,1:4] <- c(262,NA,443,349) #lnr=262,midpoint is manually determined
    b13_angle_df2[15,1:4] <- c(1,NA,492,349) #lnr=1b,midpoint is manually determined
    b13_angle_df2[16,1:4] <- c(91,NA,494,396) #lnr=91b,midpoint is manually determined
    b13_angle_df3 <- b13_angle_df2
  } #end b15 - "cor_pos"
  
  ##17
  
  if (bnr2 == 17 && p_pos == "cor_img") {
    plot(W$'3', col="white")  #black building
    w = W$'3'
  } #end b17 - "cor_img"
  
  if (bnr2 == 17 && p_pos == "cor_pos") {
    #determine new position by 'support_line_detection.R':#6
    #or with angle by 'support_sequence_of_lines.R', #9
    b13_angle_df2
    b13_angle_df3 <- b13_angle_df2
    b13_angle_df3[12,1:4] <- c(10,NA,251,455) #lnr=10,midpoint is manually determined
    b13_angle_df3[15,1:4] <- c(90,NA,297,504) #lnr=90b,midpoint is manually determined
    b13_angle_df3[16,1:4] <- c(101,NA,275,456) #lnr=101b,midpoint is manually determined
    b13_angle_df3
  } #end b17 - "cor_pos"
  
  ##21
  
  if (bnr2 == 21 && p_pos == "cor_img") {
    plot(W$'3', col="white")  #black building
    w = W$'3'
  } #end b21 - "cor_img"
  
  if (bnr2 == 21 && p_pos == "cor_pos") {
    #determine new position by 'support_line_detection.R':#6
    #or with angle by 'support_sequence_of_lines.R', #9
    b13_angle_df2
    b13_angle_df3 <- b13_angle_df2
    b13_angle_df3[2,1:4] <- c(37,NA,334,529) #lnr=37,midpoint is manually determined
    b13_angle_df3[3,1:4] <- c(254,NA,370,537) #lnr=254,midpoint is manually determined
    b13_angle_df3[4,1:4] <- c(1,NA,406,523) #lnr=1a,midpoint is manually determined
    b13_angle_df3[9,1:4] <- c(424,NA,359,532) #lnr=424,midpoint is manually determined
    b13_angle_df3[10,1:4] <- c(393,NA,382,537) #lnr=393,midpoint is manually determined
    b13_angle_df3[11,1:4] <- c(90,NA,373,551) #lnr=90b,midpoint is manually determined
    b13_angle_df3[12,1:4] <- c(90,NA,350,530) #lnr=90a,midpoint is manually determined
    b13_angle_df3[13,1:4] <- c(10,NA,286,545) #lnr=10,midpoint is manually determined
    b13_angle_df3[14,1:4] <- c(1,NA,350,587) #lnr=1b,midpoint is manually determined
    b13_angle_df3
  } #end b21 - "cor_pos"
  
  ##221
  
  if (bnr2 == 221 && p_pos == "cor_pos") {
    #determine new position by 'support_line_detection.R':#6
    #or with angle by 'support_sequence_of_lines.R', #9
    b13_angle_df2
    b13_angle_df3 <- b13_angle_df2
    b13_angle_df3[7,1:4] <- c(66,NA,66,565) #lnr=66,midpoint is manually determined
    b13_angle_df3[8,1:4] <- c(76,NA,83,563) #lnr=76,midpoint is manually determined
    b13_angle_df3[10,1:4] <- c(69,NA,115,570) #lnr=69,midpoint is manually determined
    b13_angle_df3[14,1:4] <- c(150,NA,165,577) #lnr=150,midpoint is manually determined
    b13_angle_df3[3,1:4] <- c(3,NA,41,594) #lnr=3,midpoint is manually determined
    b13_angle_df3
  } #end b221 - "cor_pos"
  
  if (bnr2 == 221 && p_pos == "cor_sek") { 
    sequence_seg
    sequence_seg <- c(1,6,66,76,191,69,27,84,3,5,916,8,7,150)
    sequence_seg2 <-sequence_seg
  } #end b221 - "cor_sek")
  
  ##b26
  
  if (bnr2 == 26 && p_pos == "cor_img") {
    plot(W$'3', col="white")  #black building
    w = W$'3'
  } #end b26 - "cor_img"
  
  if (bnr2 == 26 && p_pos == "cor_pos") {
    #determine new position by 'support_line_detection.R':#6
    #or with angle by 'support_sequence_of_lines.R', #9
    #stop("continue step by step")
    b13_angle_df2
    b13_angle_df3 <- b13_angle_df2
    b13_angle_df3[2,1:4] <- c(5,NA,323,697) #lnr=5a,midpoint is manually determined
    b13_angle_df3[6,1:4] <- c(5,NA,270,733) #lnr=5b,midpoint is manually determined
    b13_angle_df3[5,1:4] <- c(33,NA,269,710) #lnr=33,midpoint is manually determined
    b13_angle_df3[11,1:4] <- c(4,NA,188,653) #lnr=4,midpoint is manually determined
    b13_angle_df3[13,1:4] <- c(52,NA,139,678) #lnr=52,midpoint is manually determined
    b13_angle_df3[15,1:4] <- c(365,NA,133,712) #lnr=365,midpoint is manually determined
    b13_angle_df3[16,1:4] <- c(188,NA,150,717) #lnr=188,midpoint is manually determined
    b13_angle_df3[18,1:4] <- c(17,NA,184,756) #lnr=17,midpoint is manually determined
    b13_angle_df3[20,1:4] <- c(152,NA,238,801) #lnr=152,midpoint is manually determined
    b13_angle_df3[22,1:4] <- c(339,NA,241,762) #lnr=339,midpoint is manually determined
    b13_angle_df3[3,1:4] <- c(104,NA,299,692) #lnr=104,midpoint is manually determined
    b13_angle_df3[17,1:4] <- c(7,NA,170,733) #lnr=7,midpoint is manually determined
    b13_angle_df3[19,1:4] <- c(8,NA,202,783) #lnr=8,midpoint is manually determined
    b13_angle_df3
  } #end b26 - "cor_pos"
  
} #end of ISPRS4_DLR10

##end of script 'spObj_sequence_of_lines_v1.4.0.R'
################################################################################
