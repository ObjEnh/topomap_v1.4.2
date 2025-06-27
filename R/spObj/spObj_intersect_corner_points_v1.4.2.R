##name of script: 'spObj_intersect_corner_coordinates.R'
cat("version_number= ",v_nr,"\n")
#purpose: insert average angle for objects with non-orthogonal lines for special objects
#author: Joachim Höhle
#GNU General Public License (GPL)

## ISPRS_#7

if (Img_name == "ISPRS7") { 
  
  #b18
  if (bnr2==18 && p_pos == "cor_theta_av2") { 
    theta_av2_mod <- 29.8886 #manual calculation by script 'support_intersect_corner_points', script #4
    theta_av2_mod
  } #end if

} #end ISPRS7

################################################################################

if (Img_name == "ISPRS1") { 
  
  #b11
  if (bnr2 == 11 && p_pos == "cor_theta_av2") { 
    theta_average2 <- w_av(ang2,len2) #call of function
    theta_av2_mod <- theta_average2
  } #end if
  
} #end ISPRS1

################################################################################

if (Img_name == "ISPRS4") { 
  
  ##b163
  
  if (bnr2 == 163 && p_pos == "cor_theta_av2") { 
    theta_average2 <- w_av(ang2,len2) #call of function
    theta_av2_mod <- theta_average2
    theta_av2_mod 
  } #end if
  
} #end ISPRS4

################################################################################

##end of 'spObj_intersect_corner_points.R'
