##name of script: func_adj_orthog_corcoo.R
#used in 'adjustment_of_corner_coordinates.R'
#n=number of corner points (vertices)
#X=vector of approximate x-coordinates
#Y=vector of approximate y-coordinates

adj_orthog_corcoo <- function(n,X,Y) {
  X_adj <- rep(0,n) #vector of adjusted x-coordinates
  Y_adj <- rep(0,n) #vector of adjusted y-coordinates
  XY_adj <- matrix(nrow=n,ncol=3)
  BT <- matrix(nrow=(n-1),ncol=2*n) #matrix of condition equation 
  w <- rep(0,(n-1)) #vector of discrepancies
  v <- rep(0,2*n) #vector of corrections
  
  #calculation of BT and w
  
  BT[,] <- 0
  BT
  w
  endv <- (n-4)/2
  endv
  vec2 <- 0 : endv
  vec2
  
  for (n1 in vec2) { #loop vec2
    #cat("n1= ",n1,"\n")
    pk <- 4*n1
    #
    m3=1 
    m=m3 + 2*n1
    #cat("m= ",m,"\n")
    i=1+2*n1
    j=2+2*n1
    k=3+2*n1
    BT[m,(1+pk)] <- X[k] - X[j]
    BT[m,(2+pk)] <- Y[k] - Y[j]
    BT[m,(3+pk)] <- 2*X[j] - X[i] - X[k]
    BT[m,(4+pk)] <- 2*Y[j] - Y[i] - Y[k]
    BT[m,(5+pk)] <- X[i] - X[j]
    BT[m,(6+pk)] <- Y[i] - Y[j]
    w[m] <- (X[i] - X[j]) * (X[k] - X[j]) + (Y[i] - Y[j]) * (Y[k] - Y[j])
    #
    
    m3=2
    m=m3+2*n1
    #cat("m= ",m,"\n")
    i=2+2*n1
    j=3+2*n1
    k=4+2*n1
    BT[m,(3+pk)] <- X[k] - X[j]
    BT[m,(4+pk)] <- Y[k] - Y[j]
    BT[m,(5+pk)] <- 2*X[j] - X[i] - X[k]
    BT[m,(6+pk)] <- 2*Y[j] - Y[i] - Y[k]
    BT[m,(7+pk)] <- X[i] - X[j]
    BT[m,(8+pk)] <- Y[i] - Y[j]
    w[m] <- (X[i] - X[j]) * (X[k] - X[j]) + (Y[i] - Y[j]) * (Y[k] - Y[j])
    #
    
    m3=3
    m=m3+2*n1
    i=3+2*n1
    j=4+2*n1
    k=1
    #cat("m= ",m,"\n")
    BT[m,(5+pk)] <- X[k] - X[j]
    BT[m,(6+pk)] <- Y[k] - Y[j]
    BT[m,(7+pk)] <- 2*X[j] - X[i] - X[k]
    BT[m,(8+pk)] <- 2*Y[j] - Y[i] - Y[k]
    BT[(n-1),1] <- X[i] - X[j]
    BT[(n-1),2] <- Y[i] - Y[j]
    w[m] <- (X[i] - X[j])*(X[k]-X[j]) + (Y[i]-Y[j])*(Y[k]-Y[j])
  } #end of loop BT calculation (vec2-loop)
  
  
  BT
  w
  
  #adjustment 
  B <- t(BT)
  B
  BTB <- BT%*%B
  BTB
  BTBinv <- solve(BTB)
  BTBinv
  
  korr <- (BTBinv)%*%(-w)
  korr
  v <- B%*%korr
  vec <- 1 : n
  
  for (m1 in vec) { 
    #cat("m1= ", m1,"\n")
    X_adj[m1] <- X[m1] + v[(2*m1-1),1]
    Y_adj[m1] <- Y[m1] + v[2*m1,1]
  }
  
  X_adj
  Y_adj
  
  for (m1 in vec) { 
    XY_adj[m1,] <- c(m1,X_adj[m1],Y_adj[m1])
  }
  
  XY_adj
  v
  stdev <- sd(v)
  res <- list(XY_adj,stdev)
  return(res)
  
} #end of function 'func_adj_orthog_corcoo '

################################################################################