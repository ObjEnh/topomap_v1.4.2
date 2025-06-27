##name of script: func_test_gaps_onedirection.R
#purpose: finding of gaps within a point cloud (PC) representing a line segment
#         by means of a histogram and selects the continuous part of the PC
#function used in: script 'func_reduce_pointset.R'
#argument(s): hist (histogram of PC)
#authors: Joachim Höhle & Michael Höhle
#GNU General Public License (GPL)

test_gaps_onedirection <- function(hist) {

  gap_candidates <- which(hist$counts==0)

  if (length(gap_candidates)>0) {
    nTotal <- sum(hist$counts)

    gaps <- t(sapply(gap_candidates, function(i) {
      lower <- sum(hist$counts[seq_len(i)])
      upper <- nTotal - lower
      isLower <- lower >= upper
      return(c(i,isLower, max(lower,upper)/nTotal))
    }))

    bestGap <- gaps[which.max(gaps[,3]),]
    return(data.frame(cut=hist$mids[bestGap[1]],
      direction=ifelse(bestGap[2],"lower","upper"),
      proportion=bestGap[3],
      nGaps=length(gap_candidates)))
  } else {
    return(NULL)
  } #end if-else
  
} #end of function 'test_gaps_onedirection(hist)'

#end of script 'func_test_gaps_onedirection.R'

################################################################################

