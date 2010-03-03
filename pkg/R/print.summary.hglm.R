`print.summary.hglm` <-
function(Object,digits=4,...){
  cat("Call: \n")
  print(Object$call)
  cat("\n")
  cat("DISPERSION MODEL")
  cat("\n")
  if(Object$Method=="REML"){
    cat(paste("-2Log(Profile-Likelihood) =",round(-2*Object$ProfLogLik,digits),"\n"))
  } else{
    cat("WARNING: h-likelihood estimates through EQL can be biased.")
    }
  if(!is.null(Object$varFix)){
  if(is.null(Object$SummVC1)){
   cat("\n") 
  cat(paste("Dispersion parametr held Constant at:", Object$varFix))
  } else{
  cat("\n") 
  cat("Model estimates for the dispersion term:")
  print(Object$varFix)
  }
  } 
  if(!is.null(Object$SummVC1)){
  cat("\n")
  cat(paste("Model estimates for the dispersion term:","\n","Link =", 
  Object$link.disp,"\n", "Effects:\n"))
  print(round(Object$SummVC1,digits))
  cat("\n")
  cat("Dispersion = 1 is used in Gamma model on deviances to calculate the standard error(s).")
  }
  if(!is.null(Object$varRanef)){
  cat("\n")
  cat("Dispersion parameter for the random effects\n")
  print(Object$varRanef,digits=digits)
  cat("\n")
  cat(paste("Dispersion model for the random effects:","\n","Link = log","\n","Effects:\n"))
  print(round(Object$SummVC2,digits))
  cat("\n")
  cat("Dispersion = 1 is used in Gamma model on deviances to calculate the standard error(s).")
  }
  cat("\n")
  cat("MEAN MODEL")
  cat("\n")
  cat("Summary of the fixed effects estimates \n")
  printCoefmat(Object$FixCoefMat,digits=digits,P.value=TRUE,has.Pvalue=TRUE)
  if(!is.null(Object$RandCoefMat)){
  cat("\n")
  cat("Summary of the random effects estimate \n")
  print(round(Object$RandCoefMat,digits))
  }
  cat("\n")
  cat(paste(Object$Method,"estimation",Object$converge,"in",Object$iter,"iterations. \n"))
}

