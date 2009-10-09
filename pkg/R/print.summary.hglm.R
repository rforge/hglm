`print.summary.hglm` <-
function(Object,digits=4,...){
  cat("Call: \n")
  print(Object$call)
  cat("\n")
  cat("DISPERSION MODEL")
  cat("\n")
  if(Object$Method=="REML"){
    cat(paste("-2Log(Profile-Likelihood) =",round(-2*Object$ProfLogLik,digits),"\n"))
  } #else{
    #Method="HL"
    #cat(paste("-2Log(Likelihood)=",round(-2*Object$LogLik,digits),"\n"))
  #}
  if(!is.null(Object$varFix)){
  cat("\n") 
  cat("Model estimates for the dispersion term:")
  print(Object$varFix)
  } 
  cat("\n")
  cat(paste("Model estimates for the dispersion term:","\n","Link =", 
  Object$link.disp,"\n", "Effects:\n"))
  print(round(Object$SummVC1,digits))
  
  if(!is.null(Object$varRanef)){
  cat("\n")
  cat("Dispersion parameter for the random effects\n")
  print(Object$varRanef,digits=digits)
  cat("\n")
  cat(paste("Dispersion model for the random effects:","\n","Link = log","\n","Effects:\n"))
  print(round(Object$SummVC2,digits))
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

