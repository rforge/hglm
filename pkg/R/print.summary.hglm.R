`print.summary.hglm` <-
function(Object,digits=4,...){
  cat("Call: \n")
  print(Object$call)
  cat("\n")
  cat("DISPERSION MODEL")
  cat("\n")
  if(Object$Method=="REML"){
    cat(paste("-2Log(Profile-Likelihood)=",round(-2*Object$ProfLogLik,digits),"\n"))
  } #else{
    #Method="HL"
    #cat(paste("-2Log(Likelihood)=",round(-2*Object$LogLik,digits),"\n"))
  #}
  if(!is.null(Object$varFix)){
  cat("\n Dispersion parameter for the mean model:")
  print(Object$varFix)
  } 
  cat(paste("\n", "Model estimates for the dispersion term:","\n", "Link=", 
  Object$link.disp,"\n", "Effects:\n"))
  print(Object$SummVC1)
  
  if(!is.null(Object$varRanef)){
  cat("\n Dispersion parameter for the random effects\n")
  print(Object$varRanef,digits=digits)
  cat(paste("\n", "Dispersion model for the random effects:","\n", "Link=log", "\n", "Effects:\n"))
  print(Object$SummVC2)
  }
  cat("MEAN MODEL")
  cat("\n")
  cat("\n Summary of the fixed effects estimates \n")
  printCoefmat(Object$FixCoefMat,digits=digits,P.value=TRUE,has.Pvalue=TRUE)
  if(!is.null(Object$RandCoefMat)){
  cat("\n Summary of the random effects estimate \n")
  print(round(Object$RandCoefMat,digits))
  }
  cat(paste("\n",Object$Method,"estimation",Object$converge,"in",Object$iter,"iterations \n"))
}

