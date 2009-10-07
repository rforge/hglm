`print.hglm` <-
function(Object,...){
  cat("Call: \n")
  print(Object$call)
  cat("\n")
  cat("Fixed effects:\n")
  print(Object$fixef)
  cat("Random effects: \n")
  print(Object$ranef)
  if(!is.null(Object$varFix)){
  cat("\n Dispersion parameter for the mean model:")
  print(Object$varFix)
  } else{
  cat(paste("\n", "Estimates of the dispersion model:","\n", "Link=", 
  Object$link.disp,"\n", "Effects:\n"))
  print(Object$SummVC1[,1])
  }
  cat("Dispersion parameter for the random effects:")
  print(Object$varRanef)
  cat(paste("\n","Estimation",Object$Converge,"in",Object$iter,"iterations \n"))
}

