`summary.hglm` <-
function(Object,...){
  Call<-Object$call
  Method<-Object$method
  if(Object$Converge=="did not converge") stop("There is no valid estimate to produce summary statistics")
  if(is.null(Object$fixef)) stop("There in no valid estimate to produce summary statistics")
  if(is.null(Object$SeFe)) stop("There in no valid standard error estimate to produce summary statistics")
  Nfix<-length(Object$fixef)
  FixCoefMat<-matrix(numeric(4*Nfix),nrow=Nfix,byrow=TRUE)
  dimnames(FixCoefMat)<-list(names(Object$fixef),c("Estimate", "Std. Error",
   "t value", "Pr(>|t|)"))
    FixCoefMat[,1]<-as.numeric(Object$fixef)
    FixCoefMat[,2]<-as.numeric(Object$SeFe)
    FixCoefMat[,3]<-as.numeric(FixCoefMat[,1]/FixCoefMat[,2])
    FixCoefMat[,4]<-2 * pt(abs(as.numeric(FixCoefMat[,3])),Object$dfReFe, lower.tail = FALSE)
  if(!is.null(Object$ranef)){
  Nran<-length(Object$ranef)
  RandCoefMat<-matrix(numeric(2*Nran),nrow=Nran,byrow=TRUE)
  dimnames(RandCoefMat)<-list(names(Object$ranef),c("Estimate", "Std. Error"))
  RandCoefMat[,1]<-as.numeric(Object$ranef)
  RandCoefMat[,2]<-as.numeric(Object$SeRe)
  } else{
  RandCoefMat<-NULL
  }
  smst<-list(Method=Method,FixCoefMat=FixCoefMat,RandCoefMat=RandCoefMat,
  SummVC1=Object$SummVC1, SummVC2=Object$SummVC2, iter=Object$iter,
  converge=Object$Converge,call=Call,ProfLogLik=Object$ProfLogLik,
  LogLik=Object$LogLik, varFix=Object$varFix,varRanef=Object$varRanef,link.disp=Object$link.disp)
  class(smst)<-"summary.hglm"
  return(smst)
}

