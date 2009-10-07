`hglm.formula` <-
function(fixed=NULL, random=NULL, data=list(), family=gaussian(link=identity),
 rand.family=gaussian(link=identity), method="HL", conv=1e-4, maxit=20, startval=NULL,
 disp=NULL, link.disp="log", weights=NULL,...) {
  Call<-match.call()
  #### check fixed effects formula ###########
  if (!inherits(fixed, "formula") || length(fixed) != 3) {
    stop("\n Fixed-effects model must be a formula of the form \"resp ~ pred\"")
  }
  ##### Get GLM family and link #######################################
  ## Check random effetcst ############################################
  if (!inherits(random, "formula")){
    stop("\n Random part must be a one-sided formula of the form \" ~ effect|Cluster\"")
  }
  if(attr(terms(random),"response")!=0){
    stop("\n Random part must be a one-sided formula of the form \" ~ effect|Cluster\"")
  }
  if(all.names(random)[2]!="|") {
    stop("The subjects/clusters in Random must be separated by \" |\"")
  }
  #### Check the dispersion model ##########################
  if(is.null(disp)){
    disp.x<-NULL
  } else {
    if (!inherits(disp, "formula")){
      stop("\n Dispersion model must be a one-sided formula of the form \" ~ effect\"")
    }
    if(attr(terms(disp),"response")!=0){
      stop("\n Dispersion model must be a one-sided formula of the form \" ~ effect\"")
    }
   ### Create design matrix for the dispersion model #########
   DispModel<-model.frame(disp,data)
   x.disp=model.matrix(attr(DispModel,"terms"),data=DispModel)
   row.names(x.disp)<-NULL
  }
  ########## random effects part is checked #############################
  #### Create design matrix for the fixed effects #######################
  fmf<-model.frame(fixed,data)
  x<-model.matrix(attr(fmf,"terms"),data=fmf)
  row.names(x)<-NULL
  y<-model.response(fmf)
  if(is.factor(y)){
    if(family$family=="binomial"){
      FactorY<-names(table(y))
      if(length(FactorY)>2) warning("More than 2 factors in Binomial response is 
      ambiguous and the last category is considered as success")
      y<-as.numeric(y==FactorY[length(FactorY)])
    } else {
      stop(paste("response must be numeric for",family$family,"family."))
    }
  }
  ######## Create z matrix #############################################
  RanTerm<-unlist(strsplit(attr(terms(random),"term.labels"),split="|",fixed=TRUE))
  if(length(RanTerm)>2) stop("Currently only one random term is supported.")
  RanTerm<-gsub(pattern=" ",replacement="",RanTerm)
  if(!is.factor(data[1:2,RanTerm[2]])){
    message("Cluster/Subject variable is not factor type")
    if((length(RanTerm)==2) & (RanTerm[1]=="1")){     
      ranf<-paste("~","as.factor(",RanTerm[2],")","-1",sep="")
    }
    else {
      ranf<-paste("~","(",RanTerm[1],")",":as.factor(",RanTerm[2],")","-1",sep="")
    }
  } else {
   if((length(RanTerm)==2) & (RanTerm[1]=="1")){
      ranf<-paste("~",RanTerm[2],"-1",sep="")
   } else {
      ranf<-paste("~","(",RanTerm[1],")",":",RanTerm[2],"-1",sep="")
    }
  }
  ranf<-as.formula(ranf)
  rmf<-model.frame(ranf,data)
  z<-model.matrix(attr(rmf,"terms"),data=rmf)
  row.names(z)<-NULL
  val<-hglm.default(X=x,y,Z=z,family=family,rand.family=rand.family, x.disp=x.disp,
  link.disp=link.disp,method=method,conv=conv,maxit=maxit,startval=startval,...)
  val$call<-Call
  return(val)
}

