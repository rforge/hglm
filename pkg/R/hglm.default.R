`hglm.default` <-
function(X,y,Z=NULL,family=gaussian(link=identity),
rand.family=gaussian(link=identity), method="HL",conv=1e-4,maxit=20,startval=NULL,
fixed=NULL,random=NULL,X.disp=NULL, Z.disp=NULL,link.disp="log",data=NULL, weights=NULL,...){
  Call<-match.call()
  x<-as.matrix(X)
  y<-as.numeric(y)
  if(nrow(x)!=length(y)) {stop("Length of X and y differ.")} else{
    nobs<-nrow(x)
  }
  ### Check data consistency #######################################
  if(!is.null(Z)){
    z<-as.matrix(Z)
    if(nrow(x)!=nrow(z)) stop("Length of X and Z differ.")
    k<-ncol(z) ### gets number of clusters
  } else {
    k<-0
    z<-Z
  }
  if (!is.null(X.disp)) {
    x.disp<-as.matrix(X.disp)
   }  else {
    x.disp<-NULL
    }
  #### Check prior weights ########
  if(is.null(weights)){
    prior.weights<-rep(1,(nobs+k))
  } else {
  if(!is.numeric(weights) || any(weights<0)) stop("Weights must be a numeric vector of positive values")
  if(length(weights)<nobs) stop("Length of the weights differ from the length of the data")
    prior.weights<-c(weights,rep(1,k))
  }
  ##### Data consistency checked ######################################
  ##### Get GLM family and link #######################################
  if (is.character(family)) family <- get(family)
  if (is.function(family)) family <- eval(family)
  if (is.character(rand.family)) rand.family <- get(rand.family)
  if (is.function(rand.family)) rand.family <- eval(rand.family)
  ##### GLM family and link are checked #################################
  ##### Only the GLM families (Lee et al. 2006) will pass this test ############
  #### Get augmented response, psi (Lee et al. (2006)) ###########
  if(rand.family$family=="gaussian"){
    psi<-rep(0,k)
  } else if(rand.family$family=="Gamma"||rand.family$family=="inverse.gamma"){
    psi<-rep(1,k)
  } else if(rand.family$family=="Beta") {
    psi<-rep(1/2,k)
  } else {
    stop(paste("random.family=",rand.family$family," is not recognized as a member of the GLM family"))
  }
  if(!is.character(link.disp)) link.disp<-deparse(link.disp)
  if (link.disp=="log") {
    DispFamily<-Gamma(link="log")
  } 
  else if (link.disp=="identity") {
    DispFamily<-Gamma(link="identity")
  } 
  else if (link.disp=="inverse") {
    DispFamily<-Gamma(link="inverse")
  } 
  else {  
    stop("link.disp must be a valid link for the Gamma family GLM") 
  }
  #### Check starting values ################################################
  if(!is.null(startval)){
    if(!is.numeric(startval)) stop("Non-numeric starting value is not allowed")
    if(length(startval)<(ncol(x)+k)) stop("Too few starting values. A valid startval should contain 
    starting values for the fixed effects, random effects and the dispersion parameters")
    if((k>0) & (length(startval)<(ncol(x)+k+1))) stop("Too few starting values")
    if((k>0) & ((family$family=="gaussian")||(family$family=="Gamma")) & 
      (length(startval)<(ncol(x)+k+2))) stop("Too few starting values")
    b.hat<-startval[1:ncol(x)]
    if(length(startval)>(ncol(x)+k+1)){
    init.sig.e<-as.numeric(startval[(ncol(x)+k+2)])
    } else{
    init.sig.e<-1
    }
    if(!is.null(z)) {
      init.u<-startval[(ncol(x)+1):(ncol(x)+k)]
      init.sig.u<-as.numeric(startval[(ncol(x)+k+1)])
      if(min(init.sig.e,init.sig.u)<1e-4) stop("Unacceptable initial value is supplied for the variance parameter")
    } else {
      if(init.sig.e<1e-4) stop("Unacceptable initial value is supplied for the variance parameter")
    }
  }
  ### Generate default initial values of the fixed effects via a GLM ############
  if(is.null(z)|is.null(startval)){
    g1<-glm(y~x-1,family=family,weights=weights)
    g1$call<-Call
    if(is.null(z)) {
     ## message("This is not a mixed model")
      b.hat<-as.numeric(coef(g1))
      init.sig.e<-as.numeric(deviance(g1)/g1$df.residual)
      v.i<-NULL
      ##return(g1)
      rm(g1)
    } else{
      b.hat<-as.numeric(coef(g1))
      v.i<-rep(0,k)
      init.sig.u<-(init.sig.e<-as.numeric(0.6*deviance(g1)/g1$df.residual))*.66
      rm(g1)
      ### give a 40/60 share of the total error vraince to u and e ######### 
      ### For normal model and identity link init.b and init.u has no use ######
      if(init.sig.u<1e-4){
        init.sig.u<-init.sig.e<-.1
        message("0.1 is chosen as the initial values for the variance parameter")
      }
    }
  }

  #### Create Augmented data ##############################################
  if(!is.null(colnames(x))){
    x.names<-colnames(x)
    colnames(x)<-NULL
  } else {
    x.names<-paste("X.",1:ncol(x),sep="")
  }
  if (!is.null(z)) {
    if(!is.null(colnames(z))){
      z.names<-colnames(z)
      colnames(z)<-NULL
    } else {
      z.names<-paste("Z.",1:ncol(z),sep="")
    }
    Augy<-c(y,psi)
    AugXZ<-cbind(rbind(x,matrix(rep(0,ncol(x)*k),nrow=k,byrow=TRUE)),rbind(z,diag(k)))
    colnames(AugXZ)<-c(x.names,z.names)
  } else {
    Augy<-y
    AugXZ<-x
  }
  ##############################################################################
  ###### Begin Parameter estimation ############################################
  iter<-1
  if (!is.null(z)) phi<-rep(init.sig.u,k)  ## Random effects variance
    tau<-rep(init.sig.e,nobs)
    eta.i<-as.numeric(x%*%b.hat)
    eta0<-eta.i
    mu.i<-family$linkinv(eta.i)
    dmu_deta<-family$mu.eta(eta.i)
    zi<-eta.i+(y-mu.i)/dmu_deta
    if (!is.null(z)) {
      zmi<-psi
      Augz<-c(zi,zmi)
      du_dv<-rand.family$mu.eta(psi)
      w<-sqrt(as.numeric(c(((dmu_deta)^2/family$variance(mu.i))*(1/tau),
      ((du_dv)^2/rand.family$variance(psi))*(1/phi)))*prior.weights)
    } else {
      w<-sqrt(as.numeric(((dmu_deta)^2/family$variance(mu.i))*(1/tau))*prior.weights)
    }
    n<-NROW(Augy)
    p<-NCOL(AugXZ)
    ny<-NCOL(Augy)
  while(iter<=maxit){
    g.mme<-GLM.MME(Augy, AugXZ, starting.delta=c(b.hat,v.i), tau, phi, 
      n.fixed=ncol(x), n.random=k, weights.sqrt=w, prior.weights, family, 
      rand.family, maxit, conv, tol=1e-7)
    b.hat<- g.mme$b.hat
    eta.i<- g.mme$eta.i
    v.i<- g.mme$v.i
    Augz<- g.mme$Augz
    dev<- g.mme$dev
    hv<- g.mme$hv
    #rm(g.mme)
    mu.i<-family$linkinv(eta.i)
    dmu_deta<-family$mu.eta(eta.i)
    if (!is.null(z)) {
      ui<-rand.family$linkinv(v.i)
      du_dv<-rand.family$mu.eta(v.i)
    }
    if (is.null(x.disp)) {
      g11<-glm((as.numeric(dev[1:nobs]/(1-hv[1:nobs])))~1,family=DispFamily,
        weights=as.numeric((1-hv[1:nobs])/2))
        if(length(g11$coef)==1){
      sigma2e <-DispFamily$linkinv(as.numeric(g11$coef[1]))
      } else{
      sigma2e<-NULL
      }
      tau<-as.numeric(g11$fitted) 
    } else {
      g11<-glm((as.numeric(dev[1:nobs]/(1-hv[1:nobs])))~x.disp-1,
        family=DispFamily,weights=as.numeric((1-hv[1:nobs])/2))
        if(length(g11$coef)==1){
      sigma2e <-DispFamily$linkinv(as.numeric(g11$coef[1]))
      }  else{
      sigma2e<-NULL
      }
      tau<-as.numeric(g11$fitted) #### Error variance updated
    }
    if (!is.null(z)) {
      g12<-glm((as.numeric(dev[(nobs+1):n]/(1-hv[(nobs+1):n])))~1,
        family=Gamma(link=log),weights=as.numeric((1-hv[(nobs+1):n])/2))
      sigma2u <-exp(as.numeric(g12$coef[1])) 
      phi<-rep(sigma2u,k)  ## Random effects variance updated
    } else {
      sigma2u<-NULL
    }
    if(sum((eta0-eta.i)^2)<conv*sum(eta.i^2) & iter>1) break
    eta0 <- eta.i
    if (!is.null(z)) w<-sqrt(as.numeric(c(((dmu_deta)^2/family$variance(mu.i))*(1/tau),
    ((du_dv)^2/rand.family$variance(ui))*(1/phi)))*prior.weights)
    if (is.null(z)) w<-sqrt(as.numeric(((dmu_deta)^2/family$variance(mu.i))*(1/tau))*prior.weights)
    iter<-iter+1
  }
  names(b.hat)=x.names
  if (!is.null(z)) names(ui)=z.names
  fixef<-b.hat                        
  if (!is.null(z)) {
    ranef<-ui
  } else {
    ranef<-NULL
    phi<-NULL
  }
  val<-list(call=Call, fixef=fixef, ranef=ranef, varFix=sigma2e, 
  varRanef=sigma2u, iter=iter, Converge="did not converge", SeFe=NULL, SeRe=NULL,
     dfReFe=NULL, SummVC1=NULL, SummVC2=NULL, method=method, dev=dev, hv=hv,link.disp=link.disp)
  if(iter<maxit){
   val$Converge<-"converged"
   ##### Calculate the standard errors of the fixed and random effects #########
   val$dfReFe<-nobs-sum(hv)
   p1<-1:p
     QR<-g.mme$qr
   covmat<-chol2inv(QR$qr[p1,p1,drop=FALSE])
   SeFeRe<-sqrt(diag(covmat))
   val$SeFe<-SeFeRe[1:NCOL(x)]
   val$SeRe<-SeFeRe[(NCOL(x)+1):p]
   ###### Extract the summary table for the dispersion parameter(s)
   SummVC1<-summary(g11)
   val$SummVC1<-SummVC1$coefficients[,1:2]
   if(!is.null(z)){
   SummVC2<-summary(g12)
   val$SummVC2<-SummVC2$coefficients[,1:2]
   }
    ##### Calculate Profile Likelihood ##########
    Sigma <- diag(tau)
    if (!is.null(z)) {
      D <- diag(phi)
      V <- z%*%D%*%t(z)+Sigma
    } else {
      V <- Sigma
    }
    V.inv <- solve(V)
    temp <- y-x%*%fixef
    logdet.V <- sum(log(eigen(V,only.values=TRUE)$values))
    profile <- as.numeric(-nobs/2*log(2*pi)-1/2*logdet.V-1/2*t(temp)%*%V.inv%*%temp-1/2*log(det(t(x)%*%V.inv%*%x))+k/2*log(2*pi))
    #loglihood <- as.numeric(-nobs/2*log(2*pi)-1/2*logdet.V-1/2*t(temp)%*%V.inv%*%temp)
    if(method=="REML"){
      val$ProfLogLik<-profile
    }
  } else {
    val$iter<-iter-1
  }
  class(val)<-"hglm"
  return(val)
}

