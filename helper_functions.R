### a wrapper to make image() more convenient

img = function(x,ylab,xlab,axes,col,na.zero=F,breaks,do.breaks=T,...){
 cc = colorRampPalette(c("black","chartreuse"))
 if(any(x<0) & any(x>0)){
	cc = colorRampPalette(c("royalblue","royalblue4","black","orangered","goldenrod1"))
 }
 if(missing(ylab)) ylab=""
 if(missing(xlab)) xlab=""
 if(missing(axes)) axes=F
 if(missing(col)) col=cc(256)
 if(na.zero) x[is.na(x)] = 0
 if(do.breaks & missing(breaks)){
  mx = max(abs(x))
  qt = max(abs(quantile(x,c(0.01,0.99))))
  bk = c(-1*mx,seq(-1*qt,qt,length.out=255),mx)
  image(0:ncol(x),0:nrow(x),t(x),ylim=c(nrow(x),0),ylab=ylab,   
   xlab=xlab,axes=axes,col=col,breaks=bk,...)
 }else{
 image(0:ncol(x),0:nrow(x),t(x),ylim=c(nrow(x),0),ylab=ylab,   
  xlab=xlab,axes=axes,col=col,breaks=breaks,...)
 }
 box()
}

### wrappers for bootstrapping to get the confidence intervals
bootlM = function(x,y){
 keep = !is.na(x) & !is.na(y)
 x0 = x[keep]
 y0 = y[keep]
 these = sample(length(x0),replace=T)
 x0 = x0[these]
 y0 = y0[these]

 approx(lowess(x0,y0),xout=seq(0,80,1),rule=2)$y
}


### with input x and y, generate a lowess trend with 95% confidence intervals
lboot = function(x,y,subset,nboot=1000,return.boot=F){
 if(length(x)!=length(y)) stop("x and y have different lengths")
 if(missing(subset)){
  subset=1:length(x)
 }
 x = x[subset]
 y = y[subset]

 ### generate nboot lowess fits
 b = replicate(nboot,bootlM(x,y))

 qu = apply(b,1,quantile,0.975)
 ql = apply(b,1,quantile,0.025)
 qm = rowMeans(b)
 xq = 0:80

 out = list()
 out$x = xq
 out$y = qm
 out$ci = list(upr=qu,lwr=ql)
 out$points = list(x=x,y=y)
 if(return.boot){
  out$boot = b
 }
 return(out)
}

### function to plot the output of lboot
plotl = function(obj,pt.col,l.col,shade.col,plot.points=F,xlim=c(0,80),
	ylim,add=F,plot.ci=T,...){
 if(missing(ylim)){
  ylim=range(obj$y,na.rm=T)
  ylim = (c(-1,1)*abs(diff(ylim))) + ylim
 }
 if(!add){
  plot(obj,xlim=xlim,ylim=ylim,xlab="age (years)",type='l',col=l.col,...)
  if(plot.ci){
   polygon(c(obj$x,rev(obj$x)),c(obj$ci$lwr,rev(obj$ci$upr)),col=shade.col,border=l.col)
  }
 }else{
  lines(obj,col=l.col,...)
  if(plot.ci){
   polygon(c(obj$x,rev(obj$x)),c(obj$ci$lwr,rev(obj$ci$upr)),col=shade.col,border=l.col)
  }
 }
 if(plot.points){
  points(obj$points,pch=16,col=pt.col)
 }

}





