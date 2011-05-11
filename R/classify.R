classify=function(newdata,fairobject){
feature=fairobject$feature
muhat1=fairobject$m1
muhat2=fairobject$m2
cova=fairobject$cova

if(class(fairobject)!="list")
{
stop("please make a a fair object")
return(0)
}else if(class(newdata)!="numeric")
{
stop("please make newdata a numeric")
return(0)
}else if(length(newdata)!=length(muhat1))
{
stop("please make newdata,m1,m2,cova the same length")
return(0)
}else
{
w=-(as.numeric(cova[feature]%*%diag(outer(newdata[feature]-1/2*(muhat1[feature]+muhat2[feature]),muhat1[feature]-muhat2[feature])))>0)+2
}
return(list(class=w))
}





