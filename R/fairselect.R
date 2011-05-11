fairselect=function(training,testing,method){
if(sum(abs(abs(training[,1]-1/2)-1/2))!=0|sum(abs(abs(testing[,1]-1/2)-1/2))!=0)
{
stop("please make the first column a binary variable")
return(0)
}else if((class(training)!="matrix")|(class(testing)!="matrix"))
{
stop("please make your training and testing dataset a matrix")
return(0)
}else if(!(dim(training)[2]==dim(testing)[2]))
{
stop("please make the same predictors for the training and the testing data")
return(0)
}else if((method!="ttest")&(method!="oracle"))
{
stop("please make method either 'ttest' or 'oracle'")
return(0)
}else
{
#training data
p=dim(training)[2]-1
nk1=dim(training)[1]
count1=rep(0,p)
count2=rep(0,p)

y1=training[training[,1]==0,]
y2=training[training[,1]==1,]

muhat1=colSums(y1[,-1])/dim(y1)[1]
muhat2=colSums(y2[,-1])/dim(y2)[2]
dd=rep(0,p)
for(i in 1:p)
{
dd[i]=(1/2*(var(y1[,i+1])+var(y2[,i+1])))^(-1)
}

##testing data
nk2=dim(testing)[1]
y3=testing[testing[,1]==0,]
y4=testing[testing[,1]==1,]

if(method=="ttest")
{
t=rep(0,p)

for(k in 1:p)
{
t[k]=(muhat1[k]-muhat2[k])/sqrt(var(y1[,k+1])/dim(y1)[1]+var(y2[,k+1])/dim(y2)[1])
}
tt=order(abs(t),decreasing=T)

for(m in 1:p)
{
for(i in 1:dim(y3)[1])
{
count1[m]=count1[m]+(as.numeric(dd[tt[1:m]]%*%diag(outer(y3[i,tt[1:m]+1]-1/2*(muhat1[tt[1:m]]+muhat2[tt[1:m]]),muhat1[tt[1:m]]-muhat2[tt[1:m]])))<0)
}
for(i in 1:dim(y4)[1])
{
count1[m]=count1[m]+(as.numeric(dd[tt[1:m]]%*%diag(outer(y4[i,tt[1:m]+1]-1/2*(muhat1[tt[1:m]]+muhat2[tt[1:m]]),muhat1[tt[1:m]]-muhat2[tt[1:m]])))>0)
}
count1[m]=count1[m]/(nk2)
}
#par(mfrow=c(1,2))
#plot(1:p,count1,main="misclassficationratettest")
#plot(1:80,count1[1:80],main="misclassficationratettestemplify")
v=min(count1)
f=as.numeric(tt[1:which(count1==min(count1))[1]])
return(list(method=method,value=v,feature=f,m1=muhat1,m2=muhat2,cova=dd))
}else if(method=="oracle")
{
s=rep(0,p)
for(k in 1:p)
{
s[k]=muhat1[k]-muhat2[k]
}
ss=order(abs(s),decreasing=T)
for(m in 1:p)
{
for(i in 1:dim(y3)[1])
{
count2[m]=count2[m]+(as.numeric(dd[ss[1:m]]%*%diag(outer(y3[i,ss[1:m]+1]-1/2*(muhat1[ss[1:m]]+muhat2[ss[1:m]]),muhat1[ss[1:m]]-muhat2[ss[1:m]])))<0)
}
for(i in 1:dim(y4)[1])
{
count2[m]=count2[m]++(as.numeric(dd[ss[1:m]]%*%diag(outer(y4[i,ss[1:m]+1]-1/2*(muhat1[ss[1:m]]+muhat2[ss[1:m]]),muhat1[ss[1:m]]-muhat2[ss[1:m]])))>0)
}
count2[m]=count2[m]/nk2
}
#par(mfrow=c(1,2))
#plot(1:p,count2,main="misclassficationrateoracle")
#plot(1:80,count2[1:80],main="misclassficationrateoracleamplify")
v=min(count2)
f=as.numeric(ss[1:which(count2==min(count2))[1]])
return(list(method=method,value=v,feature=f,m1=muhat1,m2=muhat2,cova=dd))
}

}

}





