library('zoo')
is.normal=function(dataset){
if((class(dataset)!="numeric")&&(class(dataset)!="integer")){
  stop("Class of dataset must be numeric or integer")
}
#plot histogram of dataset
p1=hist(dataset,right=FALSE,ann=FALSE,density=20)
title(main=paste("Histogram of", deparse(substitute(dataset))),ylab="Frequency",xlab="x")
#create normal breaks
breaks_norm=pnorm(p1$breaks,mean=mean(dataset),sd=sd(dataset))
null.probs_norm=rollapply(breaks_norm, 2, function(dataset) dataset[2]-dataset[1])
#run chi-squared test to check for normal distribution
norm_test=chisq.test(p1$counts,p=null.probs_norm,rescale.p = TRUE,simulate.p.value = TRUE,B=9999)
norm_test
#extract p-value from chisq.test()
pvalue=norm_test$p.value
#create ouput list
output=list(p.value=0,is.significant=character())
#set p-value in output list to pvalue from chisq.test
output$p.value=pvalue
#check for significance, with alpha=.1
if(pvalue>.1){
  output$is.significant="Data may be significantly consistent with a Normal Distribution"
}else{
  output$is.significant="Data is NOT significantly consistent with a Normal Distribution"
}
return(output)
}
is.normal()
pvalues=matrix(0,ncol(testdataKF),2)
pvalues[,1]=colnames(testdataKF)
for(i in 1:ncol(testdataKF)){
  pvalues[i,2]=is.normal(testdataKF[,i])$p.value
}





