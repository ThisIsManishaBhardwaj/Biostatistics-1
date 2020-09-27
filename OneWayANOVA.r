#Twenty two patients that underwent bypass surgery were divided into three groups based on their respiration patterns. For each patient, the concentration of folic acid in red blood cells (inunits of ng/ml) was measured 24 hours after the surgery. The data is given below:
#Group-1 : 243, 251, 275, 291, 347, 354, 380, 392
#Group-2 : 206, 210, 226, 249, 255, 273, 285, 295, 309
#Group-3 : 241, 258, 270, 293, 328
#Using one-way ANOVA, test the null hypothesis that the three population means ?? 1 , ?? 2 and ?? 3 are the same to a significance level of ?? = 0.05. State the null and the alternate hypotheses.What are the conclusions?
#Write an R script to perform this analysis.

#type the datasets

X1 = c(243, 251, 275, 291, 347, 354, 380, 392)
X2 = c(206, 210, 226, 249, 255, 273, 285, 295, 309)
X3 = c(241, 258, 270, 293, 328)

alis=list(X1,X2,X3)

alpha=0.05 
m=length(alis)

print("Null hypothesis = mu1=mu2=mu3")
print("Alternate hypothesis = mu1!=mu2!=mu3")

#compute n
n=0
for(i in 1:m)
{
	n=n+length(alis[[i]])
}
print(m)
print(n)	

sqsum=0
wholesum=0
rowsumsq=0

#loop i over datasets
for(i in 1:m)
{
	sqsum=sqsum+sum(alis[[i]]^2)
	wholesum=wholesum+sum(alis[[i]])
	rowsumsq=rowsumsq+(1/length(alis[[i]]))*sum(alis[[i]])^2
}
#print(sqsum)
#print(sumsq)
sumsq=wholesum^2
SSTO=sqsum-(1/n)*sumsq
SST=rowsumsq-(1/n)*sumsq
print(paste("SSTO=",SSTO))
print(paste("SST=",SST))	
SSE=SSTO-SST
Fstatistic=(SST/(m-1))/(SSE/(n-m))
print(paste("F statistic=",Fstatistic))
Fcritical=qf(1-alpha,m-1,n-m)
print(paste("F critical=",Fcritical))
if(Fstatistic>Fcritical)
{
	print(paste("Null hypothesis is rejected to the level of alpha =",alpha))
}else
{
	print(paste("Null hypothesis is accepted to the level of alpha =",alpha))	
}	

