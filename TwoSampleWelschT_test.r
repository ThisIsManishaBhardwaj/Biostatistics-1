#A medial research was aimed at finding whether two populations of infants differ with respect to mean age at which they walked independently. The age when 9 babies from each group walked independently were noted as below:
#Group-A : 10.2, 9.5, 10.1, 10.0, 9.8, 10.9, 11.4, 10.8, 9.7 (age in months)
#Group-B : 11.0, 11.2, 10.1, 11.4, 11.7, 11.2, 10.8, 11.6, 10.9 (age in months)
#Assuming that the parent distributions of the data sets follow a Gaussian distribution with unknown and unequal variance  values, write an R script to perform appropriate t-test to test the null hypothesis. Let ?? = 0.04 be the significance level of the test.Print the null and alternate hypotheses for this test.
#a function for performing two sample Welsch t test

two_sample_welsch_t_test<-function(X,Y,alpha,alternative)
{

	if(!alternative%in%c("two.sided","lesser","greater"))
	{

		print("Input error:alternative can only be two.sided, lesser or greater")
		on.exit()
	}
	
	xbar=mean(X)
	ybar=mean(Y)
	n=length(X)
	m=length(Y)
	sx=sd(X)
	sy=sd(Y)
	rn=(((sx^2)/n)+((sy^2)/m))^2
        rd=((1/(n-1))*((sx^2)/n)^2)+((1/(m-1))*((sy^2)/m)^2)
        r=rn/rd
	w=(xbar-ybar)/sqrt(((sx^2)/n)+((sy^2)/m))
	t=w
	df=r
	pvalue=0
	if(t<0)
	{
	
		pvalue=pt(t,df)
	}else if(t==0)
	{
	
		pvalue=0.5
	}else
	{
		pvalue=1-pt(t,df)
	}
	print(paste("pvalue=",pvalue))
	
	#hypothesis testing
	
	if(alternative=="two.sided")
	{
		
		talpha=qt((1-(alpha/2)),df)
		if((t>talpha)|(t<(-talpha)))
		{
		
			print("Null hypothesis rejected")
			print("two.sided alternative hypothesis accepted")
			print("population means are not equal")
			
		}else
		{
		
			print("Null hypothesis accepted")
			print("two.sided alternative hypothesis rejected")
			print("population means are equal")
		}	
	}
	
	if(alternative=="lesser")
	{
	
		talpha=qt(1-alpha,df)
		if(t<(-talpha))
		{
		
			print("Null hypothesis rejected")
			print("alternative hypothesis accepted")
			print("population mean of X is lesser than population mean of Y")
		}
		else
		{
		
			print("Null hypothesis accepted")
			print("alternative hypothesis rejected")
			print("population mean OF X is greater than population mean of Y")
		}
	}	
		
	else if(alternative=="greater")
	{
		
		talpha=qt(1-alpha,df)
		if(t>talpha)
		{
		
			print("Null hypothesis rejected")
			print("alternative hypothesis accepted")
			print("population mean of X is greater than population mean of Y")
		}
		
		else
		{
		
			print("Null hypothesis accepted")
			print("alternative hypothesis rejected")
			print("population mean of X is lesser than population mean of Y")
		}
	}		
		
	print(paste("alpha=",alpha))
	print(paste("sample size of X=",n))
	print(paste("sample size of Y=",m))
	print(paste("t=",t))
	print(paste("Degrees of freedom=",df))
	

}


X=c(10.2, 9.5, 10.1, 10.0, 9.8, 10.9, 11.4, 10.8, 9.7)
Y=c(11.0, 11.2, 10.1, 11.4, 11.7, 11.2, 10.8, 11.6, 10.9)
alpha=0.04
alternative="two.sided"
two_sample_welsch_t_test(X,Y,alpha,alternative)
