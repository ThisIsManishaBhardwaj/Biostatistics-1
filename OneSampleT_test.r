#The following are the systolic blood pressure (mmHg) of 14 patients undergoing drug therapy for hypertension:
#X = {183, 152, 178, 157, 194, 163, 144, 114, 178, 152, 118, 158, 172, 138 }
#Assuming the systolic blood pressure follows a Gaussian distribution, write an R script to perform an appropriate test to conclude, based on this data, whether the population mean is less than 165.
#Print the hypothesis and results clearly.


#a function for performing one sample t test
one_sample_t_test<-function(X,mu,alpha,alternative)
{

	if(!alternative%in%c("two.sided","lesser","greater"))
	{

		print("Input error:alternative can only be two.sided, lesser or greater")
		on.exit()
	}
	
	xbar=mean(X)
	sx=sd(X)
	n=length(X)
	t=(xbar-mu)/(sx/sqrt(n))
	pvalue=0
	if(t<0)
	{
	
		pvalue=pt(t,n-1)
	}else if(t==0)
	{
	
		pvalue=0.5
	}else
	{
		pvalue=1-pt(t,n-1)
	}
	print(paste("pvalue=",pvalue))
	
	#hypothesis testing
	
	if(alternative=="two.sided")
	{
		
		talpha=qt((1-(alpha/2)),n-1)
		if((t>talpha)|(t<(-talpha)))
		{
		
			print("Null hypothesis rejected")
			print("two.sided alternative hypothesis accepted")
			print(paste("population mean not equal to ",mu))
			
		}else
		{
		
			print("Null hypothesis accepted")
			print("two.sided alternative hypothesis rejected")
			print(paste("population mean equal to ",mu))
		}	
	}
	
	if(alternative=="lesser")
	{
	
		talpha=qt(1-alpha,n-1)
		if(t<(-talpha))
		{
		
			print("Null hypothesis rejected")
			print("alternative hypothesis accepted")
			print(paste("population mean lesser than ",mu))
		}
		else
		{
		
			print("Null hypothesis accepted")
			print("alternative hypothesis rejected")
			print(paste("population mean greater than ",mu))
		}
	}	
		
	else if(alternative=="greater")
	{
		
		talpha=qt(1-alpha,n-1)
		if(t>talpha)
		{
		
			print("Null hypothesis rejected")
			print("alternative hypothesis accepted")
			print(paste("population mean greater than ",mu))
		}
		
		else
		{
		
			print("Null hypothesis accepted")
			print("alternative hypothesis rejected")
			print(paste("population mean lesser than ",mu))
		}
	}		
		
	print(paste("alpha=",alpha))
	print(paste("sample size=",n))
	print(paste("t=",t))
	
}

X=c(183, 152, 178, 157, 194, 163, 144, 114, 178, 152, 118, 158, 172, 138)
mu=165
alpha=0.05
alternative="lesser"
one_sample_t_test(X,mu,alpha,alternative)
