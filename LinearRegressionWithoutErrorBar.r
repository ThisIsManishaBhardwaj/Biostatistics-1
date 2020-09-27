#The following scores represent a nurse's assessment (N) and a physician's assessment (D) of the condition of 10 patients at the time of admission to a trauma center.
#N : 18, 13, 18, 15, 10, 12, 8, 4, 7, 3
#D : 23, 20, 18, 16, 14, 11, 10, 7, 6, 4
#Write an R script to perform a least-square fit to a straight line through this data to express D as a function of N.
#Plot the data points and the fitted straight line on the same graph with different colors. Write the slope and intercept values on the graph.
#linear regression without error bar
#i.e. all data points have same variance

N = c(18, 13, 18, 15, 10, 12, 8, 4, 7, 3)
D = c(23, 20, 18, 16, 14, 11, 10, 7, 6, 4)
if(!length(N)==length(D))
{
	print("Inequal number of data points")
	on.exit()
}	
n=length(N)
delta=(sum(N^2)*n)-((sum(N))^2)
print(delta)

b0=((sum(D))*(sum(N^2)))-((sum(N))*(sum(N*D)))
b0=b0/delta
print(b0)

b1=n*(sum(N*D))-(sum(N)*sum(D))
b1=b1/delta
print(b1)

Dfitted=b0+(b1*N)
print(Dfitted)

plot(N,D,col="blue")
lines(N,Dfitted,col="red")
ybar=mean(D)
rsq=sum((Dfitted-ybar)^2)/sum((D-ybar)^2)
print(paste("r square=",rsq))
