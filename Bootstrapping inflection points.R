#This script runs a bootstrapping function to generate estimates of the inflection point of a logistic regression
data=read.csv('NLS final data.csv') 
data=data[data$age!=10,] #remove day 10 larvae from the data set
data$runs=as.factor(data$runs)
data$age=as.factor(data$age)
data$W.kg.scaled=scale(data$W.kg)
data$W.kg=scale(data$W.kg)
str(data)


library(boot)
library(lme4)
library(rethinking)
library(DHARMa) 

#This is the final statistical model used for the experiment based on AIC scores 
data8.glmer=glmer(cbind(success, fail)~age+W.kg+I(W.kg^2)+(1|runs), data=data, family=binomial, control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5)))
summary(data8.glmer)
plot(resid(data8.glmer)~predict(data8.glmer))
plot(simulateResiduals(data8.glmer))

x=seq(from=-1.2, to =4, by=.001)
diff9=NULL
diff11=NULL
slope9=NULL
slope11=NULL

#Creates the function to calculate inflection points based on the model to feed into the bootstrap function
inflection=function(data, indices)
{
d=data[indices,]
data8.glmer=glmer(cbind(success, fail)~age+W.kg+I(W.kg^2)+(1|runs), data=d, family=binomial, control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5)))	

#Collects the model estimates for the intercept and slopes
int=fixef(data8.glmer)[1]
age11=fixef(data8.glmer)[2]
W.kg=fixef(data8.glmer)[3]
W.kg2=fixef(data8.glmer)[4]

#From the model estimates, creates a predicted curve for both day 9 and day 11 larvae
y=1/(1+exp(-1*(int+W.kg*x+W.kg2*x^2))) #day 9
z=1/(1+exp(-1*(int+age11+W.kg*x+W.kg2*x^2))) #day 11

#Once the predicted curve is created, a for loop is used to calculate the inflection point of the curve. For the purposes of this experiment, the inflection point is defined as the point on the curve where the slope is the greatest. 
for(i in 1:length(y))
	{diff9[i]=y[i+1]-y[i]
	slope9[i]=diff9[i]/.001 #Slope is calculated by finding the difference between two y values and dividing it by the x difference between them (0.001)
	slope9=slope9[!is.na(slope9)]
	
	#for day 11
	diff11[i]=z[i+1]-z[i]
	slope11[i]=diff11[i]/.001
	slope11=slope11[!is.na(slope11)]}
	
	#Creates a vector of turbulence values (the same ones as the x values) to find the turbulence level at which the inflection point occurs
	turb=seq(from=-1.2, to=3.999, by=.001)
	inflec9=data.frame(turb, slope9)
	inflec11=data.frame(turb, slope11)

	inflec.point.9=inflec9$turb[inflec9$slope9==max(inflec9$slope9)]
	inflec.point.11=inflec11$turb[inflec11$slope11==max(inflec11$slope11)]
	test.dataframe=c(inflec.point.9, inflec.point.11)
	
	#Creates a plot as the bootstrap is running just for visualization
	lines(y~x, col=col.alpha(1, .1))
	lines(z~x, col=col.alpha(2, .1))
	return(test.dataframe)
}

plot(prop~W.kg, ylab='Proportion of Larvae Settled', xlab='Scaled Turbulence Intensity (W/Kg)', col=c('1','2')[data$age], pch=19, data=data)
data.boot=boot(data=data, statistic=inflection, R=100) #Runs the bootstrap function, which takes roughly 10 minutes or so if R = 1000

#Calculates the 95th percentile confidence intervals of the calculated inflection point of both ages
boot.ci(data.boot, type='bca', conf=.95, index=1)
boot.ci(data.boot, type='bca', conf=.95, index=2)

# test=data.boot$t
# test=data.frame(test)
# head(test)
# write.table(test, "inflection point list 10000.csv", sep = ',')