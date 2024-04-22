# Team: Byte Coders
#Participants: Barnashree Mondal, G.Y.Manisha, and Priyanka Show


library(ggplot2)
library(dplyr)
library(MASS)
library(glmnet)

#Loading datasets for two different subjects
d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)


#merge the datasets
d<-rbind.data.frame(d1,d2)
data<-dplyr::mutate(d,subject=c(rep("Mathematics",nrow(d1)),rep("Portuguese",nrow(d2))))


#Convering dataset to numeric matrix
data = model.matrix(~., data=data)
data = data[,-1]


#Correlation analysis
library(corrplot)
data1<-data
colnames(data1)
data1<-data.frame(data1)

dataA<-dplyr::select(data1,schoolMS,
                     sexM, 
                     age, 
                     addressU, 
                     famsizeLE3, 
                     subjectPortuguese,
                     G1,G2, G3)
M1 = cor(dataA)
corrplot(M1, method = 'number')

dataB<-dplyr::select(data1,PstatusT, 
                     Medu, 
                     Fedu, 
                     Mjobservices,
                     Mjobteacher, 
                     Fjobhealth, 
                     Fjobother, 
                     Fjobservices, 
                     Fjobteacher, 
                     reasonhome, 
                     reasonother, 
                     reasonreputation, 
                     guardianmother, 
                     G1, G2, G3)
M2 = cor(dataB)
corrplot(M2, method = 'number')

dataC<-dplyr::select(data1,guardianother, 
                     traveltime, 
                     studytime, 
                     failures, 
                     schoolsupyes, 
                     famsupyes, 
                     paidyes, 
                     activitiesyes, 
                     nurseryyes, 
                     higheryes, 
                     G1, G2, G3)
M3 = cor(dataC)
corrplot(M3, method = 'number')

dataD<-dplyr::select(data1, romanticyes, 
                     famrel, 
                     freetime, 
                     goout, 
                     Dalc, 
                     Walc, 
                     health, 
                     absences,
                     internetyes,
                     G1, G2, G3)
M4 = cor(dataD)
corrplot(M4, method = 'number')



# Assuming you have four correlation matrices stored in M1, M2, M3, and M4

# Load necessary library
library(corrplot)

# Create a layout for the plots (2 rows, 2 columns)
layout(matrix(1:4, nrow = 2, byrow = TRUE))

# Plot each correlation matrix in a separate panel
corrplot(M1, method = 'number', main = "Matrix 1")
corrplot(M2, method = 'number', main = "Matrix 2")
corrplot(M3, method = 'number', main = "Matrix 3")
corrplot(M4, method = 'number', main = "Matrix 4")

# Visualize correlation matrix using heatmap
heatmap(M)


#########***#########   DATA PREPROCESSING   #########***#########

### create response variable
y = d1$G3  #For grade 3 Math
#y = d1$G1  #For grade 1 Math
#y = d1$G2  #For grade 2 Math
#y = d2$G3  #For grade 3 Por
#y = d2$G1  #For grade 1 Por
#y = d2$G2  #For grade 2 Por

n = length(y)

### create model design matrix (without intercept)


X = model.matrix(~., data=d1[,1:30]) # for Math
#X = model.matrix(~., data=d2[,1:30]) # for Por
X = X[,-1]

### check size of X
dim(X)

#########***#########   OLS REGRESSION   #########***#########

### fit linear regression model via OLS
olsmod = lm(y ~ ., data=data.frame(X))

### summarize model results
olsmod.sum = summary(olsmod)
olsmod.sum

### extract model coefficients
olscoef = coef(olsmod)
olscoef

#########***#########   P = 0.05 SELECTION   #########***#########

### find significant coefficients at 0.05 level
ix = which(olsmod.sum$coefficients[-1,4] < 0.05)

### refit model with significant coefficients
pvalmod.05 = lm(y ~ ., data=data.frame(X[,ix]))
summary(pvalmod.05)
### extract model coefficients
pvalcoef.05 = as(matrix(0, length(olscoef), 1), "dgCMatrix")
ix = match(names(coef(pvalmod.05)), names(olscoef))
pvalcoef.05[ix] = coef(pvalmod.05)
rownames(pvalcoef.05) = names(olscoef)


#########***#########   P = 0.15 SELECTION   #########***#########

### find significant coefficients at 0.15 level
ix = which(olsmod.sum$coefficients[-1,4] < 0.15)

### refit model with significant coefficients
pvalmod.15 = lm(y ~ ., data=data.frame(X[,ix]))


summary(pvalmod.15)
### extract model coefficients
pvalcoef.15 = as(matrix(0, length(olscoef), 1), "dgCMatrix")
ix = match(names(coef(pvalmod.15)), names(olscoef))
pvalcoef.15[ix] = coef(pvalmod.15)
rownames(pvalcoef.15) = names(olscoef)


#########***#########   STEPWISE REGRESSION (AIC)   #########***#########

### AIC selection and direction="both" by default
stepmod.aic = step(olsmod, trace=0)

### get stepwise coefficients
stepcoef.aic = as(matrix(0, length(olscoef), 1), "dgCMatrix")
ix = match(names(coef(stepmod.aic)), names(olscoef))
stepcoef.aic[ix] = coef(stepmod.aic)
rownames(stepcoef.aic) = names(olscoef)


#########***#########   STEPWISE REGRESSION (BIC)   #########***#########

### BIC selection (and direction="both" by default)
stepmod.bic = step(olsmod, trace=0, k=log(n))
summary(stepmod.bic)
### get stepwise coefficients
stepcoef.bic = as(matrix(0, length(olscoef), 1), "dgCMatrix")
ix = match(names(coef(stepmod.bic)), names(olscoef))
stepcoef.bic[ix] = coef(stepmod.bic)
rownames(stepcoef.bic) = names(olscoef)


#########***#########   RIDGE REGRESSION   #########***#########

### fit ridge regression model using GCV to select lambda
lamseq = seq(0,300,length=1000)
ridgemod = lm.ridge(y ~ ., data=data.frame(X), lambda=lamseq)
summary(ridgemod)
### plot the ridge trace (to ensure we found minimum)
#quartz(width=8,height=4)
plot(ridgemod$lambda, ridgemod$GCV, xlab="Lambda", ylab="GCV")
lines(rep(lamseq[which.min(ridgemod$GCV)],2), range(ridgemod$GCV), lty=3)
#dev.copy2pdf(file="~/Desktop/psych-penreg/ridge-gcv.pdf")

### find lambda that minimizes GCV
gcvmin = which.min(ridgemod$GCV)

### extract model coefficients
ridgecoef.min = coef(ridgemod)[gcvmin,]

summary(ridgecoef.min)
#########***#########   LASSO REGRESSION   #########***#########

### create fold assignments for 10-fold CV
set.seed(1)
foldid = sample(rep(1:10, length.out=n))

### 10-fold CV to estimate lambda
cvlasso = cv.glmnet(X, y, foldid=foldid, alpha=1)

### plot results
#quartz(width=8,height=4)
plot(cvlasso)
#dev.copy2pdf(file="~/Desktop/psych-penreg/lasso-mse.pdf")

### get the coefficients
lassocoef.min = coef(cvlasso, s="lambda.min")
lassocoef.1se = coef(cvlasso, s="lambda.1se")


#########***#########   ELASTIC NET REGRESSION   #########***#########

### create fold assignments for 10-fold CV
set.seed(1)
foldid = sample(rep(1:10, length.out=n))

### 10-fold CV to estimate lambda and alpha
alphaseq = seq(0,1,length=21)
cvlist = vector("list",length(alphaseq))
for(k in 1:length(alphaseq)){
  cvlist[[k]] = cv.glmnet(X, y, foldid=foldid, alpha=alphaseq[k])
}

### plot alphaseq vs CV-MSE
#quartz(width=8,height=8)
par(mfrow=c(2,1))
mincv = sapply(cvlist, function(x) min(x$cvm))
plot(alphaseq, mincv, xlab="Alpha", ylab="Mean-Squared Error", type="b")

### get the minimum
minid = which.min(mincv)
minid
alphaseq[minid]

### plot results for minimum
plot(cvlist[[minid]])
#dev.copy2pdf(file="~/Desktop/psych-penreg/enet-mse.pdf")

### get the coefficients
enetcoef.min = coef(cvlist[[minid]], s="lambda.min")
enetcoef.1se = coef(cvlist[[minid]], s="lambda.1se")


#########***#########   TABLE OF COEFFICIENTS   #########***#########

### unpenalized coefficients (table 3)
utab = round(cbind(olscoef,pvalcoef.05,pvalcoef.15,stepcoef.aic,stepcoef.bic),3)
colnames(utab) = c("ols","p0.05","p0.15","step.aic","step.bic")
utab

### penalized coefficients (table 4)
ptab = round(cbind(ridgecoef.min,lassocoef.min,lassocoef.1se,enetcoef.min,enetcoef.1se),3)
colnames(ptab) = c("ridgecoef","lasso.min","lasso.1se","enet.min","enet.1se")
ptab


#########***#########   MSPE SIMULATION   #########***#########

nrep = 100
methods = factor(c("ols","p0.05","p0.15","step.aic","step.bic",
                   "ridge","lasso.min","lasso.1se","enet.min","enet.1se"),
                 ordered=TRUE, levels=c("ols","p0.05","p0.15","step.aic","step.bic",
                                        "ridge","lasso.min","lasso.1se","enet.min","enet.1se"))
msetab = matrix(0, nrep, length(methods))
lamseq = seq(0, 300, length=1000)
alphaseq = seq(0, 1, length=21)

set.seed(55455)
for(i in 1:nrep){
  
  # print progress
  cat("rep:",i,"\n")
  
  # create training and testing data
  testID = sample.int(n, n*0.2) # 80 X 20 Training testing
  ytest = y[testID]
  Xtest = X[testID,]
  
  ytrain = y[-testID]
  Xtrain = X[-testID,]
  
  # ols regression
  olsmod = lm(ytrain ~ ., data=data.frame(Xtrain))
  msetab[i,1] = mean( (ytest - cbind(1,Xtest) %*% coef(olsmod))^2 )
  
  # p = 0.05
  olsmod.sum = summary(olsmod)
  ix = which(olsmod.sum$coefficients[-1,4] < 0.05)
  p05mod = lm(ytrain ~ ., data=data.frame(Xtrain[,ix]))
  msetab[i,2] = mean( (ytest - cbind(1,Xtest[,ix]) %*% coef(p05mod))^2 )
  
  # p = 0.15
  ix = which(olsmod.sum$coefficients[-1,4] < 0.15)
  p15mod = lm(ytrain ~ ., data=data.frame(Xtrain[,ix]))
  msetab[i,3] = mean( (ytest - cbind(1,Xtest[,ix]) %*% coef(p15mod))^2 )
  
  # stepwise regression (aic)
  stepmod = step(olsmod, trace=0)
  ix = match(names(stepmod$coefficients),names(olsmod$coefficients))
  msetab[i,4] = mean( (ytest - cbind(1,Xtest)[,ix] %*% coef(stepmod))^2 )
  
  # stepwise regression (bic)
  stepmod = step(olsmod, trace=0, k=log(length(ytrain)))
  ix = match(names(stepmod$coefficients),names(olsmod$coefficients))
  msetab[i,5] = mean( (ytest - cbind(1,Xtest)[,ix] %*% coef(stepmod))^2 )
  
  # ridge regression
  ridgemod = lm.ridge(ytrain ~ ., data=data.frame(Xtrain), lambda=lamseq)
  gcvmin = which.min(ridgemod$GCV)
  msetab[i,6] = mean( (ytest - cbind(1,Xtest) %*% coef(ridgemod)[gcvmin,])^2 )
  
  # get folds for lasso and elastic net
  foldid = sample(rep(1:10, length.out=length(ytrain)))
  
  # lasso regression
  cvlasso = cv.glmnet(Xtrain, ytrain, foldid=foldid, alpha=1)
  msetab[i,7] = mean( (ytest - cbind(1,Xtest) %*% coef(cvlasso, s="lambda.min"))^2 )
  msetab[i,8] = mean( (ytest - cbind(1,Xtest) %*% coef(cvlasso, s="lambda.1se"))^2 )
  
  # elastic net regression
  cvlist = vector("list",length(alphaseq))
  for(k in 1:length(alphaseq)){
    cvlist[[k]] = cv.glmnet(Xtrain, ytrain, foldid=foldid, alpha=alphaseq[k])
  }
  minid = which.min(sapply(cvlist, function(x) min(x$cvm)))
  msetab[i,9] = mean( (ytest - cbind(1,Xtest) %*% coef(cvlist[[minid]], s="lambda.min"))^2 )
  msetab[i,10] = mean( (ytest - cbind(1,Xtest) %*% coef(cvlist[[minid]], s="lambda.1se"))^2 )
  
}

### box plots of MSPE
library(RColorBrewer)
#quartz(width=12, height=5)
Methods = rep(methods, each=nrep)
MSE = c(msetab)
colors = brewer.pal(10, "Set3")
boxplot(MSE ~ Methods, col=colors, xlab="Methods", ylab="Mean-Squared Error", ylim=c(6,12))
for(j in c(7,9,11)) lines(c(0,11), c(j,j), lty=3)
#dev.copy2pdf(file="~/Desktop/psych-penreg/box-mse.pdf")

### mean MSPE
meanmse = apply(msetab,2,mean)
names(meanmse) = methods
meanmse

### percent best MSPE
best = apply(msetab, 1, which.min)
prctbest = summary(factor(best))
prctbest

### MSPE for first rep
colnames(msetab) = levels(methods)
msetab[1,]

msetab[,6]

data<-data.frame(msetab)

library(tidyr)

# Use pivot_longer to transform the dataframe
data <- pivot_longer(data, cols = everything(), names_to = "method", values_to = "results")
data<-data.frame(data)



fun_mean <- function(x){
  return(data.frame(y=mean(x),label=round(mean(x,na.rm=T),2)))}
fun_mean1 <- function(x){
  return(data.frame(y=mean(x),label=paste0("\u00B1",round((1.96*sd(x,na.rm=T))/sqrt(length(x)),2))))}

ggplot(data,aes(method, results,fill=method))+geom_boxplot()+xlab("Methods")+
  ylab("Mean-Squared Error")+ggtitle("Box plots of mean-squared prediction error across 100 random splits of the data")+
  stat_summary(fun.y=mean, colour="red", geom="point",shape=18, size=2,show_guide = FALSE) +
  stat_summary(fun.data = fun_mean, geom="text", col="black",vjust=-4.0)+
  stat_summary(fun.data = fun_mean1, geom="text", col="black",vjust=-3.0)+coord_cartesian(ylim = c(0, 25))+
  scale_fill_brewer(palette="RdYlBu")+
  scale_color_brewer(palette="RdYlBu")+
  ##geom_text(data = means, aes(label = round(eer,4), y = eer + 0.08))+
  theme(legend.background = element_rect(fill = "white"),legend.position="bottom",panel.background = element_rect(fill = "white", colour = "grey50"), axis.ticks.x = element_blank())

