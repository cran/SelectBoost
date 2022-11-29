## ----setup, include = FALSE---------------------------------------------------
#file.edit(normalizePath("~/.Renviron"))
LOCAL <- identical(Sys.getenv("LOCAL"), "TRUE")
#LOCAL=TRUE
knitr::opts_chunk$set(purl = LOCAL)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE------------------------------------------------------------
#  devtools::install_github("fbertran/SelectBoost", ref = "doMC")

## ---- eval = LOCAL------------------------------------------------------------
library(SelectBoost)
group<-c(1:9,1) #10 variables
cor_group<-rep(0.95,9)
CM<-simulation_cor(group,cor_group)
CM

## ---- eval = LOCAL------------------------------------------------------------
set.seed(3141)
N<-10
X<-simulation_X(N,CM)
X

## ---- eval = LOCAL------------------------------------------------------------
set.seed(3141)
supp<-c(1,1,1,0,0,0,0,0,0,0)
minB<-1
maxB<-2
stn<-50
firstdataset=simulation_DATA(X,supp,minB,maxB,stn)
firstdataset

## ---- cache=TRUE, eval = LOCAL------------------------------------------------
set.seed(3141)
NDatasets=200
for(i in 1:NDatasets){
X<-simulation_X(N,CM)
assign(paste("DATA_exemple1_nb_",i,sep=""),simulation_DATA(X,supp,minB,maxB,stn))
}

## ---- cache=TRUE, eval = LOCAL------------------------------------------------
corr_sum=matrix(0,length(group),length(group))
for(i in 1:NDatasets){
corr_sum=corr_sum+cor(get(paste("DATA_exemple1_nb_",i,sep=""))$X)
}
corr_mean=corr_sum/NDatasets

## ---- fig.width=7, eval = LOCAL-----------------------------------------------
corr_mean
plot(abs(corr_mean))

## ---- eval = LOCAL------------------------------------------------------------
coef_sum=rep(0,length(group))
names(coef_sum)<-paste("x",1:length(group),sep="")
error_counter=0
for(i in 1:NDatasets){
tempdf=data.frame(cbind(Y=get(paste("DATA_exemple1_nb_",i,sep=""))$Y,
                        get(paste("DATA_exemple1_nb_",i,sep=""))$X))
tempcoef=coef(lm(Y~.-1,data=tempdf))
if(is.null(tempcoef)){
cat("Error in lm fit, skip coefficients\n")
error_counter=error_counter+1
  } else{
coef_sum=coef_sum+abs(tempcoef)
}
}
error_counter
coef_mean=coef_sum/NDatasets

## ---- eval = LOCAL------------------------------------------------------------
coef_mean
barplot(coef_mean)
abline(h=(minB+maxB)/2,lwd=2,lty=2,col="blue")

## ---- cache=TRUE, eval = LOCAL------------------------------------------------
set.seed(3141)
stn <- 5000
for(i in 1:NDatasets){
X<-simulation_X(N,CM)
assign(paste("DATA_exemple1_bis_nb_",i,sep=""),simulation_DATA(X,supp,minB,maxB,stn))
}

## ---- cache=TRUE, eval = LOCAL------------------------------------------------
stn_ratios=rep(0,NDatasets)
for(i in 1:NDatasets){
stn_ratios[i]<-get(paste("DATA_exemple1_nb_",i,sep=""))$sigma/
  get(paste("DATA_exemple1_bis_nb_",i,sep=""))$sigma
}
all(sapply(stn_ratios,all.equal,10))

## ---- cache=TRUE, eval = LOCAL------------------------------------------------
coef_sum_bis=rep(0,length(group))
names(coef_sum_bis)<-paste("x",1:length(group),sep="")
error_counter_bis=0
for(i in 1:NDatasets){
tempdf=data.frame(cbind(Y=get(paste("DATA_exemple1_bis_nb_",i,sep=""))$Y,
                        get(paste("DATA_exemple1_bis_nb_",i,sep=""))$X))
tempcoef=coef(lm(Y~.-1,data=tempdf))
if(is.null(tempcoef)){
cat("Error in lm fit, skip coefficients\n")
error_counter_bis=error_counte_bisr+1
  } else{
coef_sum_bis=coef_sum_bis+abs(tempcoef)
}
}
error_counter_bis
coef_mean_bis=coef_sum_bis/NDatasets

## ---- eval = LOCAL------------------------------------------------------------
coef_mean_bis
barplot(coef_mean_bis)
abline(h=(minB+maxB)/2,lwd=2,lty=2,col="blue")

## ---- eval = LOCAL------------------------------------------------------------
group<-rep(1,50) #50 variables
cor_group<-rep(0.5,1)

## ---- cache=TRUE, eval = LOCAL------------------------------------------------
set.seed(3141)
N<-20
supp<-c(1,1,1,1,1,rep(0,45))
minB<-1
maxB<-2
stn<-50
for(i in 1:200){
C<-simulation_cor(group,cor_group)
X<-simulation_X(N,C)
assign(paste("DATA_exemple2_nb_",i,sep=""),simulation_DATA(X,supp,minB,maxB,stn))
}

## ---- eval = LOCAL------------------------------------------------------------
corr_sum=matrix(0,length(group),length(group))
for(i in 1:NDatasets){
corr_sum=corr_sum+cor(get(paste("DATA_exemple2_nb_",i,sep=""))$X)
}
corr_mean=corr_sum/NDatasets

## ---- fig.keep='none', fig.width=7, eval = LOCAL------------------------------
corr_mean[1:10,1:10]
plot(abs(corr_mean))

## ---- cache=TRUE, eval = LOCAL------------------------------------------------
coef_sum=rep(0,length(group))
coef_lasso_sum=rep(0,length(group))
names(coef_sum)<-paste("x",1:length(group),sep="")
names(coef_lasso_sum)<-paste("x",1:length(group),sep="")
error_counter=0
for(i in 1:NDatasets){
tempdf=data.frame(cbind(Y=get(paste("DATA_exemple2_nb_",i,sep=""))$Y,
                        get(paste("DATA_exemple2_nb_",i,sep=""))$X))
tempcoef=coef(lm(Y~.-1,data=tempdf))
require(lars)
lasso.1 <- lars::lars(x=get(paste("DATA_exemple2_nb_",i,sep=""))$X,
              y=get(paste("DATA_exemple2_nb_",i,sep=""))$Y, type="lasso", 
              trace=FALSE, normalize=FALSE, intercept = FALSE)
# cv.lars() uses crossvalidation to estimate optimal position in path
cv.lasso.1 <- lars::cv.lars(x=get(paste("DATA_exemple2_nb_",i,sep=""))$X,
              y=get(paste("DATA_exemple2_nb_",i,sep=""))$Y, 
              plot.it=FALSE, type="lasso")
# Use the "+1SE rule" to find best model: 
#    Take the min CV and add its SE ("limit").  
#    Find smallest model that has its own CV within this limit (at "s.cv.1")
limit <- min(cv.lasso.1$cv) + cv.lasso.1$cv.error[which.min(cv.lasso.1$cv)]
s.cv.1 <- cv.lasso.1$index[min(which(cv.lasso.1$cv < limit))]
# Print out coefficients at optimal s.
coef_lasso_sum=coef_lasso_sum+abs(coef(lasso.1, s=s.cv.1, mode="fraction"))
if(is.null(tempcoef)){
cat("Error in lm fit, skip coefficients\n")
error_counter=error_counter+1
  } else{
coef_sum=coef_sum+abs(tempcoef)
}
}
error_counter
coef_mean=coef_sum/NDatasets
coef_lasso_mean=coef_lasso_sum/NDatasets

## ---- eval = LOCAL------------------------------------------------------------
coef_mean
barplot(coef_mean)
abline(h=(minB+maxB)/2,lwd=2,lty=2,col="blue")

## ---- eval = LOCAL------------------------------------------------------------
coef_lasso_mean
barplot(coef_lasso_mean,ylim=c(0,1.5))
abline(h=(minB+maxB)/2,lwd=2,lty=2,col="blue")

## ---- fig.keep='none', cache=TRUE, eval = LOCAL-------------------------------
require(CascadeData)
data(micro_S)
data(micro_US)
require(Cascade)
micro_US<-as.micro_array(micro_US,c(60,90,240,390),6)
micro_S<-as.micro_array(micro_S,c(60,90,240,390),6)
S<-geneSelection(list(micro_S,micro_US),list("condition",c(1,2),1),-1)
Sel<-micro_S@microarray[S@name,]

## ---- fig.keep='none', cache=TRUE, eval = LOCAL-------------------------------
summary(S)

## ---- fig.keep='last', cache=TRUE, eval = LOCAL-------------------------------
plot(S)

## ---- cache=TRUE, eval = LOCAL------------------------------------------------
set.seed(3141)
supp<-c(1,1,1,1,1,rep(0,95))
minB<-1
maxB<-2
stn<-50
NDatasets=200

for(i in 1:NDatasets){
X<-t(as.matrix(Sel[sample(1:nrow(Sel),100),]))
Xnorm<-t(t(X)/sqrt(colSums(X*X)))
assign(paste("DATA_exemple3_nb_",i,sep=""),simulation_DATA(Xnorm,supp,minB,maxB,stn))
}

## ---- eval=FALSE--------------------------------------------------------------
#  plot(cor(Xnorm))
#  mixOmics::cim(cor(Xnorm))

## ---- cache=TRUE, eval = LOCAL------------------------------------------------
coef_sum=rep(0,length(supp))
coef_lasso_sum=rep(0,length(supp))
names(coef_sum)<-paste("x",1:length(supp),sep="")
names(coef_lasso_sum)<-paste("x",1:length(supp),sep="")
error_counter=0
for(i in 1:NDatasets){
tempdf=data.frame(cbind(Y=get(paste("DATA_exemple3_nb_",i,sep=""))$Y,
                        get(paste("DATA_exemple3_nb_",i,sep=""))$X))
tempcoef=coef(lm(Y~.-1,data=tempdf))
require(lars)
lasso.1 <- lars::lars(x=get(paste("DATA_exemple3_nb_",i,sep=""))$X,
              y=get(paste("DATA_exemple3_nb_",i,sep=""))$Y, type="lasso", 
              trace=FALSE, normalize=FALSE, intercept = FALSE)
# cv.lars() uses crossvalidation to estimate optimal position in path
cv.lasso.1 <- lars::cv.lars(x=get(paste("DATA_exemple3_nb_",i,sep=""))$X,
              y=get(paste("DATA_exemple3_nb_",i,sep=""))$Y, 
              plot.it=FALSE, normalize=FALSE, intercept = FALSE, type="lasso")
# Use the "+1SE rule" to find best model: 
#    Take the min CV and add its SE ("limit").  
#    Find smallest model that has its own CV within this limit (at "s.cv.1")
limit <- min(cv.lasso.1$cv) + cv.lasso.1$cv.error[which.min(cv.lasso.1$cv)]
s.cv.1 <- cv.lasso.1$index[min(which(cv.lasso.1$cv < limit))]
# Print out coefficients at optimal s.
coef_lasso_sum=coef_lasso_sum+abs(coef(lasso.1, s=s.cv.1, mode="fraction"))
if(is.null(tempcoef)){
cat("Error in lm fit, skip coefficients\n")
error_counter=error_counter+1
  } else{
coef_sum=coef_sum+abs(tempcoef)
}
}
error_counter
coef_mean=coef_sum/NDatasets
coef_lasso_mean=coef_lasso_sum/NDatasets

## ---- eval = LOCAL------------------------------------------------------------
coef_mean
barplot(coef_mean)
abline(h=(minB+maxB)/2,lwd=2,lty=2,col="blue")

## ---- eval = LOCAL------------------------------------------------------------
coef_lasso_mean
barplot(coef_lasso_mean,ylim=c(0,1.5))
abline(h=(minB+maxB)/2,lwd=2,lty=2,col="blue")

## ---- fig.keep='none', cache=TRUE, eval = LOCAL-------------------------------
require(CascadeData)
data(micro_S)
data(micro_US)
require(Cascade)
micro_US<-as.micro_array(micro_US,c(60,90,240,390),6)
micro_S<-as.micro_array(micro_S,c(60,90,240,390),6)
S<-geneSelection(list(micro_S,micro_US),list("condition",c(1,2),1),101)
Sel<-micro_S@microarray[S@name,]

## ---- fig.keep='none', cache=TRUE, eval = LOCAL-------------------------------
summary(S)

## ---- fig.keep='last', cache=TRUE, eval = LOCAL-------------------------------
plot(S)

## ---- cache=TRUE, eval = LOCAL------------------------------------------------
suppt<-rep(1:4,6)
supp<-c(1,1,1,1,1,rep(0,95)) #not used since we use one of the probeset expressions as response
minB<-1 #not used since we use one of the probeset expressions as response
maxB<-2 #not used since we use one of the probeset expressions as response
stn<-50 #not used since we use one of the probeset expressions as response
NDatasets<-101

set.seed(3141)
for(i in 1:NDatasets){
  #the explanatory variables are the values for the 1st, 2nd and 3rd timepoints
  X<-t(as.matrix(Sel[-i,suppt!=4]))
  Xnorm<-t(t(X)/sqrt(colSums(X*X)))
  DATA<-simulation_DATA(Xnorm,supp,minB,maxB,stn)
  #the reponses are the values for the 2nd, 3rd and 4th timepoints
  DATA$Y<-as.vector(t(Sel[i,suppt!=1]))
  assign(paste("DATA_exemple4_nb_",i,sep=""),DATA)
  rm(DATA)
}

## ---- eval=FALSE--------------------------------------------------------------
#  plot(cor(Xnorm))
#  mixOmics::cim(cor(Xnorm))

## ---- cache=TRUE, eval = LOCAL------------------------------------------------
coef_sum=rep(0,length(supp)+1)
coef_lasso_sum=rep(0,length(supp)+1)
names(coef_sum)<-paste("x",1:(length(supp)+1),sep="")
names(coef_lasso_sum)<-paste("x",1:(length(supp)+1),sep="")
error_counter=0
for(i in 1:NDatasets){
tempdf=data.frame(cbind(Y=get(paste("DATA_exemple4_nb_",i,sep=""))$Y,
                        get(paste("DATA_exemple4_nb_",i,sep=""))$X))
tempcoef=coef(lm(Y~.-1,data=tempdf))
require(lars)
lasso.1 <- lars::lars(x=get(paste("DATA_exemple4_nb_",i,sep=""))$X,
              y=get(paste("DATA_exemple4_nb_",i,sep=""))$Y, type="lasso", 
              trace=FALSE, normalize=FALSE, intercept = FALSE)
# cv.lars() uses crossvalidation to estimate optimal position in path
cv.lasso.1 <- lars::cv.lars(x=get(paste("DATA_exemple4_nb_",i,sep=""))$X,
              y=get(paste("DATA_exemple4_nb_",i,sep=""))$Y, 
              plot.it=FALSE, normalize=FALSE, intercept = FALSE, type="lasso")
# Use the "+1SE rule" to find best model: 
#    Take the min CV and add its SE ("limit").  
#    Find smallest model that has its own CV within this limit (at "s.cv.1")
limit <- min(cv.lasso.1$cv) + cv.lasso.1$cv.error[which.min(cv.lasso.1$cv)]
s.cv.1 <- cv.lasso.1$index[min(which(cv.lasso.1$cv < limit))]
# Print out coefficients at optimal s.
coef_lasso_sum[-i]=coef_lasso_sum[-i]+abs(coef(lasso.1, s=s.cv.1, mode="fraction"))
if(is.null(tempcoef)){
cat("Error in lm fit, skip coefficients\n")
error_counter=error_counter+1
  } else{
coef_sum[-i]=coef_sum[-i]+abs(tempcoef)
}
}
error_counter
coef_mean=coef_sum/NDatasets
coef_lasso_mean=coef_lasso_sum/NDatasets

## ---- eval = LOCAL------------------------------------------------------------
head(coef_mean, 40)
barplot(coef_mean)
abline(h=(minB+maxB)/2,lwd=2,lty=2,col="blue")

## ---- eval = LOCAL------------------------------------------------------------
head(coef_lasso_mean, 40)
barplot(coef_lasso_mean)

## ---- cache=TRUE, eval = LOCAL------------------------------------------------
  group<-rep(1,500) #500 variables
  cor_group<-rep(0.5,1)

## ---- cache=TRUE, eval = LOCAL------------------------------------------------
set.seed(3141)
N<-25
supp<-c(1,1,1,1,1,rep(0,495))
minB<-1
maxB<-2
stn<-50
for(i in 1:NDatasets){
  C<-simulation_cor(group,cor_group)
  X<-simulation_X(N,C)
  assign(paste("DATA_exemple5_nb_",i,sep=""),simulation_DATA(X,supp,minB,maxB,stn))
}

## ---- eval = LOCAL------------------------------------------------------------
corr_sum=matrix(0,length(group),length(group))
for(i in 1:NDatasets){
corr_sum=corr_sum+cor(get(paste("DATA_exemple5_nb_",i,sep=""))$X)
}
corr_mean=corr_sum/NDatasets

## ---- fig.keep='none', fig.width=7, eval = LOCAL------------------------------
corr_mean[1:10,1:10]
plot(abs(corr_mean))

## ---- cache=TRUE, eval = LOCAL------------------------------------------------
coef_sum=rep(0,length(group))
coef_lasso_sum=rep(0,length(group))
names(coef_sum)<-paste("x",1:length(group),sep="")
names(coef_lasso_sum)<-paste("x",1:length(group),sep="")
error_counter=0
for(i in 1:NDatasets){
tempdf=data.frame(cbind(Y=get(paste("DATA_exemple5_nb_",i,sep=""))$Y,
                        get(paste("DATA_exemple5_nb_",i,sep=""))$X))
tempcoef=coef(lm(Y~.-1,data=tempdf))
require(lars)
lasso.1 <- lars::lars(x=get(paste("DATA_exemple5_nb_",i,sep=""))$X,
              y=get(paste("DATA_exemple5_nb_",i,sep=""))$Y, type="lasso", 
              trace=FALSE, normalize=FALSE, intercept = FALSE)
# cv.lars() uses crossvalidation to estimate optimal position in path
cv.lasso.1 <- lars::cv.lars(x=get(paste("DATA_exemple5_nb_",i,sep=""))$X,
              y=get(paste("DATA_exemple5_nb_",i,sep=""))$Y, 
              plot.it=FALSE, type="lasso")
# Use the "+1SE rule" to find best model: 
#    Take the min CV and add its SE ("limit").  
#    Find smallest model that has its own CV within this limit (at "s.cv.1")
limit <- min(cv.lasso.1$cv) + cv.lasso.1$cv.error[which.min(cv.lasso.1$cv)]
s.cv.1 <- cv.lasso.1$index[min(which(cv.lasso.1$cv < limit))]
# Print out coefficients at optimal s.
coef_lasso_sum=coef_lasso_sum+abs(coef(lasso.1, s=s.cv.1, mode="fraction"))
if(is.null(tempcoef)){
cat("Error in lm fit, skip coefficients\n")
error_counter=error_counter+1
  } else{
coef_sum=coef_sum+abs(tempcoef)
}
}
error_counter
coef_mean=coef_sum/NDatasets
coef_lasso_mean=coef_lasso_sum/NDatasets

## ---- eval = LOCAL------------------------------------------------------------
head(coef_mean, 40)
barplot(coef_mean)
abline(h=(minB+maxB)/2,lwd=2,lty=2,col="blue")

## ---- eval = LOCAL------------------------------------------------------------
head(coef_lasso_mean, 40)
barplot(coef_lasso_mean,ylim=c(0,1.5))
abline(h=(minB+maxB)/2,lwd=2,lty=2,col="blue")

