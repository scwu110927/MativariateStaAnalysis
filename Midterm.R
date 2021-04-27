
#1####
library(psych)
library(stats)
library(RCurl)

sales <- read.table("Sales.txt", h = TRUE)
pairs.panels(sales, method = "pearson", ellipses = FALSE)
pca.sales <- princomp(sales, cor = TRUE)
summary(pca.sales)
loadings(pca.sales)
pcs.sales <- predict(pca.sales)
eigen <- eigen(cor(sales))
plot(eigen$values, type = 'b')
plot(pcs.sales[,1:2], type = 'n', xlab = '1st PC', ylab = '2nd PC', 
     main = '2D of PCA')
text(pcs.sales[,1:2], row.names(sales))
sign.pc<-function(x,R=1000,m=length(x), cor=T,...){
  # run PCA
  pc.out<-princomp(x,cor=cor,...)
  # the proportion of variance of each PC
  pve=(pc.out$sdev^2/m)[1:m]
  # a matrix with R rows and m columns that contains
  # the proportion of variance explained by each pc
  # for each randomization replicate.
  pve.perm<-matrix(NA,ncol=m,nrow=R)
  for(i in 1:R){
    # permutation each column
    x.perm<-apply(x,2,sample)
    # run PCA
    pc.perm.out<-princomp(x.perm,cor=cor,...)
    # the proportion of variance of each PC.perm
    pve.perm[i,]=(pc.perm.out$sdev^2/m)[1:m]
  }
  # calcalute the p-values
  pval<-apply(t(pve.perm)>pve,1,sum)/R
  return(list(pve=pve,pval=pval))
}
sign.pc(sales, cor = T)

sales.mle <- factanal(sales, factors = 2, scores="Bartlett")
sales.mle
1-sales.mle$uniq
plot(sales.mle$scores[, 1], sales.mle$scores[, 2], type = "n",
     xlab = 'Factor 1', ylab = 'Factor 2', main = '2D of FA')
text(sales.mle$scores[, 1:2], row.names(sales))

#2####
library(robustbase)
library(lme4)
library(MVN)
library(CCP)
air <- read.table("Air_Pollution.txt", h = TRUE)
air$PSI <- NULL
pairs.panels(air, method = "pearson", ellipses = FALSE)
newair <- cbind(log(air[, 1:6]), air[, 7])
colnames(newair)[7] <- "Rain"
air[air$Rain<10, ]
newair <- newair[newair$Rain != 7, ]
pairs.panels(newair , method = "pearson", ellipses = FALSE)
mvn(newair, mvnTest = c('royston'), desc = FALSE)
x <- scale(newair[,1:3])
y <- scale(newair[,4:7])
cxy <- cancor(x, y)
p.asym(cxy$cor, nrow(newair), 3, 4)
scorex <- x %*% cxy$xcoef[,1]
scorey <- y %*% cxy$ycoef[,1]
plot(scorex, scorey, type = "n")
text(scorex, scorey, row.names(newair), cex = .6)



