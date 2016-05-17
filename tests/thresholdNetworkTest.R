
MAT0809 = as.matrix(
  read.csv(
    "/home/oliver/Work/R/WorldStocksData/sp500subPrime/cor/cor2008-09-03.dat",
    row.names = 1,
    header = TRUE
  )
)
MAT0809 = MAT0809 + t(MAT0809)

percolation=rankBasedThresholdNetwork(MAT,d = 2)
mixPercolation=rankBasedThresholdNetwork(MAT,mix=TRUE,d = 2)

percolation0809=rankBasedThresholdNetwork(MAT0809,d = 3)
mixPercolation0809=rankBasedThresholdNetwork(MAT0809,mix=TRUE,d = 3)

GRAPH= rankBasedThresholdNetwork(MAT)


## s_max
cairo_ps("~/test/percolationComponent.eps")
par(mfcol=c(1,2))
plot(mixPercolation$edgeNodeRatioAs,mixPercolation$norGiantSizeAs,type='l',lwd=2,xlab="t",ylab=expression(S[max]/n))
lines(mixPercolation$edgeNodeRatioDe,mixPercolation$norGiantSizeDe,type='l',lwd=2,xlab="t",col="red",ylab=expression(S[max]/n))
abline(v=1,col="blue")

plot(percolation$edgeNodeRatioAs,percolation$norGiantSizeAs,type='l',lwd=2,xlab="t",ylab=expression(S[max]/n))
lines(percolation$edgeNodeRatioDe,percolation$norGiantSizeDe,type='l',lwd=2,xlab="t",col="red",ylab=expression(S[max]/n))
abline(v=1,col="blue")
dev.off()

## <s^2>/<s>
cairo_ps("~/test/PercolationMeanComponent.eps")
par(mfcol=c(1,2),mar=c(2,2,2,2))
plot(mixPercolation$edgeNodeRatioAs,mixPercolation$meanClusterSizeAs,type='l',lwd=2,xlab="t",ylab=expression(S[mean]))
lines(mixPercolation$edgeNodeRatioDe,mixPercolation$meanClusterSizeDe,type='l',lwd=2,xlab="t",col="red",ylab=expression(S[max]/n))

plot(percolation$edgeNodeRatioAs,percolation$meanClusterSizeAs,type='l',lwd=2,xlab="t",ylab=expression(S[mean]))
lines(percolation$edgeNodeRatioDe,percolation$meanClusterSizeDe,type='l',lwd=2,xlab="t",col="red",ylab=expression(S[mean]))
dev.off()



cairo_ps("~/test/percolationComponent0809.eps")
par(mfcol=c(1,2))
plot(mixPercolation0809$edgeNodeRatioAs,mixPercolation0809$norGiantSizeAs,type='l',lwd=2,xlab="t",ylab=expression(S[max]/n))
lines(mixPercolation$edgeNodeRatioDe,mixPercolation$norGiantSizeDe,type='l',lwd=2,xlab="t",col="red",ylab=expression(S[max]/n))
abline(v=1,col="blue")

plot(percolation0809$edgeNodeRatioAs,percolation0809$norGiantSizeAs,type='l',lwd=2,xlab="t",ylab=expression(S[max]/n))
lines(percolation0809$edgeNodeRatioDe,percolation0809$norGiantSizeDe,type='l',lwd=2,xlab="t",col="red",ylab=expression(S[max]/n))
abline(v=1,col="blue")
dev.off()

## <s^2>/<s>
cairo_ps("~/test/PercolationMeanComponent0809.eps")
par(mfcol=c(2,2),mar=c(2,2,2,2))
plot(mixPercolation0809$edgeNodeRatioAs,mixPercolation0809$meanClusterSizeAs,type='l',lwd=2,xlab="t",ylab=expression(S[mean]))
plot(mixPercolation0809$edgeNodeRatioDe,mixPercolation0809$meanClusterSizeDe,type='l',lwd=2,xlab="t",col="red",ylab=expression(S[max]/n))
abline(v=1,col="blue")

plot(percolation0809$edgeNodeRatioAs,percolation0809$meanClusterSizeAs,type='l',lwd=2,xlab="t",ylab=expression(S[mean]))
plot(percolation0809$edgeNodeRatioDe,percolation0809$meanClusterSizeDe,type='l',lwd=2,xlab="t",col="red",ylab=expression(S[mean]))
abline(v=1,col="blue")
dev.off()



#### random matrix test############
randomMat[upper.tri(randomMat)]<- rnorm(399*400/2,mean = mean(MAT[upper.tri(MAT)]),sd =sd(MAT[upper.tri(MAT)]))
diag(randomMat)<- 0
randomMat<- randomMat+t(randomMat)
rownames(randomMat)<- c(1:400)
colnames(randomMat)<- c(1:400)
percolationRand=rankBasedThresholdNetwork(randomMat,d=5)
plot(percolationRand$edgeNodeRatioAs,percolationRand$norGiantSizeAs,type='l',lwd=3)
lines(percolationRand$edgeNodeRatioDe,percolationRand$norGiantSizeDe,type='l',lwd=3,col="red")



NormalMat=matrix(0,400,400)
NormalMat[upper.tri(NormalMat)]=rnorm(399*400/2,mean=0,sd=1)
NormalMat=NormalMat+t(NormalMat)
rownames(NormalMat)<- c(1:400)
colnames(NormalMat)<- c(1:400)
percolationNormal=rankBasedThresholdNetwork(NormalMat,d=5)
plot(percolationNormal$edgeNodeRatioAs,percolationNormal$norGiantSizeAs,type='l',lwd=3)
lines(percolationNormal$edgeNodeRatioDe,percolationNormal$norGiantSizeDe,type='l',lwd=3,col="red")
mixPercolationNormal=rankBasedThresholdNetwork(NormalMat,d=5,mix = TRUE)



gammMat=matrix(0,400,400)
gammMat[upper.tri(gammMat)]=rgamma(399*400/2,1)
gammMat=gammMat+t(gammMat)
rownames(gammMat)= c(1:400)
colnames(gammMat)=c(1:400)
percolationGamma=rankBasedThresholdNetwork(gammMat,d=5)
plot(percolationGamma$edgeNodeRatioAs,percolationGamma$norGiantSizeAs,type='l',lwd=2)


unionMat=matrix(0,400,400)
unionMat[upper.tri(unionMat)]=runif(399*400/2)
unionMat=unionMat+t(unionMat)
rownames(unionMat)=c(1:400)
colnames(unionMat)=c(1:400)
percolationUnion=rankBasedThresholdNetwork(unionMat,d=5)
plot(percolationUnion$edgeNodeRatioAs,percolationUnion$norGiantSizeAs,type='l',lwd=2)
  
  