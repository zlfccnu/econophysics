
MAT0809 = as.matrix(
  read.csv(
    "/home/oliver/Work/R/WorldStocksData/sp500subPrime/cor/cor2008-09-03.dat",
    row.names = 1,
    header = TRUE
  )
)
MAT0809 = MAT0809 + t(MAT0809)

percolation=rankBasedThresholdNetworkStatiscs(MAT,d = 2)
mixPercolation=rankBasedThresholdNetworkStatiscs(MAT,mix=TRUE,d = 2)

percolation0809=rankBasedThresholdNetwork(MAT0809,d = 2)
mixPercolation0809=rankBasedThresholdNetwork(MAT0809,mix=TRUE,d = 2)

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
percolationRand=rankBasedThresholdNetworkStatiscs(randomMat,d=2)
plot(percolationRand$edgeNodeRatioAs,percolationRand$norGiantSizeAs,type='l',lwd=3)
lines(percolationRand$edgeNodeRatioDe,percolationRand$norGiantSizeDe,type='l',lwd=3,col="red")



NormalMat=matrix(0,3000,3000)
NormalMat[upper.tri(NormalMat)]=rnorm(2999*3000/2,mean=0,sd=1)
NormalMat=NormalMat+t(NormalMat)
rownames(NormalMat)<- c(1:3000)
colnames(NormalMat)<- c(1:3000)
percolationNormal=rankBasedThresholdNetworkStatiscs(NormalMat,d=2)
mixPercolationNormal=rankBasedThresholdNetworkStatiscs(NormalMat,d=2,mix = TRUE)

par(mfcol=c(1,2),mar=c(2,2,2,2))
plot(percolationNormal$edgeNodeRatioAs,percolationNormal$norGiantSizeAs,type='l',lwd=3)
lines(percolationNormal$edgeNodeRatioDe,percolationNormal$norGiantSizeDe,type='l',lwd=3,col="red")
abline(v=1,h=1)
plot(mixPercolationNormal$edgeNodeRatioAs,mixPercolationNormal$norGiantSizeAs,type='l',lwd=2)
lines(mixPercolationNormal$edgeNodeRatioDe,mixPercolationNormal$norGiantSizeDe,col="red",lwd=2)
abline(v=1,h=1)



gammMat=matrix(0,2000,2000)
gammMat[upper.tri(gammMat)]=rgamma(1999*2000/2,1)
gammMat=gammMat+t(gammMat)
rownames(gammMat)= c(1:2000)
colnames(gammMat)=c(1:2000)
percolationGamma=rankBasedThresholdNetworkStatiscs(gammMat,d=2)
mixPercolationGamma=rankBasedThresholdNetworkStatiscs(gammMat,mix = TRUE,d=2)
par(mfcol=c(1,2),mar=c(2,2,2,2))
plot(percolationGamma$edgeNodeRatioAs,percolationGamma$norGiantSizeAs,type='l',lwd=2)
lines(percolationGamma$edgeNodeRatioDe,percolationGamma$norGiantSizeDe,type='l',lwd=2,col="red")
abline(v=1,h=1)
plot(mixPercolationGamma$edgeNodeRatioAs,mixPercolationGamma$norGiantSizeAs,type='l',lwd=2)
lines(mixPercolationGamma$edgeNodeRatioDe,mixPercolationGamma$norGiantSizeDe,lwd=2,col="red")
abline(v=1,h=1)


unionMat=matrix(0,2000,2000)
unionMat[upper.tri(unionMat)]=runif(1999*2000/2)
unionMat=unionMat+t(unionMat)
rownames(unionMat)=c(1:2000)
colnames(unionMat)=c(1:2000)
percolationUnion=rankBasedThresholdNetworkStatiscs(unionMat,d=2)
mixPercolationUnion=rankBasedThresholdNetworkStatiscs(unionMat,mix=TRUE,d=2)
par(mfcol=c(1,2),mar=c(2,2,2,2))
plot(percolationUnion$edgeNodeRatioAs,percolationUnion$norGiantSizeAs,type='l',lwd=2)
lines(percolationUnion$edgeNodeRatioDe,percolationUnion$norGiantSizeDe,lwd=2,col="red")
abline(v=1,h=1)
plot(mixPercolationUnion$edgeNodeRatioAs,mixPercolationUnion$norGiantSizeAs,lwd=2,type='l')
lines(mixPercolationUnion$edgeNodeRatioDe,mixPercolationUnion$norGiantSizeDe,lwd=2,col="red")
abline(v=1,h=1)
  
  