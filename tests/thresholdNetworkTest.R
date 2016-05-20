## 0809######
MAT0809 = as.matrix(
  read.csv(
    "/home/oliver/Work/R/WorldStocksData/sp500HeteroTimeWindow/sp500MonthlyCor/2008-09-01.dat",
    row.names = 1,
    header = TRUE
  )
)
MAT0809 = MAT0809 + t(MAT0809)

percolation0809=rankBasedThresholdNetworkStatiscs(MAT0809,d = 3)
mixPercolation0809=rankBasedThresholdNetworkStatiscs(MAT0809,mix=TRUE,d = 3)

GRAPH0809= rankBasedThresholdNetwork(MAT0809)
par(mar=c(2,2,2,2))
plot(GRAPH[[1]][[442]],vertex.label=NA,layout=layout.fruchterman.reingold,vertex.size=3)
power.law.fit(degree(GRAPH[[1]][[442]])+1,xmin=3)
## s_max
cairo_ps("~/test/percolationComponent0809.eps")
par(mfcol=c(1,2))
plot(percolation0809$edgeNodeRatioAs,percolation0809$norGiantSizeAs,type='l',lwd=2,xlab="t",ylab=expression(S[max]/n))
lines(percolation0809$edgeNodeRatioDe,percolation0809$norGiantSizeDe,type='l',lwd=2,xlab="t",col="red",ylab=expression(S[max]/n))
plot(mixPercolation0809$edgeNodeRatioAs,mixPercolation0809$norGiantSizeAs,type='l',lwd=2,xlab="t",ylab=expression(S[max]/n))
lines(mixPercolation0809$edgeNodeRatioDe,mixPercolation0809$norGiantSizeDe,type='l',lwd=2,xlab="t",col="red",ylab=expression(S[max]/n))
dev.off()

## <s^2>/<s>
cairo_ps("~/test/PercolationMeanComponent0809.eps")
plot(percolation0809$edgeNodeRatioDe,percolation0809$meanClusterSizeDe/max(na.omit(percolation0809$meanClusterSizeDe)),type='l',lwd=2,xlab="t",ylab=expression(S[mean]))
lines(percolation0809$edgeNodeRatioAs,percolation0809$meanClusterSizeAs/max(na.omit(percolation0809$meanClusterSizeAs)),type='l',lwd=2,xlab="t",ylab=expression(S[mean]),col="red")
dev.off()

## 0505###
MAT0505 = as.matrix(
  read.csv(
    "/home/oliver/Work/R/WorldStocksData/sp500HeteroTimeWindow/sp500MonthlyCor/2005-05-01.dat",
    row.names = 1,
    header = TRUE
  )
)
MAT0505 = MAT0505 + t(MAT0505)

percolation0505=rankBasedThresholdNetworkStatiscs(MAT0505,d = 3)
mixPercolation0505=rankBasedThresholdNetworkStatiscs(MAT0505,mix=TRUE,d = 3)

GRAPH0505= rankBasedThresholdNetwork(MAT0505)
par(mar=c(2,2,2,2))
plot(GRAPH[[1]][[442]],vertex.label=NA,layout=layout.fruchterman.reingold,vertex.size=3)
power.law.fit(degree(GRAPH[[1]][[442]])+1,xmin=3)


## s_max
cairo_ps("~/test/percolationComponent0505.eps")
par(mfcol=c(1,2))
plot(percolation0505$edgeNodeRatioAs,percolation0505$norGiantSizeAs,type='l',lwd=2,xlab="t",ylab=expression(S[max]/n))
lines(percolation0505$edgeNodeRatioDe,percolation0505$norGiantSizeDe,type='l',lwd=2,xlab="t",col="red",ylab=expression(S[max]/n))
plot(mixPercolation0505$edgeNodeRatioAs,mixPercolation0505$norGiantSizeAs,type='l',lwd=2,xlab="t",ylab=expression(S[max]/n))
lines(mixPercolation0505$edgeNodeRatioDe,mixPercolation0505$norGiantSizeDe,type='l',lwd=2,xlab="t",col="red",ylab=expression(S[max]/n))
dev.off()

## <s^2>/<s>
cairo_ps("~/test/PercolationMeanComponent0505.eps")
plot(percolation0505$edgeNodeRatioDe,percolation0505$meanClusterSizeDe/max(na.omit(percolation0505$meanClusterSizeDe)),type='l',lwd=2,xlab="t",ylab=expression(S[mean]))
lines(percolation0505$edgeNodeRatioAs,percolation0505$meanClusterSizeAs/max(na.omit(percolation0505$meanClusterSizeAs)),type='l',lwd=2,xlab="t",ylab=expression(S[mean]),col="red")
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



## normal distribution########
for(i in c(500,1000,2000,3000,4000)){
  normalMat=matrix(0,i,i)
  normalMat[upper.tri(normalMat)]=rnorm((i-1)*i/2)
  normalMat=normalMat+t(normalMat)
  rownames(normalMat)= c(1:i)
  colnames(normalMat)=c(1:i)
  assign(paste0("percolationNormal",i),rankBasedThresholdNetworkStatiscs(normalMat,d=2))
  assign(paste0("mixPercolationNormal",i),rankBasedThresholdNetworkStatiscs(normalMat,mix = TRUE,d=2))
  print(i)
}

cairo_ps("~/test/normal500-4000.eps")
par(mfcol=c(1,2),mar=c(4,4,2,2))
plot(percolationNormal500$edgeNodeRatioDe,percolationNormal500$norGiantSizeDe,type='l',lwd=2,main="De",xlab = "t",ylab=expression(S[max]/n))
lines(percolationNormal1000$edgeNodeRatioDe,percolationNormal1000$norGiantSizeDe,type='l',lwd=2,col="red")
lines(percolationNormal2000$edgeNodeRatioDe,percolationNormal2000$norGiantSizeDe,type='l',lwd=2,col="green")
lines(percolationNormal3000$edgeNodeRatioDe,percolationNormal3000$norGiantSizeDe,type='l',lwd=2,col="blue")
lines(percolationNormal4000$edgeNodeRatioDe,percolationNormal4000$norGiantSizeDe,type='l',lwd=2,col="purple")

plot(percolationNormal500$edgeNodeRatioAs,percolationNormal500$norGiantSizeAs,type='l',lwd=2,main = "As",xlab = "t",ylab=expression(S[max]/n))
lines(percolationNormal1000$edgeNodeRatioAs,percolationNormal1000$norGiantSizeAs,type='l',lwd=2,col="red")
lines(percolationNormal2000$edgeNodeRatioAs,percolationNormal2000$norGiantSizeAs,type='l',lwd=2,col="green")
lines(percolationNormal3000$edgeNodeRatioAs,percolationNormal3000$norGiantSizeAs,type='l',lwd=2,col="blue")
lines(percolationNormal4000$edgeNodeRatioAs,percolationNormal4000$norGiantSizeAs,type='l',lwd=2,col="purple")
dev.off()

cairo_ps("~/meanClusterSizeNormal.eps")
plot(percolationNormal500$edgeNodeRatioAs,percolationNormal500$meanClusterSizeAs/max(na.omit(percolationNormal500$meanClusterSizeAs)),type='l',xlim = c(0,2),xlab = "t",ylab="mean size")
lines(percolationNormal1000$edgeNodeRatioAs,percolationNormal1000$meanClusterSizeAs/max(na.omit(percolationNormal1000$meanClusterSizeAs)),type='l',xlim = c(0,2),xlab = "t",ylab="mean size",col="red")
lines(percolationNormal2000$edgeNodeRatioAs,percolationNormal2000$meanClusterSizeAs/max(na.omit(percolationNormal2000$meanClusterSizeAs)),type='l',xlim = c(0,2),xlab = "t",ylab="mean size",col="green")
lines(percolationNormal3000$edgeNodeRatioAs,percolationNormal3000$meanClusterSizeAs/max(na.omit(percolationNormal3000$meanClusterSizeAs)),type='l',xlim = c(0,2),xlab = "t",ylab="mean size",col="blue")
lines(percolationNormal4000$edgeNodeRatioAs,percolationNormal4000$meanClusterSizeAs/max(na.omit(percolationNormal4000$meanClusterSizeAs)),type='l',xlim = c(0,2),xlab = "t",ylab="mean size",col="purple")
dev.off()




## gamma distribution#######
for(i in c(500,1000,2000,3000,4000)){
  gammaMat=matrix(0,i,i)
  gammaMat[upper.tri(gammaMat)]=rgamma((i-1)*i/2,1)
  gammaMat=gammaMat+t(gammaMat)
  rownames(gammaMat)= c(1:i)
  colnames(gammaMat)=c(1:i)
  assign(paste0("percolationGamma",i),rankBasedThresholdNetworkStatiscs(gammaMat,d=2))
  assign(paste0("mixPercolationGamma",i),rankBasedThresholdNetworkStatiscs(gammaMat,mix = TRUE,d=2))
  print(i)
}

cairo_ps("~/test/gamma500-4000.eps")
par(mfcol=c(1,2),mar=c(4,4,2,2))
plot(percolationGamma500$edgeNodeRatioDe,percolationGamma500$norGiantSizeDe,type='l',lwd=2,main="De",xlab = "t",ylab=expression(S[max]/n))
lines(percolationGamma1000$edgeNodeRatioDe,percolationGamma1000$norGiantSizeDe,type='l',lwd=2,col="red")
lines(percolationGamma2000$edgeNodeRatioDe,percolationGamma2000$norGiantSizeDe,type='l',lwd=2,col="green")
lines(percolationGamma3000$edgeNodeRatioDe,percolationGamma3000$norGiantSizeDe,type='l',lwd=2,col="blue")
lines(percolationGamma4000$edgeNodeRatioDe,percolationGamma4000$norGiantSizeDe,type='l',lwd=2,col="purple")

plot(percolationGamma500$edgeNodeRatioAs,percolationGamma500$norGiantSizeAs,type='l',lwd=2,main = "As",xlab = "t",ylab=expression(S[max]/n))
lines(percolationGamma1000$edgeNodeRatioAs,percolationGamma1000$norGiantSizeAs,type='l',lwd=2,col="red")
lines(percolationGamma2000$edgeNodeRatioAs,percolationGamma2000$norGiantSizeAs,type='l',lwd=2,col="green")
lines(percolationGamma3000$edgeNodeRatioAs,percolationGamma3000$norGiantSizeAs,type='l',lwd=2,col="blue")
lines(percolationGamma4000$edgeNodeRatioAs,percolationGamma4000$norGiantSizeAs,type='l',lwd=2,col="purple")
dev.off()


cairo_ps("~/test/meanClusterSizeGamma.eps")
plot(percolationGamma500$edgeNodeRatioAs,percolationGamma500$meanClusterSizeAs/max(na.omit(percolationGamma500$meanClusterSizeAs)),type='l',xlim = c(0,2),xlab = "t",ylab="mean size")
lines(percolationGamma1000$edgeNodeRatioAs,percolationGamma1000$meanClusterSizeAs/max(na.omit(percolationGamma1000$meanClusterSizeAs)),type='l',xlim = c(0,2),xlab = "t",ylab="mean size",col="red")
lines(percolationGamma2000$edgeNodeRatioAs,percolationGamma2000$meanClusterSizeAs/max(na.omit(percolationGamma2000$meanClusterSizeAs)),type='l',xlim = c(0,2),xlab = "t",ylab="mean size",col="green")
lines(percolationGamma3000$edgeNodeRatioAs,percolationGamma3000$meanClusterSizeAs/max(na.omit(percolationGamma3000$meanClusterSizeAs)),type='l',xlim = c(0,2),xlab = "t",ylab="mean size",col="blue")
lines(percolationGamma4000$edgeNodeRatioAs,percolationGamma4000$meanClusterSizeAs/max(na.omit(percolationGamma4000$meanClusterSizeAs)),type='l',xlim = c(0,2),xlab = "t",ylab="mean size",col="purple")
dev.off()

## union distribution#####
for(i in c(500,1000,2000,3000,4000)){
  unifMat=matrix(0,i,i)
  unifMat[upper.tri(unifMat)]=runif((i-1)*i/2)
  unifMat=unifMat+t(unifMat)
  rownames(unifMat)= c(1:i)
  colnames(unifMat)=c(1:i)
  assign(paste0("percolationUnif",i),rankBasedThresholdNetworkStatiscs(unifMat,d=2))
  assign(paste0("mixPercolationUnif",i),rankBasedThresholdNetworkStatiscs(unifMat,mix = TRUE,d=2))
  print(i)
}

cairo_ps("~/test/unif500-4000.eps")
par(mfcol=c(1,2),mar=c(4,4,2,2))
plot(percolationUnif500$edgeNodeRatioDe,percolationUnif500$norGiantSizeDe,type='l',lwd=2,main="De",xlab = "t",ylab=expression(S[max]/n))
lines(percolationUnif1000$edgeNodeRatioDe,percolationUnif1000$norGiantSizeDe,type='l',lwd=2,col="red")
lines(percolationUnif2000$edgeNodeRatioDe,percolationUnif2000$norGiantSizeDe,type='l',lwd=2,col="green")
lines(percolationUnif3000$edgeNodeRatioDe,percolationUnif3000$norGiantSizeDe,type='l',lwd=2,col="blue")
lines(percolationUnif4000$edgeNodeRatioDe,percolationUnif4000$norGiantSizeDe,type='l',lwd=2,col="purple")

plot(percolationUnif500$edgeNodeRatioAs,percolationUnif500$norGiantSizeAs,type='l',lwd=2,main = "As",xlab = "t",ylab=expression(S[max]/n))
lines(percolationUnif1000$edgeNodeRatioAs,percolationUnif1000$norGiantSizeAs,type='l',lwd=2,col="red")
lines(percolationUnif2000$edgeNodeRatioAs,percolationUnif2000$norGiantSizeAs,type='l',lwd=2,col="green")
lines(percolationUnif3000$edgeNodeRatioAs,percolationUnif3000$norGiantSizeAs,type='l',lwd=2,col="blue")
lines(percolationUnif4000$edgeNodeRatioAs,percolationUnif4000$norGiantSizeAs,type='l',lwd=2,col="purple")
dev.off()

cairo_ps("~/meanClusterSizeUnif.eps")
plot(percolationUnif500$edgeNodeRatioAs,percolationUnif500$meanClusterSizeAs/max(na.omit(percolationUnif500$meanClusterSizeAs)),type='l',xlim = c(0,2),xlab = "t",ylab="mean size")
lines(percolationUnif1000$edgeNodeRatioAs,percolationUnif1000$meanClusterSizeAs/max(na.omit(percolationUnif1000$meanClusterSizeAs)),type='l',xlim = c(0,2),xlab = "t",ylab="mean size",col="red")
lines(percolationUnif2000$edgeNodeRatioAs,percolationUnif2000$meanClusterSizeAs/max(na.omit(percolationUnif2000$meanClusterSizeAs)),type='l',xlim = c(0,2),xlab = "t",ylab="mean size",col="green")
lines(percolationUnif3000$edgeNodeRatioAs,percolationUnif3000$meanClusterSizeAs/max(na.omit(percolationUnif3000$meanClusterSizeAs)),type='l',xlim = c(0,2),xlab = "t",ylab="mean size",col="blue")
lines(percolationUnif4000$edgeNodeRatioAs,percolationUnif4000$meanClusterSizeAs/max(na.omit(percolationUnif4000$meanClusterSizeAs)),type='l',xlim = c(0,2),xlab = "t",ylab="mean size",col="purple")
dev.off()
##### power law distribution#######
for(i in c(500,1000,2000,3000,4000)){
  powerMat=matrix(0,i,i)
  powerMat[upper.tri(powerMat)]=rplcon((i-1)*i/2,xmin = 1,2)
  powerMat=powerMat+t(powerMat)
  rownames(powerMat)= c(1:i)
  colnames(powerMat)=c(1:i)
  assign(paste0("percolationPower",i),rankBasedThresholdNetworkStatiscs(powerMat,d=2))
  assign(paste0("mixPercolationPower",i),rankBasedThresholdNetworkStatiscs(powerMat,mix = TRUE,d=2))
  print(i)
}

cairo_ps("~/test/power500-4000.eps")
par(mfcol=c(1,2),mar=c(4,4,2,2))
plot(percolationPower500$edgeNodeRatioDe,percolationPower500$norGiantSizeDe,type='l',lwd=2,main="De",xlab = "t",ylab=expression(S[max]/n))
lines(percolationPower1000$edgeNodeRatioDe,percolationPower1000$norGiantSizeDe,type='l',lwd=2,col="red")
lines(percolationPower2000$edgeNodeRatioDe,percolationPower2000$norGiantSizeDe,type='l',lwd=2,col="green")
lines(percolationPower3000$edgeNodeRatioDe,percolationPower3000$norGiantSizeDe,type='l',lwd=2,col="blue")
lines(percolationPower4000$edgeNodeRatioDe,percolationPower4000$norGiantSizeDe,type='l',lwd=2,col="purple")

plot(percolationPower500$edgeNodeRatioAs,percolationPower500$norGiantSizeAs,type='l',lwd=2,main = "As",xlab = "t",ylab=expression(S[max]/n))
lines(percolationPower1000$edgeNodeRatioAs,percolationPower1000$norGiantSizeAs,type='l',lwd=2,col="red")
lines(percolationPower2000$edgeNodeRatioAs,percolationPower2000$norGiantSizeAs,type='l',lwd=2,col="green")
lines(percolationPower3000$edgeNodeRatioAs,percolationPower3000$norGiantSizeAs,type='l',lwd=2,col="blue")
lines(percolationPower4000$edgeNodeRatioAs,percolationPower4000$norGiantSizeAs,type='l',lwd=2,col="purple")
dev.off()


cairo_ps("~/meanClusterSizePower.eps")
plot(percolationPower500$edgeNodeRatioAs,percolationPower500$meanClusterSizeAs/max(na.omit(percolationPower500$meanClusterSizeAs)),type='l',xlim = c(0,2),xlab = "t",ylab="mean size")
lines(percolationPower1000$edgeNodeRatioAs,percolationPower1000$meanClusterSizeAs/max(na.omit(percolationPower1000$meanClusterSizeAs)),type='l',xlim = c(0,2),xlab = "t",ylab="mean size",col="red")
lines(percolationPower2000$edgeNodeRatioAs,percolationPower2000$meanClusterSizeAs/max(na.omit(percolationPower2000$meanClusterSizeAs)),type='l',xlim = c(0,2),xlab = "t",ylab="mean size",col="green")
lines(percolationPower3000$edgeNodeRatioAs,percolationPower3000$meanClusterSizeAs/max(na.omit(percolationPower3000$meanClusterSizeAs)),type='l',xlim = c(0,2),xlab = "t",ylab="mean size",col="blue")
lines(percolationPower4000$edgeNodeRatioAs,percolationPower4000$meanClusterSizeAs/max(na.omit(percolationPower4000$meanClusterSizeAs)),type='l',xlim = c(0,2),xlab = "t",ylab="mean size",col="purple")
dev.off()
####
MAT=matrix(0,30,30)
MAT[upper.tri(MAT)]=rnorm(29*30/2,mean=0,sd=1)
MAT=MAT+t(MAT)
rownames(MAT)<- c(1:30)
colnames(MAT)<- c(1:30)
