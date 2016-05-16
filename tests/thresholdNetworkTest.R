
MAT = as.matrix(
  read.csv(
    "/home/oliver/Work/R/WorldStocksData/sp500subPrime/cor/cor2006-02-01.dat",
    row.names = 1,
    header = TRUE
  )
)
MAT = MAT + t(MAT)



percolation=rankBasedThresholdNetwork(MAT)
mixPercolation=rankBasedThresholdNetwork(MAT,mix=TRUE)

par(mfcol=c(1,2))
plot(mixPercolation$edgeNodeRatioAs,mixPercolation$norGiantSizeAs,type='l',lwd=2,xlab="t",ylab=expression(S[max]/n))
lines(mixPercolation$edgeNodeRatioDe,mixPercolation$norGiantSizeDe,type='l',lwd=2,xlab="t",col="red",ylab=expression(S[max]/n))
abline(v=1,col="blue")

plot(percolation$edgeNodeRatioAs,percolation$norGiantSizeAs,type='l',lwd=2,xlab="t",ylab=expression(S[max]/n))
lines(percolation$edgeNodeRatioDe,percolation$norGiantSizeDe,type='l',lwd=2,xlab="t",col="red",ylab=expression(S[max]/n))
abline(v=1,col="blue")

par(mfcol=c(2,2),mar=c(2,2,2,2))
plot(mixPercolation$edgeNodeRatioAs,mixPercolation$meanClusterSizeAs,type='l',lwd=2,xlab="t",ylab=expression(S[mean]))
plot(mixPercolation$edgeNodeRatioDe,mixPercolation$meanClusterSizeDe,type='l',lwd=2,xlab="t",col="red",ylab=expression(S[max]/n))
abline(v=1,col="blue")

plot(percolation$edgeNodeRatioAs,percolation$meanClusterSizeAs,type='l',lwd=2,xlab="t",ylab=expression(S[mean]))
plot(percolation$edgeNodeRatioDe,percolation$meanClusterSizeDe,type='l',lwd=2,xlab="t",col="red",ylab=expression(S[mean]))
abline(v=1,col="blue")

