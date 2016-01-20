### WTF
fbmH0.75=diff(circFBM(n = 100000,H = 0.75))

temp0.75=hurstExponent(x= fbmH0.75,nVec=nVec,sampleNum=NULL,thread = 3,qVec=seq(-5,5,0.1),detrendOrder=4,sampleMethod=2)
tempFDFA0.3<- F_DFA(x= fbmH0.3,nVec=nVec,sampleNum=NULL,thread = 3,qVec=seq(-5,5,0.1),detrendOrder=4,sampleMethod=2)

plot(nVec,tempFDFA0.3[,2],type='l',log="xy")
for(i in 3:102){
  lines(nVec,tempFDFA0.3[,i])
}

legendreTransform(h_q = temp0.75,qVec = seq(-5,5,0.1),D_f = 1)%>>%plot(ylim=c(0,1.1),xlim=c(0,2))


tempF2DFA0.75= F2_DFA(x= fbmH0.75,nVec=nVec,sampleNum=NULL,thread = 3,detrendOrder=4,sampleMethod=2)
tempF2DFA0.3=F2_DFA(x= fbmH0.3,nVec=nVec,sampleNum=NULL,thread = 3,detrendOrder=4,sampleMethod=2)

tempF2DFABrown=F2_DFA(x= rnorm(100000),nVec=nVec,sampleNum=NULL,thread = 3,detrendOrder=4,sampleMethod=2)

f2_DFA<- tempF2DFABrown



temperature=seq(1.17,3.62,0.05)
HurstExponentMatrixNonoverlap=matrix(data = 0,nrow = 401,ncol = 50,dimnames = list(sprintf("q%.1f",seq(-20,20,0.1)),sprintf("T%.2f",temperature)))

HurstExponentMatrixNonoverlapSD=matrix(data = 0,nrow = 401,ncol = 50,dimnames = list(sprintf("q%.1f",seq(-20,20,0.1)),sprintf("T%.2f",temperature)))

for(temper in temperature){
  temp=read.csv(sprintf("~/Work/Project/IsingDFA/rData/GHE/detrend3Nonoverlap/GHE3th_t%.2f.dat",temper),header = TRUE)
  tempDFA=sapply(temp, mean)
  tempSD=sapply(temp, sd)
  HurstExponentMatrixNonoverlap[,sprintf("T%.2f",temper)]=tempDFA
  HurstExponentMatrixNonoverlapSD[,sprintf("T%.2f",temper)]=tempSD
  print(temper)
}

