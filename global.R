library(rhandsontable)
library(shiny)
####  UPLOAD RPE.csv  ####  
a<-as.matrix(read.csv("RPE.csv",fileEncoding='UTF-8'))
a[is.na(a)]<-0
a<-a[,-1]
RPE<- matrix(as.numeric(a), dim(a), dimnames = dimnames(a))
RPE<-t(RPE)

####  UPLOAD PPE.csv  ####  
a2<-as.matrix(read.csv("PPE.csv",na.strings = 0))
a2<-a2[,-1]
a2[is.na(a2)]<-0
PPE<- matrix(as.numeric(a2), dim(a2), dimnames = dimnames(a2))
PPE = -1*PPE
PPE<-t(PPE)

####  UPLOAD output.csv  ####  
DF<-read.csv("output.csv")
n = dim(DF)[1]
DF$PropoRegPrice=DF$CurrentRegPrice


####  UPLOAD Ppack.csv  ####  
Ppack<-read.csv("Ppack.csv")

####   Function  ####  
fun<-function(Depth,Freq,PropoRegPrice){
  n=dim(DF)[1]
  m<-matrix(NA,ncol=2,nrow=44)
  for(i in 1:n){
    # m[i,]<- fun(i,Freq,Depth,
    #              DF$CurrentRegPrice,PropoRegPrice,
    #              DF$Packsize,DF$Baseline,PPE,RPE,
    #              DF$BONUSPACKFre,DF$GIFTPACKFre,DF$MultiPackFre,Ppack)
    CurrentRegPrice<-DF$CurrentRegPrice
    Packsize<-DF$Packsize
    CurrentBaseline<-DF$Baseline
    Bonus<- DF$BONUSPACKFre
    Gift<-DF$GIFTPACKFre
    Multi<-DF$MultiPackFre
    BonusUplift<-exp(Ppack$BONUSPACK[i])-1
    CausalBonusEff<-1+BonusUplift*Bonus[i]
    GiftUplift<-exp(Ppack$GIFTPACK[i])-1
    CausalGiftEff<-1+GiftUplift*Gift[i] 
    MultiUplift<-exp(Ppack$MultiPack[i])-1
    CausalMultiEff<-1+MultiUplift*Multi[i] 
    CausalEff<-CausalBonusEff*CausalGiftEff*CausalMultiEff
    weightedDepth<-1-(Freq*(1-Depth)^diag(PPE)+(1-Freq))^(1/diag(PPE))
    TransferredPPE<-(1-weightedDepth)^PPE[i,]
    TotalPPE<-prod(TransferredPPE)
    TransferredRPE<-((PropoRegPrice[i]/PropoRegPrice)/(CurrentRegPrice[i]/CurrentRegPrice))^RPE[i,]
    TransferredRPESelf <- (PropoRegPrice[i]/CurrentRegPrice[i])^RPE[i,i] #revised by jasmine
    TotalRPE<-prod(TransferredRPE)*TransferredRPESelf #revised by jasmine
    EstimateBaseline <- CurrentBaseline[i]*TotalRPE
    EstimateSalesVolume<- EstimateBaseline*TotalPPE*CausalEff
    WeightedPrice<-(EstimateBaseline*PropoRegPrice[i]*(1-Depth[i])*Freq[i]*(1-Depth[i])^PPE[i,i]+
                      EstimateBaseline*PropoRegPrice[i]*(1-Freq[i]))/
      (EstimateBaseline*Freq[i]*(1-Depth[i])^PPE[i,i]+EstimateBaseline*(1-Freq[i]))
    EstimateSalesValue<-EstimateSalesVolume*WeightedPrice/Packsize[i]
    m[i,]<-c(EstimateSalesVolume,EstimateSalesValue)
  }
  x2<-data.frame(m)
  names(x2)=c("SalesVolume","SalesValue")
  return(x2)
}

