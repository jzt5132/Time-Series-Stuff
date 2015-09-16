

###########Asymmetry##################################################
Asym<-matrix(NA,ncol=8,nrow=8)
for (i in 1:7){
  for (j in (i+1):8){
    aem <- ecmAsyFit(y=a[,j], x=a[,i],lag=1, model="linear", split=FALSE)
    tes <- ecmAsyTest(aem)$out[1,6]
    Asym[i,j]<-tes
  }
}

for (j in 1:7){
  for (i in (j+1):8){
    aem <- ecmAsyFit(y=a[,j], x=a[,i],lag=1, model="linear", split=FALSE)
    tes <- ecmAsyTest(aem)$out[1,6]
    Asym[i,j]<-tes
  }
}


Asym1<-matrix(NA,ncol=8,nrow=8)
for (i in 1:7){
  for (j in (i+1):8){
    aem <- ecmAsyFit(y=a[,j], x=a[,i],lag=1, model="mtar", split=TRUE)
    tes <- ecmAsyTest(aem)$out[1,6]
    Asym1[i,j]<-tes
  }
}

for (j in 1:7){
  for (i in (j+1):8){
    aem <- ecmAsyFit(y=a[,j], x=a[,i],lag=1, model="mtar", split=TRUE)
    tes <- ecmAsyTest(aem)$out[1,6]
    Asym1[i,j]<-tes
  }
}



###Print details######
for (i in 1:7){
  for (j in (i+1):8){
    if (Asym[i,j]<=0.05){
      aem <- ecmAsyFit(y=a[,j], x=a[,i],lag=1, model="linear", split=FALSE)
      print(aem);
    }
  }
}

for (j in 1:7){
  for (i in (j+1):8){
    if (Asym[i,j]<=0.05){
      aem <- ecmAsyFit(y=a[,j], x=a[,i],lag=1, model="linear", split=FALSE)
      print(aem);
    }
  }
}
