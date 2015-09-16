DS<-matrix(data=c(a<-c(1,3,2,6,9,2,2,3,4,1),b<-c(2,3,3,4,5,6,2,1,1,6),f<-c(0,2,4,4,7,6,7,1,2,2),d<-c(0,0,0,1,0,2,1,3,1,2),e<-c(9,2,3,1,1,1,0,2,5,6)),
           nrow=10,ncol=5)

group<-c("case", "case", "case", "case", "case", "control", "control",  
"control", "control", "control")
group<-factor(group)
est<-matrix(data=NA,nrow=2,ncol=5,  
dimnames=list(estimate=c("intercept","factor"),dataset=c("a","b","f","d","e")))
SE<-matrix(data=NA,nrow=2,ncol=5,dimnames=list(STD=c("intercept","factor"),dataset=c("a","b","f","d","e")))
t.stat<-matrix(data=NA,nrow=2,ncol=5,dimnames=list(T.stat=c("intercept","factor"),dataset=c("a","b","f","d","e")))
p.value<-matrix(data=NA,nrow=2,ncol=5,dimnames=list(p.vale=c("intercept","factor"),dataset=c("a","b","f","d","e")))
for(i in 1:5)
             {
      results<-lm(DS[,i]~group)
      est[,i]<-summary(results)$coefficient[,1]
      SE[,i]<-summary(results)$coefficient[,2]
      t.stat[,i]<-summary(results)$coefficient[,3]
      p.value[,i]<-summary(results)$coefficient[,4]

      }
  Data.a<-matrix(data=c(est[,1],SE[,1],t.stat[,1],p.value[,1]),nrow=2,ncol=4)
  Data.b<-matrix(data=c(est[,2],SE[,2],t.stat[,2],p.value[,2]),nrow=2,ncol=4)
  Data.f<-matrix(data=c(est[,3],SE[,3],t.stat[,3],p.value[,3]),nrow=2,ncol=4)
  Data.d<-matrix(data=c(est[,4],SE[,4],t.stat[,4],p.value[,4]),nrow=2,ncol=4)
  Data.e<-matrix(data=c(est[,5],SE[,5],t.stat[,5],p.value[,5]),nrow=2,ncol=4)
  summary.table<-rbind(Data.a,Data.b,Data.f,Data.d,Data.e)
   
Summary.Table<-matrix(data=summary.table,nrow=10,ncol=4,dimnames=list(DATA=c("intercept.a","factor.a","intercept.b","factor.b",
                                      
"intercept.f","factor.f","intercept.d","factor.d",
                                      
"intercept.e","factor.e"),TEST=c("Estimate","S.E","t.stat","p.value")))
  Summary.Table


