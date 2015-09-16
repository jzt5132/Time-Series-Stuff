
#######################IF/Else######################################

bp.matrix=matrix(data=NA, rows=5, cols=25)

pp.matrix=matrix(data=NA, rows=5, columns=5)

pp2.matrix=matrix(data=NA, r=5, col=10)

pp3.matrix=matrix(data=NA, rows=5, columns=20)

for i in (1:5) { for j in (1:5){

lr.reg=lm(a.fe[,j]~a.fe[,i])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(i,j)],diff(a.fe[,c(i,j)]),error.lagged)
a.fe2=window(a.fe2,start=c(1990,2),end=c(2011,8))
colnames(a.fe2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a.fe2)


if sctest(dy2~error.lagged+dy1, type="RE",data=a.fe2)$p.value<0.05
 bp=removeNA(summary(breakpoints(dy2~error.lagged+dy1,data=a.fe2))$breakpoint[1,])[1]
 bp.matrix[i,(5j-4)]=bp
  if sctest(dy2[1:(bp-1)]~error.lagged[1:(bp-1)]+dy1[1:(bp-1)], type="RE",data=a.fe2)$p.value<0.05
   bp2=removeNA(summary(breakpoints(dy2[1:(bp-1)]~error.lagged[1:(bp-1)]+dy1[1:(bp-1)],data=a.fe2))$breakpoint[1,])[1]
   bp.matrix[i,(5j-3)]=bp2
   pp3.matrix[i,(4j-3)]=sctest(dy2[1:(bp2-1)]~error.lagged[1:(bp2-1)]+dy1[1:(bp2-1)], type="RE",data=a.fe2)$p.value
   pp3.matrix[i,(4j-2)]=sctest(dy2[bp2:bp]~error.lagged[bp2:bp]+dy1[bp2:bp], type="RE",data=a.fe2)$p.value
  else
   pp2.matrix[i,(2j-1)]=sctest(dy2[1:(bp-1)]~error.lagged[1:(bp-1)]+dy1[1:(bp-1)], type="RE",data=a.fe2)$p.value
	
 if sctest(dy2[bp:259]~error.lagged[bp:259]+dy1[bp;259], type="RE",data=a.fe2)$p.value<0.05
   bp3=removeNA(summary(breakpoints(dy2[bp:259]~error.lagged[bp:259]+dy1[bp;259],data=a.fe2))$breakpoint[1,])[1]
   bp.matrix[i,(5j-3)]=bp2
   pp3.matrix[i,(4j-3)]=sctest(dy2[1:(bp2-1)]~error.lagged[1:(bp2-1)]+dy1[1:(bp2-1)], type="RE",data=a.fe2)$p.value
   pp3.matrix[i,(4j-2)]=sctest(dy2[bp2:bp]~error.lagged[bp2:bp]+dy1[bp2:bp], type="RE",data=a.fe2)$p.value
  else
   pp2.matrix[i,(2j-1)]=sctest(dy2[1:(bp-1)]~error.lagged[1:(bp-1)]+dy1[1:(bp-1)], type="RE",data=a.fe2)$p.value

