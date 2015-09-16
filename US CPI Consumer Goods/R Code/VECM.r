###VECM####
VARselect(a, lag.max = 5, type = "both")
vecm=ca.jo(a, type = "trace", ecdet = "trend", K =2 ,spec = "longrun",season=12)
vecm.r1=cajorls(vecm, r = 1)
var.1=vec2var(vecm,r=1)
