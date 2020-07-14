LD_p <- read.table("Train.sample.prob",head=T)
Pool_p <- read.table("Pooled.sample.prob",head=T)
Pool_p <- Pool_p[1:94,]
roc_T <- roc(RESPONSE~ Death.Vali.RF,data=LD_p)
roc_M <- roc(RESPONSE~ Death.Vali.RF,data=Pool_p)
roc.test(roc_T,roc_M)
