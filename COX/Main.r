library(pec)
library(prodlim)
library(rms)
library(reshape2)
#TIMING
timePoint <- 1:30
titlePoint <- paste0("T",timePoint)
#####
##### Build COX models
d <- read.table("Train.input.Cox.profile",head=T)
d$status <- 0
d[which(d$cohort=="Death"),]$status = 1
DeathP <- c()
i <- 1
while (i <= dim(d)[1]){
coxmodel <- cph(Surv(SurvTime,status)~Age+admissionDateTOinitialSymptomDate+breath+Cario_Disease+T2D+CAD+SP+chestOppression+pluseOxygen+RE_Disease+COPD+HTD+TEM+sputum+IfCluster+Dyspnea+latentperiod+WBC+AST+LDH+urea+NEU_PER+albumin+Cre+chest+FirstAddmissionHA_D_Dimer+NEU+CK+PLT+DBil+LYMPH_PER+EO_PER+CRP+FirstAddmissionHA_PT,data=d,surv=TRUE)
P <- predictSurvProb(coxmodel,newdata=d[i,],times=timePoint)
P <- 1-P
rownames(P) = d$Sample[i]
colnames(P) = titlePoint
P <- data.frame(P,cohort=d$cohort[i])
DeathP <- rbind(DeathP,P)
i = i + 1
}
####### compare rf
RF_P <- read.table("RF.prob.txt",head=T)
RF_P <- RF_P[which(RF_P$cohort=="Live" | RF_P$cohort=="Death"),]
RF_P$cohort <- as.character(RF_P$cohort)
RF_roc <- roc(cohort~prob,RF_P,ci=TRUE)
cox_roc <- roc(cohort~T20,DeathP,ci=TRUE)
roc.test(RF_roc,cox_roc)
####### find  death P cutoff of cox model
#######	
coxmodelWhole <- cph(Surv(SurvTime,status)~Age+admissionDateTOinitialSymptomDate+breath+Cario_Disease+T2D+CAD+SP+chestOppression+pluseOxygen+RE_Disease+COPD+HTD+TEM+sputum+IfCluster+Dyspnea+latentperiod+WBC+AST+LDH+urea+NEU_PER+albumin+Cre+chest+FirstAddmissionHA_D_Dimer+NEU+CK+PLT+DBil+LYMPH_PER+EO_PER+CRP+FirstAddmissionHA_PT,data=d,surv=TRUE)
# Henan and wuhan Independant samples
IND <- read.table("Independant.input.profile",head=T)
P_IND <- predictSurvProb(coxmodelWhole,newdata=IND,times=timePoint)
P_IND <- 1- P
colnames(P_IND) <- titlePoint
rownames(P_IND) <- IND$Sample
P_IND <- data.frame(P_IND,cohort=IND$cohort)
# visual
#subA
AUC = round(RF_roc$ci,digits=3)
AUCB = round(cox_roc$ci,digits=3)
plot(RF_roc,col="#86CEEB")
plot(cox_roc,col="#B03060",add=TRUE)
mtext(paste0("Mixed: ",AUC[2],"( 95% CI: ",AUC[1],"-",AUC[3],")"),side=1,line=-7,color="#86CEEB")
mtext(paste0("Age: ",AUCB[2],"( 95% CI: ",AUCB[1],"-",AUCB[3],")"),side=1,line=-6,color="#B03060")
mtext(paste0("p:0.8505"),side=1,line=-5,color="gray20")
#subB
mainB = data.frame(T5=DeathP$T5,T10=DeathP$T10,T15=DeathP$T15,T20=DeathP$T20,T25=DeathP$T25,T30=DeathP$T30,cohort=DeathP$cohort)
mainB$cohort = factor(mainB$cohort,levels=c("Live","Death"))
mainB_L <- melt(mainB)
ggboxplot(mainB_L,"variable","value",color="cohort",add="jitter",palette=c("#86CEEB","#B03060"),xlab="",ylab="Death Probability") + theme(legend.position="right")

#subC
mainC = data.frame(T5=P_IND$T5,T10=P_IND$T10,T15=P_IND$T15,T20=P_IND$T20,T25=P_IND$T25,T30=P_IND$T30,cohort=P_IND$cohort)
mainC$cohort =	factor(mainC$cohort,levels=c("Mild","WH_live","HN_Death"))
mainC_L <- melt(mainC)
ggboxplot(mainC_L,"variable","value",color="cohort",add="jitter",palette=c("#86CEEB","gray20","#B03060"),xlab="",ylab="Death Probability") + theme(legend.position="right")
