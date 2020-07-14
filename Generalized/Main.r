library(pROC)
library(ggpubr)	

P <- read.table("COVID.samples.prob.txt",head=T)
P_train <- P[which(P$cohort=="Live" | P$cohort=="Death"),]
P_train$cohort <- as.character(P_train$cohort)

i = 1
gAUC <- c()
while (i <= 100){
set.seed(i)
P_L <- P[sample(which(P$cohort=="Live"),40),]
P_D <- P[which(P$cohort=="Death"),]
P_sample <- rbind(P_L,P_D)
P_sample$cohort <- as.character(P_sample$cohort)
gAUC[i] = auc(cohort~prob,data=P_sample)
i = i + 1
}
