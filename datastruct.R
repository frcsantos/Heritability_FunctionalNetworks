library("xlsx")
library("tidyverse")

setwd("~/Fer/scripts/heritability")
Mz <- read.xlsx("../../dados/Dados/Demographics.xlsx", 1, header = T)
Dz <- read.xlsx("../../dados/Dados/Demographics.xlsx", 2, header = T)
pairing <- read.xlsx("../../dados/Dados/List_twin_pairs.xlsx", 1, header = F)

##Configure
#columns: FAMID?, Zigozity, ROI1-2_T1, ROI1-2_T2, ROI1-3_T1, ROI1-3_T2....SEXO (1:MALE, 0:FEMALE), IDADE
#pairing: cut ambiguous

tmpmono <- merge(Mz, pairing, by.x="Subject", by.y="X1")[,c(1,9,3,4,7,8)]
tmpdi <- merge(Dz, pairing, by.x="Subject", by.y="X1")[,c(1,8,2,3,6,7)]
merged <- rbind(tmpmono,tmpdi)
final_tmp<-distinct(merged, Family_ID, .keep_all=T)
colnames(final_tmp) <- c("Subject_T1", "Subject_T2", "Zigosity","Family_ID","Gender","Age")

#Generalize later: Rest(x), (x).csv Functions will be welcome here

paths <- list.files("../../dados/Dados/Rest1", pattern="mat_conn_finn_r1.csv", recursive=T, full.names=T)
#toytest:
#paths <- c('../../dados/Dados/Rest1/All_MZ_R1/969476/mat_conn_finn_r1.csv','../../dados/Dados/Rest1/All_MZ_R1/971160/mat_conn_finn_r1.csv')

rois <- data.frame(matrix(nrow= length(paths), ncol = 35778))
aux = 1
cnames <- c()
rnames <- c()

for (i in paths){
  
  roisline <- c()
  roicor <- read.csv(i, header=F)
  if (aux == 1){
    for (c in (1 : (ncol(roicor)-1))) {
      for (l in ((c+1) : ncol(roicor))) {
        cnames <- c(cnames,paste("ROI", l, c, sep="_"))
      }
    }
    colnames(rois) <- cnames
  }
  ind <- lower.tri(roicor, diag=FALSE) # Miraculous function for saving time
  roisline <- roicor[ind]
  rnames <- c(rnames, strsplit(i,"/")[[1]][7])
  rois[aux,] <- roisline
  aux=aux+1
  print (i)
}
rownames(rois)<-rnames

final_tmp2 <- merge(final_tmp, rois, by.x="Subject_T1", by.y=0)
final <- merge(final_tmp2, rois, by.x="Subject_T2", by.y=0)
