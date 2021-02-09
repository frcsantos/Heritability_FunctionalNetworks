library("xlsx")
library("tidyverse")

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

paths1 <- list.files("../../dados/Dados/Rest1", pattern="mat_conn_finn_r1.csv", recursive=T, full.names=T)
paths2 <- list.files("../../dados/Dados/Rest2", pattern="mat_conn_finn_r2.csv", recursive=T, full.names=T)
#toytest:
#paths <- c('../../dados/Dados/Rest1/All_MZ_R1/969476/mat_conn_finn_r1.csv','../../dados/Dados/Rest1/All_MZ_R1/971160/mat_conn_finn_r1.csv')

dataconstruct <- function(paths) {
  
  edges <- data.frame(matrix(nrow= length(paths), ncol = 35778))
  aux = 1
  cnames <- c()
  rnames <- c()
  
  for (i in paths){
    
    edgesline <- c()
    edgecor <- read.csv(i, header=F)
    if (aux == 1){
      for (c in (1 : (ncol(edgecor)-1))) {
        for (l in ((c+1) : ncol(edgecor))) {
          cnames <- c(cnames,paste("ROI", l, c, sep="_"))
        }
      }
      colnames(edges) <- cnames
    }
    ind <- lower.tri(edgecor, diag=FALSE) # Miraculous function for saving time
    edgesline <- edgecor[ind]
    rnames <- c(rnames, strsplit(i,"/")[[1]][7])
    edges[aux,] <- edgesline
    aux=aux+1
    print (i)
  }
  rownames(edges)<-rnames
  
  final_tmp2 <- merge(final_tmp, edges, by.x="Subject_T1", by.y=0)
  final <- merge(final_tmp2, edges, by.x="Subject_T2", by.y=0)
  colnames(final) <- sub ("\\.x", "_T1", colnames(final))
  colnames(final) <- sub ("\\.y", "_T2", colnames(final))
  return(final)
}

R1 <- dataconstruct(paths1)
R2 <- dataconstruct(paths2)

final <- cbind(R1[,1:6],(R1[,7:ncol(R1)] + R2[,7:ncol(R2)])/2) #getting mean from R1 and R2

write.table(final,"final_table", sep="\t", row.name=F, quote=F)
