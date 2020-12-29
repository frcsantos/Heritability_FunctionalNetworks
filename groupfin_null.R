library("umx")
library("tidyverse")
library(MASS)

table<-read.csv("./shen_268_parcellation_networklabels.csv", header=F)

table$V2 <- as.factor(table$V2)
tipos <- levels(table$V2)


#Receiving processed table
rois <- read_delim("./final_table.txt", col_names=T, delim="\t")

#Change sex to 0 and 1 ##
rois <- mutate(rois, Gender = if_else(Gender=="M", 1, 0))


#Multiply all by 100
rois[,7:dim(rois)[2]] <-  rois[,7:dim(rois)[2]]*100
        
#Eliminating sex and age influence
roinames <- colnames(rois[,7:((length(colnames(rois))/2)+3)])
rois$age1 = rois$age2 = rois$Age
rois$gender1 = rois$gender2 = rois$Gender

#Getting parameters  
for (l in as.numeric(tipos))
{
	if (l >= 3){ 
	roi_ind=c()
	aux <- filter(table, V2==l)
	for (j in (1:(dim(aux)[1]-1)))
	{
		for (k in ((j+1):dim(aux)[1]))
		{
		roi_ind <- c(roi_ind,paste("ROI",aux[k,1],aux[j,1],"T1", sep="_"))
		}
	}

	n=1000
	nel=10
	for (i in 1:n){
		ids <- sample(roi_ind, size=nel, replace=F)
        selDVs=gsub('.{1}$','',ids)
        tmp = umx_residualize(selDVs, "age", suffixes = 1:2, rois)				## Adapt script to sex and age
        roi_resid = suppressWarnings(umx_residualize(selDVs, "gender", suffixes = 1:2, tmp))
		roi_resid$Zigosity <- sample(roi_resid$Zigosity)					##Randomize twin types (mono and di)
        mzData <- as.data.frame(roi_resid[roi_resid$Zigosity=="MZ",])
        dzData <- as.data.frame(roi_resid[roi_resid$Zigosity=="DZ",])
        m1 = suppressMessages(umxACEv(selDVs = selDVs, sep = "", dzData = dzData, mzData = mzData))
		A1 = (sum(diag(m1$output$algebras$top.A_std)))/nel
		C1 = (sum(diag(m1$output$algebras$top.C_std)))/nel
		E1 = (sum(diag(m1$output$algebras$top.E_std)))/nel
		r <- cbind(A1,C1,E1)
		print(i)
		write.table(r, paste("multivariate_",l, "_null", sep=""), sep="\t", quote=F, append=T, col.names=F, row.names=F)
	}
} 
}
