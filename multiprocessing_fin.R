library("umx")
library("tidyverse")

#Receiving processed table
rois <- read_delim("final_table.txt", col_names=T, delim="\t")

#Change sex to 0 and 1 ##
rois <- mutate(rois, Gender = if_else(Gender=="M", 1, 0))


#Multiply all by 100
rois[,7:dim(rois)[2]] <-  rois[,7:dim(rois)[2]]*100
        
#Eliminating sex and age influence
roinames <- colnames(rois[,7:((length(colnames(rois))/2)+3)])
rois$age1 = rois$age2 = rois$Age
rois$gender1 = rois$gender2 = rois$Gender

#Getting parameters  

n=8000
nel=10
#registerDoParallel(5)
#system.time({
#r <- foreach (i, .combine=rbind) %dopar%{
for (i in 1:n){
	range <- (dim(rois)[2]+6-4)/2 #6 columns before edges and 3 columns after
	ids <- sample(colnames(rois)[7:range], size=nel, replace=F)
	selDVs=gsub('.{1}$','',ids)
        tmp = umx_residualize(selDVs, "age", suffixes = 1:2, rois) ## Adapt script to sex and age
        roi_resid = suppressWarnings(umx_residualize(selDVs, "gender", suffixes = 1:2, tmp))
        mzData <- as.data.frame(roi_resid[roi_resid$Zigosity=="MZ",])
        dzData <- as.data.frame(roi_resid[roi_resid$Zigosity=="DZ",])
        m1 = suppressMessages(umxACEv(selDVs = selDVs, sep = "", dzData = dzData, mzData = mzData))
	A1 = sum(diag(m1$output$algebras$top.A_std))/nel
	C1 = sum(diag(m1$output$algebras$top.C_std))/nel
	E1 = sum(diag(m1$output$algebras$top.E_std))/nel
	r <- cbind(A1,C1,E1)
	write.table(r,"cmultivariate.txt", sep="\t", quote=F, append=T, col.names=F, row.names=F)
	
}
#})

