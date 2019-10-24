library("umx")
library("tidyverse")
library(parallel)
library(MASS)
library(foreach)
library(doParallel)
library(iterators)

#Receiving processed table
rois <- read_delim("final_table", col_names=T, delim="\t")

#Change sex to 0 and 1 ##
rois <- mutate(rois, Gender = if_else(Gender=="M", 1, 0))


#Multiply all by 100
rois[,7:dim(rois)[2]] <-  rois[,7:dim(rois)[2]]*100
        
#Eliminating sex and age influence
roinames <- colnames(rois[,7:((length(colnames(rois))/2)+3)])
rois$age1 = rois$age2 = rois$Age
rois$gender1 = rois$gender2 = rois$Gender

#Getting parameters  

registerDoParallel(5)
system.time({
r <- foreach (i=1:1000, .combine=rbind) %dopar%{
        selDVs=gsub('.{1}$','',roinames[i])
        tmp = umx_residualize(selDVs, "age", suffixes = 1:2, rois) ## Adapt script to sex and age
        roi_resid = suppressWarnings(umx_residualize(selDVs, "gender", suffixes = 1:2, tmp))
        mzData <- as.data.frame(roi_resid[roi_resid$Zigosity=="MZ",])
        dzData <- as.data.frame(roi_resid[roi_resid$Zigosity=="DZ",])
        m1 = suppressMessages(umxACEv(selDVs = selDVs, sep = "", dzData = dzData, mzData = mzData))
	aux <- umxCI(m1, run="yes",showErrorCodes = FALSE)
	a <- t(as.data.frame(aux$output$confidenceIntervals[1,]))
	c <- t(as.data.frame(aux$output$confidenceIntervals[2,]))
	e <- t(as.data.frame(aux$output$confidenceIntervals[3,]))
        cbind(a,c,e)
	}
})


write.table(r,"univariate_safe.txt", sep="\t", row.name=T, quote=F)

rownames(r) <- roinames[1:1000]
colnames(r) <- c("lower_A","A","upper_A","lower_C","C","upper_C","lower_E","E","upper_E")

write.table(r,"univariate.txt", sep="\t", row.name=T, quote=F)

