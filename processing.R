library("umx")
library("tidyverse")
library(parallel)
library(MASS)
library(foreach)
library(doParallel)
library(iterators)

#Receiving processed table
edges <- read_delim("final_table", col_names=T, delim="\t")

#Change sex to 0 or 1 ##
edges <- mutate(edges, Gender = if_else(Gender=="M", 1, 0))


#Multiply all by 100
edges[,7:dim(edges)[2]] <-  edges[,7:dim(edges)[2]]*100
        
#Sex and age settings
edgenames <- colnames(edges[,7:((length(colnames(edges))/2)+3)])
edges$age1 = edges$age2 = edges$Age
edges$gender1 = edges$gender2 = edges$Gender

#Getting parameters  

registerDoParallel(5)
system.time({
r <- foreach (i=1:length(edgenames), .combine=rbind) %dopar%{
    selDVs=gsub('.{1}$','',edgenames[i])
    tmp = umx_residualize(selDVs, "age", suffixes = 1:2, edges) ## Adapt script to sex and age
    edge_resid = suppressWarnings(umx_residualize(selDVs, "gender", suffixes = 1:2, tmp))
    mzData <- as.data.frame(edge_resid[edge_resid$Zigosity=="MZ",])
    dzData <- as.data.frame(edge_resid[edge_resid$Zigosity=="DZ",])
    m1 = suppressMessages(umxACEv(selDVs = selDVs, sep = "", dzData = dzData, mzData = mzData))
	aux <- umxCI(m1, run="yes",showErrorCodes = FALSE)
	a <- t(as.data.frame(aux$output$confidenceIntervals[1,]))
	c <- t(as.data.frame(aux$output$confidenceIntervals[2,]))
	e <- t(as.data.frame(aux$output$confidenceIntervals[3,]))
        cbind(a,c,e)
	}
})


write.table(r,"univariate_safe.txt", sep="\t", row.name=T, quote=F)

rownames(r) <- edgenames
colnames(r) <- c("lower_A","A","upper_A","lower_C","C","upper_C","lower_E","E","upper_E")

write.table(r,"univariate.txt", sep="\t", row.name=T, quote=F)

