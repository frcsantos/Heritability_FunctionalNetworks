library("umx")
library("tidyverse")
library(MASS)

table<-read.csv("./gordon_333_parcellation_networklabels.csv", header=F)

table$V2 <- as.factor(table$V2)
networks <- levels(table$V2)


#Receiving processed table
edges <- read_delim("final_table_gordon", col_names=T, delim="\t")

#Change sex to 0 or 1 ##
edges <- mutate(edges, Gender = if_else(Gender=="M", 1, 0))


#Multiply all by 100
edges[,7:dim(edges)[2]] <-  edges[,7:dim(edges)[2]]*100
        
#Sex and age settings
edgenames <- colnames(edges[,7:((length(colnames(edges))/2)+3)])
edges$age1 = edges$age2 = edges$Age
edges$gender1 = edges$gender2 = edges$Gender

#Getting parameters  
for (l in as.numeric(networks))
{	
	if (l!=11){
		n=1000
		nel=10
	}
	else {
		n=1
		nel=6
	}
	edge_ind=c()
	aux <- filter(table, V2==l)
	for (j in (1:(dim(aux)[1]-1)))
	{
		for (k in ((j+1):dim(aux)[1]))
		{
		edge_ind <- c(edge_ind,paste("ROI",aux[k,1],aux[j,1],"T1", sep="_"))
		}
	}
	for (i in 1:n){
		selected_edges <- sample(edge_ind, size=nel, replace=F)
		selDVs=gsub('.{1}$','',selected_edges)
		tmp = umx_residualize(selDVs, "age", suffixes = 1:2, edges)
		edge_resid = suppressWarnings(umx_residualize(selDVs, "gender", suffixes = 1:2, tmp))
		mzData <- as.data.frame(edge_resid[edge_resid$Zigosity=="MZ",])
		dzData <- as.data.frame(edge_resid[edge_resid$Zigosity=="DZ",])
		m1 = suppressMessages(umxACEv(selDVs = selDVs, sep = "", dzData = dzData, mzData = mzData))
		A1 = sum(diag(m1$output$algebras$top.A_std))/nel
		C1 = sum(diag(m1$output$algebras$top.C_std))/nel
		E1 = sum(diag(m1$output$algebras$top.E_std))/nel
		r <- cbind(A1,C1,E1)
		print(i)
		write.table(r, paste("cmultivariate_",l,sep=""), sep="\t", quote=F, append=T, col.names=F, row.names=F)
	}
} 
}

