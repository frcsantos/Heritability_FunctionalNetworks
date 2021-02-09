library("umx")
library("tidyverse")
library("MASS")

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
n=13000
nel=10
for (i in 1:n){
	range <- (dim(edges)[2]+6-4)/2 # 6 columns before edges and 4 columns after
	selected_edges <- sample(colnames(edges)[7:range], size=nel, replace=F)
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
	write.table(r,"cmultivariate.txt", sep="\t", quote=F, append=T, col.names=F, row.names=F)
	
}

