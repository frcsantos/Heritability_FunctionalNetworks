library("umx")

rois<-read.table("final_table", header=T)
mzData<-rois[rois$Zigosity=="MZ",]
dzData<-rois[rois$Zigosity=="DZ",]
selDVs=c("ROI_2_1_T")
m1 = umxACE(selDVs = selDVs, sep = "", dzData = dzData, mzData = mzData)

