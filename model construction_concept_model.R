rm(list=ls())#删除目前所有的变量

library(foreign)
library(wesanderson)
library(psych)
library(pheatmap)
library(ade4)
library(readxl)
library(ggplot2)
library(Hmisc)

#模型构建：概念模型的表征相似性矩阵
##以中国“五年计划”政策为例
CLHLSdata <- read_excel(".../concept_model.xlsx")
CLHLSdata <- as.data.frame(CLHLSdata)
data_plan <-CLHLSdata[1,c(2:9)]
data_plan<-t(data_plan )
plan1<- as.data.frame(data_plan)
plan1.dists <- dist(plan1)
aplan1<-as.matrix(plan1.dists)
aplan1<-1-(aplan1-min(aplan1))/(max(aplan1)-min(aplan1))
dev.off()
dev.new()
pheatmap(aplan1,cluster_rows = FALSE, cluster_cols = FALSE,border_color = "gray",
         color = c(colorRampPalette(colors = c("#097CFF","white"))(40),
                   colorRampPalette(colors = c("white","white"))(8),
                   colorRampPalette(colors = c("white","red"))(52)),
         legend_breaks=seq(0,1,0.2),angle_col=45)



##以文化松紧度概念为例
datatightness<-t(CLHLSdata[3,c(2:23)])
datatightnessa<- as.data.frame(datatightness)
datatightnessa.dists<- dist(datatightnessa)
adatatightnessa<-as.matrix(datatightnessa.dists)
adatatightnessa<-1-(adatatightnessa-min(adatatightnessa))/(max(adatatightnessa)-min(adatatightnessa))
pheatmap(adatatightnessa,border_color = "gray",
         color = c(colorRampPalette(colors = c("#097CFF","white"))(45),
                   colorRampPalette(colors = c("white","white"))(10),
                   colorRampPalette(colors = c("white","red"))(45)),
         legend_breaks=seq(0,1,0.2), angle_col=45)

##以水稻种植面积为例
rice<-t(CLHLSdata[4,c(2:23)])
ricea<- as.data.frame(rice)
ricea.dists<- dist(ricea)
aricea<-as.matrix(ricea.dists)
aricea<-1-(aricea-min(aricea))/(max(aricea)-min(aricea))
pheatmap(aricea,cluster_rows = FALSE, cluster_cols = FALSE,border_color = "gray",
         color = c(colorRampPalette(colors = c("#097CFF","white"))(70),
                   colorRampPalette(colors = c("white","white"))(8),
                   colorRampPalette(colors = c("white","red"))(22)),
         legend_breaks=seq(0,1,0.2),angle_col=45)