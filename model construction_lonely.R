rm(list=ls())#删除目前所有的变量

library(foreign)
library(wesanderson)
library(psych)
library(pheatmap)
library(ade4)
library(readxl)
library(ggplot2)
library(Hmisc)
#模型构建：跨时间的单变量表征相似性矩阵（国家尺度，时间维度）
##以老年人孤独感为例
CLHLSdata <- read_excel(".../lonely.xlsx")
lonely<-CLHLSdata[1,c(2:9)]
lonely<-as.data.frame(lonely)
lonely<-as.matrix(dist(t(lonely)))
lonely<-1-lonely/max(lonely)
#图2
bk <- c(seq(0,0.85,by=0.01),seq(0.86,1,by=0.01))
pheatmap(lonely,cluster_rows = FALSE, cluster_cols = FALSE,border_color = "gray",
         color = c(colorRampPalette(colors = c("#097CFF","white"))(90),
                   colorRampPalette(colors = c("white","red"))(10)),
         legend_breaks=seq(0,1,0.1),
         breaks=bk,angle_col=45)
