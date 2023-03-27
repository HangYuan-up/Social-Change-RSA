rm(list=ls())#删除目前所有的变量

library(foreign)
library(wesanderson)
library(psych)
library(pheatmap)
library(ade4)
library(readxl)
library(ggplot2)
library(Hmisc)

#模型构建：跨地区的多变量表征相似性矩阵（国家尺度，意间-空间维度）
##以老年人心理健康为例
CLHLSdata <- read_excel(".../mentalhealth_cross_region.xlsx")
CLHLSdata <- as.data.frame(CLHLSdata)
#beijing
mentalbeijing<- CLHLSdata[c(1:7),c(3:10)]
mentalbeijing<- as.data.frame(mentalbeijing)
library(Hmisc)
mentalbeijing_total <- rcorr(as.matrix(mentalbeijing))
beijing<-as.matrix(mentalbeijing_total$r)
beijingr<-as.matrix(beijing[lower.tri(beijing,diag =FALSE)])

#tianjin
mentaltianjin<- CLHLSdata[c(10:16),c(3:10)]
mentaltianjin<- as.data.frame(mentaltianjin)
mentaltianjin_total <- rcorr(as.matrix(mentaltianjin))
tianjin<-as.matrix(mentaltianjin_total$r)
tianjinr<-as.matrix(tianjin[lower.tri(tianjin,diag =FALSE)])

#hebei
mentalhebei<- CLHLSdata[c(19:25),c(3:10)]
mentalhebei<- as.data.frame(mentalhebei)
mentalhebei_total <- rcorr(as.matrix(mentalhebei))
hebei<-as.matrix(mentalhebei_total$r)
hebeir<-as.matrix(hebei[lower.tri(hebei,diag =FALSE)])

#shanxi
mentalshanxi<- CLHLSdata[c(28:34),c(3:10)]
mentalshanxi<- as.data.frame(mentalshanxi)
mentalshanxi_total <- rcorr(as.matrix(mentalshanxi))
shanxi<-as.matrix(mentalshanxi_total$r)
shanxir<-as.matrix(shanxi[lower.tri(shanxi,diag =FALSE)])

#liaoning
mentalliaoning<- CLHLSdata[c(37:43),c(3:10)]
mentalliaoning<- as.data.frame(mentalliaoning)
library(Hmisc)
mentalliaoning_total <- rcorr(as.matrix(mentalliaoning))
liaoning<-as.matrix(mentalliaoning_total$r)
liaoningr<-as.matrix(liaoning[lower.tri(liaoning,diag =FALSE)])

#jilin
mentaljilin<- CLHLSdata[c(46:52),c(3:10)]
mentaljilin<- as.data.frame(mentaljilin)
mentaljilin_total <- rcorr(as.matrix(mentaljilin))
jilin<-as.matrix(mentaljilin_total$r)
jilinr<-as.matrix(jilin[lower.tri(jilin,diag =FALSE)])

#heilongjiang
mentalheilongjiang<- CLHLSdata[c(55:61),c(3:10)]
mentalheilongjiang<- as.data.frame(mentalheilongjiang)
mentalheilongjiang_total <- rcorr(as.matrix(mentalheilongjiang))
heilongjiang<-as.matrix(mentalheilongjiang_total$r)
heilongjiangr<-as.matrix(heilongjiang[lower.tri(heilongjiang,diag =FALSE)])

#shanghai
mentalshanghai<- CLHLSdata[c(64:70),c(3:10)]
mentalshanghai<- as.data.frame(mentalshanghai)
mentalshanghai_total <- rcorr(as.matrix(mentalshanghai))
shanghai<-as.matrix(mentalshanghai_total$r)
shanghair<-as.matrix(shanghai[lower.tri(shanghai,diag =FALSE)])

#jiangsu
mentaljiangsu<- CLHLSdata[c(73:79),c(3:10)]
mentaljiangsu<- as.data.frame(mentaljiangsu)
mentaljiangsu_total <- rcorr(as.matrix(mentaljiangsu))
jiangsu<-as.matrix(mentaljiangsu_total$r)
jiangsur<-as.matrix(jiangsu[lower.tri(jiangsu,diag =FALSE)])

#zhejiang
mentalzhejiang<- CLHLSdata[c(82:88),c(3:10)]
mentalzhejiang<- as.data.frame(mentalzhejiang)
mentalzhejiang_total <- rcorr(as.matrix(mentalzhejiang))
zhejiang<-as.matrix(mentalzhejiang_total$r)
zhejiangr<-as.matrix(zhejiang[lower.tri(zhejiang,diag =FALSE)])

#anhui
mentalanhui<- CLHLSdata[c(91:97),c(3:10)]
mentalanhui<- as.data.frame(mentalanhui)
mentalanhui_total <- rcorr(as.matrix(mentalanhui))
anhui<-as.matrix(mentalanhui_total$r)
anhuir<-as.matrix(anhui[lower.tri(anhui,diag =FALSE)])

#fujian
mentalfujian<- CLHLSdata[c(100:106),c(3:10)]
mentalfujian<- as.data.frame(mentalfujian)
mentalfujian_total <- rcorr(as.matrix(mentalfujian))
fujian<-as.matrix(mentalfujian_total$r)
fujianr<-as.matrix(fujian[lower.tri(fujian,diag =FALSE)])

#jiangxi
mentaljiangxi<- CLHLSdata[c(109:115),c(3:10)]
mentaljiangxi<- as.data.frame(mentaljiangxi)
mentaljiangxi_total <- rcorr(as.matrix(mentaljiangxi))
jiangxi<-as.matrix(mentaljiangxi_total$r)
jiangxir<-as.matrix(jiangxi[lower.tri(jiangxi,diag =FALSE)])

#shandong
mentalshandong<- CLHLSdata[c(118:124),c(3:10)]
mentalshandong<- as.data.frame(mentalshandong)
mentalshandong_total <- rcorr(as.matrix(mentalshandong))
shandong<-as.matrix(mentalshandong_total$r)
shandongr<-as.matrix(shandong[lower.tri(shandong,diag =FALSE)])

#henan
mentalhenan<- CLHLSdata[c(127:133),c(3:10)]
mentalhenan<- as.data.frame(mentalhenan)
mentalhenan_total <- rcorr(as.matrix(mentalhenan))
henan<-as.matrix(mentalhenan_total$r)
henanr<-as.matrix(henan[lower.tri(henan,diag =FALSE)])

#hubei
mentalhubei<- CLHLSdata[c(136:142),c(3:10)]
mentalhubei<- as.data.frame(mentalhubei)
mentalhubei_total <- rcorr(as.matrix(mentalhubei))
hubei<-as.matrix(mentalhubei_total$r)
hubeir<-as.matrix(hubei[lower.tri(hubei,diag =FALSE)])

#hunan
mentalhunan<- CLHLSdata[c(145:151),c(3:10)]
mentalhunan<- as.data.frame(mentalhunan)
mentalhunan_total <- rcorr(as.matrix(mentalhunan))
hunan<-as.matrix(mentalhunan_total$r)
hunanr<-as.matrix(hunan[lower.tri(hunan,diag =FALSE)])

#guangdong
mentalguangdong<- CLHLSdata[c(154:160),c(3:10)]
mentalguangdong<- as.data.frame(mentalguangdong)
mentalguangdong_total <- rcorr(as.matrix(mentalguangdong))
guangdong<-as.matrix(mentalguangdong_total$r)
guangdongr<-as.matrix(guangdong[lower.tri(guangdong,diag =FALSE)])

#guangxi
mentalguangxi<- CLHLSdata[c(163:169),c(3:10)]
mentalguangxi<- as.data.frame(mentalguangxi)
mentalguangxi_total <- rcorr(as.matrix(mentalguangxi))
guangxi<-as.matrix(mentalguangxi_total$r)
guangxir<-as.matrix(guangxi[lower.tri(guangxi,diag =FALSE)])

#chongqing
mentalchongqing<- CLHLSdata[c(172:178),c(3:10)]
mentalchongqing<- as.data.frame(mentalchongqing)
mentalchongqing_total <- rcorr(as.matrix(mentalchongqing))
chongqing<-as.matrix(mentalchongqing_total$r)
chongqingr<-as.matrix(chongqing[lower.tri(chongqing,diag =FALSE)])

#sichuan
mentalsichuan<- CLHLSdata[c(181:187),c(3:10)]
mentalsichuan<- as.data.frame(mentalsichuan)
mentalsichuan_total <- rcorr(as.matrix(mentalsichuan))
sichuan<-as.matrix(mentalsichuan_total$r)
sichuanr<-as.matrix(sichuan[lower.tri(sichuan,diag =FALSE)])

#shaanxi
mentalshaanxi<- CLHLSdata[c(190:196),c(3:10)]
mentalshaanxi<- as.data.frame(mentalshaanxi)
mentalshaanxi_total <- rcorr(as.matrix(mentalshaanxi))
shaanxi<-as.matrix(mentalshaanxi_total$r)
shaanxir<-as.matrix(shaanxi[lower.tri(shaanxi,diag =FALSE)])
################
mentalchange<-cbind(beijingr,tianjinr,shanxir,shaanxir,hebeir,shandongr,anhuir,henanr,
                    shanghair,liaoningr,jilinr,fujianr,chongqingr,zhejiangr,guangdongr,
                    guangxir,heilongjiangr,hubeir,jiangxir,sichuanr,hunanr,jiangsur)
colnames(mentalchange)<-c("北京","天津","山西","陕西","河北","山东","安徽","河南"
                          ,"上海","辽宁","吉林","福建","重庆","浙江","广东","广西"
                          ,"黑龙江","湖北","江西","四川","湖南","江苏")

mentalchange<-as.data.frame(mentalchange)
library(Hmisc)
mentalchange_total <- rcorr(as.matrix(mentalchange))
pheatmap(mentalchange_total$r,
         cluster_rows = FALSE, cluster_cols = FALSE,
         border_color = "gray",
         color = c(colorRampPalette(colors = c("#097CFF","white"))(45),
                   colorRampPalette(colors = c("white","white"))(5),
                   colorRampPalette(colors = c("white","red"))(50)),
         legend_breaks=seq(-1,1,0.4),angle_col=45)