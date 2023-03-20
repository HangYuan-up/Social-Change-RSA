rm(list=ls())
library(foreign)
library(wesanderson)
library(psych)
library(pheatmap)
library(ade4)
library(readxl)
library(ggplot2)
library(Hmisc)


#2.2.1模型构建：跨时间的单变量表征相似性矩阵（国家尺度，时间维度）
##以老年人孤独感为例
CLHLSdata <- read_excel(".../lonely.xlsx")
lonely<-CLHLSdata[1,c(2:9)]
lonely<-as.data.frame(lonely)
lonely<-as.matrix(dist(t(lonely)))
lonely<-1-lonely/max(lonely)
bk <- c(seq(0,0.85,by=0.01),seq(0.86,1,by=0.01))
pheatmap(lonely,cluster_rows = FALSE, cluster_cols = FALSE,border_color = "gray",
         color = c(colorRampPalette(colors = c("#097CFF","white"))(90),
                   colorRampPalette(colors = c("white","red"))(10)),
         legend_breaks=seq(0,1,0.1),
         breaks=bk,angle_col=45)


#2.2.2模型构建：跨时间的多变量表征相似性矩阵（国家尺度，意间-时间维度）
##以老年人心理健康为例
CLHLSdata <- read_excel(".../mentalhealth.xlsx")
mentalhealth<- CLHLSdata[c(1:7),c(2:9)]
mentalhealth<- as.data.frame(mentalhealth)
mentalhealth_1998.dists <- dist(mentalhealth[,1])
mentalhealth_2000.dists <- dist(mentalhealth[,2])
mentalhealth_2002.dists <- dist(mentalhealth[,3])
mentalhealth_2005.dists <- dist(mentalhealth[,4])
mentalhealth_2008.dists <- dist(mentalhealth[,5])
mentalhealth_2011.dists <- dist(mentalhealth[,6])
mentalhealth_2014.dists <- dist(mentalhealth[,7])
mentalhealth_2018.dists <- dist(mentalhealth[,8])
a1998<-as.matrix(mentalhealth_1998.dists)
a2000<-as.matrix(mentalhealth_2000.dists)
a2002<-as.matrix(mentalhealth_2002.dists)
a2005<-as.matrix(mentalhealth_2005.dists)
a2008<-as.matrix(mentalhealth_2008.dists)
a2011<-as.matrix(mentalhealth_2011.dists)
a2014<-as.matrix(mentalhealth_2014.dists)
a2018<-as.matrix(mentalhealth_2018.dists)
a1998<-1-(a1998-min(a1998))/(max(a1998)-min(a1998))
a2000<-1-(a2000-min(a2000))/(max(a2000)-min(a2000))
a2002<-1-(a2002-min(a2002))/(max(a2002)-min(a2002))
a2005<-1-(a2005-min(a2005))/(max(a2005)-min(a2005))
a2008<-1-(a2008-min(a2008))/(max(a2008)-min(a2008))
a2011<-1-(a2011-min(a2011))/(max(a2011)-min(a2011))
a2014<-1-(a2014-min(a2014))/(max(a2014)-min(a2014))
a2018<-1-(a2018-min(a2018))/(max(a2018)-min(a2018))
mentalhealth1998<-1-a1998/max(a1998)
mentalhealth1998<-as.matrix(mentalhealth1998[lower.tri(mentalhealth1998)])
mentalhealth2000<-1-a2000/max(a2000)
mentalhealth2000<-as.matrix(mentalhealth2000[lower.tri(mentalhealth2000)])
mentalhealth2002<-1-a2002/max(a2002)
mentalhealth2002<-as.matrix(mentalhealth2002[lower.tri(mentalhealth2002)])
mentalhealth2005<-1-a2005/max(a2005)
mentalhealth2005<-as.matrix(mentalhealth2005[lower.tri(mentalhealth2005)])
mentalhealth2008<-1-a2008/max(a2008)
mentalhealth2008<-as.matrix(mentalhealth2008[lower.tri(mentalhealth2008)])
mentalhealth2011<-1-a2011/max(a2011)
mentalhealth2011<-as.matrix(mentalhealth2011[lower.tri(mentalhealth2011)])
mentalhealth2014<-1-a2014/max(a2014)
mentalhealth2014<-as.matrix(mentalhealth2014[lower.tri(mentalhealth2014)])
mentalhealth2018<-1-a2018/max(a2018)
mentalhealth2018<-as.matrix(mentalhealth2018[lower.tri(mentalhealth2018)])
mentalhealth_total<-cbind(mentalhealth1998,mentalhealth2000,mentalhealth2002,mentalhealth2005,
                          mentalhealth2008,mentalhealth2011,mentalhealth2014,mentalhealth2018)
mentalhealth_totalr<-cor(mentalhealth_total,method="pearson")
pheatmap(mentalhealth_totalr,cluster_rows = FALSE, cluster_cols = FALSE,border_color = "gray",
         color = c(colorRampPalette(colors = c("#097CFF","white"))(90),
                   colorRampPalette(colors = c("white","red"))(10)),
         legend_breaks=seq(0.74,1,0.1),angle_col=45)


#2.2.3模型构建：跨时间的多变量表征相似性矩阵（地区尺度，意间-时间维度）
##以北京老年人认知功能为例
CLHLSdata <- read_excel(".../cognition_beijing.xlsx")
cognition_beijing<- CLHLSdata[c(1:6),c(2:9)]
cognition_beijing<- as.data.frame(cognition_beijing)
cognition_beijing_1998.dists <- dist(cognition_beijing[,1])
cognition_beijing_2000.dists <- dist(cognition_beijing[,2])
cognition_beijing_2002.dists <- dist(cognition_beijing[,3])
cognition_beijing_2005.dists <- dist(cognition_beijing[,4])
cognition_beijing_2008.dists <- dist(cognition_beijing[,5])
cognition_beijing_2011.dists <- dist(cognition_beijing[,6])
cognition_beijing_2014.dists <- dist(cognition_beijing[,7])
cognition_beijing_2018.dists <- dist(cognition_beijing[,8])
a1998<-as.matrix(cognition_beijing_1998.dists)
a2000<-as.matrix(cognition_beijing_2000.dists)
a2002<-as.matrix(cognition_beijing_2002.dists)
a2005<-as.matrix(cognition_beijing_2005.dists)
a2008<-as.matrix(cognition_beijing_2008.dists)
a2011<-as.matrix(cognition_beijing_2011.dists)
a2014<-as.matrix(cognition_beijing_2014.dists)
a2018<-as.matrix(cognition_beijing_2018.dists)
a1998<-1-(a1998-min(a1998))/(max(a1998)-min(a1998))
a2000<-1-(a2000-min(a2000))/(max(a2000)-min(a2000))
a2002<-1-(a2002-min(a2002))/(max(a2002)-min(a2002))
a2005<-1-(a2005-min(a2005))/(max(a2005)-min(a2005))
a2008<-1-(a2008-min(a2008))/(max(a2008)-min(a2008))
a2011<-1-(a2011-min(a2011))/(max(a2011)-min(a2011))
a2014<-1-(a2014-min(a2014))/(max(a2014)-min(a2014))
a2018<-1-(a2018-min(a2018))/(max(a2018)-min(a2018))
cognition_beijing1998<-1-a1998/max(a1998)
cognition_beijing1998<-as.matrix(cognition_beijing1998[lower.tri(cognition_beijing1998)])
cognition_beijing2000<-1-a2000/max(a2000)
cognition_beijing2000<-as.matrix(cognition_beijing2000[lower.tri(cognition_beijing2000)])
cognition_beijing2002<-1-a2002/max(a2002)
cognition_beijing2002<-as.matrix(cognition_beijing2002[lower.tri(cognition_beijing2002)])
cognition_beijing2005<-1-a2005/max(a2005)
cognition_beijing2005<-as.matrix(cognition_beijing2005[lower.tri(cognition_beijing2005)])
cognition_beijing2008<-1-a2008/max(a2008)
cognition_beijing2008<-as.matrix(cognition_beijing2008[lower.tri(cognition_beijing2008)])
cognition_beijing2011<-1-a2011/max(a2011)
cognition_beijing2011<-as.matrix(cognition_beijing2011[lower.tri(cognition_beijing2011)])
cognition_beijing2014<-1-a2014/max(a2014)
cognition_beijing2014<-as.matrix(cognition_beijing2014[lower.tri(cognition_beijing2014)])
cognition_beijing2018<-1-a2018/max(a2018)
cognition_beijing2018<-as.matrix(cognition_beijing2018[lower.tri(cognition_beijing2018)])
cognition_beijing_total<-cbind(cognition_beijing1998,cognition_beijing2000,cognition_beijing2002,cognition_beijing2005,
                               cognition_beijing2008,cognition_beijing2011,cognition_beijing2014,cognition_beijing2018)
cognition_beijing_totalr<-cor(cognition_beijing_total,method="pearson")
pheatmap(cognition_beijing_totalr,cluster_rows = FALSE, cluster_cols = FALSE,border_color = "gray",
         color = c(colorRampPalette(colors = c("#097CFF","white"))(82),
                   colorRampPalette(colors = c("white","red"))(18)),
         legend_breaks=seq(0.54,1,0.2),angle_col=45)


#2.2.5模型构建：跨地区的多变量表征相似性矩阵（国家尺度，意间-空间维度）
##以老年人心理健康为例
CLHLSdata <- read_excel(".../mentalhealth_cross_regional.xlsx")
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



#2.2.6模型构建：跨地区的多变量表征相似性矩阵（国家尺度，意间-时间-空间维度）
##以1998年老年人心理健康为例
CLHLSdata <- read_excel(".../mentalhealth_cross_regional_1998.xlsx")
CLHLSdata <- as.data.frame(CLHLSdata)
mentalhealth<- CLHLSdata[c(1:7),c(2:23)]
mentalhealth<- as.data.frame(mentalhealth)
mentalhealth_1.dists <- dist(mentalhealth[,1])
mentalhealth_2.dists <- dist(mentalhealth[,2])
mentalhealth_3.dists <- dist(mentalhealth[,3])
mentalhealth_4.dists <- dist(mentalhealth[,4])
mentalhealth_5.dists <- dist(mentalhealth[,5])
mentalhealth_6.dists <- dist(mentalhealth[,6])
mentalhealth_7.dists <- dist(mentalhealth[,7])
mentalhealth_8.dists <- dist(mentalhealth[,8])
mentalhealth_9.dists <- dist(mentalhealth[,9])
mentalhealth_10.dists <- dist(mentalhealth[,10])
mentalhealth_11.dists <- dist(mentalhealth[,11])
mentalhealth_12.dists <- dist(mentalhealth[,12])
mentalhealth_13.dists <- dist(mentalhealth[,13])
mentalhealth_14.dists <- dist(mentalhealth[,14])
mentalhealth_15.dists <- dist(mentalhealth[,15])
mentalhealth_16.dists <- dist(mentalhealth[,16])
mentalhealth_17.dists <- dist(mentalhealth[,17])
mentalhealth_18.dists <- dist(mentalhealth[,18])
mentalhealth_19.dists <- dist(mentalhealth[,19])
mentalhealth_20.dists <- dist(mentalhealth[,20])
mentalhealth_21.dists <- dist(mentalhealth[,21])
mentalhealth_22.dists <- dist(mentalhealth[,22])
a1<-as.matrix(mentalhealth_1.dists)
a2<-as.matrix(mentalhealth_2.dists)
a3<-as.matrix(mentalhealth_3.dists)
a4<-as.matrix(mentalhealth_4.dists)
a5<-as.matrix(mentalhealth_5.dists)
a6<-as.matrix(mentalhealth_6.dists)
a7<-as.matrix(mentalhealth_7.dists)
a8<-as.matrix(mentalhealth_8.dists)
a9<-as.matrix(mentalhealth_9.dists)
a10<-as.matrix(mentalhealth_10.dists)
a11<-as.matrix(mentalhealth_11.dists)
a12<-as.matrix(mentalhealth_12.dists)
a13<-as.matrix(mentalhealth_13.dists)
a14<-as.matrix(mentalhealth_14.dists)
a15<-as.matrix(mentalhealth_15.dists)
a16<-as.matrix(mentalhealth_16.dists)
a17<-as.matrix(mentalhealth_17.dists)
a18<-as.matrix(mentalhealth_18.dists)
a19<-as.matrix(mentalhealth_19.dists)
a20<-as.matrix(mentalhealth_20.dists)
a21<-as.matrix(mentalhealth_21.dists)
a22<-as.matrix(mentalhealth_22.dists)
a1<-1-(a1-min(a1))/(max(a1)-min(a1))
a2<-1-(a2-min(a2))/(max(a2)-min(a2))
a3<-1-(a3-min(a3))/(max(a3)-min(a3))
a4<-1-(a4-min(a4))/(max(a4)-min(a4))
a5<-1-(a5-min(a5))/(max(a5)-min(a5))
a6<-1-(a6-min(a6))/(max(a6)-min(a6))
a7<-1-(a7-min(a7))/(max(a7)-min(a7))
a8<-1-(a8-min(a8))/(max(a8)-min(a8))
a9<-1-(a9-min(a9))/(max(a9)-min(a9))
a10<-1-(a10-min(a10))/(max(a10)-min(a10))
a11<-1-(a11-min(a11))/(max(a11)-min(a11))
a12<-1-(a12-min(a12))/(max(a12)-min(a12))
a13<-1-(a13-min(a13))/(max(a13)-min(a13))
a14<-1-(a14-min(a14))/(max(a14)-min(a14))
a15<-1-(a15-min(a15))/(max(a15)-min(a15))
a16<-1-(a16-min(a16))/(max(a16)-min(a16))
a17<-1-(a17-min(a17))/(max(a17)-min(a17))
a18<-1-(a18-min(a18))/(max(a18)-min(a18))
a19<-1-(a19-min(a19))/(max(a19)-min(a19))
a20<-1-(a20-min(a20))/(max(a20)-min(a20))
a21<-1-(a21-min(a21))/(max(a21)-min(a21))
a22<-1-(a22-min(a22))/(max(a22)-min(a22))
mentalhealth1<-1-a1/max(a1)
mentalhealth2<-1-a2/max(a2)
mentalhealth3<-1-a3/max(a3)
mentalhealth4<-1-a4/max(a4)
mentalhealth5<-1-a5/max(a5)
mentalhealth6<-1-a6/max(a6)
mentalhealth7<-1-a7/max(a7)
mentalhealth8<-1-a8/max(a8)
mentalhealth9<-1-a9/max(a9)
mentalhealth10<-1-a10/max(a10)
mentalhealth11<-1-a11/max(a11)
mentalhealth12<-1-a12/max(a12)
mentalhealth13<-1-a13/max(a13)
mentalhealth14<-1-a14/max(a14)
mentalhealth15<-1-a15/max(a15)
mentalhealth16<-1-a16/max(a16)
mentalhealth17<-1-a17/max(a17)
mentalhealth18<-1-a18/max(a18)
mentalhealth19<-1-a19/max(a19)
mentalhealth20<-1-a20/max(a20)
mentalhealth21<-1-a21/max(a21)
mentalhealth22<-1-a22/max(a22)
mentalhealth1<-as.matrix(mentalhealth1[lower.tri(mentalhealth1)])
mentalhealth2<-as.matrix(mentalhealth2[lower.tri(mentalhealth2)])
mentalhealth3<-as.matrix(mentalhealth3[lower.tri(mentalhealth3)])
mentalhealth4<-as.matrix(mentalhealth4[lower.tri(mentalhealth4)])
mentalhealth5<-as.matrix(mentalhealth5[lower.tri(mentalhealth5)])
mentalhealth6<-as.matrix(mentalhealth6[lower.tri(mentalhealth6)])
mentalhealth7<-as.matrix(mentalhealth7[lower.tri(mentalhealth7)])
mentalhealth8<-as.matrix(mentalhealth8[lower.tri(mentalhealth8)])
mentalhealth9<-as.matrix(mentalhealth9[lower.tri(mentalhealth9)])
mentalhealth10<-as.matrix(mentalhealth10[lower.tri(mentalhealth10)])
mentalhealth11<-as.matrix(mentalhealth11[lower.tri(mentalhealth11)])
mentalhealth12<-as.matrix(mentalhealth12[lower.tri(mentalhealth12)])
mentalhealth13<-as.matrix(mentalhealth13[lower.tri(mentalhealth13)])
mentalhealth14<-as.matrix(mentalhealth14[lower.tri(mentalhealth14)])
mentalhealth15<-as.matrix(mentalhealth15[lower.tri(mentalhealth15)])
mentalhealth16<-as.matrix(mentalhealth16[lower.tri(mentalhealth16)])
mentalhealth17<-as.matrix(mentalhealth17[lower.tri(mentalhealth17)])
mentalhealth18<-as.matrix(mentalhealth18[lower.tri(mentalhealth18)])
mentalhealth19<-as.matrix(mentalhealth19[lower.tri(mentalhealth19)])
mentalhealth20<-as.matrix(mentalhealth20[lower.tri(mentalhealth20)])
mentalhealth21<-as.matrix(mentalhealth21[lower.tri(mentalhealth21)])
mentalhealth22<-as.matrix(mentalhealth22[lower.tri(mentalhealth22)])

mentalhealth_total<-cbind(mentalhealth1,mentalhealth2,mentalhealth3,mentalhealth4,
                          mentalhealth5,mentalhealth6,mentalhealth7,mentalhealth8,
                          mentalhealth9,mentalhealth10,mentalhealth11,mentalhealth12,
                          mentalhealth13,mentalhealth14,mentalhealth15,mentalhealth16,
                          mentalhealth17,mentalhealth18,mentalhealth19,mentalhealth20,
                          mentalhealth21,mentalhealth22)
mentalhealth_totalr<-cor(mentalhealth_total,method="pearson")
pheatmap(mentalhealth_totalr,cluster_rows = FALSE, cluster_cols = FALSE,border_color = "gray",
         color = c(colorRampPalette(colors = c("#097CFF","white"))(80),
                   colorRampPalette(colors = c("white","red"))(20)),
         legend_breaks=seq(0.38,1,0.2),angle_col=45)




#2.2.7模型构建：概念模型的表征相似性矩阵
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
adatatightnessa.dist<-dist(adatatightnessa)
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






