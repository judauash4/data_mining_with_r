column_names <- c("season","size","speed","mxPH","mnO2","Cl","NO3","NH4","oPO4","PO4","Chla"
                  ,"a1","a2","a3","a4","a5","a6","a7")

#值為'XXXXXXX'的是na
data <- read.table(file="Analysis.txt",
                   header=FALSE,
                   col.names=column_names,
                   na.strings="XXXXXXX") 

##### 檢視資料 ##########################################################

#以機率畫出直方圖
hist(data$mxPH,probability=TRUE,main='Histogram of max pH value',ylim=0:1) 
lines(density(data$mxPH,na.rm=TRUE))
rug(jitter(data$mxPH))
#箱型圖,不好看
boxplot(x=data$oPO4,boxwex=0.15,ylab='oPO4')
rug(jitter(data$oPO4),side=2)
abline(h=mean(data$oPO4,na.rm=TRUE),lty=2)
#散布圖
plot(x=data$NH4)
abline(h=mean(data$NH4,na.rm=TRUE),lty=1)
abline(h=mean(data$NH4,na.rm=TRUE)+sd(data$NH4,na.rm=TRUE),lty=2)
abline(h=median(data$NH4,na.rm=TRUE),lty=3)
identify(data$NH4) #非常有意思,以滑鼠點選方式標示資料

library(lattice)
#lattice箱型圖,好看多了
bwplot(size~a1,data=data,ylab='river size',xlab='algae a1')

minO2 <- equal.count(na.omit(data$mnO2),number=4,overlap=1/5)
stripplot(season ~ a3|minO2,data=data[!is.na(data$mnO2),])

##### 處理未知數值 ######################################################

#找出含有未知(NA)數值的資料列
data[!complete.cases(data),]

#共有16筆資料列含有NA
nrow(data[!complete.cases(data),])

#sample 62, 199有太多NA了，直接刪除資料列
data <- data[-c(62,199),]

# 由於mxPH為常態分布，因此適合用mean來填補sample 48遺失的值
data[48,'mxPH'] <- mean(data$mxPH,na.rm=TRUE)

# 由於Chla為傾斜分布，因此不適合用mean來填補，所以用median來填補
data[is.na(data$Chla),'Chla'] <- median(data$Chla,na.rm=TRUE)

# 但，上述方式有時並非最好的辦法
# 利用相關係數找出變數之間的關聯性(排除1:3名義尺度變數)
# 找到oPO4與PO4有高相關性
data_cor <- cor(data[,4:18],use="complete.obs")
symnum(data_cor)

# 找出oPO4與PO4的 線性模式
lm(data=data,formula=oPO4~PO4)

# oPO4 = -15.6142 + 0.6466 * PO4
fillPO4 <- function(oPO4) {
  if(is.na(oPO4)) return(NA)
  else {
    return ( (oPO4 - (-15.6142)) / 0.6466 )
  }
}

data[28,'PO4'] <- fillPO4(data[28,'oPO4'])

histogram(~mxPH|season,data=data)
histogram(~mxPH|size,data=data)
histogram(~mxPH|size*speed,data=data)
stripplot(size~mxPH|speed,data=data,jitter=TRUE)

### 用類似案例填補未知數值 #################################

library(cluster)
dist.mtx <- as.matrix(daisy(data,stand=TRUE))

# row: 28  38  48  55  56  57  58  59  60  61  62 115 160 183
which(!complete.cases(data))

# 試玩row 38
# 前10名與row 38最相近的列 54 22 64 11 30 25 53 37 24 18
sort(dist.mtx[38,])[2:11]
best10 <- as.integer(names(sort(dist.mtx[38,])[2:11]))

# row 38的"mnO2"需要被填補
is.na(data[38,])
# 使用前10名最相近列的"mnO2"的中位數
data[38,"mnO2"] <- median(data[best10,"mnO2"],na.rm=TRUE)

# 以上算法寫成function
fill_by_dist <- function(target_row) {
  if(is.na(target_row)) return(NA)
  else {
    dist.mtx <- as.matrix(daisy(data,stand=TRUE))
    best10 <- as.integer(names(sort(dist.mtx[target_row,])[2:11]))
    na_columns <- which(is.na(data[target_row,]))
    if(length(na_columns) == 1) {
      # na_columns == vector 不可用apply
      data[target_row,na_columns] <- median(data[best10,na_columns],na.rm=TRUE)
    }else{
      # na_columns == list 才可用apply
      data[target_row,na_columns] <- apply(data[best10,na_columns],2,median,na.rm=TRUE)
    }
  }
}














