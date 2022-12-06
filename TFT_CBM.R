###################
##  TFT CBM분석  ##
###################
source("C:/Users/User/github/Function/R/bootstrap.R")
source("C:/Users/User/github/Function/R/t_square.R")
source("C:/Users/User/github/Function/R/msetRegression.R")

library(corrplot)
library(MASS)
library(pracma)
library(autoencoder)

df = read.csv("C:/Users/user/github/Anomaly-Detection/data/ph1.csv", fileEncoding = 'CP949') # train
df2 = read.csv("C:/Users/user/github/Anomaly-Detection/data/ph2.csv", fileEncoding = 'CP949') # normal test
df3 = read.csv("C:/Users/user/github/Anomaly-Detection/data/ph2_out.csv", fileEncoding = 'CP949') # abnormal test

head(df)

df_tr = df[,8:49] # 필요 정보만 select
df_te = df2[,8:49] # 필요 정보만 select
df_te_2 = df3[,8:49] # 필요 정보만 select

te = rbind(df_te, df_te_2)

df_t2 = t_square(df_tr, te, 0.05)

plot(df_t2$Tsq_mat, type = 'l', ylim = c(0,600))

abline(h = c(df_t2$CL), col = 'red')

length(which(df_t2$Tsq_mat > df_t2$CL))/2000
length(which(df_t2$Tsq_mat < df_t2$CL))/2000


# 유의수준 변경에 따른 alpha, beta error 
mat = matrix(0,1000,3)

for (i in 1:1000) {
  t2 = t_square(df_tr, te, i/1000)
  mat[i,1] = i/1000
  mat[i,2] = length(which(t2$Tsq_mat[1:1000] > t2$CL))/2000 # alpha error
  mat[i,3] = length(which(t2$Tsq_mat[1001:2000] < t2$CL))/2000 # beta error
}

plot(mat[,2:3], type = 'o') # 유의 수준 변경에 따른 alpha, beta error 변화


