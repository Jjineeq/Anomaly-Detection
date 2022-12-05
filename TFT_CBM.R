###################
##  TFT CBM분석  ##
###################
source("C:/Users/User/github/Anomaly-Detection/CBM.R")
library(corrplot)
library(MASS)
library(pracma)

df = read.csv("C:/Users/user/github/Anomaly-Detection/data/ph1.csv",header = T, fileEncoding = 'CP949') # train
df2 = read.csv("C:/Users/user/github/Anomaly-Detection/data/ph2.csv",header = T, fileEncoding = 'CP949') # normal test
df3 = read.csv("C:/Users/user/github/Anomaly-Detection/data/ph2_out.csv",header = T, fileEncoding = 'CP949') # abnormal test

head(df)

df_tr = df[,8:49] # 필요 정보만 select
df_te = df2[,8:49] # 필요 정보만 select
df_te_2 = df3[,8:49] # 필요 정보만 select

head(df_tr)
head(df_te)

corrplot(cor(df_tr))
corrplot(cor(df_te))

df_t2 = t_square(df_tr,df_te,0.05)
df_t2_ab = t_square(df_tr, df_te_2, 0.05)

### 정상 plot 이후 상한선 설정 
plot(df_t2$Tsq_mat, type = 'o') 
abline(h = c(df_t2$CL), col = 'red')

### 비정상 plot 이후 상한선 설정 
plot(df_t2_ab$Tsq_mat, type = 'o') #418번째 오류 있음
abline(h = c(df_t2_ab$CL), col = 'red') # 확인 어려움

#### 일단 정상만 진행
length(which(df_t2$Tsq_mat>df_t2$CL))/1000
length(which(df_t2_ab$Tsq_mat>df_t2$CL))/1000

mat_mat = matrix(0,1000,3)
for (i in 1:100) {
  df_t2 = t_square(df_te, df_tr, i/100)
  mat_mat[i,1] = i/100
  mat_mat[i,2] = length(which(df_t2$Tsq_mat>df_t2$CL))/1000 # alpha error
  mat_mat[i,3] = length(which(df_t2_ab$Tsq_mat<df_t2$CL))/1000 # beta error
}

plot(mat_mat[,2:3],type = 'o')

mat_mat1 = matrix(0,1000,3)
for(i in 1:100){
  ir_cbm = cbm(df_tr,df_te,i/100,k=30)
  cbm_boot = boot_cl(ir_cbm$cbm_res,i/100)
  mat_mat1[i,1] = i/100
  mat_mat1[i,2] = length(which(ir_cbm$cbm_res[1:50]>cbm_boot$cl))/1000
  mat_mat1[i,3] = length(which(ir_cbm$cbm_res[51:150]<cbm_boot$cl))/1000
}

points(mat_mat1[,2:3],col='red',type='o')

