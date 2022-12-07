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

plot(df_t2$Tsq_mat, type = 'o', ylim = c(0,600))
abline(h = c(df_t2$CL), col = 'red')

length(which(df_t2$Tsq_mat > df_t2$CL))/3000
length(which(df_t2$Tsq_mat < df_t2$CL))/3000


# 유의수준 변경에 따른 alpha, beta error 
mat = matrix(0,1000,3)

t2 = t_square(df_tr, te, i/1000)
plot(t2$Tsq_mat,ylim = c(0,1000))
s2 = bootlimit1(s1,i/1000,100)

s1 = t_square(df_tr, df_tr, i/1000)

plot(t2$Tsq_mat)

for (i in 1:1000) {
  s2 = bootlimit1(s1$Tsq_mat,i/1000,100)
  mat[i,1] = i/1000
  mat[i,2] = length(which(t2$Tsq_mat[1:1000] > s2))/1000 # alpha error
  mat[i,3] = length(which(t2$Tsq_mat[1001:2000] < s2))/1000 # beta error
}

plot(mat[,2:3])

plot(mat[,2:3], type = 'o', xlim = c(0,1), ylim = c(0,1)) # 유의 수준 변경에 따른 alpha, beta error 변화

tft_cbm = cbm(df_tr, te, 0.05, 3)

bootlimit1(df_tr, 0.05, 100)

mat2 = matrix(0,1000,3)

plot(tft_cbm$cbm_res,ylim=c(0,1000),type='o')

tft_cbm = cbm(df_tr, te, 0.05, 5)
s3 = cbm(df_tr, df_tr, 0.05, 5)
for(i in 1:1000){
  #tft_cbm = cbm(df_tr, te, i/100, 3)
  tft_boot = bootlimit1(s3$cbm_res, i/1000, 100)
  mat2[i,1] = i/1000
  mat2[i,2] = length(which(tft_cbm$cbm_res[1:1000]>tft_boot))/1000
  mat2[i,3] = length(which(tft_cbm$cbm_res[1001:2000]<tft_boot))/1000
}
mat2
plot(mat[,2:3],type='o')
points(mat2[,2:3],col='red',type='o')

tail(mat2[,2])
tail(mat2[,3])

plot(mat2[,2])

###
t_square_solve = function(trdat, tedat,alpha){
  obs = nrow(trdat)
  dim = ncol(trdat)
  mu = colMeans(trdat)
  
  CL = qf(1-alpha, dim, obs -dim)*(dim*(obs+1)*(obs-1)/(obs*(obs-dim)))
  sinv = solve(cov(trdat))
  mu_mat = repmat(mu, nrow(tedat),1)
  dte = as.matrix(tedat-mu_mat)
  
  Tsq_mat = matrix(numeric(0), nrow(tedat),1)
  
  for (i in 1:nrow(tedat)) {
    Tsq_mat[i,1] = as.double(dte[i,]) %*% sinv %*% t(t(as.double(dte[i,])))
  }
  
  ret = list(
    Tsq_mat = Tsq_mat,
    CL = CL
  )
  return(ret)
}

###
mat3 = matrix(0,1000,3)

t2_solve = t_square_solve(df_tr, te, i/1000)
plot(t2_solve$Tsq_mat,ylim = c(0,1000))

s1_solve = t_square_solve(df_tr, df_tr, i/1000)
s2_solve = bootlimit1(s1_solve,i/1000,100)


plot(t2_solve$Tsq_mat,ylim=c(0,1000))

for (i in 1:1000) {
  s2_solve = bootlimit1(s1_solve$Tsq_mat,i/1000,100)
  mat3[i,1] = i/1000
  mat3[i,2] = length(which(t2_solve$Tsq_mat[1:1000] > s2_solve))/1000 # alpha error
  mat3[i,3] = length(which(t2_solve$Tsq_mat[1001:2000] < s2_solve))/1000 # beta error
}

plot(mat[,2:3],type='o')
points(mat3[,2:3],col='red',type='o')


