source("C:/Users/User/github/Function/R/t_square.R")

ir = iris[,1:4]

library(corrplot)

corrplot(cor(ir))

library(MASS)
library(pracma)

ir_t2 = t_square(ir[1:55,],ir,0.05)

plot(ir_t2$Tsq_mat,type='o')
abline(h=c(ir_t2$CL),col='red')
abline(v=c(50),col='blue',lwd=3)

#alpha가 작을 수록 false alarm의 갯수가 적어짐

####alpha error 성능은~~
length(which(ir_t2$Tsq_mat[1:50]>ir_t2$CL))/50


####beta error 성능은~~
length(which(ir_t2$Tsq_mat[51:150]<ir_t2$CL))/100

mat_mat = matrix(0,100,3)
for(i in 1:100){
  ir_t2 = t_square(ir[1:55,],ir,i/100)
  mat_mat[i,1] = i/100
  mat_mat[i,2] = length(which(ir_t2$Tsq_mat[1:50]>ir_t2$CL))/50
  mat_mat[i,3] = length(which(ir_t2$Tsq_mat[51:150]<ir_t2$CL))/100
}

plot(mat_mat[,2:3],type='o')

###
trdat = ir
tedat = ir
k=3
alpha = 0.05
cbm = function(trdat, tedat, alpha,k) {
  
  tr_k = kmeans(trdat,k)
  s1 = cbind(trdat,tr_k$cluster)
  cbm_mat = matrix(0,nrow(tedat),k)
  
  for(i in 1:k){
    s2 = subset(s1,s1$`tr_k$cluster`==i)
    
    obs = nrow(s2)
    dim = ncol(trdat)
    
    mu = colMeans(s2[,1:dim])
    
    CL = qf(1-alpha,dim,obs - dim)*(dim*(obs+1)*(obs-1)/(obs*(obs-dim)))
    
    sinv = ginv(cov(s2[,1:dim]))  
    
    mu_mat = repmat(mu, nrow(tedat),1)
    dte = tedat-mu_mat
    
    Tsq_mat = matrix(numeric(0), nrow(tedat),1) 
    
    for( j in 1:nrow(tedat)) {
      Tsq_mat[j,1] = as.double(dte[j,]) %*% sinv %*% t(t(as.double(dte[j,])))
    }
    cbm_mat[,i] = Tsq_mat
  }
  cbm_res = apply(cbm_mat,1,min)
  ret = list(cbm_res=cbm_res)

  return (ret)                                    
}

trdat_test = cbm(trdat, tedat, 0.01,3)

bootlimit1 = function(stat, alpha, m){ # stat : 추론이 필요한 통계량, alpha : 유의확률, m : 복원추출 횟수 
  perc_matrix = matrix(numeric(0), 1, m)    
  
  for(i in 1:m){
    sample_temp = sample(stat, size = nrow(as.data.frame(stat)), replace = TRUE, prob = NULL) # 각 데이터에 대한 복원 추출 수행
    sample_temp = as.data.frame(sample_temp) 
    
    perc_matrix[,i] <- quantile(sample_temp[,1] , 1-alpha);  # 해당 샘플 데이터에 대한 특정 quantile(분위수) 추론 (alpha에 해당하는)            
    
  }
  
  CL= mean(perc_matrix)  # 복원추출된 붓스트랩 샘플에 대한 특정 분위수의 평균값 추정
  return(CL)
  
}

s1 = bootlimit1(trdat, 0.05, 100)


####

for(i in 1:100){
  ir_cbm = cbm(ir[1:55,],ir,i/100,k=3)
  cbm_boot = boot_cl(ir_cbm$cbm_res,i/100)
  mat_mat[i,1] = i/100
  mat_mat[i,2] = length(which(ir_cbm$cbm_res[1:50]>cbm_boot$cl))/50
  mat_mat[i,3] = length(which(ir_cbm$cbm_res[51:150]<cbm_boot$cl))/100
}

points(mat_mat[,2:3],col='red',type='o')
