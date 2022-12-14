# rm(list=ls())
# src_dir = 'C:\\Users\\User\\github\\data\\rul_hrs\\rul_hrs.csv' # 경로 설정
# src_file = list.files(src_dir)
# for(i in 1:length(src_file)) source(paste(src_dir, src_file[i], sep=''), encoding='utf-8')


wd =  'C:\\Users\\User\\github\\data\\rul_hrs\\rul_hrs.csv' # 경로 설정

data = read.csv(wd)

# rul 
rul = data$rul

# 고장 7개에 대한 센싱 값 추출
failuredf = list()
failuredf[[1]] = data[1:which(rul==0)[1],3:52]
for(i in 1:6){
  failuredf[[(i+1)]] = data[(which(rul==0)[i]+1):which(rul==0)[i+1],3:52]
}

# # 고장 2에 대한 센서
# failureidx = 2
# 
# # 정상분포 설정
# train = failuredf[[failureidx]]$sensor_05[1:1000]
# train_mat = as.matrix(train)
# 
# # 모니터링 구간간
# test = failuredf[[failureidx]]$sensor_05
# test_mat = as.matrix(test)
# 
# # MSET linear regression
# msetLR= mset_regress(train_mat, test_mat)
# 
# # degradation model with mset linear regression
# trDegradation = degradation_model(msetLR$residual_tr)
# tsDegradation = degradation_model(msetLR$residual_ts)

setwd("C:\\Users\\User\\github\\Anomaly-Detection\\RUL")

for (i in 1:7) {
  failureidx = i
  for (j in 1:50) {
    train = as.matrix(failuredf[[failureidx]][1:1000,j])
    test = as.matrix(failuredf[[failureidx]][,j])
    msetLR = mset_regress(train, test)
    trDegradation = degradation_model(msetLR$residual_tr)
    tsDegradation = degradation_model(msetLR$residual_ts)
    png(paste0("cycle ",i," sensor ",j,".png"))
    plot(tsDegradation)
    dev.off()
  }
}

