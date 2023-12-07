## 23210980044 加兴华

library(xlsx);library(quadprog)

data <- read.xlsx('./hw1.xlsx',1);data
R <- 100*data[,2:4]; head(R)
mean_vect <- apply(R,2,mean); print(mean_vect)
cov_mat <- cov(R); print(cov_mat)
num_assets <- 3
Amat <- rbind(rep(1,num_assets),diag(num_assets))
# 使用quadprog包求解每个设定μ下的sd和权重
muP <- seq(0,0.25,length=300)
sdP <- muP
Amat <- rbind(rep(1,3),mean_vect)
weights <- matrix(0,nrow=300,ncol=num_assets)
for(i in 1:length(muP)){
  bvec=c(1,muP[i])
  result=solve.QP(Dmat=2*cov_mat,dvec=rep(0,num_assets),Amat=t(Amat),bvec=bvec,meq=2)
  sdP[i]=sqrt(result$value)
  weights[i,]=result$solution
}
head(weights)
min_var_ind <- (sdP==min(sdP)); print(sdP[min_var_ind])  #最小方差组合
efficient_fronter_ind <- (muP>muP[min_var_ind])

sharpe <- muP[efficient_fronter_ind]/sdP[efficient_fronter_ind ]; # 计算μf=0时的夏普比率
tan_ind <- (sharpe==max(sharpe)); print(weights[tan_ind,])  # 切点组合

# 画图
plot(sdP,muP,type="l") # 抛物线
abline(v=sdP[min_var_ind],col="blue") # 最小方差垂线
lines(sdP[efficient_fronter_ind ],muP[efficient_fronter_ind],col='red') # 有效前沿
points(sdP[tan_ind ],muP[tan_ind], col = "green", pch=4, lwd=3) # 最优组合点

