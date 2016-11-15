##############################
# estimate daylength in Jerusalem
# day length in Jerusalem ranges between 10 to 14 h
# estimate day length for each half of the year and concatanate
daylength<-seq(from = 10, to = 14, length.out =((365-1)/2))
daylength2<-seq(from = 14, to = 10, length.out =((365)/2))
daylengthJer<-c(daylength,daylength2)

#fit the right daylength according to the day of the year
daynum<-metdata$yd
daylengthbynum=rep(0,length(daynum))

for (ii in 1:length(daylengthbynum)) {
  daylengthbynum[ii]=daylengthJer[daynum[ii]]
}

#add day hours to data matrix
metdata=cbind(metdata,daylengthbynum)


##############################
# calculate et using the PM function of N.Tague
et<-penman_montieth (Tair=metdata$tavg,vpd = metdata$vpd, Rnet=metdata$rnet,gs=metdata$gs, ga=metdata$ga, dayl=metdata$daylengthbynum, CP=1010, Pair=101325)

# evaluate a new gs using a basic if function
new_gs <-modified_gs(gs=metdata$gs, Tair=metdata$tavg)
et_newgs<-penman_montieth (Tair=metdata$tavg,vpd = metdata$vpd, Rnet=metdata$rnet,gs=new_gs, ga=metdata$ga, dayl=metdata$daylengthbynum, CP=1010, Pair=101325)

# plot both et calculated values
plot(et, main='comparison of original and modified ET',col="blue",ylab='ET', xlab='day')
points(et_newgs, main='comparison of original and modified ET',col="red")
# plot a comparison of the two et values
plot(et,et_newgs,type="p",col="red", main='comparison of original and modified ET',xlab='PM derived ET', ylab='modified ET using new gs value')

#adjust gs such that 
# evaluate a new gs using a basic if function
new_gs2 <-modified_gs2(gs=metdata$gs, Tair=metdata$tavg, vpd=metdata$vpd)
et_newgs2<-penman_montieth (Tair=metdata$tavg,vpd = metdata$vpd, Rnet=metdata$rnet,gs=new_gs2, ga=metdata$ga, dayl=metdata$daylengthbynum, CP=1010, Pair=101325)

# plot both et calculated values
plot(et,type='l', main='comparison of original and modified ET',col="blue",ylab='ET', xlab='day')
lines(et_newgs, col="red")
lines(et_newgs2, col="green")

# plot a comparison of the two et values
plot(et,et_newgs2,type="p",col="red", main='comparison of original and modified ET',xlab='PM derived ET', ylab='modified ET using new gs value')

##############################
# sensitivity analysis of the PM equation
# calculate et using the PM function of N.Tague and estimate its sensitivity to several factors

#define a color vector for plotting data
colorvec<-c(rep('blue', 20), 'black',rep('red', 20))


senseitivityfactor=seq(from=0.00 ,to=2.0 ,by=0.05)
ettemp=matrix(data=NA,nrow=length(et),ncol=length(senseitivityfactor))
for (ii in 1:length(senseitivityfactor)){
ettemp[1:length(et),ii]<-penman_montieth (Tair=metdata$tavg*senseitivityfactor[ii],vpd = metdata$vpd, Rnet=metdata$rnet,gs=metdata$gs, ga=metdata$ga, dayl=metdata$daylengthbynum, CP=1010, Pair=101325)
}

plot(et,type="l",col="black", main='comparison of original and modified ET',xlab='PM derived ET', ylab='modified ET using new gs value')
for(ii in 1:length(senseitivityfactor)){
lines(ettemp[,ii],col=colorvec[ii])}
lines(et,type="l",col="black", main='comparison of original and modified ET',xlab='PM derived ET', ylab='modified ET using new gs value')

###################################
senseitivityfactor=seq(from=0.00 ,to=2.0 ,by=0.05)
et_sensitivity=array(data=0,dim=c(length(et),length(senseitivityfactor),6))
# define the colums to be evaluated for sensitivity
sensitivity_vec=c(8,9,10,11,12,17)
jj=1;

for (jj in 1:length(sensitivity_vec)) {
  for (ii in 1:length(senseitivityfactor)){
    temp_metdata=metdata
    temp_metdata[,sensitivity_vec[jj]]=metdata[,sensitivity_vec[jj]]*senseitivityfactor[ii]
    et_sensitivity[1:length(et),ii,jj]<-penman_montieth (Tair=temp_metdata$tavg,vpd = temp_metdata$vpd, Rnet=temp_metdata$rnet,gs=temp_metdata$gs, ga=temp_metdata$ga, dayl=temp_metdata$daylengthbynum, CP=1010, Pair=101325)
  }
}

plot(et,type="l",col="black", main='comparison of original and modified ET',xlab='PM derived ET', ylab='modified ET using new gs value')
for(ii in 1:length(senseitivityfactor)){
  lines(ettemp[,ii],col=colorvec[ii])}
lines(et,type="l",col="black", main='comparison of original and modified ET',xlab='PM derived ET', ylab='modified ET using new gs value')

