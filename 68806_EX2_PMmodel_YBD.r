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

# calculate et using the PM function of N.Tague
et<-penman_montieth (Tair=metdata$tavg,vpd = metdata$vpd, Rnet=metdata$rnet,gs=metdata$gs, ga=metdata$ga, dayl=daylengthbynum, CP=1010, Pair=101325)

# evaluate a new gs using a basic if function
new_gs <-modified_gs(gs=metdata$gs, Tair=metdata$tavg)
et_newgs<-penman_montieth (Tair=metdata$tavg,vpd = metdata$vpd, Rnet=metdata$rnet,gs=new_gs, ga=metdata$ga, dayl=daylengthbynum, CP=1010, Pair=101325)

# plot both et calculated values
plot(et, main='comparison of original and modified ET',col="blue",ylab='ET', xlab='day')
points(et_newgs, main='comparison of original and modified ET',col="red")
# plot a comparison of the two et values
plot(et,et_newgs,type="p",col="red", main='comparison of original and modified ET',xlab='PM derived ET', ylab='modified ET using new gs value')

#adjust gs such that 
# evaluate a new gs using a basic if function
new_gs2 <-modified_gs2(gs=metdata$gs, Tair=metdata$tavg, vpd=metdata$vpd)
et_newgs2<-penman_montieth (Tair=metdata$tavg,vpd = metdata$vpd, Rnet=metdata$rnet,gs=new_gs2, ga=metdata$ga, dayl=daylengthbynum, CP=1010, Pair=101325)

# plot both et calculated values
plot(et,type='l', main='comparison of original and modified ET',col="blue",ylab='ET', xlab='day')
lines(et_newgs, col="red")
lines(et_newgs2, col="green")

# plot a comparison of the two et values
plot(et,et_newgs2,type="p",col="red", main='comparison of original and modified ET',xlab='PM derived ET', ylab='modified ET using new gs value')

