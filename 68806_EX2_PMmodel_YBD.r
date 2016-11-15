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
