###### Set up Work Space and Data #####
library(R.matlab)
library(ggplot2)

setwd("C:/Users/patri/Google Drive/School/S19 - STA141A S. Chen/Project/") 
ephysroot = "C:/Users/patri/Google Drive/School/S19 - STA141A S. Chen/Project/Neuralpixel/Data/"; # path of the data set 
mstr = 'Krebs'; # Mouse names
exp_data<- readMat(paste(ephysroot,mstr,'_reduced.mat',sep=''));

###### FaceSVD values over time ######
plot(exp_data$faceSVD[1:3000], type="l", main = "FaceSVD Values Over First 90s" , xlab = "Time in 30ms Windows", ylab = "faceSVD value")

###### Setting the Time Frame ######
time_frame = 1:3000

###### Firing Rate Plots for individual regions; CP ######
num_fired_vec <- rep(0, length(time_frame))
region_CP <- exp_data$stall.CP #Change brain region here

for(i in 1:length(time_frame)) {
  num_fired_CP <- sum(region_CP[,i])
  num_fired_vec[i] <- num_fired_CP
}
firing_rate_vec <- num_fired_vec / (.03) #Each time interval is 30ms = 0.03s

#CP Firing Rate Plot w/ smooth best-fit line
ggplotCP = ggplot(as.data.frame(exp_data$stall.CP[1:3000]), aes(x = time_frame, y = firing_rate_vec))
ggplotCP + geom_line() + geom_smooth() + labs(title="CP Firing Rate Plot", x="Time in 30ms Windows", y="Frequency (Hz)")

###### Second Region; FrMoCtx ######
num_fired_vec2 <- rep(0, length(time_frame))
region_FrMoCtx <- exp_data$stall.FrMoCtx #Change brain region here

for(i in 1:length(time_frame)) {
  num_fired_FrMoCtx <- sum(region_FrMoCtx[,i])
  num_fired_vec2[i] <- num_fired_FrMoCtx
}
firing_rate_vec2 <- num_fired_vec2 / (.03) # Each time interval is 30ms = 0.03s

#FrMoCtx Firing Rate Plot w/ smooth best-fit line
ggplotFrMoCtx = ggplot(as.data.frame(exp_data$stall.FrMoCtx[1:3000]), aes(x = time_frame, y = firing_rate_vec))
ggplotFrMoCtx + geom_line() + geom_smooth() + labs(title="FrMoCtx Firing Rate Plot", x="Time in 30ms Windows", y="Frequency (Hz)")

##### Third Region; SC ######
num_fired_vec3 <- rep(0, length(time_frame))
region_SC <- exp_data$stall.SC #Change brain region here

for(i in 1:length(time_frame)) {
  num_fired_SC <- sum(region_SC[,i])
  num_fired_vec3[i] <- num_fired_SC
}
firing_rate_vec3 <- num_fired_vec3 / (.03) #Each time interval is 30ms = 0.03s

#CS Firing Rate Plot w/ smooth best-fit line
ggplotSC = ggplot(as.data.frame(exp_data$stall.SC[1:3000]), aes(x = time_frame, y = firing_rate_vec3))
ggplotSC + geom_line() + geom_smooth() + labs(title="CS Firing Rate Plot", x="Time in 30ms Windows", y="Frequency (Hz)")

##### Fourth Region; V1 ######
num_fired_vec4 <- rep(0, length(time_frame))
region_V1 <- exp_data$stall.V1 #Change brain region here

for(i in 1:length(time_frame)) {
  num_fired_V1 <- sum(region_V1[,i])
  num_fired_vec4[i] <- num_fired_V1
}
firing_rate_vec4 <- num_fired_vec4 / (.03) #Each time interval is 30ms = 0.03s

#V1 Firing Rate Plot w/ smooth best-fit line
ggplotV1 = ggplot(as.data.frame(exp_data$stall.V1[1:3000]), aes(x = time_frame, y = firing_rate_vec4))
ggplotV1 + geom_line() + geom_smooth() + labs(title="V1 Firing Rate Plot", x="Time in 30ms Windows", y="Frequency (Hz)")

##### Fifth Region; MB ######
num_fired_vec5 <- rep(0, length(time_frame))
region_MB <- exp_data$stall.MB #Change brain region here

for(i in 1:length(time_frame)) {
  num_fired_MB <- sum(region_MB[,i])
  num_fired_vec5[i] <- num_fired_MB
}
firing_rate_vec5 <- num_fired_vec5 / (.03) #Each time interval is 30ms = 0.03s

#MB Firing Rate Plot w/ smooth best-fit line
ggplotMB = ggplot(as.data.frame(exp_data$stall.MB[1:3000]), aes(x = time_frame, y = firing_rate_vec5))
ggplotMB + geom_line() + geom_smooth() + labs(title="MB Firing Rate Plot", x="Time in 30ms Windows", y="Frequency (Hz)")

##### Sixth Region; TH ######
num_fired_vec6 <- rep(0, length(time_frame))
region_TH <- exp_data$stall.TH #Change brain region here

for(i in 1:length(time_frame)) {
  num_fired_TH <- sum(region_TH[,i])
  num_fired_vec6[i] <- num_fired_TH
}
firing_rate_vec6 <- num_fired_vec6 / (.03) #Each time interval is 30ms = 0.03s

#TH Firing Rate Plot w/ smooth best-fit line
ggplotTH = ggplot(as.data.frame(exp_data$stall.TH[1:3000]), aes(x = time_frame, y = firing_rate_vec6))
ggplotTH + geom_line() + geom_smooth() + labs(title="TH Firing Rate Plot", x="Time in 30ms Windows", y="Frequency (Hz)")

##### Seventh Region; HPF ######
num_fired_vec7 <- rep(0, length(time_frame))
region_HPF <- exp_data$stall.HPF #Change brain region here

for(i in 1:length(time_frame)) {
  num_fired_HPF <- sum(region_HPF[,i])
  num_fired_vec7[i] <- num_fired_HPF
}
firing_rate_vec7 <- num_fired_vec7 / (.03) #Each time interval is 30ms = 0.03s

#HPF Firing Rate Plot w/ smooth best-fit line
ggplotHPF = ggplot(as.data.frame(exp_data$stall.HPF[1:3000]), aes(x = time_frame, y = firing_rate_vec7))
ggplotHPF + geom_line() + geom_smooth() + labs(title="HPF Firing Rate Plot", x="Time in 30ms Windows", y="Frequency (Hz)")

##### Eigth Region; LS ######
num_fired_vec8 <- rep(0, length(time_frame))
region_LS <- exp_data$stall.LS #Change brain region here

for(i in 1:length(time_frame)) {
  num_fired_LS <- sum(region_LS[,i])
  num_fired_vec8[i] <- num_fired_LS
}
firing_rate_vec8 <- num_fired_vec8 / (.03) #Each time interval is 30ms = 0.03s

#LS Firing Rate Plot w/ smooth best-fit line
ggplotLS = ggplot(as.data.frame(exp_data$stall.LS[1:3000]), aes(x = time_frame, y = firing_rate_vec8))
ggplotLS + geom_line() + geom_smooth() + labs(title="LS Firing Rate Plot", x="Time in 30ms Windows", y="Frequency (Hz)")

##### Ninth Region; SomMoCtx ######
num_fired_vec9 <- rep(0, length(time_frame))
region_SomMoCtx <- exp_data$stall.SomMoCtx #Change brain region here

for(i in 1:length(time_frame)) {
  num_fired_SomMoCtx <- sum(region_SomMoCtx[,i])
  num_fired_vec9[i] <- num_fired_SomMoCtx
}
firing_rate_vec9 <- num_fired_vec9 / (.03) #Each time interval is 30ms = 0.03s

#SomMoCtx Firing Rate Plot w/ smooth best-fit line
ggplotSomMoCtx = ggplot(as.data.frame(exp_data$stall.SomMoCtx[1:3000]), aes(x = time_frame, y = firing_rate_vec9))
ggplotSomMoCtx + geom_line() + geom_smooth() + labs(title="SomMoCtx Firing Rate Plot", x="Time in 30ms Windows", y="Frequency (Hz)")

### All Regions w/ Best Fit
All = ggplot(as.data.frame(exp_data$stall.CP[1:3000]), aes(x = time_frame, y = firing_rate_vec1), color = "blue1")
All + geom_line(alpha = 0) + geom_smooth() +
  geom_line(data = as.data.frame(exp_data$stall.FrMoCtx[1:3000]), aes(x = time_frame, y = firing_rate_vec2), alpha = 0.0) +
  geom_smooth(method="gam", formula=firing_rate_vec2 ~ s(x, bs = "cs"),colour="green") +
  geom_line(data = as.data.frame(exp_data$stall.SC[1:3000]), aes(x = time_frame, y = firing_rate_vec6), alpha = 0.0) +
  geom_smooth(method="gam", formula=firing_rate_vec6 ~ s(x, bs = "cs"),colour="red") +
  geom_line(data = as.data.frame(exp_data$stall.V1[1:3000]), aes(x = time_frame, y = firing_rate_vec9), alpha = 0.0) +
  geom_smooth(method="gam", formula=firing_rate_vec9 ~ s(x, bs = "cs"),colour="yellow") +
  geom_line(data = as.data.frame(exp_data$stall.MB[1:3000]), aes(x = time_frame, y = firing_rate_vec5), alpha = 0.0) +
  geom_smooth(method="gam", formula=firing_rate_vec5 ~ s(x, bs = "cs"),colour="orange") + 
  geom_line(data = as.data.frame(exp_data$stall.TH[1:3000]), aes(x = time_frame, y = firing_rate_vec8), alpha = 0.0) +
  geom_smooth(method="gam", formula=firing_rate_vec8 ~ s(x, bs = "cs"),colour="purple") + 
  geom_line(data = as.data.frame(exp_data$stall.HPF[1:3000]), aes(x = time_frame, y = firing_rate_vec3), alpha = 0.0) +
  geom_smooth(method="gam", formula=firing_rate_vec3 ~ s(x, bs = "cs"),colour="brown") + 
  geom_line(data = as.data.frame(exp_data$stall.LS[1:3000]), aes(x = time_frame, y = firing_rate_vec4), alpha = 0.0) +
  geom_smooth(method="gam", formula=firing_rate_vec4 ~ s(x, bs = "cs"),colour="pink") +
  geom_line(data = as.data.frame(exp_data$stall.SomMoCtx[1:3000]), aes(x = time_frame, y = firing_rate_vec7), alpha = 0.0) +
  geom_smooth(method="gam", formula=firing_rate_vec7 ~ s(x, bs = "cs"),colour="steelblue2") + 
  ggtitle("Best-Fit Firing Rate For all Brain Regions") + coord_cartesian(xlim=c(0,3000), ylim = c(0, 3000)) 



All = ggplot(as.data.frame(exp_data$stall.CP[1:3000]), aes(x = time_frame, y = firing_rate_vec), color = "blue1")
All + geom_line(data = as.data.frame(exp_data$stall.MB[1:3000]), aes(x = time_frame, y = firing_rate_vec5), alpha = 0.0) +
  geom_smooth(method="gam", formula=firing_rate_vec5 ~ s(x, bs = "cs"),colour="orange") + 
  geom_line(data = as.data.frame(exp_data$stall.TH[1:3000]), aes(x = time_frame, y = firing_rate_vec6), alpha = 0.0) +
  geom_smooth(method="gam", formula=firing_rate_vec6 ~ s(x, bs = "cs"),colour="brown") + 
  geom_line(data = as.data.frame(exp_data$stall.LS[1:3000]), aes(x = time_frame, y = firing_rate_vec8), alpha = 0.0) +
  geom_smooth(method="gam", formula=firing_rate_vec8 ~ s(x, bs = "cs"),colour="pink") + 
  ggtitle("Best-Fit Firing Rate For MB, TH, LS") + coord_cartesian(xlim=c(0,3000), ylim = c(0, 3000)) 

###### Overall Firing Rate Plot for ALL Regions ######################################################
brain_regions_index = 4:12
overall_num_fired_vec <- rep(0, length(time_frame))
overall_sum_vec <- rep(0, length(time_frame))

for(i in 1:length(brain_regions_index)) {
  overall_region <- exp_data[[(brain_regions_index[i])]]
  
  for(i in 1:length(time_frame)) {
    overall_num_fired <- sum(overall_region[,i])
    overall_num_fired_vec[i] <- overall_num_fired
  }
  overall_sum_vec <- overall_sum_vec + overall_num_fired_vec
}

overall_firing_rate_vec <- overall_sum_vec / (.03) #Each time interval is 30ms = 0.3s

OFRP_BF = ggplot(as.data.frame(overall_num_fired), aes(x = time_frame, y = overall_firing_rate_vec), color = "mediumblue")
All + geom_line(alpha = 0) + geom_smooth()

###### Overlaying one region onto overall firing rate plot ######
plot(x = time_frame, y = overall_firing_rate_vec, ylim = c(0,25000), main = "Overall Firing Rate vs SomMoCtx Firing Rate", xlab = "Time", ylab = "Frequency (HZ)", type="l", col = rgb(0, 0, 0, alpha = 0.5), ann=T, cex.lab=0.8, lwd=1) #Overall Firing Rate plot
lines(x = time_frame, y = firing_rate_vec, col=rgb(red=1, green = 0, blue = 0, alpha = 0.5), lty=1)
legend("topleft" ,legend=c("Overall","SomMoCtx"), col=c("black","red", alpha = 0.5), cex = 0.7, lty=c(1,1), ncol=2)


###### Plotting and overlaying multiple regions FRP onto one plot ######
plot(x = time_frame, y = firing_rate_vec, ylim = c(0,3500),
     main = "LS vs SomMoCtx Firing Rate Plots", xlab = "Time", ylab = "Frequency (HZ)", type="l",
     col=rgb(red = 0, green = 0, blue=1, alpha = 0.5) , ann=T, cex.lab=0.8, lwd=1)

lines(x = time_frame, y = firing_rate_vec2, col=rgb(red=1, green = 0, blue = 0, alpha = 0.5), lty=1) # Second region overlay
#lines(x = time_frame, y = firing_rate_vec3, col=rgb(red=0, green = 1, blue = 0, alpha = 0.5), lty=1) # Third region overlay
legend("topleft" ,legend=c("LS","SomMoCtx"), col=c("blue","red"), cex = 0.7, lty=c(1,1), ncol=2)
