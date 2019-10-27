###### Set up Work Space and Data #####
# install.packages("R.matlab")
setwd("C:/Users/patri/Google Drive/School/S19 - STA141A S. Chen/Project/") 
library(R.matlab)
ephysroot = "C:/Users/patri/Google Drive/School/S19 - STA141A S. Chen/Project/Neuralpixel/Data/"; # path of the data set 
mstr = 'Krebs'; # Mouse names
exp_data<- readMat(paste(ephysroot,mstr,'_reduced.mat',sep=''));

###### FaceSVD values over time ######
plot(exp_data$faceSVD[0:3000], type="l")

###### Firing Rate Plots for individual regions; First Region ######
time_frame = 1:3000 #Change time frame here
num_fired_vec <- rep(0, length(time_frame))
region <- exp_data$stall.LS #Change brain region here

for(i in 1:length(time_frame)) {
  num_fired <- sum(region[,i])
  num_fired_vec[i] <- num_fired
}
firing_rate_vec <- num_fired_vec / (.03) #Each time interval is 30ms = 0.03s

###### Second Region ######
num_fired_vec2 <- rep(0, length(time_frame))
region2 <- exp_data$stall.SomMoCtx #Change brain region here

for(i in 1:length(time_frame)) {
  num_fired2 <- sum(region2[,i])
  num_fired_vec2[i] <- num_fired2
}
firing_rate_vec2 <- num_fired_vec2 / (.03) # Each time interval is 30ms = 0.03s

##### Third Region ######
num_fired_vec3 <- rep(0, length(time_frame))
region3 <- exp_data$stall.SC #Change brain region here

for(i in 1:length(time_frame)) {
  num_fired3 <- sum(region3[,i])
  num_fired_vec3[i] <- num_fired3
}
firing_rate_vec3 <- num_fired_vec3 / (.03) #Each time interval is 30ms = 0.03s

##### Plotting and overlaying multiple regions onto one plot ######
plot(x = time_frame, y = firing_rate_vec, ylim = c(0,3500), main = "LS vs SomMoCtx Firing Rate Plots", xlab = "Time", ylab = "Frequency (HZ)", type="l", col=rgb(red = 0, green = 0, blue=1, alpha = 0.5) , ann=T, cex.lab=0.8, lwd=1)
lines(x = time_frame, y = firing_rate_vec2, col=rgb(red=1, green = 0, blue = 0, alpha = 0.5), lty=1) # Second region overlay
#lines(x = time_frame, y = firing_rate_vec3, col=rgb(red=0, green = 1, blue = 0, alpha = 0.5), lty=1) # Third region overlay
legend("topleft" ,legend=c("LS","SomMoCtx"), col=c("blue","red"), cex = 0.7, lty=c(1,1), ncol=2)

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

###### Overlaying one region onto overall firing rate plot ######
plot(x = time_frame, y = overall_firing_rate_vec, ylim = c(0,25000), main = "Overall Firing Rate vs SomMoCtx Firing Rate", xlab = "Time", ylab = "Frequency (HZ)", type="l", col = rgb(0, 0, 0, alpha = 0.5), ann=T, cex.lab=0.8, lwd=1) #Overall Firing Rate plot
lines(x = time_frame, y = firing_rate_vec, col=rgb(red=1, green = 0, blue = 0, alpha = 0.5), lty=1) #Overlay First region from above

###### Legend for Overall vs Region plot ######
legend("topleft" ,legend=c("Overall","SomMoCtx"), col=c("black","red", alpha = 0.5), cex = 0.7, lty=c(1,1), ncol=2)

###### Data visualization ######
library(ggplot2)
library(carat)

ggplot(as.data.frame(exp_data$stall.CP[1:3000]))
