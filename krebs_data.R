
#install.packages('tree')
#install.packages('dplyr')
library(dplyr)
library(R.matlab)
library(qwraps2)
library(ggplot2)
library(tree)
library(STAR)
ephysroot = "C:/Users/patri/Google Drive/School/S19 - STA141A S. Chen/Project/Neuralpixel/Data/"; # path of the data set  #include=FALSE???
mstr = 'Krebs'; # mouse names
### Load saved data:
exp_data<- readMat(paste(ephysroot,mstr,'_reduced.mat',sep=''));
# check out data structure

#stall.SC_vector <- t(exp_data$stall.SC)
#stall.SC_vector




###PCA below  for stall.SC region###########
exp_data_stall.SC <- exp_data %>% select(stall.SC[1:176, 0:300])
(exp_data_stall.SC_PCA <- exp_data_stall.SC %>% prcomp(center = TRUE, scale = TRUE)) 
plot(exp_data_stall.SC_PCA,type="l")
#########################################




#summary
str(exp_data)
plot()brain_regions= ls(exp_data)[-c(1:3)]; #this is saving regions 1 to 3 as vectors? 

n_time= dim(exp_data$stall.SC)[c(1,2)]; #length of matrices(neurons and units of time)
n_time

n_sv = dim(exp_data$faceSVD)[1]; #same as n_time?
n_sv 

#exacting spike train... 
i_region = 1; # pick a region
time_range = c(0, 5000); # Pick a time window (indices of time frame) 

spikes_this_region = exp_data$stall.V1 #extract the spike train for this region 
#visualize the spike train 
n_neurons = dim(spikes_this_region)[1];

plot(x=0,y=0,pch=16,col="white",type="l",lwd=3,ylim=c(0,n_neurons),xlim=time_range,cex.lab=1,cex.main=1,ylab='Neuron',xlab='Time frame',main='HPF HippoCampal FormationSpikes') #yaxt='n',xaxt='n',


#for loop, creates plot for spikes... 
for(i_neuron in 1:n_neurons){
  spk_times =which(spikes_this_region[i_neuron,time_range[1]:time_range[2]]>0);
  if (length(spk_times)>0){
    points(y=rep(i_neuron,length(spk_times)),x=spk_times+time_range[1]-1,col="#000000",pch='.',cex=2)
  }
}

rep(exp_data$stall.CP)




install.packages('psych')
library(psych)
describe(exp_data$stall.CP)

exp_data<- readMat(paste(ephysroot,mstr,'_reduced.mat',sep=''));
var(exp_data$stall.MB)
var
brain_regions= ls(exp_data)[-c(1:3)]; # creates a list of the brain regions defined in exp_data list
n_sv= dim(exp_data$faceSVD)[1];
n_time= dim(exp_data$faceSVD)[2];

i_region = 1; # pick brain region "stall.CP"
time_range = c(0, 2000); # pick a time window to display

## Change REGION here
spikes_this_region = exp_data$stall.MB

n_neurons = dim(spikes_this_region)[1]; # 176 neurons in the CP region of the brain
plot(x=0,y=0,pch=16,col="white",type="l",lwd=3,ylim=c(0,n_neurons),xlim=time_range,cex.lab=1,cex.main=1,ylab='Neuron',xlab='Time frame',main='Spikes V1') #yaxt='n',xaxt='n',

# loop through all 176 neurons and plot only the neurons that have a value greater than 0
for(i_neuron in 1:n_neurons){
  # spk_times is a vector of the times for which i_neuron has a value greater than 0
  spk_times =which(spikes_this_region[i_neuron,time_range[1]:time_range[2]]>0);
  if (length(spk_times)>0){
    points(y=rep(i_neuron,length(spk_times)),x=spk_times+time_range[1]-1,col="blue",pch='.',cex=2)
  }
}

## Change REGION here
spikes_this_region = exp_data$stall.V1

n_neurons = dim(spikes_this_region)[1]; # 176 neurons in the CP region of the brain

# loop through all 176 neurons and plot only the neurons that have a value greater than 0
for(i_neuron in 1:n_neurons){
  # spk_times is a vector of the times for which i_neuron has a value greater than 0
  spk_times =which(spikes_this_region[i_neuron,time_range[1]:time_range[2]]>0);
  if (length(spk_times)>0){
    points(y=rep(i_neuron,length(spk_times)),x=spk_times+time_range[1]-1,col="red",pch='.',cex=2)
  }
}

###avg frame### 
tmp=exp_data$avgframe;
tmp= apply(tmp, 2, rev)
par(mfrow=c(1,1))
image(t(tmp),col=heat.colors(20) )
###################################

     