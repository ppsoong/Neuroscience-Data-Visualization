### DIFFERENT BRAIN REGION

# faceSVD
time_frame = 0:3000
num_fired_vec <- rep(0, 1000) # can change time frame here

region <-exp_data$faceSVD # can change brain region here

for(i in 1:length(time_frame)) {
     num_fired <- sum(region[,i])
     num_fired_vec[i] <- num_fired
}

firing_rate_vec <- num_fired_vec / (.03) # each time interval is 30ms = 0.03s
plot(x = time_frame, y = firing_rate_vec, main = "Firing rates for faceSVD", xlab = "Time", ylab = "Frequency (HZ)", type="l", ann=T, cex.lab=0.8, lwd=1) 

## CP
num_fired_vec1 <- rep(0, length(time_frame))
region1 <-exp_data$stall.CP # can change brain region here

for(i in 1:length(time_frame)) {
     num_fired1 <- sum(region1[,i])
     num_fired_vec1[i] <- num_fired1
}

firing_rate_vec1 <- num_fired_vec1 / (.03) # each time interval is 30ms = 0.03s
plot(x = time_frame, y = firing_rate_vec1, main = "Firing rates for CP", xlab = "Time", ylab = "Frequency (HZ)", type="l", ann=T, cex.lab=0.8, lwd=1) 


## FrMoCtx
num_fired_vec2 <- rep(0, length(time_frame))
region2 <-exp_data$stall.FrMoCtx # can change brain region here

for(i in 1:length(time_frame)) {
     num_fired2 <- sum(region2[,i])
     num_fired_vec2[i] <- num_fired2
}

firing_rate_vec2 <- num_fired_vec2 / (.03) # each time interval is 30ms = 0.03s
plot(x = time_frame, y = firing_rate_vec2,main = "Firing rates for FrMoCtx", xlab = "Time", ylab = "Frequency (HZ)", type="l", ann=T, cex.lab=0.8, lwd=1) 


## HPF
num_fired_vec3 <- rep(0, length(time_frame))
region3 <-exp_data$stall.HPF # can change brain region here

for(i in 1:length(time_frame)) {
     num_fired3 <- sum(region3[,i])
     num_fired_vec3[i] <- num_fired3
}

firing_rate_vec3 <- num_fired_vec3 / (.03) # each time interval is 30ms = 0.03s
plot(x = time_frame, y = firing_rate_vec3, main = "Firing rates for HPF",xlab = "Time", ylab = "Frequency (HZ)", type="l", ann=T, cex.lab=0.8, lwd=1) 


## LS
num_fired_vec4 <- rep(0, length(time_frame))
region4 <-exp_data$stall.LS # can change brain region here

for(i in 1:length(time_frame)) {
     num_fired4 <- sum(region4[,i])
     num_fired_vec4[i] <- num_fired4
}

firing_rate_vec4 <- num_fired_vec4 / (.03) # each time interval is 30ms = 0.03s
plot(x = time_frame, y = firing_rate_vec4, main = "Firing rates for LS",xlab = "Time", ylab = "Frequency (HZ)", type="l", ann=T, cex.lab=0.8, lwd=1) 


## MB
num_fired_vec5 <- rep(0, length(time_frame))
region5 <-exp_data$stall.MB # can change brain region here

for(i in 1:length(time_frame)) {
     num_fired5 <- sum(region5[,i])
     num_fired_vec5[i] <- num_fired5
}

firing_rate_vec5 <- num_fired_vec5 / (.03) # each time interval is 30ms = 0.03s
plot(x = time_frame, y = firing_rate_vec5, main = "Firing rates for MB",xlab = "Time", ylab = "Frequency (HZ)", type="l", ann=T, cex.lab=0.8, lwd=1) 


## SC
num_fired_vec6 <- rep(0, length(time_frame))
region6 <-exp_data$stall.SC # can change brain region here

for(i in 1:length(time_frame)) {
     num_fired6 <- sum(region6[,i])
     num_fired_vec6[i] <- num_fired6
}

firing_rate_vec6 <- num_fired_vec6 / (.03) # each time interval is 30ms = 0.03s
plot(x = time_frame, y = firing_rate_vec6, main = "Firing rates for SC",xlab = "Time", ylab = "Frequency (HZ)", type="l", ann=T, cex.lab=0.8, lwd=1) 


## SoMoCtx
num_fired_vec7 <- rep(0, length(time_frame))
region7 <-exp_data$stall.SomMoCtx # can change brain region here

for(i in 1:length(time_frame)) {
     num_fired7 <- sum(region7[,i])
     num_fired_vec7[i] <- num_fired7
}

firing_rate_vec7 <- num_fired_vec7 / (.03) # each time interval is 30ms = 0.03s
plot(x = time_frame, y = firing_rate_vec7, main = "Firing rates for soMoCtx",xlab = "Time", ylab = "Frequency (HZ)", type="l", ann=T, cex.lab=0.8, lwd=1) 


## TH
num_fired_vec8 <- rep(0, length(time_frame))
region8 <-exp_data$stall.TH # can change brain region here

for(i in 1:length(time_frame)) {
     num_fired8 <- sum(region8[,i])
     num_fired_vec8[i] <- num_fired8
}

firing_rate_vec8 <- num_fired_vec8 / (.03) # each time interval is 30ms = 0.03s
plot(x = time_frame, y = firing_rate_vec8, main = "Firing rates for TH",xlab = "Time", ylab = "Frequency (HZ)", type="l", ann=T, cex.lab=0.8, lwd=1) 


## V1
num_fired_vec9 <- rep(0, length(time_frame))
region9 <-exp_data$stall.V1 # can change brain region here

for(i in 1:length(time_frame)) {
     num_fired9 <- sum(region9[,i])
     num_fired_vec9[i] <- num_fired9
}

firing_rate_vec9 <- num_fired_vec9 / (.03) # each time interval is 30ms = 0.03s
plot(x = time_frame, y = firing_rate_vec9, main = "Firing rates for V1",xlab = "Time", ylab = "Frequency (HZ)", type="l", ann=T, cex.lab=0.8, lwd=1) 


