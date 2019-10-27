### DIFFERENT BRAIN REGION
setwd("C:/Users/sales/Desktop/2019/Spring/STA141A/Project") 
library(R.matlab)
ephysroot = 'C:/Users/sales/Desktop/2019/Spring/STA141A/Project/'; # path of the data set 
mstr = 'Krebs'; # Mouse names
exp_data<- readMat(paste(ephysroot,mstr,'_reduced.mat',sep=''));

# faceSVD
time_frame = 1:3000 
SVD_val_vec <- rep(1, 3000) # can change time frame here

SVD_values <-exp_data$faceSVD # can change brain region here

for(i in 1:length(time_frame)) {
     SVD_val <- sum(SVD_values[,i])
     SVD_val_vec[i] <- SVD_val
}

avg_SVD_val <- SVD_val_vec / 50 # Sum of all SVD values divided bu number of values
plot(x = time_frame, y = firing_rate_vec, main = "Avg faceSVD Values Over Time", xlab = "Time", ylab = "Avg faceSVD Value", type="l", ann=T, cex.lab=0.8, lwd=1) 

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



## Overlaid ggplots by groups ###################################################################

## First Group: V1(num_fired_vec9) and MB(num_fired_vec5) 
ggplot = ggplot(as.data.frame(exp_data$stall.V1[1:3000]), aes(x = time_frame, y = num_fired_vec9, alpha=0.5))
p1 <- ggplot + geom_line(colour="blue",alpha=.5) + 
  geom_smooth(colour="blue",alpha=1) + geom_line(data = as.data.frame(exp_data$stall.MB[1:3000]), 
  aes(x = time_frame, y = num_fired_vec5, colour="#000099")) + geom_smooth(method="gam", 
  formula=num_fired_vec5 ~ s(x, bs = "cs"),colour="red")+ ggtitle("V1 (BLUE) vs. MB (RED)") + 
  theme(legend.position = "none")


## Second Group: TH(num_fired_vec8) and HPF(num_fired_vec3)
ggplot = ggplot(as.data.frame(exp_data$stall.V1[1:3000]), aes(x = time_frame, y = num_fired_vec8, alpha=0.5))
p2 <- ggplot + geom_line(colour="blue",alpha=.5) + 
  geom_smooth(colour="blue",alpha=1) + geom_line(data = as.data.frame(exp_data$stall.HPF[1:3000]), 
  aes(x = time_frame, y = num_fired_vec3, colour="#000099")) + geom_smooth(method="gam",
  formula=num_fired_vec3 ~ s(x, bs = "cs"),colour="red") + ggtitle("TH(BLUE) vs. HPF(RED)") + 
  theme(legend.position = "none")

## Third Group: LS(num_fired_vec4) and SomMoCtx(num_fired_vec7)
ggplot = ggplot(as.data.frame(exp_data$stall.LS[1:3000]), aes(x = time_frame, y = num_fired_vec4, alpha=0.5))
p3 <- ggplot + geom_line(colour="blue",alpha=.5) + 
  geom_smooth(colour="blue",alpha=1) + geom_line(data = as.data.frame(exp_data$stall.SomMoCtx[1:3000]), 
  aes(x = time_frame, y = num_fired_vec7, colour="#000099")) + geom_smooth(method="gam",
  formula=num_fired_vec7 ~ s(x, bs = "cs"),colour="red") + ggtitle("LS(BLUE) vs. SomMoCtx(RED)") + 
  theme(legend.position = "none")

## Fourth Group: CP(num_fired_vec1) and FrMoCtx(num_fired_vec2) and SC(num_fired_vec6)
ggplot = ggplot(as.data.frame(exp_data$stall.CP[1:3000]), aes(x = time_frame, y = num_fired_vec1, alpha=0.5))
p4 <- ggplot + geom_line(colour="blue",alpha=.3) + 
  geom_smooth(colour="blue",alpha=1) + geom_line(data = as.data.frame(exp_data$stall.FrMoCtx[1:3000]), 
  aes(x = time_frame, y = num_fired_vec2, colour="red"),alpha=.3) + geom_smooth(method="gam",
  formula=num_fired_vec2 ~ s(x, bs = "cs"), alpha=1, colour="red") + geom_line(data = 
  as.data.frame(exp_data$stall.SC[1:3000]), 
  aes(x = time_frame, y = num_fired_vec6,colour="green"),alpha=.3) + geom_smooth(method="gam",
  formula=num_fired_vec6 ~ s(x, bs = "cs"),colour="green") + 
  ggtitle("CP(BLUE) vs. FrMoCtx(RED) vs. SC(GREEN)") + 
  theme(legend.position = "none")

multiplot(p1, p2, p3, p4, cols=2)


### multiplot Function from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/ #########
### Used from formatting the plots in a grid of 2-by-2 plots ################################################
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



# ### Best fit plots for overall plots ##########################################################
# 
# sum_vec <- exp_data$stall.CP[1:3000] + exp_data$stall.FrMoCtx[1:3000] +  exp_data$stall.HPF[1:3000] +
#     exp_data$stall.LS[1:3000] + exp_data$stall.MB[1:3000] + exp_data$stall.SC[1:3000] +
#     + exp_data$stall.SomMoCtx[1:3000] + exp_data$stall.TH[1:3000] + exp_data$stall.V1[1:3000]
# 
# sum_fired_vec <- num_fired_vec1 + num_fired_vec2 + num_fired_vec3 + num_fired_vec4 + 
#                  num_fired_vec5 + num_fired_vec6 + num_fired_vec5 + num_fired8 + num_fired9
# 
# ggplot = ggplot(as.data.frame(sum_vec), aes(x = time_frame, y = sum_fired_vec, alpha=0.5))
# ggplot + geom_line(colour="blue",alpha=.5) + 
#   geom_smooth(colour="blue",alpha=1) + geom_line(data = as.data.frame(exp_data$stall.TH[1:3000]), 
#     aes(x = time_frame, y = num_fired_vec8, colour="#000099")) + geom_smooth(method="gam",                 
#     formula=num_fired_vec8 ~ s(x, bs = "cs"),colour="red") + ggtitle("Overall(BLUE) vs. TH(RED)") + 
#   theme(legend.position = "none")