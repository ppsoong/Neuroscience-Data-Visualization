library(ggplot2)
# library(caret)

ggplot = ggplot(as.data.frame(exp_data$stall.HPF[1:3000]), aes(x = time_frame, y = firing_rate_vec))
ggplot + geom_line() + geom_smooth() + geom_line(data = as.data.frame(exp_data$stall.SomMoCtx[1:3000]), aes(x = time_frame, y = firing_rate_vec2, colour="#000099")) + geom_smooth(method="gam", formula=firing_rate_vec2 ~ s(x, bs = "cs"),colour="green")
