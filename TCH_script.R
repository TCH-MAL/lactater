dataInterp = LTP2plot$plot_env$data_interpolated
dataFitted = LTP2plot$plot_env$fitted_values
dataRaw = LTP2plot$plot_env$data



LTP1point = LTP1plot$plot_env$breakpoint_y_value
LTP2point = LTP2plot$plot_env$breakpoint_y_value


LTP1pointX= dataFitted[which.min(abs(LTP1point-dataFitted$value)),]
LTP2pointX= dataFitted[which.min(abs(LTP2point-dataFitted$value)),]

x_labels = LTP1plot$plot_env$breaks_and_labels$labels
x_breaks = LTP1plot$plot_env$breaks_and_labels$breaks


ggplot() +
  geom_line(data=dataFitted, aes(x=intensity, y=value) ,color = "red") +
  geom_point(data=dataRaw, aes(x=intensity, y=lactate), size = 5, alpha = 0.2, colour = "gray")+
  geom_point(aes(x=LTP1pointX$intensity, y=LTP1point), size = 5, alpha = 0.3, colour="blue")+
  geom_point(aes(x=LTP2pointX$intensity, y=LTP2point), size = 5, alpha = 0.3, colour="blue")+
  #scale_x_continuous(name="Power (W)", breaks=seq(100,300,20)) +
  scale_x_continuous(name = "Power (W)", breaks = x_breaks, labels = x_labels) +
  ylab("Blood Lactate (mmol/L)")+
  ggtitle("Lactate Turning Points (LTP1 & LTP2)")+
  theme_light()



ParvoData$TIME = as.numeric(ParvoData$TIME)
ParvoData <- ParvoData[-1,]

ParvoDataCropped = subset(ParvoData,TIME >= 30 & TIME <= 70)
dataFittedCropped = subset(dataFitted, intensity >=120 & intensity <=280)
time_col = seq(30, 70, length.out = nrow(dataFittedCropped))

dataFittedCropped$time = time_col






library(janitor)
ParvoDataCropped = clean_names(ParvoDataCropped)

library(gtable)
library(tidyverse)
library(grid)
library(gridExtra)

p2 = ggplot() +
  geom_line(data = ParvoDataCropped, aes(x = time, y = as.numeric(vo2)), color = 'green')

p3 = ggplot() +
  geom_line(data = ParvoDataCropped, aes(x = time, y = as.numeric(vco2)), color = 'blue')

p4 = ggplot() +
  geom_line(data = ParvoDataCropped, aes(x = time, y = as.numeric(ve)), color = 'black')

p5 = ggplot() +
  geom_line(data = ParvoDataCropped, aes(x = time, y = as.numeric(hr)), color = 'red')

g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)
g4 <- ggplotGrob(p4)
g5 <- ggplotGrob(p5)


g <- rbind(g2, g3, g4, g5, size = "first")
g$widths <- unit.pmax(g2$widths, g3$widths, g4$widths, g5$widths)
grid.newpage()
grid.draw(g)

