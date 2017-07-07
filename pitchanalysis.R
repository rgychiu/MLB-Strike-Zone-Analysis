library(data.table)
library(ggplot2)
library(gridExtra)

##What are the factors that affect the strike zone? (Base state)

pitchdata <- as.data.table(read.csv('pitch_data.csv'))
pitchdata <- pitchdata[(description == "Ball")|(description == "Called Strike"),
                       list(description, px, pz, on_1b, on_2b, on_3b)][order(description)]

##no bases
qplot(px,pz,data=pitchdata[is.na(on_1b) & is.na(on_2b) & is.na(on_3b)], 
      col = description, 
      xlim=c(-6,6), 
      ylim=c(-4,8), 
      main = "NO BASES") + 
      geom_segment(x=-1.5,y=-20, xend = -1.5,yend = 20, col="black") + 
      geom_segment(x=1.5,y=-20, xend = 1.5,yend = 20, col="black") + 
      geom_segment(x=-20,y=1,xend = 20,yend = 1, col="black") + 
      geom_segment(x=-20,y=4, xend = 20,yend = 4, col="black")

##only on 1st base
qplot(px,pz,data=pitchdata[on_1b>1 & is.na(on_2b) & is.na(on_3b)], 
      col = description, 
      xlim=c(-6,6), 
      ylim=c(-4,8), 
      main = "ONLY FIRST") + 
      geom_segment(x=-1.5,y=-20, xend = -1.5,yend = 20, col="black") + 
      geom_segment(x=1.5,y=-20, xend = 1.5,yend = 20, col="black") + 
      geom_segment(x=-20,y=1,xend = 20,yend = 1, col="black") + 
      geom_segment(x=-20,y=4, xend = 20,yend = 4, col="black")

##only on 2nd base
qplot(px,pz,data=pitchdata[on_2b>1 & is.na(on_1b) & is.na(on_3b)], 
      col = description, xlim=c(-6,6), 
      ylim=c(-4,8), 
      main = "ONLY SECOND") + 
      geom_segment(x=-1.5,y=-20, xend = -1.5,yend = 20, col="black") + 
      geom_segment(x=1.5,y=-20, xend = 1.5,yend = 20, col="black") + 
      geom_segment(x=-20,y=1,xend = 20,yend = 1, col="black") + 
      geom_segment(x=-20,y=4, xend = 20,yend = 4, col="black")

##only on 3rd base
qplot(px,pz,data=pitchdata[on_3b>1 & is.na(on_1b) & is.na(on_2b)], 
      col = description, 
      xlim=c(-6,6), 
      ylim=c(-4,8), 
      main = "ONLY THIRD") + 
      geom_segment(x=-1.5,y=-20, xend = -1.5,yend = 20, col="black") + 
      geom_segment(x=1.5,y=-20, xend = 1.5,yend = 20, col="black") + 
      geom_segment(x=-20,y=1,xend = 20,yend = 1, col="black") + 
      geom_segment(x=-20,y=4, xend = 20,yend = 4, col="black")

##1st and 2nd
qplot(px,pz,data=pitchdata[on_1b>1 & on_2b>1 & is.na(on_3b)], 
      col = description, 
      xlim=c(-6,6), 
      ylim=c(-4,8), 
      main = "FIRST AND SECOND") + 
      geom_segment(x=-1.5,y=-20, xend = -1.5,yend = 20, col="black") + 
      geom_segment(x=1.5,y=-20, xend = 1.5,yend = 20, col="black") + 
      geom_segment(x=-20,y=1,xend = 20,yend = 1, col="black") + 
      geom_segment(x=-20,y=4, xend = 20,yend = 4, col="black")

##bases loaded
qplot(px,pz,data=pitchdata[on_1b>1 & on_2b>1 & on_3b>1], 
      col = description, 
      xlim=c(-6,6), 
      ylim=c(-4,8), 
      main = "BASES LOADED") + 
      geom_segment(x=-1.5,y=-20, xend = -1.5,yend = 20, col="black") + 
      geom_segment(x=1.5,y=-20, xend = 1.5,yend = 20, col="black") + 
      geom_segment(x=-20,y=1,xend = 20,yend = 1, col="black") + 
      geom_segment(x=-20,y=4, xend = 20,yend = 4, col="black")

##1st and 3rd
qplot(px,pz,data=pitchdata[on_1b>1 & on_3b>1 & is.na(on_2b)], 
      col = description, 
      xlim=c(-6,6), 
      ylim=c(-4,8), 
      main = "FIRST AND THIRD") + 
      geom_segment(x=-1.5,y=-20, xend = -1.5,yend = 20, col="black") + 
      geom_segment(x=1.5,y=-20, xend = 1.5,yend = 20, col="black") + 
      geom_segment(x=-20,y=1,xend = 20,yend = 1, col="black") + 
      geom_segment(x=-20,y=4, xend = 20,yend = 4, col="black")

##2nd and 3rd
qplot(px,pz,data=pitchdata[on_2b>1 & on_3b>1 & is.na(on_1b)], 
      col = description, 
      xlim=c(-6,6), 
      ylim=c(-4,8), 
      main = "SECOND AND THIRD") + 
      geom_segment(x=-1.5,y=-20, xend = -1.5,yend = 20, col="black") + 
      geom_segment(x=1.5,y=-20, xend = 1.5,yend = 20, col="black") +
      geom_segment(x=-20,y=1,xend = 20,yend = 1, col="black") + 
      geom_segment(x=-20,y=4, xend = 20,yend = 4, col="black")

##save to external file to compare all 8 plots in one window
pdf("pitchanalytics.pdf", width =25, height = 10)
grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8, ncol=4)
dev.off()
