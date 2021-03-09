library(ggplot2)

#By Month
By_Month <- data.frame(Month=c("Srawan","Bhadra", "Asoj", "Kartik", "Mangsir", "Poush", "Magh","Falgun", "Chaitra", "Baisakh", "Jestha", "Asar"),
                       Number=c(27,23,19,10,16,20,17,22,20,22,53,0))

By_Month$Month <- factor(By_Month$Month, levels = By_Month$Month)

Cases_By_Month<-ggplot(By_Month, aes(x=Month, y=Number, group = 1)) + 
  geom_line(stat = "identity", color="orange") + geom_point(color="darkorange1") +
  geom_text(aes(label = round(Number, 1)),
            vjust = "inward", hjust = "outward", size=2.8,
            show.legend = FALSE)

Cases_By_Month <- Cases_By_Month + labs(title = "Number of Trafficking Cases Registered by Months",
                                      subtitle = "Fiscal Year 2018/19 (Asar data not present)")

Cases_By_Month + theme(axis.text.x = element_text(angle = 50, hjust = 1)) + ggsave("1.pdf")

#By State
By_State <- data.frame(State= c("State 1", "State 2", "State 3", "State 4(Gandaki)", "State 5", "State 6(Karnali)",
                                "State 7", "Valley", "Bureau"),
                       Fiscal_Year_2017_2018= c(51,23,21,6,91,6,26,39,42),
                       Fiscal_Year_2018_2019= c(40,15,15,5,33,7,26,33,16))

library(reshape2)

By_State <- melt(By_State, id.vars='State')

Dist_State <- ggplot(By_State, aes(x=State, y=value, fill=variable))+geom_bar(stat="identity", position = "dodge") +
  labs(title = "Number of Trafficking Cases Registered by State",
       subtitle = "FY 2075/75 - FY 2075/76") +
       theme(axis.text.x = element_text(angle = 50, hjust = 1)) + ggsave("2.pdf")

#By Region
Region1 <- data.frame(Region = c("EDR", "MDR", "WDR", "MWDR", "FWDR", "VALLEY", "BUREAU"),
                      Cases = c(33,39,38,31,19,55,12))

Region1$Region <- factor(Region1$Region, levels = Region1$Region)
region <- ggplot(Region1, aes(x=Region, y=Cases))+geom_bar(stat="identity", color="yellow", fill="salmon1") + 
  labs(title="Number of Trafficking cases in Nepal by Region", subtitle="FY 2016/2017")
region + ggsave("3.pdf")

#By Region in Percentage

Region1$fraction = Region1$Cases / sum(Region1$Cases)
Region1$ymax = cumsum(Region1$fraction)
Region1$ymin = c(0, head(Region1$ymax, n=-1))

Region1$Region <- factor(Region1$Region, levels = c("EDR", "MDR", "WDR", "MWDR", "FWDR", "VALLEY", "BUREAU"))

p1 = ggplot(Region1, aes(fill=Region, ymax=ymax, ymin=ymin, xmax=7, xmin=3))+
  geom_rect(color="blue")+
  coord_polar(theta="y")+
  xlim(c(1,7))

By_Region<-p1 + scale_fill_brewer("Region") +
  theme(axis.text.x=element_blank()) + theme(legend.position=c(0.97, .5)) +
  labs(title = "Number of Trafficking Cases in Nepal by Nepal Police by Region", subtitle="FY 2016/17") +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  theme(legend.title = element_text(size=9, face="bold")) +
  theme(legend.text = element_text(size = 6, face = "bold")) 

percentlabels<- round(100*(Region1$fraction), 2)

Dist_Region<- By_Region +
  geom_label(
    aes(label = percentlabels,
        x = 5.5,
        y = (ymin + ymax) / 2),
    inherit.aes = TRUE,
    show.legend = FALSE
  )

Dist_Region + ggsave("4.pdf")

#By Modus Operandi
library(tidyverse)
library(patchwork)

Pr <- data.frame(Modus_Operandi = c("Using Medicine", "Fake Marriage", "Threats", "Lure/Decption",
                                    "Employment", "Tourt/Travel", "Others"),
                 Fiscal_Year_2016_2017 = c(2,5,1,73,0,206,24),
                 Fiscal_Year_2017_2018 = c(9,18,31,385,58,37,0),
                 Fiscal_Year_2018_2019 = c(1,2,7,108,10,20,0))
Pr
l <- ggplot(Pr, aes(x=Modus_Operandi, y=Fiscal_Year_2016_2017))+
  geom_bar(stat='identity', fill="forestgreen")+
  ylab("Cases") + theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  labs(x="", subtitle="FY 2016/17")
l
m <- ggplot(Pr, aes(x=Modus_Operandi, y=Fiscal_Year_2017_2018))+
  geom_bar(stat='identity', fill="coral") + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  labs(y="", title="Trafficking Cases by Modus Operandi", subtitle="FY 2017/18")
m

n <- ggplot(Pr, aes(x=Modus_Operandi, y=Fiscal_Year_2018_2019))+
  geom_bar(stat='identity', fill="light blue")+
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  labs(x="", y="", subtitle="FY 2018/19")
n

l + m + n + plot_layout(widths = 3) + ggsave("5.pdf")
