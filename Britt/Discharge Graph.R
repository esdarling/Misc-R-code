rm(list = ls())

library(ggplot2)
library(scales)
library(extrafont)

library(here)
library(tidyverse)
library(lubridate)


font_import()
loadfonts(device='win')

#setwd("//abconservation1.sharepoint.com@SSL/DavWWWRoot/resprogs/Fisheries/Projects/Owl River/2017-18/2017 Data")
here()

DD<-read.csv("Owl River Daily Discharge 2011-17.csv")
DD<-read.csv(here("Britt", "Owl River Daily Discharge 2011-17.csv"), 
             header = TRUE, strip.white = TRUE, stringsAsFactors = FALSE)
head(DD)

DD$year<-as.factor(DD$year)
DD$date <- as.Date(DD$date, format = "%Y-%m-%d")
unique(DD$date)
str(DD)

#dates work well in the lubridate package
DD$lubridate <- ymd(DD$date)
DD$lubridate.month <- month(DD$lubridate)
unique(DD$lubridate.month)

unique(DD$lubridate)

test <- md(paste(month(DD$lubridate), day(DD$lubridate), sep = "-"))
is.Date(test)

breaks.major <- c(0, 31, 61, 92, 123)
breaks.minor <- c(30, 45, 60, 75, 90)

head(DD)

ggplot(data = DD, aes(x = lubridate, y = discharge,
      group = factor(year(DD$lubridate)),
      color = factor(year(DD$lubridate))))+
  geom_line() +
  geom_point() +
  labs(x = "Month", color = "Year") +
  theme_classic()

ggplot(DD, aes(x=lubridate, y=discharge, fill=year)) +
  geom_point(aes(color=year), size=0.5) +
  geom_line(aes(color=year), size=1.5)  +
  theme_bw(base_size = 12) + 
  scale_colour_grey() +
  scale_x_continuous(limits = c(0, 123), breaks=c(31,61,92))+
  ylim(0, 45)+
  xlab("Month")+
  annotate("text", x = c(15,45,75, 105), y=0, label = c("May", "June", "July", "August"), family = "Times New Roman", size=5)+
  labs(y=expression(Daily~discharge~(m^{3}/s)))+
 #labs(title="Daily discharge on the Owl River \nfrom May to August in 2011, 2014 and 2017")+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major=element_blank())+
  theme(panel.background=element_blank())+
  theme(panel.border=element_rect(size=0.5, fill=NA))+
  #theme(plot.margin=unit(c(1,1,2,1), "lines"))+
  theme(axis.text.x=element_blank())+
  theme(axis.title.x=element_text(family = "Times New Roman", size=16, vjust=-0.05))+
  theme(axis.text.y=element_text(family = "Times New Roman", size=12, colour="black"))+
  theme(axis.title.y=element_text(family = "Times New Roman", size=16, vjust=1.5))+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(family = "Times New Roman", size=16))+
  theme(legend.key=element_blank())+
  theme(strip.text.y = element_text(size=16, vjust=0.8),
        strip.background = element_rect(colour="black", fill=NA))


