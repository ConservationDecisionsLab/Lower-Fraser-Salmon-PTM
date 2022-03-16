## threshold plot
library(here)
library(tidyr)
library(ggplot2)
library(cowplot)

#load benefit*feasibility estimates
persistence<- read.csv("data/Benefits_CoGov*.csv")
pers2<- read.csv("data/Benefits_long*.csv")

pers.long <-
  gather(persistence,
         key = CU,
         value = Probability,
         2:20)

pers.long2 <-
  gather(pers2,
         key = CU,
         value = Probability,
         2:20)

levels(pers.long$Strategy)[1]<- "CoGov" #rename ALL in CoGov
pers.long<- subset(pers.long, Strategy%in%c("CoGov"))
pers.long2<- subset(pers.long2, Strategy%in%c("BSL", "ALL"))

all<- subset(pers.long2, Strategy%in%c("ALL"))
pers.long$CoGovOnly<- as.matrix(pers.long$Probability-all$Probability)
pers.long3<- pers.long[,c(1,2,4)]
colnames(pers.long3)[3]<- "Probability"


pers.long4<- rbind(pers.long2, pers.long3)
pers.long4$Strategy<- factor(pers.long4$Strategy, levels = c("CoGov", "ALL", "BSL"))
pers.long4$CU<- as.factor(pers.long4$CU)

levels(pers.long4$CU)
CU.levels<- c("Chinook Boundary Bay Fall 0.3","Chinook Lower Fraser Fall 0.3","Chinook Lower Fraser Spring 1.3","Chinook Lower Fraser Summer 1.3","Chinook Lower Fraser Upper Pitt Summer 1.3","Chinook Maria Slough Summer 0.3","Chum Lower Fraser","Coho Boundary Bay","Coho Lower Fraser","Coho Lillooet", "Pink Fraser", "Sockeye Chilliwack Early Summer", "Sockeye Cultus Late", "Sockeye Harrison Down Late (Big Silver)", "Sockeye Harrison (River type)", "Sockeye Harrison Up Late (Weaver)", "Sockeye Lillooet/Harrison Late (Birkenhead)", "Sockeye Pitt Early Summer", "Sockeye Widgeon (River type)")

levels(pers.long4$CU)<- CU.levels

### Figure 5 CUs that achieve conservation threshold under baseline, with management, and with Indigenous led co-governance 

ggplot(pers.long4) + 
  geom_col(aes(x = CU, y = Probability,fill = Strategy)) + 
  scale_y_continuous(limits = c(0,100), breaks= c(0, 20, 40, 60, 80, 100), expand = c(0,0)) +
  xlab("Conservation Unit") + 
  ylab("Chance of green status assessment") +
  theme(axis.text.x = element_text(angle = 75, size = 10, hjust = 1),axis.text.y = element_text(size = 10), axis.title = element_text(size = 11), plot.margin = margin(0,0,0,0, "cm")) + 
  geom_hline(yintercept = 50, colour = "black", linetype = 2) +
  geom_hline(yintercept = 60, colour = "black", linetype = 2) +
  scale_fill_manual(labels = c("With Co-governance", "With Management", "Business As Usual"), values = c('#BB69AD','#117733','#44AA99')) +
  theme(legend.position = "top", legend.text = element_text(size = 11), legend.title = element_blank(), legend.key.size = unit(8, "point"), legend.box.margin = margin(0,0,0,0)) 
  

ggsave(filename="Figure_5.pdf", width = 160, height = 160, units = "mm", path = "figures/")
