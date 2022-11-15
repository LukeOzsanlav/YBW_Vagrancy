## Importing data ##

setwd("C:/Users/Robbie/Documents/4th year modules/Research/Rdata")

startdata<-read.table("alldatachiff.txt",header=T)
head(startdata)

install.packages(c('dplyr','tidyr'))

library(dplyr)
library(tidyr)

startdata %>% summarise_all(class)

startdata = startdata %>% mutate(date = as.Date(as.character(date),format='%d/%d/%Y'),
                                 age = factor(age),
                                 ogc = factor(ogc),
                                 period = factor(period)) 
head(startdata)

plot(startdata %>% group_by(period) %>% summarise_at(vars(weight),.funs = mean,na.rm=T))

install.packages("ggplot2")

library(ggplot2)


head(startdata)

?gather

## putting all data together making all factors correct data type and then removing unesessary data columns before ignoring other ## 
## irrelevent columns ##
data.gather = startdata %>% 
  mutate(ogc = as.numeric(as.character(ogc))) %>% 
  select(-c(bd,bw,bn,wpss))%>%
  gather(measure,value,
         -c(idnumber,ring,species,subspecies,rn,ringer,time,date,jdate,month,nanjizal,period,moult,age))

head(data.gather)

## histograms of all measuremnts split by subspecies ##
plot1 = ggplot(data.gather,
       aes(x=value,
           fill=subspecies)) +
  geom_histogram() +
  facet_grid(subspecies~measure,scales='free')
 # facet_wrap(~measure,scales='free')
print(plot1)

##ggsave(filename='Histograms by subspecies.pdf',plot=plot1,width=12,height=6,device='pdf')


## boxplots with a notch for all meaurement distributions split by subspecies ##
plot2= ggplot(data.gather,
       aes(x=subspecies,
           fill=subspecies,
           y=value ))+
  geom_boxplot(notch=TRUE)+
  facet_wrap(~measure,scales='free')
print(plot2)

##ggsave(filename= 'Notched boxplots by subspecies.pdf',plot=plot2,width=12,height=6,device='pdf')


## boxplots with no notch for all meaurement distributions split by subspecies ##
plot3=ggplot(data.gather,
       aes(x=subspecies,
           fill=subspecies,
           y=value ))+
  geom_boxplot(notch=FALSE)+
  facet_wrap(~measure,scales='free')
print(plot3)

##ggsave(filename= 'Boxplots by subspecies.pdf',plot=plot3,width=12,height=6,device='pdf')

## liniar smoothed plots of all measurements for all three subspecies against capture data ##
plot4= ggplot(data.gather,
       aes(x=jdate,
           fill=subspecies,
           y=value,colour=subspecies))+
  geom_point()+
  geom_smooth(se=FALSE)+
  facet_wrap(~measure,scales="free")
print(plot4)

##ggsave(filename='Scattergraph of trends over time by subspecies.pdf',plot=plot4,width=12,height=6,device='pdf')


## liniar smoother plots of all meaurements for collybitta against capture date ##
plot5= ggplot(data.gather %>% filter(subspecies=="C"),
       aes(x=jdate,
           y=value))+
  geom_point()+
  geom_smooth(se=TRUE)+
  facet_wrap(~measure,scales="free")
print(plot5)

##ggsave(filename='Scattergraph of trends over time of Collybitta.pdf',plot=plot5,width=12,height=6,device='pdf')

## liniar smoother plots of all meaurements for collybitta against capture date with separated points##
plot6= ggplot(data.gather %>% filter(subspecies=="C"),
              aes(x=jdate,
                  y=value))+
  geom_jitter()+
  geom_smooth(se=TRUE)+
  facet_wrap(~measure,scales="free")
print(plot6)

##ggsave(filename='Scattergraph (jitter) of trends over time of Collybitta.pdf',plot=plot6,width=12,height=6,device='pdf')


