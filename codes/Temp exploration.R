#Explore temperature changes over time

bitter_temp

#plot temp by year
bitter_temp %>% ggplot(aes(year, avgtemp,  color=location))+
  geom_point()+
  geom_errorbar(aes(ymin= avgtemp-SE, ymax = avgtemp+SE), width = 0.2)+
  geom_line(aes(group=location))

#split areas up for easier view
bitter_temp %>% filter(location=="Barlow Cove"|location=="Excursion Inlet"|location=="Glacier Bay"
                       |location=="Icy Strait"|location=="Juneau"|location=="Lynn Sisters"|
                         location=="Stephens Passage"|location=="Holkham Bay")%>% 
  ggplot(aes(year, avgtemp,  color=location))+
  geom_point()+
  geom_errorbar(aes(ymin= avgtemp-SE, ymax = avgtemp+SE), width = 0.2)+
  geom_line(aes(group=location))+
  xlab("Year")+ylab("Average Temperature (C)")+labs(color="Location")

bitter_temp %>% filter(location=="Deadman Reach"|location=="Gambier Bay"|location=="Port Camden"|
                         location=="Port Frederick"|location=="Pybus Bay"|location=="Rodman Bay"|
                         location=="Seymour Canal"|location=="Thomas Bay")%>% 
  ggplot(aes(year, avgtemp,  color=location))+
  geom_point()+
  geom_errorbar(aes(ymin= avgtemp-SE, ymax = avgtemp+SE), width = 0.2)+
  geom_line(aes(group=location))+
  xlab("Year")+ylab("Average Temperature")+labs(color="Location")

# look at temps within Thomas Bay for outliers
hist(ThomasBay$meantemp) 

na.omit(tidtemp)-> tidtemp %>% filter(location=="Thomas Bay" & year == "2016")

tidtemp %>% filter(location=="Thomas Bay" & year == "2016")
hist(tidtemp$meantemp)
range(tidtemp$meantemp)
