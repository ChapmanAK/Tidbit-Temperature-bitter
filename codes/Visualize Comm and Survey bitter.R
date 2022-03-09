#visulize and explore temp and commerical/survey bitter data
library(ggplot2)
library(reshape2)

#compare bitter from survey to commerical 
#Excursion
comm_survey_bitter_temp %>% filter(location=="Excursion Inlet") %>% select(year, per_bitter, com_bitter_per)->Excursion_CS_BT
 melt(Excursion_CS_BT, id.vars = "year")-> Excursion_CS_BT.melt

 Excursion_CS_BT.melt%>% 
  ggplot(aes(x=year, y=value, fill=variable))+
     geom_bar(stat = "identity", position = "dodge")+
   xlab("Year")+ylab("Bitter Percent")+
   ggtitle("Excursion Inlet")
 

#Gambier Bay
 comm_survey_bitter_temp %>% filter(location=="Gambier Bay") %>%
   select(year, per_bitter, com_bitter_per) %>% 
   melt(id.vars="year") %>% 
   ggplot(aes(x=year, y=value, fill=variable))+
   geom_bar(stat = "identity", position = "dodge")+
   xlab("Year")+ylab("Bitter Percent")+
   ggtitle("Gambier Bay")

 #Glacier Bay
 comm_survey_bitter_temp %>% filter(location=="Glacier Bay") %>%
   select(year, per_bitter, com_bitter_per) %>% 
   melt(id.vars="year") %>% 
   ggplot(aes(x=year, y=value, fill=variable))+
   geom_bar(stat = "identity", position = "dodge")+
   xlab("Year")+ylab("Bitter Percent")+
   ggtitle("Glacier Bay")
 
 #Holkham Bay
 comm_survey_bitter_temp %>% filter(location=="Holkham Bay") %>%
   select(year, per_bitter, com_bitter_per) %>% 
   melt(id.vars="year") %>% 
   ggplot(aes(x=year, y=value, fill=variable))+
   geom_bar(stat = "identity", position = "dodge")+
   xlab("Year")+ylab("Bitter Percent")+
   ggtitle("Holkham Bay")
 
 #Icy Strait
 comm_survey_bitter_temp %>% filter(location=="Icy Strait") %>%
   select(year, per_bitter, com_bitter_per) %>% 
   melt(id.vars="year") %>% 
   ggplot(aes(x=year, y=value, fill=variable))+
   geom_bar(stat = "identity", position = "dodge")+
   xlab("Year")+ylab("Bitter Percent")+
   ggtitle("Icy Strait")
 
 #Juneau
 comm_survey_bitter_temp %>% filter(location=="Juneau") %>%
   select(year, per_bitter, com_bitter_per) %>% 
   melt(id.vars="year") %>% 
   ggplot(aes(x=year, y=value, fill=variable))+
   geom_bar(stat = "identity", position = "dodge")+
   xlab("Year")+ylab("Bitter Percent")+
   ggtitle("Juneau")
 
 #Lynn Sisters
 comm_survey_bitter_temp %>% filter(location=="Lynn Sisters") %>%
   select(year, per_bitter, com_bitter_per) %>% 
   melt(id.vars="year") %>% 
   ggplot(aes(x=year, y=value, fill=variable))+
   geom_bar(stat = "identity", position = "dodge")+
   xlab("Year")+ylab("Bitter Percent")+
   ggtitle("Lynn Sisters")
 
 #Port Camden
 comm_survey_bitter_temp %>% filter(location=="Port Camden") %>%
   select(year, per_bitter, com_bitter_per) %>% 
   melt(id.vars="year") %>% 
   ggplot(aes(x=year, y=value, fill=variable))+
   geom_bar(stat = "identity", position = "dodge")+
   xlab("Year")+ylab("Bitter Percent")+
   ggtitle("Port Camden")
 
 #Port Frederick
 comm_survey_bitter_temp %>% filter(location=="Port Frederick") %>%
   select(year, per_bitter, com_bitter_per) %>% 
   melt(id.vars="year") %>% 
   ggplot(aes(x=year, y=value, fill=variable))+
   geom_bar(stat = "identity", position = "dodge")+
   xlab("Year")+ylab("Bitter Percent")+
   ggtitle("Port Frederick")
 
 #Pybus Bay
 comm_survey_bitter_temp %>% filter(location=="Pybus Bay") %>%
   select(year, per_bitter, com_bitter_per) %>% 
   melt(id.vars="year") %>% 
   ggplot(aes(x=year, y=value, fill=variable))+
   geom_bar(stat = "identity", position = "dodge")+
   xlab("Year")+ylab("Bitter Percent")+
   ggtitle("Juneau")
 
 #Seymour Canal
 comm_survey_bitter_temp %>% filter(location=="Seymour Canal") %>%
   select(year, per_bitter, com_bitter_per) %>% 
   melt(id.vars="year") %>% 
   ggplot(aes(x=year, y=value, fill=variable))+
   geom_bar(stat = "identity", position = "dodge")+
   xlab("Year")+ylab("Bitter Percent")+
   ggtitle("Seymour Canal")
 
 #Thomas Bay
 comm_survey_bitter_temp %>% filter(location=="Thomas Bay") %>%
   select(year, per_bitter, com_bitter_per) %>% 
   melt(id.vars="year") %>% 
   ggplot(aes(x=year, y=value, fill=variable))+
   geom_bar(stat = "identity", position = "dodge")+
   xlab("Year")+ylab("Bitter Percent")+
   ggtitle("Thomas Bay")

#Looking at Commercial bitter rates and temperature  
#Excursion
 comm_survey_bitter_temp %>% filter(location=="Excursion Inlet") %>%  ggplot( aes(avgtemp, com_bitter_per, color=location))+
   geom_point()+geom_smooth(method = "lm", formula = y~x)

#Gambier Bay
comm_survey_bitter_temp %>% filter(location=="Gambier Bay") %>%  ggplot( aes(avgtemp, com_bitter_per, color=location))+
   geom_point()+geom_smooth(method = "lm", formula = y~x)
 
#Glacier Bay
comm_survey_bitter_temp %>% filter(location=="Glacier Bay") %>%  ggplot( aes(avgtemp, com_bitter_per, color=location))+
  geom_point()+geom_smooth(method = "lm", formula = y~x) 

#Holkham Bay
comm_survey_bitter_temp %>% filter(location=="Holkham Bay") %>%  ggplot( aes(avgtemp, com_bitter_per, color=location))+
  geom_point()+geom_smooth(method = "lm", formula = y~x)

#Icy Strait
comm_survey_bitter_temp %>% filter(location=="Icy Strait") %>%  ggplot( aes(avgtemp, com_bitter_per, color=location))+
  geom_point()+geom_smooth(method = "lm", formula = y~x)

#Juneau
comm_survey_bitter_temp %>% filter(location=="Juneau") %>%  ggplot( aes(avgtemp, com_bitter_per, color=location))+
  geom_point()+geom_smooth(method = "lm", formula = y~x)

#Lynn Sisters
comm_survey_bitter_temp %>% filter(location=="Lynn Sisters") %>%  ggplot( aes(avgtemp, com_bitter_per, color=location))+
  geom_point()+geom_smooth(method = "lm", formula = y~x)

#Port Camden
comm_survey_bitter_temp %>% filter(location=="Port Camden") %>%  ggplot( aes(avgtemp, com_bitter_per, color=location))+
  geom_point()+geom_smooth(method = "lm", formula = y~x)

#Port Frederick
comm_survey_bitter_temp %>% filter(location=="Port Frederick") %>%  ggplot( aes(avgtemp, com_bitter_per, color=location))+
  geom_point()+geom_smooth(method = "lm", formula = y~x)

#Pybus Bay
comm_survey_bitter_temp %>% filter(location=="Pybus Bay") %>%  ggplot( aes(avgtemp, com_bitter_per, color=location))+
  geom_point()+geom_smooth(method = "lm", formula = y~x)

#Seymour Canal
comm_survey_bitter_temp %>% filter(location=="Seymour Canal") %>%  ggplot( aes(avgtemp, com_bitter_per, color=location))+
  geom_point()+geom_smooth(method = "lm", formula = y~x)

#Thomas Bay
comm_survey_bitter_temp %>% filter(location=="Thomas Bay") %>%  ggplot( aes(avgtemp, com_bitter_per, color=location))+
  geom_point()+geom_smooth(method = "lm", formula = y~x)
