lm_eqn <- function(bitter_temp){
  m <- lm(y ~ x, bitter_temp);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}



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

#plot Bitter% by year
bitter_temp %>% ggplot(aes(year, per_bitter,  color=location))+
  geom_point()+
geom_line(aes(group=location))

#split areas up for easier view
bitter_temp %>% filter(location=="Barlow Cove"|location=="Excursion Inlet"|location=="Glacier Bay"
                       |location=="Icy Strait"|location=="Juneau"|location=="Lynn Sisters"|
                         location=="Stephens Passage"|location=="Holkham Bay")%>% 
  ggplot(aes(year, per_bitter,  color=location))+
  geom_point()+
  geom_line(aes(group=location))+
  xlab("Year")+ylab("Bitter Percent")+labs(color="Location")

bitter_temp %>% filter(location=="Deadman Reach"|location=="Gambier Bay"|location=="Port Camden"|
                         location=="Port Frederick"|location=="Pybus Bay"|location=="Rodman Bay"|
                         location=="Seymour Canal"|location=="Thomas Bay")%>% 
  ggplot(aes(year, per_bitter,  color=location))+
  geom_point()+
  geom_line(aes(group=location))+
  xlab("Year")+ylab("Bitter Percent")+labs(color="Location")
#plot bitter% and temp

summary(lm(per_bitter~avgtemp, barlowbitter))

par(mar = c(5, 4, 4, 4) + 0.3) 

plot(per_bitter~year, barlowbitter, pch=16, col=2,
     xlab="", ylab="")
par(new=T)
plot(avgtemp~year, barlowbitter, pch=17, col=3,
     axes=F, xlab="", ylab="")
axis(side=4, at=pretty(range(barlowbitter$avgtemp)))
mtext("avg temp", side=4, line = 3)
mtext("Bitter%", side = 2, line = 2)
legend("topleft", legend = c("Bitter%", "Avg Temp"),
       col = 2:3, pch = 16:17)
abline(-565.834, 0.283, col=2)
abline(-150.43580, 0.07739, col=3)

bitter_temp %>% filter(location=="Barlow Cove") %>%  ggplot( aes(avgtemp, per_bitter, color=location))+
  geom_point()+geom_smooth(method = "lm", formula = y~x)

#full plot
ggplot(bitter_temp, aes(avgtemp, per_bitter))+
  geom_smooth(method = "lm", se=T, color="black", formula = y~x) + geom_point()+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = T) 
#only a few locations with mean temps greater than 7.5 and all of them have close to 0% bitter rates
#plotting again with the removal of these rates
bitter_temp %>% filter(avgtemp < 7.5) %>%  ggplot( aes(avgtemp, per_bitter))+
  geom_smooth(method = "lm", se=T, color="black", formula = y~x) + geom_point()+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = T) 

#plot low and high bitter % separately 
#plot for larger bitter %
bitter_temp %>% filter(per_bitter >10) %>%  ggplot( aes(avgtemp, per_bitter))+
  geom_smooth(method = "lm", se=T, color="black", formula = y~x) + geom_point()+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = T) 
  
#plot for lower bitter %
  bitter_temp %>% filter(per_bitter <10, avgtemp<7.5) %>%  ggplot( aes(avgtemp, per_bitter))+
    geom_smooth(method = "lm", se=T, color="black", formula = y~x) + geom_point()+
    stat_poly_eq(formula = y~x, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = T) 



#look at each location individually 
bitter_temp %>% filter(location=="Barlow Cove") %>%  ggplot( aes(avgtemp, per_bitter, color=year))+
  geom_smooth(method = "lm", se=T, color="black", formula = y~x) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + geom_point()+
  ggtitle("Barlow Cove")


bitter_temp %>% filter(location=="Deadman Reach") %>%  ggplot( aes(avgtemp, per_bitter, color=year))+
  geom_smooth(method = "lm", se=T, color="black", formula = y~x) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + geom_point()+ggtitle("Deadman Reach")

bitter_temp %>% filter(location=="Excursion Inlet") %>%  ggplot( aes(avgtemp, per_bitter, color=year))+
  geom_smooth(method = "lm", se=T, color="black", formula = y~x) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + geom_point()+ggtitle("Excursion Inlet")

bitter_temp %>% filter(location=="Gambier Bay") %>%  ggplot( aes(avgtemp, per_bitter, color=year))+
  geom_smooth(method = "lm", se=T, color="black", formula = y~x) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + geom_point()+ggtitle("Gambier Bay")

bitter_temp %>% filter(location=="Glacier Bay") %>%  ggplot( aes(avgtemp, per_bitter, color=year))+
  geom_smooth(method = "lm", se=T, color="black", formula = y~x) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + geom_point()+ggtitle("Glacier Bay")

bitter_temp %>% filter(location=="Holkham Bay") %>%  ggplot( aes(avgtemp, per_bitter, color=year))+
  geom_smooth(method = "lm", se=T, color="black", formula = y~x) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + geom_point()+ggtitle("Holkham Bay")

bitter_temp %>% filter(location=="Icy Strait") %>%  ggplot( aes(avgtemp, per_bitter, color=year))+
  geom_smooth(method = "lm", se=T, color="black", formula = y~x) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + geom_point()+ggtitle("Icy Strait")

bitter_temp %>% filter(location=="Juneau") %>%  ggplot( aes(avgtemp, per_bitter, color=year))+
  geom_smooth(method = "lm", se=T, color="black", formula = y~x) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + geom_point()+ggtitle("Juneau")

bitter_temp %>% filter(location=="Lynn Sisters") %>%  ggplot( aes(avgtemp, per_bitter, color=year))+
  geom_smooth(method = "lm", se=T, color="black", formula = y~x) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + geom_point()+ggtitle("Lynn Sisters")

bitter_temp %>% filter(location=="Port Camden") %>%  ggplot( aes(avgtemp, per_bitter, color=year))+
  geom_smooth(method = "lm", se=T, color="black", formula = y~x) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + geom_point()+ggtitle("Port Camden")

bitter_temp %>% filter(location=="Port Frederick") %>%  ggplot( aes(avgtemp, per_bitter, color=year))+
  geom_smooth(method = "lm", se=T, color="black", formula = y~x) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + geom_point()+ggtitle("Port Frederick")

bitter_temp %>% filter(location=="Pybus Bay") %>%  ggplot( aes(avgtemp, per_bitter, color=year))+
  geom_smooth(method = "lm", se=T, color="black", formula = y~x) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + geom_point()+ggtitle("Pybus Bay")

bitter_temp %>% filter(location=="Seymour Canal") %>%  ggplot( aes(avgtemp, per_bitter, color=year))+
  geom_smooth(method = "lm", se=T, color="black", formula = y~x) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + geom_point()+ggtitle("Seymour Canal")


