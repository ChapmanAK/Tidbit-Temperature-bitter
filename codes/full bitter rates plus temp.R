#Use Katies code to get bitter rate by location from 2005 through 2021 then combine with mean tidbit temp for each location,
#maybe split by depth?

raw_data<-rbind(read_csv("data/05-07 Tanner RKC survey crab info.csv"), read_csv("data/08-10 Tanner RKC survey crab info.csv"),
                read_csv("data/11-13 Tanner RKC survey crab info.csv"), read_csv("data/14-15 Tanner RKC survey crab info.csv"),
                read_csv("data/16-18 Tanner RKC survey crab info.csv"),read_csv("data/19-21 Tanner RKC survey crab info.csv")
)

#select only Tanner crab

raw_data<- raw_data %>% filter(Species=="Bairdi tanner crab")
raw_data<- clean_names(raw_data)

raw_data %>% 
  group_by(location, year, parasite) %>% 
  summarise(total_crab = sum(number_of_specimens))-> by_survey_area

by_survey_area %>% 
  spread(parasite, total_crab) %>% clean_names() -> parasite_sum

#summary of bitter % by location for each year

parasite_sum %>% 
  mutate(total_crab = sum( bitter_crab, none_present, briarosaccus_double_externa,
                          briarosaccus_double_scar, briarosaccus_single_externa, briarosaccus_single_scar,
                          carcinonemertes_errans, na, na.rm = T),
                          per_bitter = (bitter_crab/total_crab)*100, 
         per_bitter = round(ifelse(is.na(per_bitter), 0, per_bitter), 1)) -> bitter_summary
#clean up
bitter_summary<- bitter_summary %>% select(location, year, bitter_crab, total_crab, per_bitter)

write.csv(bitter_summary,"C:\\Users\\zmchapman\\Documents\\Survey Output\\Tidbit temps\\Bitter Summary.csv", row.names = FALSE)
#look at each location by year
bitter_summary %>% 
  select(location, year, per_bitter) %>% 
  spread(year, per_bitter) %>% 
  arrange(desc(`2020`)) -> table_bitter

#same but for temp (probably remove)
#bitter_summary %>% 
#   select(location, year, avgtemp) %>% 
#   spread(year, avgtemp) %>% 
#   arrange(desc(`2020`)) -> table_temp

#now add in temp data
tidtemp<- read_csv("data/Tidbit master.csv") %>% clean_names() %>% select(year, location, depth_fathoms, meantemp)

library(plotrix)
#create avg temp for each location with SE by year
tidtemp %>% group_by(location, year) %>% 
  summarise(SE= std.error(meantemp, na.rm = T), avgtemp= mean(meantemp, na.rm = T), mean.depth= mean(depth_fathoms, na.rm=T))  -> tidtemp_avg

#combine and remove na's  
 merge(tidtemp_avg, bitter_summary, by=c("location", "year")) %>% na.omit()  -> bitter_temp
bitter_temp$avgtemp<- round(bitter_temp$avgtemp, 2)
bitter_temp$SE<- round(bitter_temp$SE, 2)
bitter_temp$mean.depth<- round(bitter_temp$mean.depth, 2)
 #summary table for temps
 bitter_temp %>% 
    select(location, year, avgtemp) %>% 
    spread(year, avgtemp)  -> table_temp
 
 #find relationship between temp and bitter percent
 #barlow data
 
 bitter_temp %>% filter(location=="Barlow Cove") -> barlowbitter
 anova(with(barlowbitter, lm(per_bitter~avgtemp+year)))
 
 