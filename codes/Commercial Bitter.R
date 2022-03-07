# get commercial bitter rates from fish tickets
Comm_catch<- read_csv("data/Tanner Fish tickets 06-22.csv")
#need to create bitter% for each stat area by year
Comm_catch %>% select("Date Fishing Began", "Season", "Stat Area", "Delivery Condition Code and Name", "Sequential Number", "Landed Weight (sum)") %>% 
  clean_names()->Comm_catch

as.POSIXct(Comm_catch$date_fishing_began, format="%m-%d-%Y")
as.Date(Comm_catch$date_fishing_began, "%m/%d/%Y")->Comm_catch$date_fishing_began
format(Comm_catch$date_fishing_began, format="%Y")->Comm_catch$year

Comm_catch %>% 
  group_by(year, stat_area, delivery_condition_code_and_name) %>% 
  summarise(full_catch = sum(landed_weight_sum))-> Comm_sum

Comm_sum %>% 
  spread(delivery_condition_code_and_name, full_catch) %>% clean_names()->comm_bitter_sum

comm_bitter_sum %>% 
  mutate(total_crab = sum( x01_whole_fish_food_fish, x75_softshell_crab,x76_bitter_crab,x79_deadloss_shellfish_only,
                           x95_personal_use_not_sold, x98_discarded_at_sea, na.rm = T),
         per_bitter = (x76_bitter_crab/total_crab)*100, 
         per_bitter = round(ifelse(is.na(per_bitter), 0, per_bitter), 1)) %>% 
  select(year,stat_area, total_crab,x76_bitter_crab, per_bitter)  -> com_bitter_summary

rename(com_bitter_summary, total_bitter= x76_bitter_crab )-> com_bitter_summary

com_bitter_summary %>% mutate(
  total_bitter = round(ifelse(is.na(total_bitter), 0, total_bitter), 1)) -> com_bitter_summary #summary of all bitter% for SE AK 2006-2021

#add names to match up areas with temperature data
within(com_bitter_summary, 
       com_bitter_summary$location <- ifelse(stat_area==11480, "Excursion Inlet",
                                                                 ifelse(stat_area==11470, "Glacier Bay",
                                                                        ifelse(stat_area==11423, "Icy Strait",
                                                                               ifelse(stat_area==11510, "Lynn Sisters",
                                                                                      ifelse(stat_area==11141, "Juneau",
                                                                                             ifelse(stat_area==11140, "Juneau",
                                                                                                    ifelse(stat_area==11431,"Port Frederick",
                                                                                                           ifelse(stat_area==11434,"Port Frederick",
                                                                                                                  ifelse(stat_area==11432,"Port Frederick",
                                                                                                                         ifelse(stat_area==11433,"Port Frederick",
                                                                                                                                ifelse(stat_area==11115,"Seymour Canal",
                                                                                                                                       ifelse(stat_area==11114,"Seymour Canal",
                                                                                                                                              ifelse(stat_area==11121, "Holkham Bay",
                                                                                                                                                     ifelse(stat_area==11023, "Gambier Bay",
                                                                                                                                                            ifelse(stat_area==11022, "Pybus Bay",
                                                                                                                                                                   ifelse(stat_area==11012, "Thomas Bay",
                                                                                                                                                                          ifelse(stat_area==10940, "Port Camden",
                                                                                                                                                                                 ifelse(stat_area==10941,"Port Camden",
                                                                                                                                                                                        ifelse(stat_area==10942,"Port Camden",
                                                                                                                                                                                               ifelse(stat_area==10943, "Port Camden",
                                                                                                                                                                                                      ifelse(stat_area==11356, "Deadman Reach",
                                                                                                                                                                                                             ifelse(stat_area==11354, "Rodman Bay",NA))))))))))))))))))))))
       )->com_names
com_names$com_bitter_summary-> new_com_sum
na.omit(new_com_sum)->new_com_sum
library(data.table)
setnames(new_com_sum, old = c("total_crab", "total_bitter", "per_bitter"), 
         new = c("com_total_crab", "com_total_bitter", "com_bitter_per"))
#subtract one from commercial dataframe so the summer/spring survey match up with the fishery
lapply(new_com_sum, transform, as.numeric(year)=year-1)


merge(bitter_temp, new_com_sum, by=c("location", "year"))->comm_survey_bitter_temp                       