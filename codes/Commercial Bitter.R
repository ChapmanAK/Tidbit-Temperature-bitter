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
  summarise(full_catch = (landed_weight_sum))


#DELETE WHEN DONE
raw_data %>% 
  group_by(location, year, parasite) %>% 
  summarise(total_crab = sum(number_of_specimens))-> by_survey_area

by_survey_area %>% 
  spread(parasite, total_crab) %>% clean_names() -> parasite_sum
#DELETE WHEN DONE