### read in coding of 87 data, parse
### compute cohen's k for IRR measure
library(tidyverse)

# read
la<-read_csv("./data/bs_arcia_coding_la87.csv") %>% 
  rename_with(~ paste(.x, "_la", sep = ""),
              .cols = state:attendance_day) %>% 
  filter(year == 1887)
kc<-read_csv("./data/bs_arcia_coding_kc87.csv")%>% 
  rename_with(~ paste(.x, "_kc", sep = ""),
              .cols = state:attendance_day)
# prior data, parse and harmonize
# restrict to boarding for comparison
# rb<-read_csv("./data/dataset_schools_ARCIA_rb87.csv") %>% 
#   filter(year == 1887) %>% 
#   filter(school_type == "boarding") %>% 
#   select(year, 
#          statesuperintendency,
#          school,
#          avg_attendance, 
#          school_capacity) %>% 
#   rename(capacity_boarding = school_capacity, 
#          attendance_boarding = avg_attendance,
#          state.name = statesuperintendency)
# xwalk<-data.frame(state.name = state.name,
#                   state = state.abb)
# 
# rb<-rb %>% 
#   left_join(xwalk) %>% 
#   select(year, state, school, attendance_boarding:capacity_boarding) %>% 
#   rename_with(~ paste(.x, "_rb", sep = ""),
#               .cols = state:attendance_boarding)
### bind all and check for pairwise agreement
temp<-la %>% 
  bind_cols(kc %>% 
              select(-year))

### check disagreements: state
table(temp$state_la == temp$state_kc)
table(temp$state_la == temp$state_kc) / nrow(temp)


temp %>% 
  filter(state_la!=state_kc) %>% 
  select(state_la, state_kc) %>% 
  print(n = 1e5)

### check disagreements: school name
temp <- temp %>% 
  mutate(school_la = tolower(school_la),
         school_kc = tolower(school_kc))

table(temp$school_la == temp$school_kc)
table(temp$school_la == temp$school_kc) / nrow(temp)

temp %>% 
  filter(school_la!=school_kc) %>% 
  select(state_la, school_la, school_kc) %>% 
  print(n = 1e5)

### check disagreements: capacity_boarding
table(temp$capacity_boarding_la == temp$capacity_boarding_kc) 
table(temp$capacity_boarding_la == temp$capacity_boarding_kc) / nrow(temp)
temp %>% 
  filter(is.na(capacity_boarding_la) & !(is.na(capacity_boarding_kc))) %>% 
  select(state_la, school_la, capacity_boarding_la, capacity_boarding_kc)

temp %>% 
  filter(capacity_boarding_la!=capacity_boarding_kc) %>% 
  select(capacity_boarding_la, capacity_boarding_kc) %>% 
  print(n = 1e5)

### check disagreements: capacity_day
table(temp$capacity_day_la == temp$capacity_day_kc) 
table(temp$capacity_day_la == temp$capacity_day_kc) / nrow(temp)
temp %>% 
  filter(is.na(capacity_day_la) & !(is.na(capacity_day_kc))) %>% 
  select(state_la, school_la, capacity_day_la, capacity_day_kc)

temp %>% 
  filter(capacity_day_la!=capacity_day_kc) %>% 
  select(capacity_day_la, capacity_day_kc) %>% 
  print(n = 1e5)

### check disagreements: attendance_day
table(temp$attendance_day_la == temp$attendance_day_kc) 
table(temp$attendance_day_la == temp$attendance_day_kc) / nrow(temp)
temp %>% 
  filter(is.na(attendance_day_la) & !(is.na(attendance_day_kc))) %>% 
  select(state_la, school_la, attendance_day_la, attendance_day_kc)

temp %>% 
  filter(attendance_day_la!=attendance_day_kc) %>% 
  select(attendance_day_la, attendance_day_kc) %>% 
  print(n = 1e5)

### check disagreements: attendance_boarding
table(temp$attendance_boarding_la == temp$attendance_boarding_kc) 
table(temp$attendance_boarding_la == temp$attendance_boarding_kc) / nrow(temp)
temp %>% 
  filter(is.na(attendance_boarding_la) & !(is.na(attendance_boarding_kc))) %>% 
  select(state_la, school_la, attendance_boarding_la, attendance_boarding_kc)

temp %>% 
  filter(attendance_boarding_la!=attendance_boarding_kc) %>% 
  select(attendance_boarding_la, attendance_boarding_kc) %>% 
  print(n = 1e5)
