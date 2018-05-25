setwd("C:/Users/PI CONSULTING/Documents/NICAR 2018")
library(tidyverse)
library(lubridate)

# load and view ca medical board disciplinary actions data
(ca_discipline <- read_csv("ca_discipline.csv"))

# look at types of disciplinary actions
(types <- ca_discipline %>% select(action_type) %>% unique())

#show cols with Na and their numbers
colSums(is.na(ca_discipline)>0)
#get names of cols with Na
names(ca_discipline)[colSums(is.na(ca_discipline)) > 0]

# filter for license revocations only
(revoked <- ca_discipline %>% filter(action_type == "Revoked"))

# doctors in Berkeley or Oakland who have had their licenses revoked 
(revoked_oak_berk1 <- ca_discipline %>% filter(action_type == "Revoked" & (city == "Oakland" | city == "Berkeley")))


#Append data using bind_rows
# doctors in Berkeley who had their licenses revoked
(revoked_berk <- ca_discipline %>% filter(action_type == "Revoked" & city == "Berkeley"))

# doctors in Oakland who had their licenses revoked
(revoked_oak <- ca_discipline %>% filter(action_type == "Revoked" & city == "Oakland"))

# doctors in Berkeley or Oakland who have had their licenses revoked
(revoked_oak_berk2 <- bind_rows(revoked_oak, revoked_berk))

#Write data to a CSV file
write_csv(revoked_oak_berk, "revoked_oak_berk.csv", na = "")


##Group and summarize data
# extract year and month from action_date
(ca_discipline <- ca_discipline %>% mutate(year = year(action_date), month = month(action_date)))

# license revokations for doctors based in California, by year
revoked_ca_year <- ca_discipline %>% filter(action_type == "Revoked" & state == "CA") %>% group_by(year) %>%
  summarize(revocations = n())
# license revokations for doctors based in California, by month for year not 2008/ can also use
# year >= 2009
(revoked_ca_month <- ca_discipline %>% filter(action_type == "Revoked"  & state == "CA" & year != 2008) %>%group_by(month) %>% summarize(revocations = n()))
