setwd("~/Documents/NICAR 2018")
library(tidyverse)
library(lubridate)
library(scales)

# load and view ca medical board disciplinary actions data
(ca_discipline <- read_csv("ca_discipline.csv"))
# load and View opioid prescription data
ca_opioids <- read_csv("ca_medicare_opioids.csv")
# load and view license data
npi_license <- read_csv("npi_license.csv")
# join those two data frames
ca_discipline_npi <- left_join(ca_discipline, npi_license)
#By default, dplyr will join by any variables with matching names, but you can also specify the variables on which to join. So this will achieve the same result:
ca_discipline_npi <- left_join(ca_discipline, npi_license, by = "license")

# look at types of disciplinary actions
(types <- ca_discipline %>% select(action_type) %>% unique())

#show cols with Na and their numbers
colSums(is.na(ca_discipline)>0)
#get names of cols with Na
names(ca_discipline)[colSums(is.na(ca_discipline)) > 0]

# filter for license revocations only
(revoked <- ca_discipline %>% filter(action_type == "Revoked"))

# doctors in Berkeley or Oakland who have had their licenses revoked 
(revoked_oak_berk1 <- ca_discipline %>% filter(action_type == "Revoked" & 
	(city == "Oakland" | city == "Berkeley")))


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
(revoked_ca_year <- ca_discipline %>% filter(action_type == "Revoked" & state == "CA") 
	%>% group_by(year) %>% summarize(revocations = n()))
# license revokations for doctors based in California, by month for year not 2008/can 
#also use year >= 2009
(revoked_ca_month <- ca_discipline %>% filter(action_type == "Revoked"  & state == "CA" 
	& year != 2008) %>%group_by(month) %>% summarize(revocations = n()))
# Create a summary, showing the number of opioid prescriptions written by each provider, 
#the total cost of the opioids prescribed, and the cost per claim
(provider_summary <- ca_opioids %>%  group_by(npi,nppes_provider_last_org_name,
	nppes_provider_first_name,nppes_provider_city,specialty_description) %>% 
	summarize(prescriptions = sum(total_claim_count),cost = sum(total_drug_cost)) 
	%>% mutate(cost_per_prescription = cost/prescriptions) %>% arrange(desc(prescriptions)))
