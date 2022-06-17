# calculate landings and $ by species by port (not port group)
# species making up the top 90% of revenue of catch
# most recent 10 years, average across the 10 years
# can try annually but must check for confidentiality

library(plyr)
library(data.table)
library(dplyr)
library(tidyverse)
library(lubridate)
library(reshape2)

############################################################
############ load data and clean it up
############################################################

# ### Load compiled fish ticket data, 2009-2018
# data not included because of confidentiality - need to request from PacFIN
pacfin.df.2009_2018 <- readRDS("~/Documents/Koehn et al. cc vulnerability/Data/pacfin_compiled_2009thru2018.rds")
dim(pacfin.df.2009_2018) # [1] 4082326     101
glimpse(pacfin.df.2009_2018)

pacfin.df.2009_2018$LANDING_DATE2 <- as.Date(pacfin.df.2009_2018$LANDING_DATE, "%d-%b-%y")

pacfin.df.2009_2018 <- pacfin.df.2009_2018 %>% 
  mutate(tdate = ymd(LANDING_DATE2))

# ## QC: check first and last date of landing for each year, and total tickets per year. 
# pacfin.df.2009_2018 %>% group_by(LANDING_YEAR) %>%
#   summarise(first_ticket = min(tdate), last_ticket = max(tdate), total_tickets = length(unique(FTID)))
# # note that the last ticket for 2016 is Sep 9.

# pacfin.df.2009_2018_ts <- pacfin.df.2009_2018 %>% group_by(tdate) %>%
#   summarise(total_tickets = length(unique(FTID))) 
# glimpse(pacfin.df.2009_2018_ts)
# 
# ggplot(pacfin.df.2009_2018_ts %>% 
#          filter(tdate > ymd('2016-09-01')) %>%
#          filter(tdate < ymd('2017-01-31')
#                 ), aes(x=tdate, y=total_tickets)) +
#   geom_point() +
#   geom_line() +
#   #scale_x_datetime(limits = c(as.Date('09-01-2016', "%m-%d-%Y"),as.Date('01-31-2017', "%m-%d-%Y"))) +
#   theme_bw()

pacfin.df.2009_2018$spid <- pacfin.df.2009_2018$PACFIN_SPECIES_CODE

############################################################
############ make a summary of data over 10 yr period
############################################################

# make a summary df with all species landed over 10yr period. will include confidential data
summary.port.allyears <- data.frame(
  pacfin.df.2009_2018 %>%
    group_by(PACFIN_PORT_CODE) %>%
    mutate(total.dollars = sum(EXVESSEL_REVENUE, na.rm=TRUE), total.mtons = sum(LANDED_WEIGHT_MTONS, na.rm=TRUE)) %>%
    group_by(PACFIN_PORT_CODE, spid) %>% 
    summarise(
      dollars = sum(EXVESSEL_REVENUE, na.rm=TRUE), 
      mtons = sum(LANDED_WEIGHT_MTONS, na.rm=TRUE),
      total.dollars = unique(total.dollars),
      total.mtons = unique(total.mtons),
      num.vessels = length(unique(VESSEL_ID)),
      .groups = "drop"
    ) %>%
    mutate(percent.rev = 100*as.numeric(dollars)/total.dollars, percent.mtons = 100*as.numeric(mtons)/total.mtons) %>%
    dplyr::select(PACFIN_PORT_CODE, spid, dollars, mtons, percent.rev, percent.mtons,num.vessels) %>% 
    arrange(PACFIN_PORT_CODE, -dollars) # sort by port and from most to least revenue
)
dim(summary.port.allyears) #6358    7
glimpse(summary.port.allyears)

############################################################
############ make a summary based on top 90% by revenue
############################################################

# make a summary df with all species landed over 10yr period that account for up to 90% of revenue. will NOT include confidential data because i redact any info about species if fewer than 3 vessels landed it.
summary.port.allyears.top90 <- data.frame(
  summary.port.allyears %>%
  group_by(PACFIN_PORT_CODE) %>%
  arrange(-percent.rev) %>%
  mutate(cum.percent.rev = 100*cumsum(as.numeric(dollars))/sum(as.numeric(dollars)), cum.percent.mtons = 100*cumsum(as.numeric(mtons))/sum(as.numeric(mtons))) %>% #calculate cumulative percentage of revenues (or mtons) for each species as percentage of revenues (mtons) across all species
  filter(cum.percent.rev <= 90 | min(cum.percent.rev)>90) %>% # subset to species that account for 90% of revenues/yields
    filter(
      rank(cum.percent.rev>90, ties.method="first")==1 | 
        cum.percent.rev<=90
    ) %>% # also subsetting to species that account for 90% of revenues/yields
    mutate( # replace all port-spid combos where <3 vessels contribute
      spid = replace(spid, num.vessels < 3, "CONFIDENTIAL"),
      dollars = replace(dollars, num.vessels < 3, "CONFIDENTIAL"),
      mtons = replace(mtons, num.vessels < 3, "CONFIDENTIAL"),
      percent.rev = replace(percent.rev, num.vessels < 3, "CONFIDENTIAL"),
      percent.mtons = replace(percent.mtons, num.vessels < 3, "CONFIDENTIAL"),
      cum.percent.rev = replace(cum.percent.rev, num.vessels < 3, "CONFIDENTIAL"),
      cum.percent.mtons = replace(cum.percent.mtons, num.vessels < 3, "CONFIDENTIAL")
      ) %>% 
    arrange(PACFIN_PORT_CODE)
)

dim(summary.port.allyears.top90) # [1] 398 9
glimpse(summary.port.allyears.top90)

write.csv(summary.port.allyears.top90,"~/Documents/Koehn et al. cc vulnerability/Data/summary.port.2009_2018.top90_06162022.csv", row.names=FALSE)


############################################################
############ make a summary based on top 90% by landings
############################################################

# make a summary df with all species landed over 10yr period that account for up to 90% of landings. will NOT include confidential data because i redact any info about species if fewer than 3 vessels landed it.
summary.port.allyears.top90landings <- data.frame(
  summary.port.allyears %>%
    group_by(PACFIN_PORT_CODE) %>%
    arrange(-percent.mtons) %>%
    mutate(cum.percent.rev = 100*cumsum(as.numeric(dollars))/sum(as.numeric(dollars)), cum.percent.mtons = 100*cumsum(as.numeric(mtons))/sum(as.numeric(mtons))) %>% #calculate cumulative percentage of revenues (or mtons) for each species as percentage of revenues (mtons) across all species
    filter(cum.percent.mtons <= 90 | min(cum.percent.mtons)>90) %>% # subset to species that account for 90% of revenues/yields
    filter(
      rank(cum.percent.mtons>90, ties.method="first")==1 | 
        cum.percent.mtons<=90
    ) %>% # also subsetting to species that account for 90% of yields
    mutate( # replace all port-spid combos where <3 vessels contribute
      spid = replace(spid, num.vessels < 3, "CONFIDENTIAL"),
      dollars = replace(dollars, num.vessels < 3, "CONFIDENTIAL"),
      mtons = replace(mtons, num.vessels < 3, "CONFIDENTIAL"),
      percent.rev = replace(percent.rev, num.vessels < 3, "CONFIDENTIAL"),
      percent.mtons = replace(percent.mtons, num.vessels < 3, "CONFIDENTIAL"),
      cum.percent.rev = replace(cum.percent.rev, num.vessels < 3, "CONFIDENTIAL"),
      cum.percent.mtons = replace(cum.percent.mtons, num.vessels < 3, "CONFIDENTIAL")
    ) %>% 
    arrange(PACFIN_PORT_CODE)
)

dim(summary.port.allyears.top90landings) # [1] 435 9
glimpse(summary.port.allyears.top90landings)

write.csv(summary.port.allyears.top90landings,"~/Documents/Koehn et al. cc vulnerability/Data/summary.port.2009_2018.top90.landings_06162022.csv", row.names=FALSE)

############################################################
############################################################
############################################################
############################################################


############################################################
############ make a summary of data over 10 yr period for 
############ 9/20-12/31 only each year
############################################################

# subset to Sep-Dec data only each year
pacfin.df.2009_2018_Q4 <- pacfin.df.2009_2018 %>% 
  filter(
    LANDING_MONTH == 9 & LANDING_DAY >=20 |
      LANDING_MONTH >= 10 & LANDING_MONTH <=12
           ) 

## QC: check first and last date of landing for each year, and total tickets per year.
pacfin.df.2009_2018_Q4 %>% group_by(LANDING_YEAR) %>%
  summarise(first_ticket = min(tdate), last_ticket = max(tdate), total_tickets = length(unique(FTID)))
# note that the last ticket for 2016 is Sep 9.

# make a summary df with all species landed over 10yr period. will include confidential data
summary.port.allyears.Q4 <- data.frame(
  pacfin.df.2009_2018_Q4 %>%
    group_by(PACFIN_PORT_CODE) %>%
    mutate(total.dollars = sum(EXVESSEL_REVENUE, na.rm=TRUE), total.mtons = sum(LANDED_WEIGHT_MTONS, na.rm=TRUE)) %>%
    group_by(PACFIN_PORT_CODE, spid) %>% 
    summarise(
      dollars = sum(EXVESSEL_REVENUE, na.rm=TRUE), 
      mtons = sum(LANDED_WEIGHT_MTONS, na.rm=TRUE),
      total.dollars = unique(total.dollars),
      total.mtons = unique(total.mtons),
      num.vessels = length(unique(VESSEL_ID)),
      .groups = "drop"
    ) %>%
    mutate(percent.rev = 100*as.numeric(dollars)/total.dollars, percent.mtons = 100*as.numeric(mtons)/total.mtons) %>%
    dplyr::select(PACFIN_PORT_CODE, spid, dollars, mtons, percent.rev, percent.mtons,num.vessels) %>% 
    arrange(PACFIN_PORT_CODE, -dollars) # sort by port and from most to least revenue
)
dim(summary.port.allyears.Q4) #4590    7
glimpse(summary.port.allyears.Q4)


############################################################
############ make a summary based on top 90% by revenue
############ 9/20-12/31 only each year
############################################################

# make a summary df with all species landed over 10yr period that account for up to 90% of revenue. will NOT include confidential data because i redact any info about species if fewer than 3 vessels landed it.
summary.port.allyears.Q4.top90 <- data.frame(
  summary.port.allyears.Q4 %>%
    group_by(PACFIN_PORT_CODE) %>%
    arrange(-percent.rev) %>%
    mutate(cum.percent.rev = 100*cumsum(as.numeric(dollars))/sum(as.numeric(dollars)), cum.percent.mtons = 100*cumsum(as.numeric(mtons))/sum(as.numeric(mtons))) %>% #calculate cumulative percentage of revenues (or mtons) for each species as percentage of revenues (mtons) across all species
    filter(cum.percent.rev <= 90 | min(cum.percent.rev)>90) %>% # subset to species that account for 90% of revenues/yields
    filter(
      rank(cum.percent.rev>90, ties.method="first")==1 | 
        cum.percent.rev<=90
    ) %>% # also subsetting to species that account for 90% of revenues/yields
    mutate( # replace all port-spid combos where <3 vessels contribute
      spid = replace(spid, num.vessels < 3, "CONFIDENTIAL"),
      dollars = replace(dollars, num.vessels < 3, "CONFIDENTIAL"),
      mtons = replace(mtons, num.vessels < 3, "CONFIDENTIAL"),
      percent.rev = replace(percent.rev, num.vessels < 3, "CONFIDENTIAL"),
      percent.mtons = replace(percent.mtons, num.vessels < 3, "CONFIDENTIAL"),
      cum.percent.rev = replace(cum.percent.rev, num.vessels < 3, "CONFIDENTIAL"),
      cum.percent.mtons = replace(cum.percent.mtons, num.vessels < 3, "CONFIDENTIAL")
    ) %>% 
    arrange(PACFIN_PORT_CODE)
)

dim(summary.port.allyears.Q4.top90) # [1] 398 9
glimpse(summary.port.allyears.Q4.top90)

write.csv(summary.port.allyears.Q4.top90,"~/Documents/Koehn et al. cc vulnerability/Data/summary.port.2009_2018.Q4.top90_06162022.csv", row.names=FALSE)


############################################################
############ make a summary based on top 90% by landings
############ 9/20-12/31 only each year
############################################################

# make a summary df with all species landed over 10yr period that account for up to 90% of landings. will NOT include confidential data because i redact any info about species if fewer than 3 vessels landed it.
summary.port.allyears.Q4.top90landings <- data.frame(
  summary.port.allyears.Q4 %>%
    group_by(PACFIN_PORT_CODE) %>%
    arrange(-percent.mtons) %>%
    mutate(cum.percent.rev = 100*cumsum(as.numeric(dollars))/sum(as.numeric(dollars)), cum.percent.mtons = 100*cumsum(as.numeric(mtons))/sum(as.numeric(mtons))) %>% #calculate cumulative percentage of revenues (or mtons) for each species as percentage of revenues (mtons) across all species
    filter(cum.percent.mtons <= 90 | min(cum.percent.mtons)>90) %>% # subset to species that account for 90% of revenues/yields
    filter(
      rank(cum.percent.mtons>90, ties.method="first")==1 | 
        cum.percent.mtons<=90
    ) %>% # also subsetting to species that account for 90% of yields
    mutate( # replace all port-spid combos where <3 vessels contribute
      spid = replace(spid, num.vessels < 3, "CONFIDENTIAL"),
      dollars = replace(dollars, num.vessels < 3, "CONFIDENTIAL"),
      mtons = replace(mtons, num.vessels < 3, "CONFIDENTIAL"),
      percent.rev = replace(percent.rev, num.vessels < 3, "CONFIDENTIAL"),
      percent.mtons = replace(percent.mtons, num.vessels < 3, "CONFIDENTIAL"),
      cum.percent.rev = replace(cum.percent.rev, num.vessels < 3, "CONFIDENTIAL"),
      cum.percent.mtons = replace(cum.percent.mtons, num.vessels < 3, "CONFIDENTIAL")
    ) %>% 
    arrange(PACFIN_PORT_CODE)
)

dim(summary.port.allyears.Q4.top90landings) # [1] 326 9
glimpse(summary.port.allyears.Q4.top90landings)

write.csv(summary.port.allyears.Q4.top90landings,"~/Documents/Koehn et al. cc vulnerability/Data/summary.port.2009_2018.Q4.top90.landings_06162022.csv", row.names=FALSE)


############################################################
############ make an annual summary of data over 10 yr period for 
############ 9/20-12/31 only each year
############################################################

# make a summary df with all species landed over 10yr period. will include confidential data
summary.port.allyears.Q4_annual <- data.frame(
  pacfin.df.2009_2018_Q4 %>%
    group_by(PACFIN_PORT_CODE, LANDING_YEAR) %>%
    mutate(total.dollars = sum(EXVESSEL_REVENUE, na.rm=TRUE), total.mtons = sum(LANDED_WEIGHT_MTONS, na.rm=TRUE)) %>%
    group_by(LANDING_YEAR, PACFIN_PORT_CODE, spid) %>% 
    summarise(
      dollars = sum(EXVESSEL_REVENUE, na.rm=TRUE), 
      mtons = sum(LANDED_WEIGHT_MTONS, na.rm=TRUE),
      total.dollars = unique(total.dollars),
      total.mtons = unique(total.mtons),
      num.vessels = length(unique(VESSEL_ID)),
      .groups = "drop"
    ) %>%
    mutate(percent.rev = 100*as.numeric(dollars)/total.dollars, percent.mtons = 100*as.numeric(mtons)/total.mtons) %>%
    dplyr::select(PACFIN_PORT_CODE, LANDING_YEAR, spid, dollars, mtons, percent.rev, percent.mtons,num.vessels) %>% 
    arrange(LANDING_YEAR, PACFIN_PORT_CODE, -dollars) # sort by year, port, and from most to least revenue
)
dim(summary.port.allyears.Q4_annual) #18472    8
glimpse(summary.port.allyears.Q4_annual)




############################################################
############ make an annual summary based on top 90% by revenue
############ 9/20-12/31 only each year
############################################################

# make a summary df with all species landed over 10yr period that account for up to 90% of revenue. will NOT include confidential data because i redact any info about species if fewer than 3 vessels landed it.
summary.port.allyears.Q4.top90_annual <- data.frame(
  summary.port.allyears.Q4_annual %>%
    group_by(LANDING_YEAR, PACFIN_PORT_CODE) %>%
    arrange(-percent.rev) %>%
    mutate(cum.percent.rev = 100*cumsum(as.numeric(dollars))/sum(as.numeric(dollars)), cum.percent.mtons = 100*cumsum(as.numeric(mtons))/sum(as.numeric(mtons))) %>% #calculate cumulative percentage of revenues (or mtons) for each species as percentage of revenues (mtons) across all species
    filter(cum.percent.rev <= 90 | min(cum.percent.rev)>90) %>% # subset to species that account for 90% of revenues/yields
    filter(
      rank(cum.percent.rev>90, ties.method="first")==1 | 
        cum.percent.rev<=90
    ) %>% # also subsetting to species that account for 90% of revenues/yields
    mutate( # replace all port-spid combos where <3 vessels contribute
      spid = replace(spid, num.vessels < 3, "CONFIDENTIAL"),
      dollars = replace(dollars, num.vessels < 3, "CONFIDENTIAL"),
      mtons = replace(mtons, num.vessels < 3, "CONFIDENTIAL"),
      percent.rev = replace(percent.rev, num.vessels < 3, "CONFIDENTIAL"),
      percent.mtons = replace(percent.mtons, num.vessels < 3, "CONFIDENTIAL"),
      cum.percent.rev = replace(cum.percent.rev, num.vessels < 3, "CONFIDENTIAL"),
      cum.percent.mtons = replace(cum.percent.mtons, num.vessels < 3, "CONFIDENTIAL")
    ) %>% 
    arrange(LANDING_YEAR, PACFIN_PORT_CODE)
)

dim(summary.port.allyears.Q4.top90_annual) # [1] 1996 10
glimpse(summary.port.allyears.Q4.top90_annual)

write.csv(summary.port.allyears.Q4.top90_annual,"~/Documents/Koehn et al. cc vulnerability/Data/summary.port.allyears.Q4.top90_annual_06162022.csv", row.names=FALSE)


############################################################
############ make an annual summary based on top 90% by landings
############ 9/20-12/31 only each year
############################################################

# make a summary df with all species landed over 10yr period that account for up to 90% of landings. will NOT include confidential data because i redact any info about species if fewer than 3 vessels landed it.
summary.port.allyears.Q4.top90landings_annual <- data.frame(
  summary.port.allyears.Q4_annual %>%
    group_by(LANDING_YEAR, PACFIN_PORT_CODE) %>%
    arrange(-percent.mtons) %>%
    mutate(cum.percent.rev = 100*cumsum(as.numeric(dollars))/sum(as.numeric(dollars)), cum.percent.mtons = 100*cumsum(as.numeric(mtons))/sum(as.numeric(mtons))) %>% #calculate cumulative percentage of revenues (or mtons) for each species as percentage of revenues (mtons) across all species
    filter(cum.percent.mtons <= 90 | min(cum.percent.mtons)>90) %>% # subset to species that account for 90% of revenues/yields
    filter(
      rank(cum.percent.mtons>90, ties.method="first")==1 | 
        cum.percent.mtons<=90
    ) %>% # also subsetting to species that account for 90% of yields
    mutate( # replace all port-spid combos where <3 vessels contribute
      spid = replace(spid, num.vessels < 3, "CONFIDENTIAL"),
      dollars = replace(dollars, num.vessels < 3, "CONFIDENTIAL"),
      mtons = replace(mtons, num.vessels < 3, "CONFIDENTIAL"),
      percent.rev = replace(percent.rev, num.vessels < 3, "CONFIDENTIAL"),
      percent.mtons = replace(percent.mtons, num.vessels < 3, "CONFIDENTIAL"),
      cum.percent.rev = replace(cum.percent.rev, num.vessels < 3, "CONFIDENTIAL"),
      cum.percent.mtons = replace(cum.percent.mtons, num.vessels < 3, "CONFIDENTIAL")
    ) %>% 
    arrange(LANDING_YEAR, PACFIN_PORT_CODE)
)

dim(summary.port.allyears.Q4.top90landings_annual) # [1] 2176 10
glimpse(summary.port.allyears.Q4.top90landings_annual)

write.csv(summary.port.allyears.Q4.top90landings_annual,"~/Documents/Koehn et al. cc vulnerability/Data/summary.port.2009_2018.Q4.top90.landings_annual_06162022.csv", row.names=FALSE)

