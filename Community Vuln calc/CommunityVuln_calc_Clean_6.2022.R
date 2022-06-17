# Code to calculate community vulnerability for Koehn et al. 2022
library(dplyr)

ports <- read.table("PacFINcommunities.csv", header = TRUE, sep = ",")
# access from dryad, created from https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/agency_ports.txt
portcities = as.data.frame(ports$Name)

portID = read.csv("PacFINports.csv", header = TRUE)
# access from dryad, created from https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/agency_ports.txt
portID$PortName = toupper(portID$PortName)

AllMetrics2017 = read.csv("2017_reliance_SVI.csv", header = TRUE)
# access from dryad
AllMetrics2017$GEO_NAME = toupper(as.character(AllMetrics2017$GEO_NAME))

'%!in%' <- function(x,y)!('%in%'(x,y))
index = which(ports$Name %!in% AllMetrics2017$GEO_NAME & ports$ALT_NAME %!in% AllMetrics2017$GEO_NAME)
ports$Name[index]

index = which(ports$Name %in% AllMetrics2017$GEO_NAME | ports$ALT_NAME %in% AllMetrics2017$GEO_NAME)
CommunityList = ports[index,]

CommunityList$ALT_NAME = (as.character(CommunityList$ALT_NAME))

CommunityUse = AllMetrics2017[which(AllMetrics2017$GEO_NAME %in% CommunityList$Name | AllMetrics2017$GEO_NAME %in% CommunityList$ALT_NAME),]

All = left_join(x = CommunityUse, y = CommunityList, by = c("GEO_NAME" = "ALT_NAME"))
which(All$Name == "SMITH RIVER-GASQUET, CA") # should be 2, good

## ALL PORTS
portIDonly = portID %>% select(1, 6)
AllportID = left_join(x = All, y = portIDonly, by = c("Name" = "Community"))

#Remove columns we don't need
AllportID = AllportID %>% select(GEO_ID2, Year, STATEABBR, GEO_NAME, MAPNAME,
                                 PRIMARY_LATITUDE, PRIMARY_LONGITUDE, PerDis, 
                                 PopCom, Pvrty, LabFrc, HsChr, HsDis, RetMig, UrbSpl,
                                 LabFrc_Rev, HsChr_Rev, ComEng, ComRel, Name, PCID)

######### FIND census tracts for all ports ######## 

library(acs)
library(sf)
library(tidyverse)

library(tigris)
library(tidycensus)
apikey = "" #enter own unique API key
census_api_key(key=apikey, install = TRUE)

### grabbing census areas that overlap each community
# test a city #####
wa.tracts <- get_acs(geography = "tract", 
                     year = 2018,
                     variables = c(tribestimate = "B02014_001"), 
                     state = "WA",
                     survey = "acs5",
                     geometry = TRUE)
pl <- places(state = "WA", cb = TRUE)
cb <- core_based_statistical_areas(cb = TRUE)

city <- filter(pl, NAME == "Camas")
subset.int<-st_intersects(x = wa.tracts, y = city)
subset.int2 <-st_touches(x = wa.tracts, y = city)

subset.int
# convert to a vector
subset.int.log = lengths(subset.int) > 0
subset.int.log2 = lengths(subset.int2) > 0
metro.tracts.int <- filter(wa.tracts, subset.int.log)
metro.tracts.int2 <- filter(wa.tracts, subset.int.log2)

ggplot() + 
  geom_sf(data = metro.tracts.int[], fill = "blue") +
  #geom_sf(data = metro.tracts.int2, fill = "blue") +
  
  geom_sf(data = city, fill = NA, color = "red")  
#geom_sf(data = wa.tracts, color = "black")
# end testing ######

mycommunitylist = AllportID[,1:7]
mycommunitylist$MAPNAME = as.character(mycommunitylist$MAPNAME)
mycommunitylist$MAPNAME[96] = "La Cañada Flintridge"

findcensustracts <- function(state) {
  statename = state
  citysubset = mycommunitylist[which(mycommunitylist$STATEABBR == state),]
  templist <- vector("list", nrow(citysubset))
  names(templist) <- citysubset$MAPNAME
  state.tracts <- get_acs(geography = "tract", 
                          year = 2018,
                          variables = c(tribestimate = "B02014_001"), 
                          state = state,
                          survey = "acs5",
                          geometry = TRUE)
  pl <- places(state = state, cb = TRUE, year = 2018)
  print(length(which(citysubset$MAPNAME %in% pl$NAME)))
  for(i in 1:nrow(citysubset)) {
    placename = pl[pl$NAME == citysubset$MAPNAME[i],]
    subset.int<-st_intersects(x = state.tracts, y = placename) #use to be st_intersects
    subset.int
    # convert to a vector
    subset.int.log = lengths(subset.int) > 0
    metro.tracts.int <- filter(state.tracts, subset.int.log)
    
    # ggplot() + 
    #    geom_sf(data = metro.tracts.int, fill = "blue") +
    #    geom_sf(data = placename, fill = NA, color = "red")
    placename2 = as.character(citysubset$MAPNAME[i])
    templist[[placename2]] = list(placename2, metro.tracts.int$NAME, metro.tracts.int$GEOID,
                                  metro.tracts.int$estimate)
  }
  return(templist)
}
Washington = findcensustracts(state = "WA") # Quiluete reservation gives 0 results for La Push
for(i in 1:length(Washington)) {
  if(length(Washington[[i]][[2]]) == 0) {
    print(Washington[[i]][[1]])
  }
}
Washington[[1]][[2]] = "Census Tract 4, Clallam County, Washington"
Washington[[1]][[3]] = "53009000400"
Washington[[1]][[4]] = 406

Oregon =  findcensustracts(state = "OR")
for(i in 1:length(Oregon)) {
  if(length(Oregon[[i]][[2]]) == 0) {
    print(Oregon[[i]][[1]])
  }
}

California = findcensustracts(state = "CA")
for(i in 1:length(California)) {
  if(length(California[[i]][[2]]) == 0) {
    print(California[[i]][[1]])
  }
}

WA_SVI = mycommunitylist[mycommunitylist$STATEABBR == "WA",]
OR_SVI = mycommunitylist[mycommunitylist$STATEABBR == "OR",]
CA_SVI = mycommunitylist[mycommunitylist$STATEABBR == "CA",]
WA_SVI$POV <- WA_SVI$UNEMP <- WA_SVI$PCI <- WA_SVI$NOHSDP <- WA_SVI$AGE65 <- WA_SVI$AGE17 <-
  WA_SVI$DISABL <- WA_SVI$SNGPNT <- WA_SVI$MINRTY <- WA_SVI$LIMENG <- WA_SVI$MUNIT <- WA_SVI$MOBILE <-
  WA_SVI$CROWD <- WA_SVI$NOVEH <- WA_SVI$GROUPQ <- WA_SVI$TOTPOP <- rep(NA, length = nrow(WA_SVI))
OR_SVI$POV <- OR_SVI$UNEMP <- OR_SVI$PCI <- OR_SVI$NOHSDP <- OR_SVI$AGE65 <- OR_SVI$AGE17 <-
  OR_SVI$DISABL <- OR_SVI$SNGPNT <- OR_SVI$MINRTY <- OR_SVI$LIMENG <- OR_SVI$MUNIT <- OR_SVI$MOBILE <-
  OR_SVI$CROWD <- OR_SVI$NOVEH <- OR_SVI$GROUPQ <- OR_SVI$TOTPOP <- rep(NA, length = nrow(OR_SVI))
CA_SVI$POV <- CA_SVI$UNEMP <- CA_SVI$PCI <- CA_SVI$NOHSDP <- CA_SVI$AGE65 <- CA_SVI$AGE17 <-
  CA_SVI$DISABL <- CA_SVI$SNGPNT <- CA_SVI$MINRTY <- CA_SVI$LIMENG <- CA_SVI$MUNIT <- CA_SVI$MOBILE <-
  CA_SVI$CROWD <- CA_SVI$NOVEH <- CA_SVI$GROUPQ <- CA_SVI$TOTPOP <- rep(NA, length = nrow(CA_SVI))

CDCSVI = read.csv("CDC_SVI_2018_WestCoast.csv", header = TRUE)
# access from https://www.atsdr.cdc.gov/placeandhealth/svi/index.html

# Need to get rid of census tracts missing data because will add -999 to average
# replace all -999 with NA and remove NAs before averaging
# according to Danielle the SVI Coordinator, 0's are true zeros (email on 7/22/2020)

library(naniar)

for(i in 1:length(Washington)) {
  indexplace = Washington[i]
  indexsvi = which(CDCSVI$LOCATION %in% indexplace[[1]][2][[1]])
  CDCSVIplace = CDCSVI[indexsvi,]
  
  CDCSVIplace2 = CDCSVIplace %>% replace_with_na_all(condition = ~.x == -999)
  
  avgCDCSVI= CDCSVIplace2 %>% summarise(
    Povmean = mean(EP_POV, na.rm = TRUE),
    Unempmean = mean(EP_UNEMP,na.rm = TRUE),
    PCImean = mean(EP_PCI,na.rm = TRUE), 
    NOHSDPmean = mean(EP_NOHSDP,na.rm = TRUE),
    age65mean = mean(EP_AGE65,na.rm = TRUE), 
    age17mean = mean(EP_AGE17,na.rm = TRUE),
    disablmean = mean(EP_DISABL,na.rm = TRUE), 
    sngpntmean = mean(EP_SNGPNT,na.rm = TRUE),
    minrtymean = mean(EP_MINRTY,na.rm = TRUE), 
    limengmean = mean(EP_LIMENG,na.rm = TRUE), 
    munitmean = mean(EP_MUNIT,na.rm = TRUE), 
    mobilemean = mean(EP_MOBILE,na.rm = TRUE),
    crowdmean = mean(EP_CROWD,na.rm = TRUE), 
    novehmean = mean(EP_NOVEH,na.rm = TRUE),
    groupqmean = mean(EP_GROUPQ,na.rm = TRUE),
    totalpop = sum(E_TOTPOP, na.rm = TRUE)
  )
  indexplace2 = which(WA_SVI$MAPNAME == indexplace[[1]][1][[1]])
  WA_SVI[indexplace2,8:23] = rev(avgCDCSVI)
}

for(i in 1:length(Oregon)) {
  indexplace = Oregon[i]
  indexsvi = which(CDCSVI$LOCATION %in% indexplace[[1]][2][[1]])
  CDCSVIplace = CDCSVI[indexsvi,]
  CDCSVIplace2 = CDCSVIplace %>% replace_with_na_all(condition = ~.x == -999)
  
  avgCDCSVI= CDCSVIplace2 %>% summarise(
    Povmean = mean(EP_POV, na.rm = TRUE),
    Unempmean = mean(EP_UNEMP,na.rm = TRUE),
    PCImean = mean(EP_PCI,na.rm = TRUE), 
    NOHSDPmean = mean(EP_NOHSDP,na.rm = TRUE),
    age65mean = mean(EP_AGE65,na.rm = TRUE), 
    age17mean = mean(EP_AGE17,na.rm = TRUE),
    disablmean = mean(EP_DISABL,na.rm = TRUE), 
    sngpntmean = mean(EP_SNGPNT,na.rm = TRUE),
    minrtymean = mean(EP_MINRTY,na.rm = TRUE), 
    limengmean = mean(EP_LIMENG,na.rm = TRUE), 
    munitmean = mean(EP_MUNIT,na.rm = TRUE), 
    mobilemean = mean(EP_MOBILE,na.rm = TRUE),
    crowdmean = mean(EP_CROWD,na.rm = TRUE), 
    novehmean = mean(EP_NOVEH,na.rm = TRUE),
    groupqmean = mean(EP_GROUPQ,na.rm = TRUE),
    totalpop = sum(E_TOTPOP, na.rm = TRUE)
  )
  indexplace2 = which(OR_SVI$MAPNAME == indexplace[[1]][1][[1]])
  OR_SVI[indexplace2,8:23] = rev(avgCDCSVI)
}

for(i in 1:length(California)) {
  indexplace = California[i]
  indexsvi = which(CDCSVI$LOCATION %in% indexplace[[1]][2][[1]])
  CDCSVIplace = CDCSVI[indexsvi,]
  CDCSVIplace2 = CDCSVIplace %>% replace_with_na_all(condition = ~.x == -999)
  
  avgCDCSVI= CDCSVIplace2 %>% summarise(
    Povmean = mean(EP_POV, na.rm = TRUE),
    Unempmean = mean(EP_UNEMP,na.rm = TRUE),
    PCImean = mean(EP_PCI,na.rm = TRUE), 
    NOHSDPmean = mean(EP_NOHSDP,na.rm = TRUE),
    age65mean = mean(EP_AGE65,na.rm = TRUE), 
    age17mean = mean(EP_AGE17,na.rm = TRUE),
    disablmean = mean(EP_DISABL,na.rm = TRUE), 
    sngpntmean = mean(EP_SNGPNT,na.rm = TRUE),
    minrtymean = mean(EP_MINRTY,na.rm = TRUE), 
    limengmean = mean(EP_LIMENG,na.rm = TRUE), 
    munitmean = mean(EP_MUNIT,na.rm = TRUE), 
    mobilemean = mean(EP_MOBILE,na.rm = TRUE),
    crowdmean = mean(EP_CROWD,na.rm = TRUE), 
    novehmean = mean(EP_NOVEH,na.rm = TRUE),
    groupqmean = mean(EP_GROUPQ,na.rm = TRUE),
    totalpop = sum(E_TOTPOP, na.rm = TRUE)
  )
  indexplace2 = which(CA_SVI$MAPNAME == indexplace[[1]][1][[1]])
  CA_SVI[indexplace2,8:23] = rev(avgCDCSVI)
}

# combine #
WestCoastSVI = rbind(CA_SVI, OR_SVI, WA_SVI)

# Each "theme" or just percent rank and combine into 1

total = rowSums(cbind(percent_rank(WestCoastSVI$POV), percent_rank(WestCoastSVI$UNEMP),
                      1-percent_rank(WestCoastSVI$PCI),percent_rank(WestCoastSVI$NOHSDP),
                      percent_rank(WestCoastSVI$AGE65), percent_rank(WestCoastSVI$AGE17),
                      percent_rank(WestCoastSVI$DISABL),percent_rank(WestCoastSVI$SNGPNT),
                      percent_rank(WestCoastSVI$MINRTY), percent_rank(WestCoastSVI$LIMENG),
                      percent_rank(WestCoastSVI$MUNIT), percent_rank(WestCoastSVI$MOBILE),
                      percent_rank(WestCoastSVI$CROWD),percent_rank(WestCoastSVI$NOVEC),
                      percent_rank(WestCoastSVI$GROUPQ)))
total = percent_rank(total)
WestCoastSVI$total = total
theme1 = rowSums(cbind(percent_rank(WestCoastSVI$POV), percent_rank(WestCoastSVI$UNEMP),
                       1-percent_rank(WestCoastSVI$PCI),percent_rank(WestCoastSVI$NOHSDP)))
WestCoastSVI$theme1 = percent_rank(theme1)
# Reverse after percentile ranking 
# variables to reverse because greater is actually better - just PCI

theme2 = rowSums(cbind(percent_rank(WestCoastSVI$AGE65), percent_rank(WestCoastSVI$AGE17),
                       percent_rank(WestCoastSVI$DISABL),percent_rank(WestCoastSVI$SNGPNT)))
WestCoastSVI$theme2 = percent_rank(theme2)

theme3 = rowSums(cbind(percent_rank(WestCoastSVI$MINRTY), percent_rank(WestCoastSVI$LIMENG)))
WestCoastSVI$theme3 = percent_rank(theme3)

theme4 = rowSums(cbind(percent_rank(WestCoastSVI$MUNIT), percent_rank(WestCoastSVI$MOBILE),
                       percent_rank(WestCoastSVI$CROWD),percent_rank(WestCoastSVI$NOVEC),
                       percent_rank(WestCoastSVI$GROUPQ)))
WestCoastSVI$theme4 = percent_rank(theme4)

WestCoastSVI$final = percent_rank(rowSums(cbind(WestCoastSVI$theme1, WestCoastSVI$theme2, 
                                                WestCoastSVI$theme3, WestCoastSVI$theme4)))

#write.table(WestCoastSVI, "CensusSVI.txt", sep="\t")

##### SVI and reliance ####
length(which(WestCoastSVI$GEO_ID2 %in% AllportID$GEO_ID2)) # all

# get reliance and percent rank it
Community_SVI_Reliance = WestCoastSVI
Community_SVI_Reliance$Reliance = rep(NA, length = 310)
Community_SVI_Reliance$PCID = rep(NA, length = 310)
for(i in 1:nrow(WestCoastSVI)) {
  id = WestCoastSVI$GEO_ID2[i]
  id2 = which(AllportID$GEO_ID2 == id)
  Community_SVI_Reliance$Reliance[i] = AllportID$ComRel[id2]
  Community_SVI_Reliance$PCID[i] = AllportID$PCID[id2]
}

# Remove Moclips, WA - no Reliance data
Community_SVI_Reliance = Community_SVI_Reliance[-284,]
Community_SVI_Reliance$Perc_Rel = percent_rank(Community_SVI_Reliance$Reliance)

###########################################################
###### Match to species risk and get community exposure ######

Alldata_final = Community_SVI_Reliance
landings = read.csv("Percentmtonlandings_4.2020_noMSC2_revenue.csv", header = TRUE)
# would need to get percent landings from PacFIN data. See code XXX

results = list()
for(i in 1:nrow(Alldata_final)) { 
  name = Alldata_final$GEO_NAME[i]
  list_temp = list("name" = name)
  #list_temp[["Alt_name"]] = Alldata_final$Name[i] 
  list_temp[["GEO_ID"]] = Alldata_final$GEO_ID2[i]
  list_temp[["port"]] = Alldata_final$PCID[i]
  list_temp[["Reliance"]] = Alldata_final$Perc_Rel[i]
  #list_temp[["Engagement"]] = Alldata_final$ComEng[i]
  list_temp[["SVI"]] = Alldata_final$total[i]
  #list_temp[["Sensitivity"]] = Alldata_final$rel_ranked[i]
  
  temp = landings[landings$PACFIN_PORT_CODE == as.character(Alldata_final$PCID[i]),]
  # list_temp[["TotalRevenue"]] = sum(as.numeric(temp$dollars))
  #newtontotal = sum(as.numeric(as.character(temp$mtons)))
  #totalrevenue = sum(as.numeric(as.character(temp$dollars)))
  # don't do out of total until remove confidential 
  list_temp[["species"]] = list(as.character(temp$spid_use),   as.numeric(as.character(temp$percent.mtons)), as.numeric(as.character(temp$mtons)),                             as.numeric(as.character(temp$dollars)),
                                as.character(temp$Special)) # if confidential landing than NAs introduced by coercion. SPID should be my translated code not the original pacFIN code 
  #if("CONFIDENTIAL" %in% temp$spid) {
  #print(name)
  #}
  name2 = str_remove(name, ',')
  
  results[[name2]] = list_temp
  if(is.na(temp$PACFIN_PORT_CODE[1] == TRUE)) {
    print(i)
    print(portcommunity$PortName[index])
  }
  #print(temp$Port[1])
}

# Any with > X% confidential - can't find exposure for, remove

per_port_con = rep(NA, length = length(unique(landings$PACFIN_PORT_CODE)))
count = 0 
missing_catch = vector()
for(i in 1:length(results)) {
  spectemp = results[[i]]$species[[1]]
  spectemp2 = results[[i]]$species[[2]] 
  index = which(spectemp == "CONFIDENTIAL")
  if(length(index) == 0) {
    #print(length(index))
    per_port_con[i] = NA
  } else {
    per_port_con[i] = sum(spectemp2[index])
    if(sum(spectemp2[index]) > 20) {
      print(results[[i]]$name)
      count = count+1 
      missing_catch = c(missing_catch, results[[i]]$GEO_ID)
    }
  }
}
hist(na.omit(per_port_con), breaks = 20)
# break at >20% or >40% 
length(Alldata_final$Reliance[Alldata_final$GEO_ID2 %in% missing_catch])
# 50 missing at break of 20

tooremove = which(Alldata_final$GEO_ID2 %in% missing_catch)

# NEED TO REMOVE AND RE-PERCENTILE RANK
Alldata_final_subset = Alldata_final[-tooremove,]
Alldata_final_subset$Perc_Rel = percent_rank(Alldata_final_subset$Reliance)

total = rowSums(cbind(percent_rank(Alldata_final_subset$POV), percent_rank(Alldata_final_subset$UNEMP),
                      1-percent_rank(Alldata_final_subset$PCI),percent_rank(Alldata_final_subset$NOHSDP),
                      percent_rank(Alldata_final_subset$AGE65), percent_rank(Alldata_final_subset$AGE17),
                      percent_rank(Alldata_final_subset$DISABL),percent_rank(Alldata_final_subset$SNGPNT),
                      percent_rank(Alldata_final_subset$MINRTY), percent_rank(Alldata_final_subset$LIMENG),
                      percent_rank(Alldata_final_subset$MUNIT), percent_rank(Alldata_final_subset$MOBILE),
                      percent_rank(Alldata_final_subset$CROWD),percent_rank(Alldata_final_subset$NOVEC),
                      percent_rank(Alldata_final_subset$GROUPQ)))
total = percent_rank(total)
Alldata_final_subset$total = total

###### NOW get landings data again #######
results2 = list()

# see text for why certain species were removed
badlist = c("OCRB","OMSK","OSCL","OSRM","OURC", "PHLB", "LSRK", "YLTL")
for(i in 1:nrow(Alldata_final_subset)) {
  name = Alldata_final_subset$GEO_NAME[i]
  list_temp = list("name" = name)
  #list_temp[["Alt_name"]] = Data_AllValues$Name[i]
  list_temp[["GEO_ID"]] = Alldata_final_subset$GEO_ID2[i]
  list_temp[["port"]] = (Alldata_final_subset$PCID[i])
  list_temp[["Reliance"]] = Alldata_final_subset$Perc_Rel[i]
  #list_temp[["Engagement"]] = Data_AllValues$ComEng[i]
  list_temp[["SVI"]] = Alldata_final_subset$total[i]
  # list_temp[["Sensitivity"]] = Data_AllValues$rel_rankednew[i]
  
  temp = landings[landings$PACFIN_PORT_CODE == as.character(Alldata_final_subset$PCID[i]),]
  list_temp[["species"]] = list(as.character(temp$spid_use),  as.numeric(as.character(temp$percent.mtons )), as.character(temp$Special))
  name2 = str_remove(name, ',')
  
  speciesremove = which(temp$spid_use == "CONFIDENTIAL" | temp$spid_use %in% badlist)
  
  # 
  if(length(speciesremove) > 0) {
    temp2 = temp[-speciesremove,]
    newtontotal = sum(as.numeric(as.character(temp2$mtons)))
    totalrevenue = sum(as.numeric(as.character(temp2$dollars)))
    
    list_temp[["species"]] = list(as.character(temp2$spid_use),
                                  as.numeric(as.character(temp2$mtons)),
                                  as.numeric(as.character(temp2$dollars)),             as.numeric(as.character(temp2$mtons))/newtontotal,                             as.numeric(as.character(temp2$dollars))/totalrevenue,
                                  as.character(temp2$Special), totalrevenue)
    names(list_temp[["species"]]) = c("species","mtons","dollars", "percentton", "percentrev","special", "TotalRevenue")
    
  } else {
    temp2 = temp
    newtontotal = sum(as.numeric(as.character(temp2$mtons)))
    totalrevenue = sum(as.numeric(as.character(temp2$dollars)))
    
    list_temp[["species"]] = list(as.character(temp2$spid_use), 
                                  as.numeric(as.character(temp2$mtons)),
                                  as.numeric(as.character(temp2$dollars)),  as.numeric(as.character(temp2$mtons))/newtontotal,                             as.numeric(as.character(temp2$dollars))/totalrevenue,
                                  as.character(temp2$Special), totalrevenue)
    names(list_temp[["species"]]) = c("species","mtons","dollars", "percentton", "percentrev","special", "TotalRevenue")
  }
  # list_temp[["known_species"]] = list(as.character(temp$spid_use[-confidential]),  as.numeric(as.character(temp$percent.mtons[-confidential])), as.character(temp$Special[-confidential]))
  # 
  # scale = 100/ sum(temp$percent.mtons[-confidential])
  # list_temp[['scaledpercent']] = (temp$percent.mtons[-confidential])*scale
  # } else {
  #    list_temp[["known_species"]] = list(as.character(temp$spid_use),  as.numeric(as.character(temp$percent.mtons)), as.character(temp$Special))
  #    scale = 100/ sum(temp$percent.mtons)
  # list_temp[['scaledpercent']] = (temp$percent.mtons)*scale
  #}
  
  results2[[name2]] = list_temp
  
  # check to make sure not missing any 
  if(is.na(temp$PACFIN_PORT_CODE[1] == TRUE)) {
    print(i)
    print(portcommunity$PortName[index])
  }
  #print(temp$Port[1])
}

speciesvec = vector()
for(i in 1:259) {
  speciesvec = c(speciesvec, as.vector(unlist(results2[[i]]['species'][[1]][1])))
}

speciesvec2 = vector()
for(i in 1:259) {
  speciesvec2 = c(speciesvec2, as.vector(unlist(results[[i]]['species'][[1]][1])))
}
portvec = vector()
for(i in 1:259) {
  portvec = c(portvec, as.vector(unlist(results2[[i]]['port'][[1]])))
}


####################################################################
#### Now link to species risk! 

speciesrisk = read.csv("speciesrisk_UPDATED_12.13.csv", header = TRUE)
# access from dryad
speciesriskbyport <- function(speciesriskuse = speciesriskuse) {
  
  for(i in 1:length(results2)) {
    spectemp = results2[[i]]$species$species
    # spectemp = results2[[i]]$known_species[[1]]
    # spectemp2 = results2[[i]]$scaledpercent
    speciesvuln_temp = rep(NA, length = length(spectemp))
    for(j in 1: length(spectemp)) {
      speciescode = spectemp[j]
      #   if(speciescode %in% badlist) { 
      #     speciesvuln_temp[j] = NA
      #   } else {
      vulnindex = which((speciesrisk$spid_use) == speciescode) # should be my code
      speciesvuln_temp[j] = speciesriskuse[vulnindex] #speciesrisk$avgrisk[vulnindex] # which risk model?
    }
    # }
    species = spectemp
    # remove = which(is.na(speciesvuln_temp))
    # if(length(remove) > 0) {
    #   species = spectemp[-remove]
    #   speciesvuln_temp2 = speciesvuln_temp[-remove]
    #   speciespercent = spectemp2[-remove]
    #   
    #   scale = 100/ sum(speciespercent)
    #   speciespercent = speciespercent*scale
    # } else {
    #   species = spectemp
    #    speciesvuln_temp2 = speciesvuln_temp
    #   speciespercent = spectemp2
    # }
    
    if("MSC2" %in% species) {
      #index = which(specievuln_temp2 == "MSC2")
      print(results2[[i]]$name) # 
    }
    print(results2[[i]]$species$percentrev)
    speciespercentrev = results2[[i]]$species$percentrev#*100 # does the 100 matter?
    speciespercentton = results2[[i]]$species$percentton#*100
    results2[[i]]$final_species = list(species, speciesvuln_temp, speciesvuln_temp*speciespercentrev, speciesvuln_temp*speciespercentton,                                     sum(speciesvuln_temp*speciespercentrev),                                   sum(speciesvuln_temp*speciespercentton))
    
  }
  return(results2)
}

gfdl = speciesriskbyport(speciesriskuse = speciesrisk$gfdlrisk )
ipsl = speciesriskbyport(speciesriskuse = speciesrisk$ipslrisk)
hadley = speciesriskbyport(speciesriskuse = speciesrisk$hadrisk)
avgspeciesrisk = speciesriskbyport(speciesriskuse = speciesrisk$avgrisk)
#scalerisk = speciesriskbyport(speciesriskuse = speciesrisk$scaledrisk)
#scalerisk2 = speciesriskbyport(speciesriskuse = speciesrisk$scaledrisk2)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
#scaleavg = speciesriskbyport(speciesriskuse = speciesrisk$scaledavg)


for(j in 1:length(gfdl)) {
  check = gfdl[[j]]$final_species[[5]]
  for(k in 1:length(check)) {
    if(is.na(check[k]) == TRUE) {
      print(gfdl[[j]]$final_species[[1]])
    }
  }
  
}

exposurerevmatrix = matrix(NA, nrow = nrow(Alldata_final_subset), ncol = 4)
exposuretonmatrix = matrix(NA, nrow = nrow(Alldata_final_subset), ncol = 4)
exposurerevpercentile = matrix(NA, nrow = nrow(Alldata_final_subset), ncol = 4)
exposuretonpercentile = matrix(NA, nrow = nrow(Alldata_final_subset), ncol = 4)
riskuse = speciesrisk %>% select(gfdlrisk, hadrisk, ipslrisk, avgrisk)
for(j in 1:4) {
  temprun = speciesriskbyport(speciesriskuse = riskuse[,j])
  for(i in 1:length(results2)) {
    exposurerevmatrix[i,j] = temprun[[i]]$final_species[[5]]
    exposuretonmatrix[i,j] = temprun[[i]]$final_species[[6]]
  }
  exposurerevpercentile[,j] = percent_rank(exposurerevmatrix[,j])
  exposuretonpercentile[,j] = percent_rank(exposuretonmatrix[,j])
}
colnames(exposurerevpercentile) = colnames(exposuretonpercentile) = c("gfdl", "hadley", "ipsl", "averageexp")
cor(exposurerevpercentile[,1], exposurerevpercentile[,2]) #0.91
cor(exposurerevpercentile[,1], exposurerevpercentile[,3]) # 0.92
cor(exposurerevpercentile[,3], exposurerevpercentile[,2]) # 0.83

namecheck = rep(NA, length(results2))
for(h in 1:length(results2)) {
  namecheck[h] = results2[[h]]$name
}
rownames(exposurerevpercentile) = namecheck
rownames(exposuretonpercentile) = namecheck

######### Calculate Risk and Vulnerability ###########

nsim = 4 # 4 exposure
FinalVulnMatrix_rev = matrix(NA, nrow = nrow(Alldata_final_subset), ncol = nsim)
rownames(FinalVulnMatrix_rev) = Alldata_final_subset$GEO_NAME
FinalVulnMatrix_ton = matrix(NA, nrow = nrow(Alldata_final_subset), ncol = nsim)
rownames(FinalVulnMatrix_ton) = Alldata_final_subset$GEO_NAME

FinalRisk_ton = matrix(NA, nrow = nrow(Alldata_final_subset), ncol = 4)
FinalRisk_rev = matrix(NA, nrow = nrow(Alldata_final_subset), ncol = 4)
rownames(FinalRisk_rev) = rownames(FinalRisk_ton) = Alldata_final_subset$GEO_NAME
sensistivity = Alldata_final_subset$Perc_Rel # doesn't range from 0 to 1 because LOTS at very lowest (0) reliance

for(v in 1:4) { 
  exposure_rev = exposurerevpercentile[,v] 
  exposure_ton = exposuretonpercentile[,v]
  SVI_temp = Alldata_final_subset$total 
  
  
  FinalRisk_ton[,v] = sqrt((exposure_ton-min(exposure_ton))^2 +(sensistivity - min(sensistivity))^2)
  FinalRisk_rev[,v] = sqrt((exposure_rev-min(exposure_rev))^2 +(sensistivity - min(sensistivity))^2)
  
  FinalVulnMatrix_ton[,v] = sqrt((exposure_ton-min(exposure_ton))^2 + (SVI_temp - min(SVI_temp))^2+(sensistivity - min(sensistivity))^2)
  FinalVulnMatrix_rev[,v] = sqrt((exposure_rev-min(exposure_rev))^2 + (SVI_temp - min(SVI_temp))^2+(sensistivity - min(sensistivity))^2)
}

quantile(FinalVulnMatrix_rev[,4], prob= 0.75) #1.14
Alldata_final_subset$risk = FinalRisk_rev[,4]
Alldata_final_subset$vuln = FinalVulnMatrix_rev[,4]

###### TABLE ######
library(formattable)
tableframe = data.frame( Alldata_final_subset$Perc_Rel,
                         exposurerevpercentile[,4],
                         Alldata_final_subset$total, FinalRisk_rev[,4],
                         FinalVulnMatrix_rev[,4])
names(tableframe) = c( "Sensitivity (reliance)", "Exposure (Average, Revenue weighted)",
                       "Social vulnerability", "Risk (average)", "Vulnerability (average)")

# makes font white if you use a dark theme
table = formattable(tableframe, list(
  #`Community` = formatter("span", style = ~ style(color = "grey", font.weight = "bold"), width = '250%'),
  "Sensitivity (reliance)" = color_tile("white", "grey55"),
  "Exposure (Average, Revenue weighted)" =color_tile("white", "grey55") ,
  "Social vulnerability" = color_tile("white", "grey55"), 
  "Risk (average)" = color_tile("white", "grey55"), 
  "Vulnerability (average)" = color_tile("white", "grey55")
))

#############################################
#### FIGURES #####
# Figure 3

speciesbycommun = matrix(NA, nrow = length(unique(landings$spid_use))+1, ncol =nrow(exposurerevpercentile) )
rownames(speciesbycommun) = c(as.character(unique(landings$spid_use)), "topindex")
speciesusing = as.character(unique(landings$spid_use))
colnames(speciesbycommun) = rownames(exposurerevpercentile)
risk = FinalRisk_rev[,4]
exposure = exposurerevpercentile[,4]
top25vuln = quantile(risk, probs = 0.75)
quantile(exposure, probs = 0.75)

for(i in 1:ncol(speciesbycommun)) {
  nameuse = avgspeciesrisk[[i]][1]
  matrixlocate = which(colnames(speciesbycommun) == nameuse)
  speciesuse = avgspeciesrisk[[i]]$final_species[[1]]
  for(j in 1:length(speciesuse)) {
    speciesindex = which(rownames(speciesbycommun) == as.character(speciesuse[j]))
    # speciesbycommun[speciesindex,matrixlocate] = (avgspeciesrisk[[i]]$final_species[[3]][j])
    # think we actually want just the percent rev, nothing about risk, so - 
    speciesbycommun[speciesindex, matrixlocate] = (avgspeciesrisk[[i]]$species$percentrev[j])
    
    # except that risk is part of exposure and its exposure that we're seeing trend with vulnerability...  
  }
  # if(nameuse %in% top25frame$names) {
  #   speciesbycommun[75,matrixlocate] = "top" 
  # } else {
  #   speciesbycommun[75,matrixlocate] = "bottom"# test that no more NA after so that all communities are covered
  # }
}
speciesbycommun2 = (t(speciesbycommun))
speciesbycommun2[is.na(speciesbycommun2)] <- 0
speciesbycommun2 = speciesbycommun2[,-3]
speciesbycommun3 = matrix(NA, nrow = nrow(speciesbycommun2), ncol = ncol(speciesbycommun2)-1)
for(i in 1:(ncol(speciesbycommun2)-1)) {
  speciesbycommun3[,i] = as.numeric(speciesbycommun2[,i])
}

#cbind(colSums(speciesbycommun3), colnames(speciesbycommun2[-74]))
index = which(colSums(speciesbycommun3) == 0) # don't include these
speciesbycommun4 = speciesbycommun3[,-index]
speciesnames = colnames(speciesbycommun2)[-c(index,74)]

colnames(speciesbycommun4) = speciesnames
rownames(speciesbycommun4) = rownames(speciesbycommun2)
WA = which(endsWith(rownames(speciesbycommun4), 'WA') == TRUE)
OR = which(endsWith(rownames(speciesbycommun4), 'OR') == TRUE)
CA = which(endsWith(rownames(speciesbycommun4), 'CA') == TRUE)

pie(colMeans(speciesbycommun4[WA,1:60]))
pie(colMeans(speciesbycommun4[OR,1:60]))
pie(colMeans(speciesbycommun4[CA,1:60]))


###### all communities by state minus duplication #########
WA = which(endsWith(rownames(speciesbycommun4), 'WA') == TRUE)
OR = which(endsWith(rownames(speciesbycommun4), 'OR') == TRUE)
CA = which(endsWith(rownames(speciesbycommun4), 'CA') == TRUE)

CA2 = Alldata_final_subset[endsWith(Alldata_final_subset$GEO_NAME, "CA"),]
SCA = which(CA2$PRIMARY_LATITUDE <= 34.45)
NCA = which(CA2$PRIMARY_LATITUDE > 34.45 )
SCAname = Alldata_final_subset$GEO_NAME[SCA]
NCAname = Alldata_final_subset$GEO_NAME[NCA]
length(which(rownames(speciesbycommun4)[CA] %in% SCAname))
length(which(rownames(speciesbycommun4)[CA] %in% NCAname))
missing = which(rownames(speciesbycommun4)[CA] %!in% c(SCAname, NCAname))
rownames(speciesbycommun4)[missing]
#[1] 44 "EL PASO DE ROBLES (PASO ROBLES), CA" - north,[2] 78"LA CANADA FLINTRIDGE, CA"  - south             
#[3] 97"MONTARA, CA" - north (point montara) ,[4] 101"MOUNTAIN VIEW CITY, CA" - north                
#[5] 137"ST. HELENA, CA" - north , [6] 139"SAN BUENAVENTURA (VENTURA), CA"  - south,
#[7] 145"SAN JUAN CAPISTRANO, CA" - south   ,[8] 150"SAN MIGUEL (SAN LUIS OBISPO COUNTY), CA" - north
#[9]176 "TOMALES, CA" -north   ,[10]187 "WESTHAVEN-MOONSTONE, CA"- north

northCA = (which(rownames(speciesbycommun4) %in% NCAname))
southCA = (which(rownames(speciesbycommun4) %in% SCAname))

speciesbycommun4 = cbind(speciesbycommun4, risk, Alldata_final_subset$total, exposure )
colnames(speciesbycommun4)[64:66] = c("Risk", "SVI", "Exposure")
WAspecies = speciesbycommun4[WA,]
ORspecies = speciesbycommun4[OR,]
NCAspecies = speciesbycommun4[northCA,]
SCAspecies = speciesbycommun4[southCA,]

allspecies_order = rep(NA, length = 63)
specieslist = colnames(speciesbycommun4)[1:63]
for(k in 1:63) {
  indexspecies = which(speciesrisk$spid_use == specieslist[k])
  allspecies_order[k] = speciesrisk$avgrisk[indexspecies]
}

speciesriskorder = as.data.frame(cbind(specieslist, allspecies_order))
speciesriskorder = speciesriskorder[order(speciesriskorder$allspecies_order, decreasing = TRUE),]

### Washington ##### 
set.seed(216)
WAremove = which(colSums(WAspecies) == 0)
WAspecies2 = WAspecies[,-WAremove]

portsuse = rownames(WAspecies2)
WAports = rep(NA, length = length(portsuse))
for(i in 1:length(portsuse)) {
  indexuse = which(AllportID$GEO_NAME == portsuse[i])
  WAports[i] = AllportID$PCID[indexuse]
}

WAspecies3 = cbind(WAspecies2, WAports, paste(rownames(WAspecies2), " (",
                                              WAports, ")", sep = ""))

WAspeciesrandom = data.frame(WAspecies3) %>% 
  group_by( WAports ) %>%
  sample_n(1)

WAspeciesrandom = WAspeciesrandom[order(WAspeciesrandom$Risk),]


library(tidyverse)
colnames(WAspeciesrandom)[31] = "communityname"
wa_gg = WAspeciesrandom %>%
  # convert data to long format
  gather( key="species",value="value",RURC,  DCRB,  USCU, ALBC,  ARTH, 
          DOVR,  PSDN,  PSHP,  PWHT, 
          CHNK,  LCOD,  SABL,  UHAG,   
          CHUM,  COHO,  PINK,  SOCK, 
          PTRL, STLH, EGLS, PCOD, REX, USKT, YTRK, GDUK, PHRG  )  %>%
  setNames(c("risk", "svi","exposure", "WAport", "communityname", "species", "value"))

wa_species_subset = c("CHNK" ,   "CHUM", "COHO" ,  "PINK" ,                
                      "SOCK", "STLH", "DCRB",  "ALBC", "SABL" ,                
                      "PSDN","PSHP",  "PWHT" , "ARTH",               
                      "DOVR" ,"LCOD" , "UHAG", "PTRL",              
                      "EGLS" ,  "PCOD",  "REX",  "USKT" ,                
                      "YTRK", "USCU", "GDUK", "PHRG", "RURC" )
wa_species_risk = rep(NA, length = length(wa_species_subset))
for(k in 1: length(wa_species_subset)) {
  indexspecies = which(speciesrisk$spid_use == wa_species_subset[k])
  wa_species_risk[k] = speciesrisk$avgrisk[indexspecies]
}

speciesriskorder = as.data.frame(cbind(wa_species_subset, wa_species_risk))
speciesriskorder = speciesriskorder[order(speciesriskorder$wa_species_risk),]
WAspeciesorder = c("PTRL", "UHAG" ,"USKT", "EGLS", "DOVR", "YTRK", "REX",  "PSHP", "DCRB" ,"LCOD", "STLH" ,"GDUK",
                   "ARTH", "COHO", "USCU", "PCOD", "PSDN", "RURC", "ALBC", "PHRG" ,"PINK", "SABL" ,"SOCK", "CHNK",
                   "PWHT", "CHUM")
wa_gg$species <- factor(wa_gg$species, c( "CHUM", "PWHT", "CHNK", "SOCK" ,"SABL", "PINK" ,"PHRG", "ALBC", "RURC", "PSDN" ,"PCOD", "USCU",
                                          "COHO", "ARTH", "GDUK" ,"STLH" ,"LCOD", "DCRB" ,"PSHP", "REX" , "YTRK", "DOVR", "EGLS", "USKT",
                                          "UHAG", "PTRL"))
# communities must be in same order as STATEspeciesrandom after ordering by risk

wa_gg$communityname <- factor(wa_gg$communityname, c(  "SEATTLE, WA (SEA)"  ,    "ANACORTES, WA (ANA)"  ,  "PORT ANGELES, WA (PAG)",
                                                       "BLAINE, WA (BLN)"  ,     "SEQUIM, WA (SEQ)"    ,   "TACOMA, WA (TAC)",      
                                                       "LA CONNER, WA (LAC)" ,   "BELLINGHAM, WA (BLL)" ,  "CHINOOK, WA (LWC)",     
                                                       "LA PUSH, WA (LAP)"   ,   "WESTPORT, WA (WPT)"  ,   "LONGVIEW, WA (OCR)",    
                                                       "EVERETT, WA (EVR)"   ,   "NEAH BAY, WA (NEA)" ))
waspecies_fullname = c("Chum", "Hake", "Chinook", "Sockeye", "Sablefish","Pink",
                       "Herring", "Albacore", "Red urchin", "Sardine", "Cod",
                       "Cucumber", "Coho", "Arrowtooth", "Geoduck","Steelhead",
                       "Lingcod", "Dungeness","Pink shrimp", "Rex sole",
                       "Yellowtail", "Dover","English sole","Skate","Hagfish","Petrale")

plot = ggplot(wa_gg,aes(x=species,y=communityname,fill=as.numeric(value)))+
  geom_tile(colour="grey",size=0.2)+ 
  guides(fill=guide_legend(title="% Revenue", reverse = T)) + 
  labs(x="",y="",title="Landings by Washington Communities")+
  # scale_fill_distiller(palette = "Oranges", trans = "reverse") +
  scale_fill_gradient(low = "white", high = "red") + 
  # scale_x_discrete(expand=c(0,0))+
  #  scale_y_continuous(expand=c(0,0),breaks=1:13,
  # labels = paste(WAspeciesrandom$rownames.WAspecies2.," (", round(WAspeciesrandom$Risk, digits = 3), ")")) +
  theme_grey(base_size=10) +
  annotate(geom="text", x=29.2, y=c(1:14), label= "",
           color="red") +
  annotate(geom="text", x=27.9, y=c(1:14), label= round(as.numeric(WAspeciesrandom$Risk), digits = 2),
           color="red") +
  annotate(geom="text", x=27.9, y=14.7, label= "Risk",
           color="red") +
  annotate(geom="text", x=28, y=15, label = "",
           color="red")
pdf("Washingtonlandings.pdf", width = 6, height = 4)
plot + theme(axis.text.x=element_text(angle=50, size=7, vjust = 1, hjust = 1)) +
  scale_x_discrete(
    labels=waspecies_fullname) + 
  theme(axis.text.y = element_text(face="bold", color="#993333", 
                                   size=7))# +
#geom_segment(aes(x = 1, y = 0, xend = 20, yend = 0), arrow = arrow(length = unit(0.5, "cm"))) 
#geom_hline(yintercept = 8.5, linetype = 2)
dev.off()

plot + theme(axis.text.x=element_text(angle=50, size=7, vjust = 1, hjust = 1 )) +
  theme(axis.text.y = element_text(face="bold", color="#993333", 
                                   size=7)) +
  geom_hline(yintercept=10.5, linetype="dashed", color = "darkred", size = 1)

### OREGON ##### 
ORremove = which(colSums(ORspecies) == 0)
ORspecies2 =ORspecies[,-ORremove]

ORportsuse = rownames(ORspecies2)
ORports = rep(NA, length = length(ORportsuse))
for(i in 1:length(ORportsuse)) {
  indexuse = which(AllportID$GEO_NAME == ORportsuse[i])
  ORports[i] = AllportID$PCID[indexuse]
}

ORspecies3 = cbind(ORspecies2, ORports, paste(rownames(ORspecies2), " (",
                                              ORports, ")", sep = ""))
ORspeciesrandom = data.frame(ORspecies3) %>% 
  group_by( ORports ) %>%
  sample_n(1)

ORspeciesrandom = ORspeciesrandom[order(ORspeciesrandom$Risk),]
colnames(ORspeciesrandom)[23] = "communityname"

or_gg = ORspeciesrandom %>%
  # convert data to long format
  gather( key="species",value="value",RURC,  DCRB,   ALBC,  ARTH,  DOVR,  PSDN,  PSHP,  PWHT, 
          CBZN, CHNK,  LCOD,  SABL,  UHAG,   
          BLCK, KLPG, MSQD, CKLE, GCLM  )  %>%
  setNames(c("risk", "SVI","exposure", "ORports", "communityname", "species", "value"))

or_species_subset = c( "RURC" ,  "DCRB" ,"ALBC",                
                       "ARTH" , "DOVR", "PSDN",                
                       "PSHP" , "PWHT",    "CBZN",                
                       "CHNK",  "LCOD",   "SABL"  ,              
                       "UHAG", "BLCK" , "KLPG"    ,            
                       "MSQD" , "CKLE", "GCLM" )
or_species_risk = rep(NA, length = length(or_species_subset))
for(k in 1: length(or_species_subset)) {
  indexspecies = which(speciesrisk$spid_use == or_species_subset[k])
  or_species_risk[k] = speciesrisk$avgrisk[indexspecies]
}

speciesriskorder = as.data.frame(cbind(or_species_subset, or_species_risk))
speciesriskorder = speciesriskorder[order(speciesriskorder$or_species_risk),]
orspeciesorder = c("UHAG", "DOVR", "CKLE", "PSHP", "DCRB" ,"BLCK", "LCOD" ,"CBZN" ,"MSQD", "ARTH", "GCLM",
                   "PSDN", "RURC" ,"ALBC", "KLPG", "SABL", "CHNK" ,"PWHT")

or_gg$species <- factor(or_gg$species, c( "PWHT", "CHNK", "SABL", "KLPG", "ALBC", "RURC", "PSDN", "GCLM", "ARTH" ,"MSQD" ,"CBZN",
                                          "LCOD", "BLCK", "DCRB", "PSHP" ,"CKLE", "DOVR", "UHAG" ))

or_gg$communityname <- factor(or_gg$communityname, c(  "NEHALEM, OR (NHL)"  ,      "BROOKINGS, OR (BRK)"  ,   
                                                       "DEPOE BAY, OR (DPO)" ,     "COOS BAY, OR (COS)",      
                                                       "PACIFIC CITY, OR (PCC)",   "GARIBALDI, OR (TLL)",     
                                                       "FLORENCE, OR (FLR)"     ,  "BANDON, OR (BDN)"     ,   
                                                       "WINCHESTER BAY, OR (WIN)", "NEWPORT, OR (NEW)"     ,  
                                                       "ASTORIA, OR (AST)"   ,     "PORT ORFORD, OR (ORF)" ,  
                                                       "GOLD BEACH, OR (GLD)" ))

plotOR = ggplot(or_gg,aes(x=species,y=communityname,fill=as.numeric(value)))+
  geom_tile(colour="grey",size=0.2)+ 
  guides(fill=guide_legend(title="% Revenue", reverse = T)) + 
  labs(x="",y="",title="Landings by Oregon Communities")+
  scale_fill_gradient(low = "white", high = "red") + 
  #scale_fill_distiller(palette = "YlOrRd", trans = "reverse") +
  # scale_x_discrete(expand=c(0,0))+
  #  scale_y_continuous(expand=c(0,0),breaks=1:13,
  # labels = paste(WAspeciesrandom$rownames.WAspecies2.," (", round(WAspeciesrandom$Risk, digits = 3), ")")) +
  theme_grey(base_size=10) +
  annotate(geom="text", x=20.4, y=c(1:13), label= "",
           color="red") +
  annotate(geom="text", x=19.5, y=c(1:13), label= round(as.numeric(ORspeciesrandom$Risk), digits = 2),
           color="red") +
  annotate(geom="text", x=19.5, y=13.6, label= "Risk",
           color="red") +
  annotate(geom="text", x=19, y=13.9, label = "",
           color="red")
orspecies_fullname = c( "Hake", "Chinook", "Sablefish", "Kelp greenling", "Albacore","Red urchin",
                        "Sardine", "Gaper clam", "Arrowtooth", "Mark. squid", "Cabazon", "Lingcod",
                        "Black rock.", "Dungeness", "Pink shrimp", "Basket cockle", "Dover", "Hagfish")


pdf("Oregonlandings.pdf", width = 6, height = 4)
plotOR + theme(axis.text.x=element_text(angle=50, size=7, vjust=1, hjust = 1)) +
  scale_x_discrete(
    labels=orspecies_fullname) + 
  theme(axis.text.y = element_text(face="bold", color="#993333", 
                                   size=7)) #+
#geom_hline(yintercept = 8.5, linetype = 2)
dev.off()

### Northern California ##### 
NCAremove = which(colSums(NCAspecies) == 0)
NCAspecies2 = NCAspecies[,-NCAremove]

NCAportsuse = rownames(NCAspecies2)
NCAports = rep(NA, length = length(NCAportsuse))
for(i in 1:length(NCAportsuse)) {
  indexuse = which(AllportID$GEO_NAME == NCAportsuse[i])
  NCAports[i] = AllportID$PCID[indexuse]
}

NCAspecies3 = cbind(NCAspecies2, NCAports, paste(rownames(NCAspecies2), " (",
                                                 NCAports, ")", sep = ""))
NCAspeciesrandom = data.frame(NCAspecies3) %>% 
  group_by( NCAports ) %>%
  sample_n(1)


NCAspeciesrandom = NCAspeciesrandom[order(NCAspeciesrandom$Risk),]
colnames(NCAspeciesrandom)[43] = "communityname"

#NCAspeciesrandom$rownames.NCAspecies2.[18] = "SAN MIGUEL, CA"

nca_gg = NCAspeciesrandom %>%
  # convert data to long format
  gather( key="species",value="value",RURC,  DCRB,  ALBC,    DOVR,  PSDN,  PSHP,BRWN, BYEL, CBZN, CHLB,
          CHNK, GPHR, LCOD, RCRB,  SABL, SRFP3, UHAG, VRML, SRFP, CLPR, LSPN,   
          PTRL,SSPN, SMLT2, WBAS, LOBS, MSQD, SMLT, RPRW, NANC,
          BLGL, SWRD, SRFP2, BSRM, CMCK, USCU2, BSRM2, PHRG )  %>%
  setNames(c("risk", "svi","exposure",  "NCAports", "communityname", "species", "value"))

nca_species_subset = c(  "RURC",  "DCRB",  "ALBC", "DOVR" , "PSDN" ,"PSHP",                 
                         "BRWN" , "BYEL" , "CBZN",   "CHLB","CHNK" , "GPHR",                 
                         "LCOD",  "RCRB" ,"SABL"  ,   "SRFP3" , "UHAG" ,"VRML",                 
                         "SRFP" ,"CLPR" ,  "LSPN" ,  "PTRL" , "SSPN", "SMLT2"  ,              
                         "WBAS" , "LOBS",  "MSQD" , "SMLT", "RPRW", "NANC",                 
                         "BLGL" ,  "SWRD" , "SRFP2" , "BSRM" ,"CMCK", "USCU2",                
                         "BSRM2"  , "PHRG"   )
nca_species_risk = rep(NA, length = length(nca_species_subset))
for(k in 1: length(nca_species_subset)) {
  indexspecies = which(speciesrisk$spid_use == nca_species_subset[k])
  nca_species_risk[k] = speciesrisk$avgrisk[indexspecies]
}

speciesriskorder = as.data.frame(cbind(nca_species_subset, nca_species_risk))
speciesriskorder = speciesriskorder[order(speciesriskorder$nca_species_risk, decreasing = TRUE),]

nca_gg$species <- factor(nca_gg$species, c("SMLT" , "CHNK" , "SABL",  "SMLT2" ,"PHRG" , "ALBC" , "LOBS" , "RURC",  "SWRD" , "BYEL", 
                                           "LSPN",  "CMCK" , "GPHR",  "NANC" , "PSDN",  "RPRW" , "CHLB" , "WBAS",  "RCRB" , "USCU2",
                                           "BSRM2", "BLGL",  "BRWN" , "SRFP2", "SRFP" , "BSRM",  "MSQD" , "CLPR",  "SRFP3", "VRML", 
                                           "CBZN" , "LCOD" , "SSPN" , "DCRB",  "PSHP" , "DOVR" , "UHAG" , "PTRL"  ))

nca_gg$communityname <- factor(nca_gg$communityname, c(  "FORTUNA, CA (OHB)"  ,      "OAKLAND, CA (OAK)" ,      
                                                         "PLEASANT HILL, CA (OSF)" , "TIMBER COVE, CA (BDG)" ,  
                                                         "FIELDBROOK, CA (OCA)"  ,   "SOLVANG, CA (OBV)" ,      
                                                         "STINSON BEACH, CA (OSM)"  ,"ARROYO GRANDE, CA (OSL)" ,
                                                         "LANCASTER, CA (OLA)"  ,    "ALAMEDA, CA (ALM)",       
                                                         "SAN FRANCISCO, CA (SF)" ,  "SAUSALITO, CA (SLT)"  ,   
                                                         "KLAMATH, CA (ODN)"  ,      "CRESCENT CITY, CA (CRS)", 
                                                         "MONTEREY, CA (MNT)" ,      "EUREKA, CA (ERK)",        
                                                         "BOLINAS, CA (BOL)" ,       "RICHMOND, CA (RCH)"  ,    
                                                         "FIELDS LANDING, CA (FLN)", "HALF MOON BAY, CA (PRN)" ,
                                                         "SANTA CRUZ, CA (CRZ)" ,    "TRINIDAD, CA (TRN)",      
                                                         "BERKELEY, CA (BKL)"  ,     "AVILA BEACH, CA (AVL)" ,  
                                                         "LITTLE RIVER, CA (OMD)"  , "MOSS LANDING, CA (MOS)",  
                                                         "MORRO BAY, CA (MRO)"   ,   "TOMALES, CA (TML)",       
                                                         "FORT BRAGG, CA (BRG)"  ,   "ALBION, CA (ALB)" ,       
                                                         "POINT ARENA, CA (ARE)" ))
ncaspecies_fullname = c("Night+Surf smelt", "Chinook", "Sablefish", "Jacksmelt","Herring",
                        "Albacore", "Spiny Lobster", "Red urchin","Swordfish", "Black&Yellow rock.",
                        "Longspine", "Mackerel", "Gopher rock.","Anchovy", "Sardine", "Ridgeback prawn",
                        "CA Halibut", "Seabass", "Rock crab", "Cucumber (2)", "Bait shrimp (Bay)",
                        "Blackgill rock.", "Brown rock.", "Surf perch (2)","Surf perch (1)", "Bait shrimp (Ghost)", "Mark. squid",
                        "Chilipepper rock.", "Surf perch (3)", "Vermilion", "Cabazon","Lingcod","Shortspine",
                        "Dungeness", "Pink shrimp", "Dover", "Hagfish", "Petrale")


NCAplot = ggplot(nca_gg,aes(x=species,y=communityname,fill=as.numeric(value)))+
  geom_tile(colour="grey",size=0.2)+ 
  guides(fill=guide_legend(title="% Revenue", reverse = T)) + 
  labs(x="",y="",title="Landings by N. California Communities")+
  #scale_fill_distiller(palette = "YlOrRd", trans = "reverse") +
  scale_fill_gradient(low = "white", high = "red") + 
  # scale_x_discrete(expand=c(0,0))+
  #  scale_y_continuous(expand=c(0,0),breaks=1:13,
  # labels = paste(WAspeciesrandom$rownames.WAspecies2.," (", round(WAspeciesrandom$Risk, digits = 3), ")")) +
  theme_grey(base_size=10) +
  annotate(geom="text", x=42, y=c(1:31), label= "",
           color="red") +
  annotate(geom="text", x=40.2, y=c(1:31), label= round(as.numeric(NCAspeciesrandom$Risk), digits = 2),
           color="red") +
  annotate(geom="text", x=40.2, y=32, label= "Risk",
           color="red") +
  annotate(geom="text", x=39, y=32.5, label = "",
           color="red")
pdf("N.Californialandings.pdf", width = 6, height = 4)
NCAplot + theme(axis.text.x=element_text(angle=50, size=7, vjust=1, hjust = 1)) +
  scale_x_discrete(
    labels=ncaspecies_fullname) + 
  theme(axis.text.y = element_text(face="bold", color="#993333", 
                                   size=7))# +
#geom_hline(yintercept = 18.5, linetype = 2)
dev.off()

### Southern California ##### 
SCAremove = which(colSums(SCAspecies) == 0)
SCAspecies2 = SCAspecies[,-SCAremove]
SCAportsuse = rownames(SCAspecies2)
SCAports = rep(NA, length = length(SCAportsuse))
for(i in 1:length(SCAportsuse)) {
  indexuse = which(AllportID$GEO_NAME == SCAportsuse[i])
  SCAports[i] = AllportID$PCID[indexuse]
}

SCAspecies3 = cbind(SCAspecies2, SCAports, paste(rownames(SCAspecies2), " (",
                                                 SCAports, ")", sep = ""))
SCAspeciesrandom = data.frame(SCAspecies3) %>% 
  group_by( SCAports ) %>%
  sample_n(1)


SCAspeciesrandom = SCAspeciesrandom[order(SCAspeciesrandom$Risk),]
colnames(SCAspeciesrandom)[29] = "communityname"

#SCAspeciesrandom$rownames.SCAspecies2.[1] = "VENTURA, CA"
sca_gg = SCAspeciesrandom %>%
  # convert data to long format
  gather( key="species",value="value",RURC, PSDN, CHLB, RCRB,  SABL,VRML, LSPN,   
          SSPN, WBAS, LOBS, SPRW, MSQD, RPRW, SHPD,
          BLGL, SWRD, PDAB, SRFP2, CMCK, USCU2, ETNA,BTNA,MAKO, TSRK )  %>%
  setNames(c("risk", "svi", "Exposure", "sCAports", "communityname", "species", "value"))

sca_species_subset = c( "RURC" ,   "PSDN" ,  "CHLB" ,                
                        "RCRB" , "SABL" ,  "VRML" ,                
                        "LSPN", "SSPN", "WBAS",                 
                        "LOBS" , "SPRW","MSQD" ,                
                        "RPRW" ,"SHPD",  "BLGL",                 
                        "PDAB" , "SWRD", "SRFP2" ,               
                        "CMCK", "USCU2" , "ETNA",                 
                        "BTNA" , "MAKO","TSRK"   )
sca_species_risk = rep(NA, length = length(sca_species_subset))

for(k in 1: length(sca_species_subset)) {
  indexspecies = which(speciesrisk$spid_use == sca_species_subset[k])
  sca_species_risk[k] = speciesrisk$avgrisk[indexspecies]
}

speciesriskorder = as.data.frame(cbind(sca_species_subset, sca_species_risk))
speciesriskorder = speciesriskorder[order(speciesriskorder$sca_species_risk, decreasing = TRUE),]

sca_gg$species <- factor(sca_gg$species, c( "SABL",  "BTNA" , "TSRK",  "LOBS",  "MAKO" , "RURC" , "ETNA",  "SWRD" , "LSPN",  "CMCK", 
                                            "PSDN",  "SHPD" , "RPRW",  "CHLB" , "WBAS",  "RCRB",  "USCU2", "BLGL" , "SRFP2", "MSQD", 
                                            "VRML" , "PDAB",  "SSPN" , "SPRW"  ))

sca_gg$communityname <- factor(sca_gg$communityname, c( "LA CANADA FLINTRIDGE, CA (OLA)"  ,     "GOLETA, CA (OBV)" ,                   
                                                        "SOLANA BEACH, CA (OSD)"        ,       "SAN BUENAVENTURA (VENTURA), CA (VEN)",
                                                        "PORT HUENEME, CA (HNM)"         ,      "SAN DIEGO, CA (SD)",                  
                                                        "OXNARD, CA (OXN)"                ,     "NEWPORT BEACH, CA (NWB)" ,            
                                                        "DANA POINT, CA (DNA)"             ,    "SANTA BARBARA, CA (SB)"  ))

scaspecies_fullname = c("Sablefish","Bluefin tuna", "Thresher", "Spiny lobster","Mako", "Red urchin",
                        "Bigeye tuna", "Swordfish", "Longspine", "Mackerel", "Sardine", "Sheephead",
                        "Ridgeback prawn", "CA halbut", "Seabass", "Rockcrab", "Cucumber (2)","Blackgill rock.",
                        "Surf perch (2)", "Mark. squid", "Vermilion", "Sanddab", "Shortspine", "Spotted prawn")

SCAplot = ggplot(sca_gg,aes(x=species,y=communityname,fill=as.numeric(value)))+
  geom_tile(colour="grey",size=0.2)+ 
  guides(fill=guide_legend(title="% Revenue", reverse = T)) + 
  labs(x="",y="",title="Landings by S. California Communities")+
  #scale_fill_distiller(palette = "YlOrRd", trans = "reverse") +
  scale_fill_gradient(low = "white", high = "red") + 
  # scale_x_discrete(expand=c(0,0))+
  #  scale_y_continuous(expand=c(0,0),breaks=1:13,
  # labels = paste(WAspeciesrandom$rownames.WAspecies2.," (", round(WAspeciesrandom$Risk, digits = 3), ")")) +
  theme_grey(base_size=10) +
  annotate(geom="text", x=26.9, y=c(1:10), label= "",
           color="red") +
  annotate(geom="text", x=25.8, y=c(1:10), label= round(as.numeric(SCAspeciesrandom$Risk), digits = 2),
           color="red") +
  annotate(geom="text", x=25.8, y=10.5, label= "Risk",
           color="red") +
  annotate(geom="text", x=26.8, y=10.8, label = "",
           color="red")

pdf("S.Californialandings.pdf", width = 6, height = 4)
SCAplot + theme(axis.text.x=element_text(angle=50, size=7, vjust=1, hjust = 1)) +
  scale_x_discrete(
    labels=scaspecies_fullname) + 
  theme(axis.text.y = element_text(face="bold", color="#993333", 
                                   size=7))# +
#geom_hline(yintercept = 2.5, linetype = 2)
dev.off()

library(ggpubr)
riskquant = quantile(risk, 0.9)
alllandings = ggarrange(
  plot + theme(axis.text.x=element_text(angle=50, size=7, vjust = 1, hjust = 1 )) +
    scale_x_discrete(
      labels=waspecies_fullname) + 
    theme(axis.text.y = element_text(face="bold", color="#993333", 
                                     size=7)) +
    geom_hline(yintercept=8.5, linetype="dashed", color = "darkred", size = 1),
  # geom_hline(yintercept = 8.5, linetype = 2),
  plotOR + theme(axis.text.x=element_text(angle=50, size=7, vjust=1, hjust = 1)) +
    scale_x_discrete(
      labels=orspecies_fullname) + 
    theme(axis.text.y = element_text(face="bold", color="#993333", 
                                     size=7)) +
    geom_hline(yintercept=9.5, linetype="dashed", color = "darkred", size = 1),
  #geom_hline(yintercept = 8.5, linetype = 2),
  NCAplot + theme(axis.text.x=element_text(angle=50, size=7, vjust=1, hjust = 1)) +
    scale_x_discrete(
      labels=ncaspecies_fullname) + 
    theme(axis.text.y = element_text(face="bold", color="#993333", 
                                     size=7)) +
    geom_hline(yintercept=26.5, linetype="dashed", color = "darkred", size = 1),
  # geom_hline(yintercept = 18.5, linetype = 2),
  SCAplot + theme(axis.text.x=element_text(angle=50, size=7, vjust=1, hjust = 1)) +
    scale_x_discrete(
      labels=scaspecies_fullname) + 
    theme(axis.text.y = element_text(face="bold", color="#993333", 
                                     size=7)) +
    geom_hline(yintercept=9.5, linetype="dashed", color = "darkred", size = 1),
  # geom_hline(yintercept = 2.5, linetype = 2),
  common.legend = TRUE, legend = "right",
  nrow = 2, ncol = 2
)

df <- data.frame()
arrowplot = ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 1) +
  geom_segment(aes(x = 10, y = 0.5, xend = 1, yend = 0.5), arrow = arrow(length = unit(0.5, "cm"))) +
  annotate("text", x = 9, y = 0.3, label = "Low species risk") +
  annotate("text", x = 1, y = 0.3, label = "High species risk") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.margin=unit(c(-0.5,1,1,1), "cm")) 
alllandings2 = ggarrange(alllandings, arrowplot, heights = c(2, 0.2),
                         ncol = 1, nrow = 2)
pdf("Landingsbystate_7.5.21.pdf", height = 9, width = 14)
alllandings2
dev.off()

### Figure 4 #####

#Figure 4 panel C
# BY STATE and THEMES
WA = which(endsWith(Alldata_final_subset$GEO_NAME, 'WA') == TRUE)
OR = which(endsWith(Alldata_final_subset$GEO_NAME, 'OR') == TRUE)
CA = which(endsWith(Alldata_final_subset$GEO_NAME, 'CA') == TRUE)

CA2 = Alldata_final_subset[endsWith(Alldata_final_subset$GEO_NAME, "CA"),]
SCA = which(CA2$PRIMARY_LATITUDE <= 34.45)
NCA = which(CA2$PRIMARY_LATITUDE > 34.45 )
topAC = quantile(Alldata_final_subset$total,0.9)

WAsocial = Alldata_final_subset[WA,]
ORsocial = Alldata_final_subset[OR,]
NCAsocial = Alldata_final_subset[NCA,]
SCAsocial = Alldata_final_subset[SCA,]

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
library(reshape2)
library(grid)
WAthemes = subset(WAsocial, select = c(GEO_NAME, theme1, theme2, theme3, theme4, total))
WAlong = melt(WAthemes, id.vars = c("GEO_NAME"))
ORthemes = subset(ORsocial, select = c(GEO_NAME, theme1, theme2, theme3, theme4,total))
ORlong = melt(ORthemes, id.vars = c("GEO_NAME"))
NCAthemes = subset(NCAsocial, select = c(GEO_NAME, theme1, theme2, theme3, theme4,total))
NCAlong = melt(NCAthemes, id.vars = c("GEO_NAME"))
SCAthemes = subset(SCAsocial, select = c(GEO_NAME, theme1, theme2, theme3, theme4,total))
SCAlong = melt(SCAthemes, id.vars = c("GEO_NAME"))
allsocthemes = subset(Alldata_final_subset, select = c(GEO_NAME, theme1, theme2, theme3, theme4,total))
alllong = melt(allsocthemes, id.vars = c("GEO_NAME"))
WAden = qplot(value, data = WAlong, 
              geom = "density", main = "Washington",
              color = variable, xlab = "") + theme_bw() +
  theme(legend.title=element_blank(),legend.text=element_text(size=10)) +
  scale_color_manual(values=c(cbp1[c(2,4:7)]), 
                     labels = c("Theme 1 ", "Theme 2", "Theme 3", "Theme 4", "Adapt. Capacity"))

ORden = qplot(value, data = ORlong, 
              geom = "density", main = "Oregon",
              color = variable, xlab = "") + theme_bw() +
  theme(legend.title=element_blank(),legend.text=element_text(size=5)) +
  scale_color_manual(values=c(cbp1[c(2,4:7)]), 
                     labels = c("Theme 1 ", "Theme 2", "Theme 3", "Theme 4", "Adapt. Capacity"))
NCAden = qplot(value, data = NCAlong, 
               geom = "density", main = "N. California",
               color = variable, xlab = "") + theme_bw() +
  theme(legend.title=element_blank(),legend.text=element_text(size=5)) +
  scale_color_manual(values=c(cbp1[c(2,4:7)]), 
                     labels = c("Theme 1 ", "Theme 2", "Theme 3", "Theme 4", "Adapt. Capacity"))
SCAden = qplot(value, data = SCAlong, 
               geom = "density", main = "S. California",
               color = variable, xlab = "") + theme_bw() +
  theme(legend.title=element_blank(),legend.text=element_text(size=5)) +
  scale_color_manual(values=c(cbp1[c(2,4:7)]), 
                     labels = c("Theme 1 ", "Theme 2", "Theme 3", "Theme 4", "Adapt. Capacity"))
qplot(theme1, total, data = allsocthemes, col = cbp1[2])  

allsocialdensity = ggarrange(WAden, ORden, NCAden, SCAden, 
                             ncol = 2, nrow = 2, legend.grob = get_legend(WAden), 
                             legend = "right", font.label = list(size = 20, color = "black", face = "bold", family = NULL))
pdf("socialbystate_2021density2.pdf", height = 8, width = 8)
annotate_figure(allsocialdensity,  left = textGrob("Density", rot = 90), bottom = textGrob("Adaptive capacity rank"))
dev.off()

#figure 4 panel B
vulnindicator = data.frame(Alldata_final_subset$vuln, Alldata_final_subset$Perc_Rel, 
                           exposurerevpercentile[,4],
                           percent_rank(Alldata_final_subset$MUNIT),
                           
                           percent_rank(Alldata_final_subset$GROUPQ), percent_rank(Alldata_final_subset$CROWD), 
                           percent_rank(Alldata_final_subset$NOVEH),
                           percent_rank(Alldata_final_subset$MOBILE),
                           percent_rank(Alldata_final_subset$MINRTY),percent_rank(Alldata_final_subset$LIMENG), 
                           percent_rank(Alldata_final_subset$DISABL),
                           percent_rank(Alldata_final_subset$AGE17), percent_rank(Alldata_final_subset$AGE65),
                           percent_rank(Alldata_final_subset$SNGPNT),  percent_rank(Alldata_final_subset$UNEMP),
                           
                           percent_rank(Alldata_final_subset$NOHSDP),
                           percent_rank(Alldata_final_subset$POV),1-percent_rank(Alldata_final_subset$PCI),
                           Alldata_final_subset$total)

names(vulnindicator) = c('Vulnerability','Reliance', 
                         'Exposure','Multiunit home',
                         'Group quarters','Crowding','No vehicle','Mobile home',
                         'Minority','Limited English',
                         'Disability',
                         'Age 17-', 'Age 65+',
                         'Single parent','Unemployment',
                         'No high. diploma', 'Poverty','Income',
                         'Adapt. cap.')
test = cor(rev(vulnindicator[,c(4:19)]))
library(corrplot)
library(RColorBrewer)
corrplot(test, method = "color", type="lower", mar = c(0,5,1,1),
         col=brewer.pal(n=8, name="Greys"), outline = TRUE,addgrid.col = 'white',
         tl.col=c("black", cbp1[2], cbp1[2], cbp1[2], cbp1[2],cbp1[4], cbp1[4], cbp1[4],cbp1[4], 'gold','gold',
                  cbp1[6], cbp1[6], cbp1[6], cbp1[6], cbp1[6]),
         tl.srt=45)

##### Figure 5 ######

quadplot = ggplot() +
  geom_rect(aes(xmin=0, xmax=0.5, ymin=0, ymax=0.68), fill="#93D500", alpha=0.35, inherit.aes = FALSE)+
  geom_rect(aes(xmin=0, xmax=0.5, ymin=0.68, ymax=1.36), fill="#ffffbf", alpha=0.5, inherit.aes = FALSE) +
  geom_rect(aes(xmin=0.5, xmax=1, ymin=0, ymax=0.68), fill="#ffffbf", alpha=0.5, inherit.aes = FALSE) +
  geom_rect(aes(xmin=0.5, xmax=1, ymin=0.68, ymax=1.36), fill="#DF3432", alpha=0.35, inherit.aes = FALSE) +
  # red before - DF3432, green before - 93D500
  geom_point(data = Alldata_final_subset, aes(x = total , y = risk, col = STATEABBR), size = 3, shape = 16) + 
  scale_colour_manual("State",values = c("black","blue","darkorange2")) +
  #geom_text_repel(data=subset(Alldata_final_subset, vuln >= 1.351),
  #               aes(x = total, y = risk,label=tolower((MAPNAME)), col = STATEABBR, fontface = "bold")) +
  #scale_colour_manual("State",values = c("black","blue","darkorange")) +
  xlab("Adaptive Capacity (high to low)")+ylab("Community Risk")+ggtitle("") + theme_classic() +
  annotate("text", x = 0.25, y = 0.25, label = "Low Community Vuln.", fontface = "bold") +
  annotate("text", x = 0.75, y = 0.25, label = "Med. Community Vuln.", fontface = "bold") +
  annotate("text", x = 0.25, y = 1.3, label = "Med. Community Vuln.", fontface = "bold") +
  annotate("text", x = 0.75, y = 1.3, label = "High Community Vuln.", fontface = "bold") 
quadplot

# social indicator theme plots 
theme1plot =  ggplot() +
  geom_point(data = Alldata_final_subset, aes(x = total , y = risk, col = theme1), size = 2, shape = 16) +
  scale_colour_gradient("Theme1", low="blue", high="orange") +
  xlab("Adaptive Capacity (high to low)")+ylab("Community Risk")+ggtitle("") + theme_classic() 

theme2plot = ggplot() +
  geom_point(data = Alldata_final_subset, aes(x = total , y = risk, col = theme2), size = 2, shape = 16) +
  scale_colour_gradient("Theme2", low="blue", high="orange") +
  xlab("Adaptive Capacity (high to low)")+ylab("Community Risk")+ggtitle("") + theme_classic() 
theme3plot = ggplot() +
  geom_point(data = Alldata_final_subset, aes(x = total , y = risk, col = theme3), size = 2, shape = 16) +
  scale_colour_gradient("Theme3", low="blue", high="orange") +
  xlab("Adaptive Capacity (high to low)")+ylab("Community Risk")+ggtitle("") + theme_classic() 
theme4plot = ggplot() +
  geom_point(data = Alldata_final_subset, aes(x = total , y = risk, col = theme4), size = 2, shape = 16) +
  scale_colour_gradient("Theme4", low="blue", high="orange") +
  xlab("Adaptive Capacity (high to low)")+ylab("Community Risk")+ggtitle("") + theme_classic()
ggarrange(theme1plot, theme2plot, theme3plot, theme4plot, nrow = 2, ncol = 2)

### Figure 6 #####

library(urbnmapr)
library(rnaturalearth)
library(ggrepel)

states_sf <- get_urbn_map("states", sf = TRUE)
world <- ne_countries(scale = "medium", returnclass = "sf")
#Alldata_final_subset$risk = FinalRisk_rev[,4]
#Alldata_final_subset$vuln = FinalVulnMatrix_rev[,4]
WAavgdif = Alldata_final_subset$risk[WA]/Alldata_final_subset$vuln[WA]
ORavgdif = Alldata_final_subset$risk[OR]/Alldata_final_subset$vuln[OR]
NCAavgdif = Alldata_final_subset$risk[NCA]/Alldata_final_subset$vuln[NCA]
SCAavgdif = Alldata_final_subset$risk[SCA]/Alldata_final_subset$vuln[SCA]


risk10percentile = quantile(Alldata_final_subset$risk, 0.95) 
states <- map_data("state")
west_coast <- subset(states, region %in% c("california", "oregon", "washington"))
plotrisk =  ggplot() + coord_fixed(1.3) +
  geom_sf(data = world) +  geom_sf(data = states_sf, fill = "grey") +
  geom_point(data = Alldata_final_subset, aes(x = PRIMARY_LONGITUDE, y = PRIMARY_LATITUDE, col = risk), size = 2, shape = 16) +
  geom_text_repel(data=subset(Alldata_final_subset, risk > 1.137),
                  aes(x = PRIMARY_LONGITUDE, y = PRIMARY_LATITUDE,
                      label=tolower((substr(GEO_NAME,1,nchar(GEO_NAME)-4)))), 
                  fontface = 'bold', col = ("black")) +
  xlab("")+ylab("")+ggtitle("a) Community Risk")+
  coord_sf(xlim = c(-130, -115), ylim = c(30,51), expand = FALSE)

plotSVI = ggplot() + coord_fixed(1.3) +
  geom_sf(data = world) +  geom_sf(data = states_sf, fill = "grey") +
  geom_point(data = Alldata_final_subset, aes(x = PRIMARY_LONGITUDE, y = PRIMARY_LATITUDE, col =total), size = 2, shape = 16) +
  geom_text_repel(data=subset(Alldata_final_subset, total > 0.95),
                  aes(x = PRIMARY_LONGITUDE, y = PRIMARY_LATITUDE,
                      label=tolower((substr(GEO_NAME,1,nchar(GEO_NAME)-4)))), 
                  fontface = 'bold', col = ("black")) +
  xlab("")+ylab("")+ggtitle("b) Adaptive Capacity") +
  coord_sf(xlim = c(-130, -115), ylim = c(30,51), expand = FALSE)

vulntop5 = quantile(Alldata_final_subset$vuln, 0.95) 
plotvuln = ggplot() + coord_fixed(1.3) +
  geom_sf(data = world) +  geom_sf(data = states_sf, fill = "grey") +
  geom_point(data = Alldata_final_subset, aes(x = PRIMARY_LONGITUDE, y = PRIMARY_LATITUDE, col = vuln), size = 2, shape = 16) +
  geom_text_repel(data=subset(Alldata_final_subset, vuln > 1.351),
                  aes(x = PRIMARY_LONGITUDE, y = PRIMARY_LATITUDE,label=tolower((substr(GEO_NAME,1,nchar(GEO_NAME)-4)))), 
                  fontface = 'bold', col = ("black")) +
  xlab("")+ylab("")+ggtitle("c) Community Vulnerability")+
  coord_sf(xlim = c(-130, -115), ylim = c(30,51), expand = FALSE)
                 

statefill = c(mean(WAavgdif), mean(ORavgdif), mean(NCAavgdif), mean(SCAavgdif))
Alldata_final_subset$Difference = (rank(Alldata_final_subset$vuln) - rank(Alldata_final_subset$risk))
plotdif = ggplot() + coord_fixed(1.3) +
  geom_sf(data = world) +  geom_sf(data = states_sf, fill = 'grey') +
  geom_point(data = Alldata_final_subset, aes(x = PRIMARY_LONGITUDE, y = PRIMARY_LATITUDE, col = Difference), size = 2, shape = 16) +
  xlab("")+ylab("")+ggtitle("d) Difference: \n Rank Vuln - Rank Risk")+
  coord_sf(xlim = c(-130, -115), ylim = c(30,51), expand = FALSE)

library(scico)
scico_palette_show(palettes = c("broc", "cork", "vik",
                                "lisbon", "tofino", "berlin",
                                "batlow", "roma"))
plotvuln+scale_color_scico(name="Vulnerability",palette = "roma", direction = -1,
                           breaks=c(0,0.4,0.8,1.2,1.64),labels=c("Low",0.4,0.8,1.2,"High"),
                           limits=c(0,1.64))

mapfigure = ggarrange( plotrisk+scale_color_scico(name="Risk",palette = "lajolla", direction = 1,
                                                  breaks=c(0,0.3,0.6,0.9,1.2,1.4),labels=c("Low",0.3,0.6,0.9,1.2,"High"),
                                                  limits=c(0,1.4))+ theme(axis.text.x=element_text(angle=50, size=5, vjust=1)),
                       plotSVI+scale_color_scico(name="Capacity",palette = "lajolla", direction = 1,
                                                 breaks=c(0,0.2,0.4,0.6,0.8,1),labels=c("High",0.2,0.4,0.6,0.8,"Low"),
                                                 limits=c(0,1))+ theme(axis.text.x=element_text(angle=50, size=5, vjust=1)),
                       plotvuln+  scale_color_scico(name="Vuln.",palette = "lajolla", direction = 1,
                                                    breaks=c(0,0.4,0.8,1.2,1.64),labels=c("Low",0.4,0.8,1.2,"High"),
                                                    limits=c(0,1.64)) + theme(axis.text.x=element_text(angle=50, size=5, vjust=1)),
                       plotdif+ scale_color_scico(name="Change",palette = "roma",
                                                  direction = -1) + theme(axis.text.x=element_text(angle=50, size=5, vjust=1)),# + geom_hline(yintercept=42, linetype="dashed", 
                       #         color = "red", size=2),
                       nrow = 2, ncol = 2
)

pdf("RiskVuln_Mapfigure_aug2021.pdf", width = 8, height = 11)
mapfigure
dev.off()
