# Creating maps of probability of occurrence for each species to then find 
# climate experienced by each species. Need data for each species from aquamaps: Aquamaps.org
# and data on location cells from Ocean Health index - https://github.com/OHI-Science/IUCN-AquaMaps

library(raster)
library(here)
#> Loading required package: sp
library(dplyr, warn.conflicts = FALSE)
library(maps)
options(gsubfn.engine = "R") # for sqldf
library(sqldf)
options(sqldf.driver = 'SQLite')
library(RColorBrewer)
#install.packages('rgdal',repos="http://www.stats.ox.ac.uk/pub/RWin")
library(rgdal)
# note when installing rgdal, do this:
#Do you want to install from sources the package which needs compilation?
#y/n: n
library(ncdf4)
here::here() # set working directory

### Laura Koehn 1/17/2020 ######
# Takes files of (1) species id's from aquamaps, (2) distributions of species
# from Aquamaps with center long and center lat and loiczid cell values and
# probability of occurence in each cell and (3) loiczid cell ids for world
# to create cdf files and maps for each species for prob. of occurence > than
# a given value
# 2/28/2020 - additional files for 26 species that make up "unspecified" species catch
# and species that were added to top 90% of landings after
# re-running Jameal's code 
# folder: Koehn_28Feb2020 ; files: koehn_hcaf_species_native_26.csv
# koehn_speciesoccursum_26.csv
##############################

# read in data files
# Data files can be found here: https://github.com/OHI-Science/IUCN-AquaMaps
# May be updated on that github repository from what was used at the time of
# Koehn et al. 2022
spp = read.csv('spp.csv') # species name, id number, iucn and fishbase info
# tic = Sys.time()
# cells_spp = read.csv('cells_spp.csv') # cell id and species id - so, cells that each
# # # species is in
# print(Sys.time() - tic) # Time difference of 1.751021 mins
# cells_spp2 = read.csv('cell_spp2.csv')
spp_OHI = read.csv('speciesoccursum_ver0816c.csv')
#
# e=extent(-134.0,-115,30,48)
e = extent(-180,-100,20,60)
# # exract cells in which species range occurs
#

# faster now only 53 species
# NOTE: need data from aquamaps that contains for each species:
# SpeciesID,	CsquareCode,	LOICZID,	CenterLat,	CenterLong,	Probability of occurrence
# Data not included with manuscript because needs to be requested from Aquamaps.org
tic = Sys.time()
sp.cells = read.csv('Koehn_15Jan2020/hcaf_species_native_16Jan2020_loiczid.csv')
sp.cellsnew = read.csv('Koehn_28Feb2020/koehn_hcaf_species_native_26.csv')
sp.cells_extra= read.csv('koehn_25Mar2020/koehn_hcaf_species_native_9.csv')
print(Sys.time() - tic)

# TESTING OHI data
#OHI data
loiczid_raster <- raster('loiczid_raster.tif') %>%
  setNames('loiczid')


loiczid_rast <- raster(ext = extent(-180, 180, -90, 90),
                       res = 0.5, crs = '+init=epsg:4326')
values(loiczid_rast) <- 1:ncell(loiczid_rast)
cid2 = values(loiczid_rast)


e2=extent(-134.0,-115,30,48)
data("wrld_simpl", package = "maptools")
# data(World)
# wrld_simpl_crop = crop(wrld_simpl, e)

# NOTE request species information with species id, genus, etc. from Aquamaps - not provided here
# columns: SpeciesID	SpecCode	Genus	Species	FBname	Kingdom	Phylum	Class	Order	Family
spp_list <- read.csv('Koehn_15Jan2020/speciesoccursum_15Jan2020.csv')
sp.list.underscores <- paste(spp_list$Genus, spp_list$Species, sep = "_")
spp_listnew <- read.csv('Koehn_28Feb2020/koehn_speciesoccursum_26.csv')
sp.list.underscoresNEW <- paste(spp_listnew$Genus, spp_listnew$Species, sep = "_")
sp.listLN <- read.csv('koehn_25Mar2020/koehn_speciesoccursum_9.csv')
sp.list.underLN <- paste(sp.listLN$Genus, sp.listLN$Species, sep = "_")

# Make files of occurrence data for each species 
for(i in 1:length(sp.list.underscores)) {
  sp <- sp.list.underscores[i]
  spp_id <- spp_list %>%
    filter(SciName == sp) %>%
    select(SpeciesID, SciName) %>%
    distinct()
  
  am_spp_cells = sp.cells
  am_spp_map   <- am_spp_cells %>%
    filter(SpeciesID == (spp_id$SpeciesID), Probability >= 0.4) # set at 0.4
  # in Koehn et al. 2022
  
  spp_map_df = am_spp_map
  
  
  cid2 %in% spp_map_df$LOICZID
 # cid %in% spp_map_df$LOICZID
  
  r.sp2 = calc(loiczid_rast, fun=function(cid2) {cid2 %in% spp_map_df$LOICZID})
  myfilename <- paste(as.character(sp.list.underscores[i]),sep=".","tif")
  writeRaster(r.sp2, myfilename, format="CDF",overwrite=TRUE)
  
  #plot(r.sp2)
  r.sp.cc2<-crop(r.sp2,e2)
  #plot(r.sp.cc2)
  writeRaster(r.sp.cc2, myfilename, format="ascii",overwrite=TRUE)
  
  
  myfilename1 <- paste("plot",as.character(sp.list.underscores[i]),sep=".","png")
  png(myfilename1)
  plot(r.sp.cc2, col=c('lightblue','red')) #lightblue
  #plot(wrld_simpl_crop, add = T, col='gray95',border='gray80')
  map('world',col='gray95',fill=T,border='gray80',add=T)
  dev.off()
  print(i)
}

## Additional species ###
for(i in 1:length(sp.list.underscoresNEW)) {
  sp <- sp.list.underscoresNEW[i]
  spp_id <- spp_listnew %>%
    filter(SciName == sp) %>%
    select(SpeciesID, SciName) %>%
    distinct()
  
  am_spp_cells = sp.cellsnew
  am_spp_map   <- am_spp_cells %>%
    filter(SpeciesID == levels(droplevels(spp_id$SpeciesID)), Probability >= 0.4)
  
  spp_map_df = am_spp_map
  
  
  cid2 %in% spp_map_df$LOICZID
  #cid %in% spp_map_df$LOICZID
  
  r.sp2 = calc(loiczid_raster, fun=function(cid2) {cid2 %in% spp_map_df$LOICZID})
  myfilename <- paste(as.character(sp.list.underscoresNEW[i]),sep=".","tif")
  writeRaster(r.sp2, myfilename, format="CDF",overwrite=TRUE)
  
  #plot(r.sp2)
  r.sp.cc2<-crop(r.sp2,e)
  #plot(r.sp.cc2)
  writeRaster(r.sp.cc2, myfilename, format="ascii",overwrite=TRUE)
  
  
  myfilename1 <- paste("plot",as.character(sp.list.underscoresNEW[i]),sep=".","png")
  png(myfilename1)
  plot(r.sp.cc2, col=c('lightblue','red')) #lightblue
  #plot(wrld_simpl_crop, add = T, col='gray95',border='gray80')
  map('world',col='gray95',fill=T,border='gray80',add=T)
  dev.off()
  print(i)
}

# even more species
for(i in 1:length(sp.list.underLN)) {
  sp <- sp.list.underLN[i]
  spp_id <- sp.listLN %>%
    filter(SciName == sp) %>%
    dplyr::select(SpeciesID, SciName) %>%
    distinct()
  
  am_spp_cells = sp.cells_LauraN 
  am_spp_map   <- am_spp_cells %>%
    filter(SpeciesID == (spp_id$SpeciesID), Probability >= 0.4)
  
  spp_map_df = am_spp_map
  
  
  cid2 %in% spp_map_df$LOICZID
  #cid %in% spp_map_df$LOICZID
  
  r.sp2 = calc(loiczid_raster, fun=function(cid2) {cid2 %in% spp_map_df$LOICZID})
  myfilename <- paste(as.character(sp.list.underLN[i]),sep=".","tif")
  writeRaster(r.sp2, myfilename, format="CDF",overwrite=TRUE)
  
  plot(r.sp2)
  r.sp.cc2<-crop(r.sp2,e2)
  #plot(r.sp.cc2)
  writeRaster(r.sp.cc2, myfilename, format="ascii",overwrite=TRUE)
  
  
  myfilename1 <- paste("plot",as.character(sp.list.underLN[i]),sep=".","png")
  png(myfilename1)
  plot(r.sp.cc2, col=c('lightblue','red')) #lightblue
  #plot(wrld_simpl_crop, add = T, col='gray95',border='gray80')
  map('world',col='gray95',fill=T,border='gray80',add=T)
  dev.off()
  print(i)
}
