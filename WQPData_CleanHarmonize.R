#This code roughly cleans and then examines the temporal data distribution by basin

library(tzdb)
library(withr)
library(readr)
library(dplyr)
library(ggplot2)
library(purrr)
library(labeling)
library(farver)
library(vroom)
library(lubridate)
library(sf)



setwd('~/Documents/SNiP/WQP_data')

#Call aquasat stream data
aquastream <- read.csv('~/Documents/SNiP/Aquasat/aquastreamTSS.csv')

  
tssOnly <- aquastream %>%
  filter(type == 'Stream') %>%
  filter(!is.na(tss)) %>%
  group_by(SiteID) %>%
  filter(n() >= 5)%>%
  filter(!is.na(long))%>%
  filter(!is.na(lat))
nrow(tssOnly)
tssOnlyUnique = tssOnly %>% distinct(SiteID, .keep_all=TRUE)
length(unique(tssOnly$SiteID))

library(mapview)
mapview(tssOnlyUnique, xcol = "long", ycol = "lat", crs = 4269, grid = FALSE)

#Checka site for fun
ct <- tssOnly %>% filter(SiteID == 'USGS-15292700')
                           
                           #c('USGS-15292780', 'USGS-15292780', 
                          #   'USGS-15292000', 'USGS-15298040',
                           #  'USGS-15294350'))
print(ct)

##Call WQP Data
##MS Basin
#df <- HUC01 <- readRDS("WQPdataraw_Sed_HUC01.rds")
#hucname = 'huc01'
#df <- HUC02 <- readRDS("WQPdataraw_Sed_HUC02.rds")
#hucname = 'huc02'
#df <- HUC03 <- readRDS("WQPdataraw_Sed_HUC03.rds")
#hucname= 'huc03'
#df <- HUC04 <- readRDS("WQPdataraw_Sed_HUC04.rds")
#hucname = 'huc04'
#df <- HUC05 <- readRDS("WQPdataraw_Sed_HUC05.rds")
#hucname = 'huc05'
#df <- HUC06 <- readRDS("WQPdataraw_Sed_HUC06.rds")
#hucname = 'huc06'
#df <- HUC07 <- readRDS("WQPdataraw_Sed_HUC07.rds")
#hucname = 'huc07'
df <- HUC08 <- readRDS("WQPdataraw_Sed_HUC08.rds")
hucname = 'huc08'
#df <- HUC09 <- readRDS("WQPdataraw_Sed_HUC09.rds")
#hucname = 'huc09'
#df <- HUC10 <- readRDS("WQPdataraw_Sed_HUC10.rds")
#hucname = 'huc10'
#df <- HUC11 <- readRDS("WQPdataraw_Sed_HUC11.rds")
#hucname = 'huc11'
#df <- HUC12 <- readRDS("WQPdataraw_Sed_HUC12.rds")
#hucname = 'huc12'
#df <- HUC13 <- readRDS("WQPdataraw_Sed_HUC13.rds")
#hucname = 'huc13'
#df <- HUC14 <- readRDS("WQPdataraw_Sed_HUC14.rds")
#hucname = 'huc14'
#df <- HUC15 <- readRDS("WQPdataraw_Sed_HUC15.rds")
#hucname = 'huc15'
#df <- HUC16 <- readRDS("WQPdataraw_Sed_HUC16.rds")
#hucname = 'huc16'
#df <- HUC17 <- readRDS("WQPdataraw_Sed_HUC17.rds")
#hucname = 'huc17'
#df <- HUC18 <- readRDS("WQPdataraw_Sed_HUC18.rds")
#hucname = 'huc18'
#df <- HUC19 <- readRDS("WQPdataraw_Sed_HUC19.rds")
#hucname = 'huc19'
#df <- HUC20 <- readRDS("WQPdataraw_Sed_HUC20.rds")
#hucname = 'huc20'
#df <- HUC21 <- readRDS("WQPdataraw_Sed_HUC21.rds")
#hucname = 'huc21'

#Checka site for fun
df_site <- df %>% filter(MonitoringLocationIdentifier == '21AWIC-3872')
print(df_site)

print(unique(df$ResultMeasure.MeasureUnitCode))

names(df)
print(unique(df$CharacteristicName))
print(unique(df$ResultSampleFractionText))
print(table(df$ResultMeasure.MeasureUnitCode))

#df_sub <- df %>%
#  filter(ResultMeasure.MeasureUnitCode == 'mg/L') %>%
#  mutate(ResultMeasureValue = as.numeric(ResultMeasureValue)) %>%
#  filter(ResultMeasureValue < 200)

#hist(df_sub$ResultMeasureValue, breaks = 100)

#Select and ROUGHLY adapt to same Characteristic Names and parameters as Aquasat
#Went ahead and removed na values that Ross et al 2019 has sprinkled throughout his code.
#MUST REFINE FOR ALL (especially N and P)
wqp.tss.renamer <- function(df){
  TSS_forms <- c("Total suspended solids", "Suspended Sediment Concentration (SSC)", "Suspended sediment Concentration (SSC)", "Fixed Suspended solids", "Sediment")
  TSS_fractionsNOT <- c('Bedload', 'Bed Sediment', 'Dissolved')
  simple.names <- df %>%
    dplyr::select(date=ActivityStartDateTime,
                  parameter=CharacteristicName,
                  units=ResultMeasure.MeasureUnitCode,
                  SiteID=MonitoringLocationIdentifier,
                  huc=HUCEightDigitCode,
                  org=OrganizationFormalName,
                  org_id=OrganizationIdentifier,
                  time=(ActivityStartDateTime),
                  value=ResultMeasureValue,
                  sample_method=SampleCollectionMethod.MethodName,
                  analytical_method=ResultAnalyticalMethod.MethodName,
                  particle_size=ResultParticleSizeBasisText,
                  date_time=(ActivityStartDateTime),
                  media=ActivityMediaName,
                  sample_depth=ActivityDepthHeightMeasure.MeasureValue,
                  sample_depth_unit=ActivityDepthHeightMeasure.MeasureUnitCode,
                  fraction=ResultSampleFractionText,
                  status=ResultStatusIdentifier,
                  hydroCondition = HydrologicCondition,
                  hydroEvent = HydrologicEvent,
                  lat=LatitudeMeasure,
                  long=LongitudeMeasure,
                  datum=HorizontalCoordinateReferenceSystemDatumName) %>%
    #Remove trailing white space in labels
    mutate(units = trimws(units)) %>%
    mutate(time = as.character(format(as_datetime(date_time), '%H:%M:%S'))) %>%
    mutate(date = as.character(format(as_datetime(date_time), '%Y-%m-%d'))) %>%
    filter(parameter %in% TSS_forms) %>%
    filter(!fraction %in% TSS_fractionsNOT) %>%
    filter(media=='Water') %>% #Must be in a river or stream
    filter(is.na(sample_depth) | sample_depth <= 3) %>% #can't throw away all values/surfacae water samples
    filter(!is.na(value)) %>% #Must exist
    filter(!is.na(date)) %>%
    filter(!is.na(lat)) %>% #Exclude sites with missing geospatial data
    filter(!is.na(long)) %>%
    distinct(SiteID,value, date, .keep_all = TRUE) #Remove duplicates - rough - did not average between multiples at same site/time
  dropped = nrow(df)-nrow(simple.names)
  print(paste('we dropped',dropped,'samples'))
  print(paste('nrows',nrow(simple.names)))
  return(simple.names)
}



#Function for nonsensical methods for TSS

nonsense.methods.fxn <- function(df){
  #There are a lot of parameter codes so we are just going to use a grepl command with key words that definitely disqualify the sample
  non.sensical.tss.methods <- df %>%
    filter(grepl("Oxygen|Nitrogen|Ammonia|Metals|E. coli|Carbon|Anion|Cation|Phosphorus|Silica|PH|HARDNESS|Nutrient|Turbidity|Temperature|Nitrate|Conductance|Alkalinity|Chlorophyll",analytical_method,ignore.case=T))
  
  
  tss.filtered.method <- df %>%
    filter(!analytical_method %in% non.sensical.tss.methods$analytical_method)
  
  print(paste('We dropped', round(nrow(non.sensical.tss.methods)/nrow(df)*100,2),'% of samples, because the method used did not make sense. These methods are:'))
  print(unique(non.sensical.tss.methods$analytical_method),wrap='',sep=' - ') #Nice function for printing long vectors horizontally separated by dash
  return(tss.filtered.method)
}

#Function to filter and synchronize units for TSS

unit.harmony.fxn <- function(df){
  #Select only units for % and filter to keep only the sand fraction data ('< 0.0625 mm','sands')
  tss.p <- df %>%
    filter(units =='%' & (particle_size %in%  c('< 0.0625 mm','sands'))) %>%
    mutate(conversion=NA,
           parameter_name='p.sand',
           harmonized_value=value,
           harmonized_unit='%')
  
  
  #Make a tss lookup table
  tss.lookup <- tibble(units=c('mg/L', 'mg/l','g/l','ug/l','ppm'),
                       conversion = c(1,1,1000,1/1000,1))
  
  #Join to the lookup table and harmonize units
  
  tss.harmonized <- df %>%
    inner_join(tss.lookup,by='units') %>%
    mutate(parameter_name = 'tss',
           harmonized_value=as.numeric(value) * as.numeric(conversion),
           harmonized_unit='mg/l') 
  
  #Combine p.sand and tss dataframes
  #remove conversion/original columns and rename harmonized columns
  tss.harmonized.p <- rbind(tss.p, tss.harmonized) %>%
    select(-value, -units, -conversion,
           value = harmonized_value,
           units = harmonized_unit)
  dropped = nrow(df)-nrow(tss.harmonized.p)
  print(paste('we dropped',dropped,'samples with unknown/unusable units'))
  print(nrow(tss.harmonized.p))
  return(tss.harmonized.p)
}

#Fxn to harmonize depth units to m
depth.harmony.fxn <- function(df) {
  #Define a depth lookup table to convert all depth data to meters. 
  depth.lookup <- tibble(sample_depth_unit=c('cm','feet','ft','in','m','meters','None'),
                         depth_conversion=c(1/100,.3048,.3048,0.0254,1,1,NA)) 
  
  #Join depth lookup table to tss data
  tss.depth <- df %>%
    dplyr::mutate(sample_depth = as.numeric(sample_depth)) %>%
    left_join(depth.lookup,by='sample_depth_unit') %>%
    #Some depth measurements have negative values (assume that is just preference)
    #I also added .01 meters because many samples have depth of zero assuming they were
    # taken directly at the surface
    #remove original/conversion columns and rename harmonized columns
    mutate(harmonized_depth=abs(sample_depth*depth_conversion)+.01) %>%
    select(-depth_conversion, -sample_depth, -sample_depth_unit,
           sample_depth = harmonized_depth) %>%
    mutate(sample_depth_unit = 'm') %>%
    filter(is.na(sample_depth) | sample_depth <= 1)
  
  # We lose lots of data by keeping only data with depth measurements
  print(paste('If we only kept samples that had depth information we would lose',round((nrow(df)-nrow(tss.depth))/nrow(df)*100,1),'% of samples'))
  dropped_depth = nrow(df)-nrow(tss.depth)
  print(paste('we dropped an additional',dropped_depth,'samples with converted depths > 1 m'))
  
  return(tss.depth)
}

#test = as_datetime(df$ActivityStartDateTime)
#head(test)
#format(test, '%H:%M:%S')
#test2 = as.character(format(test, '%Y-%m-%d'))
#head(test2)
#test1 = as.POSIXct(df$ActivityStartDateTime, origin = '1970-01-01', format="%Y-%m-%d", tz = 'UTC')
#head(test1)

#format(test1, format="%Y-%m-%d", tz = "UTC" )
#format(test1, format="%H:%M:%S", tz = "UTC" )

#Function for date and time 
#Need to add date and time splitter later
date.format.fxn <- function(df){
  date.format <- df %>% 
    dplyr::mutate(time = as.character(format(as_datetime(date_time), '%H:%M:%S'))) %>%
    dplyr::mutate(date = as.character(format(as_datetime(date_time), '%Y-%m-%d'))) %>%
    #dplyr::mutate(date_only= as.character(ifelse(is.na(as_datetime(date_time)) | time == '00:00:00',T,F))) %>%
    #dplyr::mutate(date_unity = as.character(ymd_hms(ifelse(as_datetime(date_only) == T,
    #                                                       paste(date,'00:00:00'),
    #                                                       date),
    #                                                tz='UTC'))) %>%
    #remove any time stamps that are NA
    dplyr::filter(!is.na(date))
  return(date.format)
}


#Function for datum
#From Aquasat:
#Setup a datum table to transform different datums into WGS84 for GEE and to 
#match LAGOS. Note that we make the somewhat reisky decision that others and unknowns
# are mostly NAD83 (since > 60% of the data is originally in that projection)
datum.format.fxn <- function(df){
  print(table(df$datum)) #Alter function if needed to account for misc. datum
  datum_epsg <- tibble(datum=c('NAD27','NAD83','OTHER','UNKWN','WGS84'),
                       epsg=c(4267,4269,4269,4269,4326))
  # Get distinct lat longs and sites
  inv_uniques <- df %>%
    distinct(SiteID,lat,long,datum)
  
  ## project inventory using the above lookup table
  ## Have to loop over each projection
  projected <- list() 
  
  for(i in 1:nrow(datum_epsg)){
    #Select each datum iteratively
    d1 <- datum_epsg[i,]
    # Join inventory to that datum only and then feed it that CRS and then 
    # transform to wgs84
    inv_reproj <- inv_uniques %>%
      filter(datum == d1$datum) %>%
      inner_join(d1, by='datum') %>%
      st_as_sf(.,coords=c('long','lat'),crs=d1$epsg) %>%
      st_transform(4326)
    
    projected[[i]] <- inv_reproj
  }
  
  #print(projected)
  # Note that we lose some sites in GUAM which is intentional
  # Add back in lat longs which are all WGS84 now
  inv_wgs84 <- do.call('rbind', projected) %>%
    mutate(lat = st_coordinates(.)[,2],
           long=st_coordinates(.)[,1]) %>%
    as.data.frame(.) %>%
    select(-geometry,-datum) 
  print(head(inv_wgs84))
  #print(head(df))
  
  #Put back together
  df <- inner_join(df, inv_wgs84, by = 'SiteID')
  
  return(df)
  print('done')
  }


#Function to join aquasast and basin for Landsat ID and reflectances
SNiP.Aquasat.join.fxn <- function(df){
  df_slim <- df %>%
    dplyr::select(parameter, units, SiteID, lat, long, datum, value, particle_size, date, time, date_time, hydroCondition, hydroEvent,huc)
  df_final <- inner_join(df_slim, aquastream, by = c("SiteID" ="SiteID","date" = "date"))
  return(df_final)
}

SNiP.Aquasat.join.fxn.simple <- function(df){
  df_slim <- df %>%
    dplyr::select(date=ActivityStartDateTime,
                  lat=LatitudeMeasure,
                  long=LongitudeMeasure,
                  huc=HUCEightDigitCode,
                  SiteID=MonitoringLocationIdentifier) %>%
    dplyr::mutate(lat = as.character(lat),
                  long = as.character(long)) %>%
    dplyr::mutate(date = as.character(format(as_datetime(date), '%Y-%m-%d'))) #%>%
  
  aquastream <- aquastream %>%
    dplyr::mutate(lat = as.character(lat),
                  long = as.character(long))
  df_final <- inner_join(df_slim, aquastream, by = c("SiteID" = "SiteID", "date" = "date")) %>%
    distinct(SiteID, date, .keep_all = TRUE)
  return(df_final)
}


#Create a cleaned dataset (option to harmonize with Aquasat)
df_S <- df %>% 
  wqp.tss.renamer() %>%
  nonsense.methods.fxn() %>%
  unit.harmony.fxn() %>%
  depth.harmony.fxn() %>%
  filter(!is.na(value)) %>%#Must be somewhat detectable
  mutate(value = as.numeric(value)) %>%
  filter(value > 0.01) %>%
  filter(value < 10000) %>%
  date.format.fxn() #%>%
  #datum.format.fxn() %>%
  #dplyr::select(-lat.x, -long.x) %>%
  #dplyr::rename(lat = lat.y, long=long.y)

names(df_S)

#subset <- df_S %>%
#          filter(units == 'mg/l') #%>%
          #mutate(value = as.numeric(value)) #%>%
          #filter(value > 0.01) %>%
          #filter(value < 1000) %>%
          #filter(!is.na(value)) #Must be somewhat detectable

#summary(subset$value)
hist(df_S$value, breaks = 30)

length(unique(df_S$SiteID))
nrow(df_S)

#rm(tss,tss.depth,tss.filtered,tss.lookup,tss.p,non.sensical.tss.methods,depth.lookup)

df_aquaSNiP <- df %>%   
  SNiP.Aquasat.join.fxn.simple() #%>%
  #rename(SiteID = SiteID.x, time = time.x) %>% 
  #select(-SiteID.y, -time.y)
nrow(df_aquaSNiP)
print(unique(df_aquaSNiP$SiteID))

df_aquaSNiP_5 <- df_aquaSNiP %>%
  group_by(SiteID) %>%
  filter(n() >= 5) 
print(df_aquaSNiP_5)

distinct_df_aquaSNiP <- df_aquaSNiP %>%
  dplyr::select(-lat.y, -long.y) %>%
  dplyr::rename(lat = lat.x, long=long.x) %>%
  distinct(SiteID, lat, long)

aquaCheck <- inner_join(distinct_df_aquaSNiP, aquastream, by = c("SiteID" = "SiteID")) %>%
  distinct(SiteID, date, .keep_all = TRUE)

distinct_df_aquaSNiP_5 <- aquaCheck %>%
  dplyr::select(-lat.y, -long.y) %>%
  dplyr::rename(lat = lat.x, long=long.x) %>%
  group_by(SiteID) %>%
  filter(n() >= 5)  %>%
  distinct(SiteID, lat, long)
distinct_df_aquaSNiP_5

mapDistinct5 <- distinct_df_aquaSNiP_5 %>%
  dplyr::mutate(lat = as.numeric(lat),
              long = as.numeric(long))
mapview(mapDistinct5, xcol = "long", ycol = "lat", crs = 4269, grid = FALSE)

df_notAquasat <- anti_join(df_S, df_aquaSNiP, by = c("SiteID" ="SiteID", "date" = "date"))

distinct_df_notAquasat <- df_notAquasat  %>% 
  filter(units == 'mg/l')%>%
  group_by(SiteID) %>%
  filter(n() >= 5) %>%
  distinct(SiteID, lat, long)

df_aquaMissed <- semi_join(df_notAquasat, df_aquaSNiP, by = c("SiteID"))

print(unique(df_aquaMissed$SiteID))
print(unique(df_notAquasat$SiteID))

df_check <- inner_join(df_aquaMissed, aquastream, by = c("SiteID", "date"))

#Harmonize with Aquasat to get all sites that are Landsat visible
#and 5 or more observations per site
df_S_aquaSNiP <- df_S %>%   
  SNiP.Aquasat.join.fxn()%>%
  dplyr::select(-lat.y, -long.y, -time.y) %>%
  dplyr::rename(lat = lat.x, long=long.x, time = time.x)

nrow(df_S_aquaSNiP)
print(unique(df_S_aquaSNiP$SiteID))

df_S_aquaSNiP_5 <- df_S_aquaSNiP %>%
  filter(units == 'mg/l')%>%
  group_by(SiteID) %>%
  filter(n() >= 5) 

distinct_df_S_aquaSNiP_5 <- df_S_aquaSNiP_5 %>%
  distinct(SiteID, lat, long)
print(unique(df_S_aquaSNiP_5$SiteID))


mapview(distinct_df_aquaSNiP_5 %>%
          dplyr::mutate(lat = as.numeric(lat),
                        long = as.numeric(long)), 
        xcol = "long", ycol = "lat", crs = 4269, grid = FALSE)


length(unique(df_S_aquaSNiP_5$SiteID))
table(df_S_aquaSNiP_5$SiteID)
nrow(df_S_aquaSNiP_5)


#Find out how many addnl samples are available at aquasat siteIDs
df_S_notAquasat <- anti_join(df_S, df_aquaSNiP, by = c("SiteID" ="SiteID", "date" = "date"))
print(unique(df_S_notAquasat$SiteID))

df_S_aquaMissed <- inner_join(df_S_notAquasat, df_S_aquaSNiP, by = c("SiteID"))
print(unique(df_S_aquaMissed$SiteID))

print(unique(df_S_aquaSNiP$SiteID))

distinct_df_S_notAquasat <- df_S_notAquasat  %>% 
  filter(units == 'mg/l')%>%
  group_by(SiteID) %>%
  filter(n() >= 5) %>%
  distinct(SiteID)


length(unique(df_S_notAquasat$SiteID))
nrow(df_S_notAquasat)
#head(df_S_notAquasat)

##write out any needed files
aquasat_hucX_tss <- write.csv(df_S_aquaSNiP, file = paste0('~/Documents/SNiP/DataforFusion/aquasat_',hucname,'_tss.csv'), row.names = FALSE)
wqp_hucX_aquaMissed_tss <- write.csv(df_S_aquaMissed, file = paste0('~/Documents/SNiP/DataforFusion/wqp_', hucname,'_aquaMissed_tss.csv'), row.names = FALSE)
wqp_hucX_tss_clean <- write.csv(df_S, file = paste0('~/Documents/SNiP/DataforFusion/wqp_', hucname,'_tss_clean.csv'), row.names = FALSE)
wqp_hucX_notAquasat_tss <- write.csv(df_S_notAquasat, file = paste0('~/Documents/SNiP/DataforFusion/wqp_', hucname,'_notAquasat_tss.csv'), row.names = FALSE)

aquasat_hucX_tss_siteLatLongRaw <- write.csv(distinct_df_aquaSNiP, file = paste0('~/Documents/SNiP/DataforFusion/aquasat_',hucname,'_sitelatlong.csv'), row.names = FALSE)
wqp_hucX_tss_siteLatLong <- write.csv(distinct_df_S_notAquasat, file = paste0('~/Documents/SNiP/DataforFusion/wqp_',hucname,'_notAquasat_sitelatlong.csv'), row.names = FALSE)
aquasat_hucX_tss_siteLatLong <- write.csv(distinct_df_aquaSNiP_5, file = paste0('~/Documents/SNiP/DataforFusion/aquasat_',hucname,'_5sitelatlong.csv'), row.names = FALSE)



