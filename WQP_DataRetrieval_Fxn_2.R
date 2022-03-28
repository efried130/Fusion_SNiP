#WQP data retrieval
#Updated to match dataretrieval package update
#Updated with TryCatch() to automatically skip empty basins without breaking fxn

HUCs01 <- c('0101*','0102*','0103*','0104*','0105*','0106*','0107*','0108*','0109*','0110*','0111*') #HUC 0111 is empty
HUCs02 <- c('0201*','0202*','0203*','0204*','0205*','0206*','0207*') #0201 is empty
HUCs03 <- c('0301*','0302*','0303*','0304*','0305*','0306*','0307*','0308*','0309*','0310*','0311*','0312*','0313*','0314*','0315*','0316*','0317*','0318*')
HUCs04 <- c('0401*','0402*','0403*','0404*','0405*','0406*','0407*','0408*','0409*','0410*','0411*','0412*','0413*','0414*','0415*')
HUCs09 <- c('0901*','0902*','0903*')
HUCs12 <- c('1201*','1202*','1203*','1204*','1205*','1206*','1207*','1208*','1209*','1210*','1211*') #1202* likely doesn't have any data
HUCs13 <- c('1301*','1302*','1303*','1304*','1305*','1306*','1307*','1308*')
HUCs14 <- c('1401*','1402*','1403*','1404*','1405*','1406*','1407*','1408*')
HUCs15 <- c('1501*','1502*','1503*','1504*','1505*','1506*','1507*','1508*') #1504* and 1507* likely have no data
HUCs16 <- c('1601*','1602*','1603*','1604*','1605*','1606*')
HUCs17 <- c('1701*','1702*','1703*','1704*','1705*','1706*','1707*','1708*','1709*','1710*','1711*','1712*')
HUCs18 <- c('1801*','1802*','1803*','1804*','1805*','1806*','1807*','1808*','1809*','1810*')
HUCs19 <- c('1901*','1902*','1903*','1904*','1905*','1906*')
HUCs20 <- c('2001*','2002*','2003*','2004*','2005*','2006*','2007*','2008*','2009*')
HUCs21 <- c('2101*', '2102*','2103*') #No data for ('2103*')

#MS Basin
HUCs05 <- c('0501*','0502*','0503*','0504*','0505*','0506*','0507*','0508*','0509*','0510*','0511*','0512*','0513*','0514*')
HUCs06 <- c('0601*','0602*','0603*','0604*')
HUCs07 <- c('0701*','0702*','0703*','0704*','0705*','0706*','0707*')
HUCs08 <- c('0801*','0802*','0803*','0804*','0805*','0806*','0807*','0808*','0809*')
#Note USGS has no sites for 1001*
HUCs10 <- c('1002*','1003*','1004*','1005*','1006*','1007*','1008*','1009*','1010*','1011*','1012*','1013*','1014*','1015*','1016*','1017*','1018*','1019*','1020*','1021*','1022*','1023*','1024*','1025*','1026*','1027*','1028*','1029*','1030*')
HUCs11 <- c('1101*','1102*','1103*','1104*','1105*','1106*','1107*','1108*','1109*','1110*','1111*','1112*','1113*','1114*')


#Define function to pull WQP data by sites (sorted by HUC)
extract_WQP_data = function(HUC) {
  library(iterators)
  library(lubridate)
  library(ellipsis)
  library(desc)
  library(withr)
  library(ps)
  library(backports)
  library(rstudioapi)
  library(usethis)
  library(curl,lib.loc)
  library(devtools)
  library(dataRetrieval)  #package for water quality portal
  library(foreach)
  library(doParallel)
  library(readr)
  library(dplyr)
  library(tidyr)

  #Define fxn in tryCatch  
  out <- tryCatch({
    #Define constituent parameters (CharacteristicName) needed to grab from multiple databases
    SNiP_Forms <- c("Sediment", "Turbidity", "Total suspended solids", "Suspended sediment concentration (SSC)", "Suspended Sediment Concentration (SSC)", "Nitrogen","Nitrate", "Inorganic nitrogen (nitrate and nitrite)", "Inorganic nitrogen (nitrate and nitrite) as N", "Nitrate as N", "Nitrate-N", "Nitrate-nitrogen", "Nitrate-Nitrogen", "Total Particulate Nitrogen", "Total Nitrogen, mixed forms", "Phosphorus", "Phosphorus as P", "Orthophosphate", "Orthophosphate as P", "Orthophosphate as PO4", "Ortho-Phosphate-Phosphorus")
    #Optional for single constituent pull
    S_Forms <- c("Sediment", "Turbidity", "Total suspended solids", "Suspended sediment concentration (SSC)", "Suspended Sediment Concentration (SSC)")
    N_Forms <- c("Nitrogen","Nitrate", "Inorganic nitrogen (nitrate and nitrite)", "Inorganic nitrogen (nitrate and nitrite) as N", "Nitrate as N", "Nitrate-N", "Nitrate-nitrogen", "Nitrate-Nitrogen", "Total Particulate Nitrogen", "Total Nitrogen, mixed forms")
    P_Forms <- c("Phosphorus", "Phosphorus as P", "Orthophosphate", "Orthophosphate as P", "Orthophosphate as PO4", "Ortho-Phosphate-Phosphorus" )
    
    #Extract site data by HUC with selected columns
    data_resultFiltered <- whatWQPdata(huc = HUC,
                                       siteType = "Stream",
                                       characteristicName = SNiP_Forms,
                                       startDate="1984-01-01",
                                       endDate="2021-10-01") %>%
      dplyr::select(MonitoringLocationIdentifier, resultCount, siteUrl, StateName, CountyName)
    #Filter data by number of results per site
    data_filtered <- data_resultFiltered %>% 
      dplyr::filter(resultCount >= 2)
    #Name filtered data to a new variable to call result data
    sites <- data_filtered$MonitoringLocationIdentifier
    
    data_sites <- whatWQPsites(siteNumbers = sites,
                               siteType = "Stream",
                               characteristicName = SNiP_Forms,
                               startDate="1984-01-01",
                               endDate="2021-10-01") %>%
      dplyr::select(MonitoringLocationIdentifier, HUCEightDigitCode, DrainageAreaMeasure.MeasureValue, DrainageAreaMeasure.MeasureUnitCode, ContributingDrainageAreaMeasure.MeasureValue, ContributingDrainageAreaMeasure.MeasureUnitCode, VerticalMeasure.MeasureValue, VerticalMeasure.MeasureUnitCode, VerticalAccuracyMeasure.MeasureValue, VerticalAccuracyMeasure.MeasureUnitCode, VerticalCollectionMethodName, LatitudeMeasure, LongitudeMeasure, HorizontalCoordinateReferenceSystemDatumName)
    
    #Call actual data by filtered sites
    data_retrieval <- readWQPdata(siteNumbers = sites,
                                  siteType = "Stream",
                                  characteristicName = SNiP_Forms,
                                  startDate="1984-01-01", 
                                  endDate="2021-10-01",
                                  tz = 'UTC')%>%
      dplyr::select(OrganizationIdentifier, OrganizationFormalName, ActivityDepthHeightMeasure.MeasureValue, ActivityDepthHeightMeasure.MeasureUnitCode, ActivityIdentifier, ActivityMediaName, ActivityStartDate, ActivityStartDateTime, ActivityStartTime.Time, ActivityStartTime.TimeZoneCode, MonitoringLocationIdentifier, HydrologicCondition, HydrologicEvent, CharacteristicName, ResultAnalyticalMethod.MethodName, ResultParticleSizeBasisText, ResultSampleFractionText, ResultMeasureValue, ResultMeasure.MeasureUnitCode, ResultParticleSizeBasisText, ResultSampleFractionText, ResultStatusIdentifier, SampleCollectionMethod.MethodName, USGSPCode)%>%
      dplyr::filter(!is.na(ResultMeasureValue))%>%
      dplyr::mutate_if(is.double, as.numeric) #Otherwise some results are characters, others doubles
    
    #Merge location data and results
    All_WQP_Data <- data_retrieval %>%
      left_join(data_filtered, by ='MonitoringLocationIdentifier') %>%
      left_join(data_sites, by='MonitoringLocationIdentifier')
    
  },
  
  #Handle an error
  error = function(e) {
    message(paste('Reading a HUC caused an error:', HUC))
    message('Here is the original error message:')
    message(e)
    return(NA)
  },
  
  #Handle a warning
  warning = function(cond) {
    message(paste('Reading a HUC caused a warning:', HUC))
    message('Here is the original warning message:')
    message(cond)
    return(NA)
  },
  
  #Define what should happen after
  finally = {
    message(paste('Processed HUC:', HUC))
  }
  
  
  
  )
  return(out) 
}

#Loop over each HUC sub basin
WQPdata_list = lapply(HUCs21, extract_WQP_data)

#Create a dataframe and weed out null locations or sub-basins
WQPdata_frame <- as.data.frame(do.call(rbind, WQPdata_list)) %>%
  dplyr::filter(!is.na(MonitoringLocationIdentifier))

#Save as RDS to cluster
WQPdata <- saveRDS(WQPdata_frame, file = "/Users/Ellie/Documents/UMass/SNiP/WQPdataraw_SNiP_HUC21_2.rds")




# #Create parallel workflow in the cluster with correct # of nodes specified
# cl=makeCluster(2)
# 
# WQPdata_list=parLapply(cl=cl, HUCs02, extract_WQP_data) #Loop over the HUCs of interest to create a list
# #WQPdata_frame <- bind_rows(WQPdata_list) #Transform to df
# 
# #If multiple data types, try this instead:
# WQPdata_frame <- as.data.frame(do.call(rbind, WQPdata_list)) %>%
#   dplyr::filter(!is.na(MonitoringLocationIdentifier))
# 
# #Save to cluster
# WQPdata <- saveRDS(WQPdata_frame, file = "/Users/Ellie/Documents/UMass/SNiP/WQPdata_SNiP_HUC02.rds")
# stopCluster(cl)
# 

