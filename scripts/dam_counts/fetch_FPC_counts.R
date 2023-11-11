# Function downloads from the Fish Passage Center (FPC) all fish passage data from Columbia and Snake River dams 
# Written by Tim Sippel - Washington Department of Fish and Wildlife

fetch_FPC_counts<-function(){
  if (!"tidyverse" %in% installed.packages()) install.packages("tidyverse")
  if (!"lubridate" %in% installed.packages()) install.packages("lubridate")
  library("tidyverse")
  library("lubridate")
  
# Download current year and historical data ####
  url <- "ftp://CRITFCOne:Adult21Count@ftp.fpc.org/"
  dwnload <- compose(partial(read_csv,progress=FALSE,col_types=cols()), partial(paste0, url))
  historical_data <- dwnload("AdultsHistorical.txt")
  current_data <- dwnload("AdultsforCurrentYear.txt")
  
# Set up standardized data headers ####
  headers.fpc<- c("dam", "datadate", "chin_adult", "chin_jack", "coho_adult", "coho_jack", "sockeye", "steelhead",
                  "wildsthead", "pink", "shad", "chum", "minijack", "lamprey", "chin_sp_ad", "chin_sp_jc",
                  "chin_su_ad", "chin_su_jc", "chin_fa_ad", "chin_fa_jc", "update_date", "source",  "chin_adult_t",
                  "chin_jack_t", "coho_adult_t", "sockeye_t", "steelhead_t", "minijack_t", "update",  "btrout")  

# Combined data frame for current year and historical data ####    
  # use same headers for both files
  names(current_data)<-headers.fpc
  # append both files together and only keep needed columns and ignore first 2 rows
  all.data<-as_tibble(rbind(historical_data, current_data)) %>% 
    slice(3:n()) %>% 
    select("dam", "datadate", "chin_adult", "chin_jack", "coho_adult", "coho_jack", "sockeye", "steelhead",  
           "wildsthead", "pink", "shad","chum", "minijack", "lamprey", "update_date", "source", "update", "btrout")
  
# Format date/time  columns, reorder and remove redundant columns ####
  all.data <- all.data %>% 
    mutate(Dam=dam, DataDate=datadate, Year=year(datadate), Month=month(datadate), Day=day(datadate),  
           JulianDay=as.POSIXlt(paste(as.character(Day), as.character(Month), as.character(Year), sep="-"), format="%d-%m-%y")$yday + 1) %>% 
    relocate(c(Dam, DataDate, Year, Month, Day, JulianDay), .after=datadate) %>% 
    select(-dam, -update_date, -source, -update, -datadate)

# Transform wide data format to long data format ####
  x <- all.data %>% 
    pivot_longer(cols=c("coho_adult", "coho_jack", "sockeye", "steelhead", "wildsthead", "pink", "shad", "chum", "minijack", 
                        "lamprey", "chin_adult", "chin_jack", "btrout"), values_to = "Count") %>% 
    mutate('Species'=name, 'LifeStage'=name, 'ClipStatus'=name) 

# Extract Species, LifeStage and Clip Status information to appropriate columns and finish formating    
  x <- x %>% 
    mutate(Species = case_when(Species == "coho_adult" ~ "Coho",
                               Species == "coho_jack" ~ "Coho",
                               Species == "sockeye" ~ "Sockeye",
                               Species == "steelhead" ~ "Steelhead",
                               Species == "wildsthead" ~ "Steelhead",
                               Species == "pink" ~ "Pink",
                               Species == "shad" ~ "shad",
                               Species == "chum" ~ "Chum",
                               Species == "minijack" ~ "Chinook",
                               Species == "lamprey" ~ "Lamprey",
                               Species == "chin_adult" ~ "Chinook",
                               Species == "chin_jack" ~ "Chinook",
                               Species == "btrout" ~ "Bull trout"),
           LifeStage = case_when(LifeStage == "coho_adult" ~ "Adult",
                               LifeStage == "coho_jack" ~ "Jack",
                               LifeStage == "sockeye" ~ "Adult",
                               LifeStage == "steelhead" ~ "Adult",
                               LifeStage == "wildsthead" ~ "Adult",
                               LifeStage == "pink" ~ "NA",
                               LifeStage == "shad" ~ "NA",
                               LifeStage == "chum" ~ "NA",
                               LifeStage == "minijack" ~ "Minijack",
                               LifeStage == "lamprey" ~ "NA",
                               LifeStage == "chin_adult" ~ "Adult",
                               LifeStage == "chin_jack" ~ "Jack",
                               LifeStage == "btrout" ~ "NA"),
           ClipStatus = case_when(ClipStatus == "coho_adult" ~ "Total",
                               ClipStatus == "coho_jack" ~ "Total",
                               ClipStatus == "sockeye" ~ "Total",
                               ClipStatus == "steelhead" ~ "Total",
                               ClipStatus == "wildsthead" ~ "Unclipped",
                               ClipStatus == "pink" ~ "Total",
                               ClipStatus == "shad" ~ "Total",
                               ClipStatus == "chum" ~ "Total",
                               ClipStatus == "minijack" ~ "Total",
                               ClipStatus == "lamprey" ~ "Total",
                               ClipStatus == "chin_adult" ~ "Total",
                               ClipStatus == "chin_jack" ~ "Total",
                               ClipStatus == "btrout" ~ "Total")) %>% 
    select(-name) %>% 
    relocate(Count, .after = 'ClipStatus')
}