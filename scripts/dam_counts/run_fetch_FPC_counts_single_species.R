# Script loads and runs function "fetch_FPC_counts.R" and sets up example data filters ####
# Written by Tim Sippel - Washington Department of Fish and Wildlife

# Converted to a function so we can call specific species; Brian Burke, NOAA Fisheries

fetch_FPC_counts_single_species<-function(my_species="spCK", my_age="adult") {
  
  if (!my_species %in% c("spCK","faCK","steel","sock")) {
    return(cat("Please call this function with one of the following: \"spCK\",\"faCK\",\"steel\",\"sock\""))
  }
  if (!my_age %in% c("adult","jack")) {
    return(cat("Please call this function with one of the following: \"spCK\",\"faCK\",\"steel\",\"sock\""))
  }
  
  # Load function "fetch_FPC_counts.R" ####
  source("scripts/dam_counts/fetch_FPC_counts.R") # change to directory where function "fetch_FPC_counts.R" lives
  
  # Run fetch_FPC_counts.R and store in object
  dam_counts<-fetch_FPC_counts()
  
  # Set up data selection for dams and spring julian day ranges at dams and define years to filter by ####
  dam_names<-c("BON", "MCN", "IHR", "LMN", "LGR")
  spring_julian_days<-list(BON=74:151, MCN=121:159, IHR=121:162, LMN=121:164, LGR=121:166)
  fall_julian_days<-list(BON=213:319, MCN=221:304, IHR=224:349, LMN=226:304, LGR=230:349)
  steel_julian_days<-list(BON=1:364, MCN=1:364, IHR=1:364, LMN=1:364, LGR=1:364)
  sock_julian_days<-list(BON=1:364, MCN=1:364, IHR=1:364, LMN=1:364, LGR=1:364)
  Year_start<-1981
  Year_end<-2023
  
  if (my_species=="spCK" & my_age=="adult") {
    # Filter object dam_counts by parameters defined above ####
    spCK_adult_annual <- 
      map2(dam_names, spring_julian_days, ~
             filter(dam_counts, Year %in% Year_start:Year_end, JulianDay %in% ..2, Dam == ..1, 
                    Species == "Chinook", LifeStage == "Adult") %>%
             group_by(Year) %>%
             summarize('{..1}' := sum(Count)) %>% 
             select(-Year)) %>% 
      set_names(dam_names) %>% 
      as_tibble() %>% 
      mutate(Year=Year_start:Year_end, .before=everything())
  }
  if (my_species=="spCK" & my_age=="jack") {
    # Filter object dam_counts by parameters defined above ####
    spCK_jack_annual <- 
      map2(dam_names, spring_julian_days, ~
             filter(dam_counts, Year %in% Year_start:Year_end, JulianDay %in% ..2, Dam == ..1, 
                    Species == "Chinook", LifeStage == "Jack") %>%
             group_by(Year) %>%
             summarize('{..1}' := sum(Count)) %>% 
             select(-Year)) %>% 
      set_names(dam_names) %>% 
      as_tibble() %>% 
      mutate(Year=Year_start:Year_end, .before=everything())
  }
  
  if (my_species=="faCK" & my_age=="adult") {
    # Filter object dam_counts by parameters defined above ####
    faCK_adult_annual <- 
      map2(dam_names, fall_julian_days, ~
             filter(dam_counts, Year %in% Year_start:Year_end, JulianDay %in% ..2, Dam == ..1, 
                    Species == "Chinook", LifeStage == "Adult") %>%
             group_by(Year) %>%
             summarize('{..1}' := sum(Count)) %>% 
             select(-Year)) %>% 
      set_names(dam_names) %>% 
      as_tibble() %>% 
      mutate(Year=Year_start:Year_end, .before=everything())
  }
  if (my_species=="faCK" & my_age=="jack") {
    # Filter object dam_counts by parameters defined above ####
    faCK_jack_annual <- 
      map2(dam_names, fall_julian_days, ~
             filter(dam_counts, Year %in% Year_start:Year_end, JulianDay %in% ..2, Dam == ..1, 
                    Species == "Chinook", LifeStage == "Jack") %>%
             group_by(Year) %>%
             summarize('{..1}' := sum(Count)) %>% 
             select(-Year)) %>% 
      set_names(dam_names) %>% 
      as_tibble() %>% 
      mutate(Year=Year_start:Year_end, .before=everything())
  }
  
  if (my_species=="steel") {
    # Filter object dam_counts by parameters defined above ####
    steel_adult_annual <- 
      map2(dam_names, steel_julian_days, ~
             filter(dam_counts, Year %in% Year_start:Year_end, JulianDay %in% ..2, Dam == ..1, 
                    Species == "Steelhead", LifeStage == "Adult") %>%
             group_by(Year) %>%
             summarize('{..1}' := sum(Count)) %>% 
             select(-Year)) %>% 
      set_names(dam_names) %>% 
      as_tibble() %>% 
      mutate(Year=Year_start:Year_end, .before=everything())
  }
  
  if (my_species=="sock") {
    # Filter object dam_counts by parameters defined above ####
    sock_adult_annual <- 
      map2(dam_names, sock_julian_days, ~
             filter(dam_counts, Year %in% Year_start:Year_end, JulianDay %in% ..2, Dam == ..1, 
                    Species == "Sockeye", LifeStage == "Adult") %>%
             group_by(Year) %>%
             summarize('{..1}' := sum(Count)) %>% 
             select(-Year)) %>% 
      set_names(dam_names) %>% 
      as_tibble() %>% 
      mutate(Year=Year_start:Year_end, .before=everything())
  }
  if (my_species=="spCK" & my_age=="adult") return(spCK_adult_annual)
  if (my_species=="spCK" & my_age=="jack") return(spCK_jack_annual)
  if (my_species=="faCK" & my_age=="adult") return(faCK_adult_annual)
  if (my_species=="faCK" & my_age=="jack") return(faCK_jack_annual)
  if (my_species=="steel") return(steel_adult_annual)
  if (my_species=="sock") return(sock_adult_annual)
}

