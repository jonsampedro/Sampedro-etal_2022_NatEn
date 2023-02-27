# Load libraries
# --------
#.libPaths("C:/Users/samp699/Documents/R/lib-rfasst")

library(rgcam)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(tibble)
library(gcamdata)
library(rmap)
library(here)
# --------
# Extract queries from db using rgcam
# --------

setwd(here())

#conn <- localDBConn("./db", 'database_basexdb_Sampedro-etal_2022',migabble = FALSE)
#prj <- addScenario(conn,"Sampedro-etal_2022.dat",c("GCAM_SSP4", "GCAM_SSP5", "GCAM_SSP1", "GCAM_SSP2", "GCAM_SSP3"),"./data/queries_Sampedro-etal_2022.xml")
#prj <- addScenario(conn,"Sampedro-etal_2022.dat",c("GCAM_SSP1_2p6", "GCAM_SSP2_2p6", "GCAM_SSP3_2p6", "GCAM_SSP4_2p6", "GCAM_SSP5_2p6"),"./data/queries_Sampedro-etal_2022.xml")

#saveProject(prj,"Sampedro-etal_2022.dat")


# --------
prj <- loadProject("Sampedro-etal_2022.dat")
listScenarios(prj)
QUERY_LIST <- listQueries(prj)

selected_regions<-c("Africa_Eastern","China","EU-15","India","USA")

sel_gas<-c("BC","OC","HFCs","NOx","SO2","CO2")

#Palettes
my_pal<-c("gray20","gray50","#ad440c","#ef8e27","#d01c2a","darkorchid3","#507fab","deepskyblue1","#11d081", "#00931d")
my_pal_energy<-c("#00931d","gray20","thistle2","gold2","deepskyblue1","peachpuff2","#d01c2a","#11d081")
my_pal_scen<-c("darkorchid3","forestgreen")
my_pal_ssp<-c("forestgreen","dodgerblue3","darkgoldenrod3","firebrick3","black")



# =======================================================================
# =======================================================================

#-------------DATA PROCESSING-------------
# --------
# Income
gdppc<-getQuery(prj,"subregional income") %>%
  filter(grepl("resid_d",`gcam-consumer`)) %>%
  mutate(`gcam-consumer` = gsub("resid_d","D",`gcam-consumer`)) %>%
  filter(!grepl("2p6",scenario)) %>%
  mutate(narrative = gsub("GCAM_","",scenario)) %>%
  filter(year <= 2050)

#----------------------------------------
# Population
pop<- getQuery(prj,"subregional population") %>%
  mutate(scenario = gsub("_2p6","",scenario)) %>%
  separate(scenario,c("model","narrative")) %>%
  filter(grepl("resid",`gcam-consumer`)) %>%
  separate(`gcam-consumer`,c("adj","decile")) %>%
  select(-adj, -Units) %>%
  mutate(decile = gsub("d","D",decile)) %>%
  distinct() %>%
  rename(pop_thous = value)


#----------------------------------------
# GDP
gdp<- getQuery(prj,"GDP MER by region" ) %>%
  mutate(scenario = gsub("_2p6","",scenario)) %>%
  separate(scenario,c("model","narrative"))


#----------------------------------------

#----------------------------------------
# Floorspace

flsp<-getQuery(prj,"building floorspace") %>%
  mutate(scenario = gsub("_2p6","",scenario)) %>%
  separate(scenario,c("model","narrative")) %>%
  filter(grepl("resid",building)) %>%
  separate(building,c("adj","decile")) %>%
  select(-adj, -Units) %>%
  mutate(decile = gsub("d","D",decile)) %>%
  distinct() %>%
  rename(flsp_bm2 = value)

flsp_plot<-getQuery(prj,"building floorspace") %>%
  filter(region %in% selected_regions) %>%
  filter(year >= 2015, year <= 2050) %>%
  mutate(scenario = gsub("_2p6","-2p6",scenario)) %>%
  separate(scenario,c("model","narrative"),sep = "_") %>%
  mutate(scenario = if_else(grepl("2p6",narrative),"RCP2p6","Baseline")) %>%
  mutate(narrative = gsub("-2p6","",narrative)) %>%
  filter(grepl("resid",building)) %>%
  separate(building,c("adj","decile")) %>%
  select(-adj, -Units) %>%
  mutate(decile = gsub("d","D",decile)) %>%
  distinct() %>%
  rename(flsp_bm2 = value) %>%
  spread(narrative,flsp_bm2) %>%
  mutate(min = pmin(SSP1,SSP2,SSP3,SSP4,SSP5,na.rm = TRUE),
         max = pmax(SSP1,SSP2,SSP3,SSP4,SSP5,na.rm = TRUE)) %>%
  select(-SSP1,-SSP3,-SSP4,-SSP5) %>%
  rename(flsp_bm2 = SSP2)

flsp_plot_agg<-getQuery(prj,"building floorspace") %>%
  filter(region %in% selected_regions) %>%
  filter(year >= 2015, year <= 2050) %>%
  mutate(scenario = gsub("_2p6","-2p6",scenario)) %>%
  separate(scenario,c("model","narrative"),sep = "_") %>%
  mutate(scenario = if_else(grepl("2p6",narrative),"RCP2p6","Baseline")) %>%
  mutate(narrative = gsub("-2p6","",narrative)) %>%
  filter(grepl("resid",building)) %>%
  separate(building,c("adj","decile")) %>%
  select(-adj, -Units) %>%
  mutate(decile = gsub("d","D",decile)) %>%
  distinct() %>%
  rename(flsp_bm2 = value) %>%
  group_by(scenario, region, narrative, year) %>%
  summarise(flsp_bm2 = sum(flsp_bm2)) %>%
  ungroup() %>%
  spread(narrative,flsp_bm2) %>%
  mutate(min = pmin(SSP1,SSP2,SSP3,SSP4,SSP5,na.rm = TRUE),
         max = pmax(SSP1,SSP2,SSP3,SSP4,SSP5,na.rm = TRUE)) %>%
  select(-SSP1,-SSP3,-SSP4,-SSP5) %>%
  rename(flsp_bm2 = SSP2)

#----------------------------------------
# Service output

serv<-getQuery(prj,"building service output by tech (new)" ) %>%
  mutate(sector = gsub("others","non-thermal",sector)) %>%
  filter(grepl("resid",sector)) %>%
  separate(sector,c("sector","decile"),sep = "_") %>%
  mutate(decile = gsub("d","D",decile)) %>%
  select(-output) %>%
  filter(region %in% selected_regions) %>%
  filter(year >= 2015, year <= 2050) %>%
  mutate(scenario = gsub("_2p6","-2p6",scenario)) %>%
  separate(scenario,c("model","narrative"),sep = "_") %>%
  mutate(scenario = if_else(grepl("2p6",narrative),"RCP2p6","Baseline")) %>%
  mutate(narrative = gsub("-2p6","",narrative)) %>%
  mutate(sector = gsub("resid ","",sector)) %>%
  separate(sector, c("sector","type"), sep = " ") %>%
  select(scenario,narrative,region,sector,type,subsector,technology,decile,year,value,Units) %>%
  left_join_error_no_match(pop, by = c("narrative", "region", "decile", "year")) %>%
  mutate(value_pc = (value * 1E9) / (pop_thous * 1E3)) %>%
  mutate(unit_pc = "GJ/pers")


agg.serv<-serv %>%
  select(-value_pc, -pop_thous,-unit_pc) %>%
  group_by(scenario,narrative,region,sector,decile,year,Units) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  spread(narrative,value) %>%
  mutate(min = pmin(SSP1,SSP2,SSP3,SSP4,SSP5,na.rm = TRUE),
         max = pmax(SSP1,SSP2,SSP3,SSP4,SSP5,na.rm = TRUE)) %>%
  select(-SSP1,-SSP3,-SSP4,-SSP5) %>%
  rename(value = SSP2)

agg.serv.mod<-serv %>%
  filter(type == "modern") %>%
  select(-value_pc, -pop_thous,-unit_pc) %>%
  group_by(scenario,narrative,region,sector,decile,year,Units) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  spread(narrative,value) %>%
  mutate(min = pmin(SSP1,SSP2,SSP3,SSP4,SSP5,na.rm = TRUE),
         max = pmax(SSP1,SSP2,SSP3,SSP4,SSP5,na.rm = TRUE)) %>%
  select(-SSP1,-SSP3,-SSP4,-SSP5) %>%
  rename(value = SSP2)

agg.serv2<-serv %>%
  select(-value_pc, -pop_thous,-unit_pc) %>%
  group_by(scenario,narrative,region,sector,decile,year,Units) %>%
  summarise(value = sum(value)) %>%
  ungroup() 


tot.serv.type<-serv %>%
  select(-value_pc, -pop_thous,-unit_pc) %>%
  group_by(scenario,narrative,region,sector,type,year,Units) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  spread(narrative,value) %>%
  mutate(min = pmin(SSP1,SSP2,SSP3,SSP4,SSP5,na.rm = TRUE),
         max = pmax(SSP1,SSP2,SSP3,SSP4,SSP5,na.rm = TRUE)) %>%
  select(-SSP1,-SSP3,-SSP4,-SSP5) %>%
  rename(value = SSP2)

serv.reg.fuel<-serv %>%
  mutate(subsector = if_else(technology == "hydrogen", "hydrogen",subsector)) %>%
  group_by(scenario, region,narrative, sector, subsector, year, technology, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  spread(narrative,value) %>%
  mutate(min = pmin(SSP1,SSP2,SSP3,SSP4,SSP5,na.rm = TRUE),
         max = pmax(SSP1,SSP2,SSP3,SSP4,SSP5,na.rm = TRUE)) %>%
  select(-SSP1,-SSP3,-SSP4,-SSP5) %>%
  rename(value = SSP2)


serv.reg.fuel.agg<-serv %>%
  mutate(subsector = if_else(technology == "hydrogen", "hydrogen",subsector)) %>%
  group_by(scenario, region,narrative, sector, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  spread(narrative,value) %>%
  mutate(min = pmin(SSP1,SSP2,SSP3,SSP4,SSP5,na.rm = TRUE),
         max = pmax(SSP1,SSP2,SSP3,SSP4,SSP5,na.rm = TRUE)) %>%
  select(-SSP1,-SSP3,-SSP4,-SSP5) %>%
  rename(value = SSP2)


modern.palma.serv<- getQuery(prj,"building service output by tech (new)") %>%
  mutate(sector = gsub("others","non-thermal",sector)) %>%
  filter(grepl("resid",sector)) %>%
  separate(sector,c("sector","decile"),sep = "_") %>%
  mutate(decile = gsub("d","D",decile)) %>%
  select(-output) %>%
  filter(year >= 2015, year <= 2050) %>%
  mutate(scenario = gsub("_2p6","-2p6",scenario)) %>%
  separate(scenario,c("model","narrative"),sep = "_") %>%
  mutate(scenario = if_else(grepl("2p6",narrative),"RCP2p6","Baseline")) %>%
  mutate(narrative = gsub("-2p6","",narrative)) %>%
  mutate(sector = gsub("resid ","",sector)) %>%
  separate(sector, c("sector","type"), sep = " ") %>%
  select(scenario,narrative,region,sector,type,subsector,technology,decile,year,value,Units) %>%
  filter(grepl("modern",type)) %>%
  group_by(scenario,narrative,region,sector,decile,year,Units) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  #pivot_wider(decile,value)
  spread(decile,value) %>%
  replace_na(list(D1=0)) %>%
  replace_na(list(D2=0)) %>%
  replace_na(list(D3=0)) %>%
  replace_na(list(D4=0)) %>%
  replace_na(list(D5=0)) %>%
  mutate(tot = D1 + D2 + D3 + D4 + D5 + D6 + D7 + D8 + D9 + D10,
         CumShareD10 = D10 / tot,
         CumShareD4 = (D1 + D2 + D3 + D4) / tot,
         Palma = CumShareD10 / CumShareD4) %>%
  select(scenario,narrative,subRegion = region, class = sector, year, value = Palma)

modern.palma.serv.plot.pre<-modern.palma.serv %>%
  mutate(scenario = paste0(scenario,"--2050")) %>%
  mutate(scenario = if_else(year == 2015, "2015",scenario))

modern.palma.serv.plot<-modern.palma.serv.plot.pre %>%
  filter(scenario != 2015) %>%
  filter(year == 2050) %>%
  bind_rows(modern.palma.serv.plot.pre %>% filter(scenario == 2015)) %>%
  select(-year) %>%
  mutate(scenario = as.character(scenario))

modern.palma.serv.plot.sens<-modern.palma.serv.plot %>%
  mutate(scenario = gsub("--2050","",scenario),
         scenario = paste0(scenario, "_" ,narrative),
         scenario = if_else(grepl("2015",scenario),"2015",scenario)) %>%
  distinct(scenario, subRegion, class, value) %>%
  separate(scenario, c("scenario","narrative"), sep = "_") %>%
  mutate(narrative = if_else(scenario == "2015", scenario, narrative))


modern.gini.serv.datapre<- getQuery(prj,"building service output by tech (new)") %>%
  mutate(sector = gsub("others","non-thermal",sector)) %>%
  filter(grepl("resid",sector)) %>%
  separate(sector,c("sector","decile"),sep = "_") %>%
  mutate(decile = gsub("d","D",decile)) %>%
  select(-output) %>%
  filter(year >= 2015, year <= 2050) %>%
  mutate(scenario = gsub("_2p6","-2p6",scenario)) %>%
  separate(scenario,c("model","narrative"),sep = "_") %>%
  mutate(scenario = if_else(grepl("2p6",narrative),"RCP2p6","Baseline")) %>%
  mutate(narrative = gsub("-2p6","",narrative)) %>%
  mutate(sector = gsub("resid ","",sector)) %>%
  separate(sector, c("sector","type"), sep = " ") %>%
  select(scenario,narrative,region,sector,type,subsector,technology,decile,year,value,Units) %>%
  filter(grepl("modern",type)) %>%
  group_by(scenario,narrative,region,sector,decile,year,Units) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  group_by(scenario,narrative,region,sector,year,Units) %>%
  mutate(value_reg = sum(value)) %>%
  ungroup() %>%
  mutate(share = value / value_reg) %>%
  group_by(scenario,narrative,region,sector,year,Units) %>%
  mutate(share_check = sum(share)) %>%
  ungroup() %>%
  select(scenario, narrative, region, sector, category = decile, year, share)

modern.gini.serv<-pridr::compute_gini_deciles(modern.gini.serv.datapre %>% mutate(category = gsub("D", "d", category)) %>% mutate(Category = category),
                                              inc_col = "share" , 
                                              grouping_variables = c("scenario", "narrative", "region", "sector", "year")) %>%
  rename(gini = output_name) %>%
  select(scenario, narrative, region, sector, year, gini) %>%
  distinct() %>%
  select(scenario, narrative, subRegion = region, class = sector, year, value = gini)

modern.gini.serv.plot.pre<-modern.gini.serv %>%
  mutate(scenario = paste0(scenario,"--2050")) %>%
  mutate(scenario = if_else(year == 2015, "2015",scenario))

modern.gini.serv.plot<-modern.gini.serv.plot.pre %>%
  filter(scenario != 2015) %>%
  filter(year == 2050) %>%
  bind_rows(modern.gini.serv.plot.pre %>% filter(scenario == 2015)) %>%
  select(-year) %>%
  mutate(scenario = as.character(scenario))

modern.gini.serv.plot.sens<-modern.gini.serv.plot %>%
  mutate(scenario = gsub("--2050","",scenario),
         scenario = paste0(scenario, "_" ,narrative),
         scenario = if_else(grepl("2015",scenario),"2015",scenario)) %>%
  distinct(scenario, subRegion, class, value) %>%
  separate(scenario, c("scenario","narrative"), sep = "_") %>%
  mutate(narrative = if_else(scenario == "2015", scenario, narrative))


# Write a clean summary with inequality metrics (Palma ratios and Ginis)
energy.ineq.summary<-modern.palma.serv %>%
  rename(Palma_ratio = value,
         sector = class,
         region = subRegion) %>%
  left_join_error_no_match(modern.gini.serv %>%
                             rename(region = subRegion,
                                    sector = class), by = c("scenario", "narrative", "region", "sector", "year")) %>%
  write.csv("energy_ineq_summary.csv", row.names = F)
  

#----------------------------------------
# Emissions: Read non-CO2 emssions that don't work with rgcam
nonco2<-read.csv("./workflow/data/nonco2.csv") %>%
  separate(scenario, c("scenario","x"), sep = ",") %>%
  select(-x) %>%
  gather(year, value, -scenario, -region, -sector, -ghg, -unit) %>%
  mutate(year = gsub("X","",year)) %>%
  filter(region %in% selected_regions) %>%
  filter(year >= 2015, year <= 2050) %>%
  separate(ghg, c("ghg","adj"), sep = "_") %>%
  select(-adj) %>%
  mutate(ghg = if_else(grepl("SO2",ghg),"SO2",ghg),
         ghg = if_else(grepl("HFC",ghg),"HFCs",ghg)) %>%
  group_by(scenario, region, sector, ghg, year, unit) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(scenario = gsub("_2p6","-2p6",scenario)) %>%
  separate(scenario, c("model","narrative"),sep = "_") %>%
  mutate(scenario = if_else(grepl("2p6",narrative),"RCP2p6","Baseline")) %>%
  mutate(narrative = gsub("-2p6","",narrative)) %>%
  select(scenario, narrative, region, sector, ghg, year, value, unit) %>%
  filter(ghg %in% sel_gas) %>%
  mutate(value = if_else(unit == "Gg", value * 1E-3, value),
         unit  ="Tg")

resid.nonco2<- nonco2 %>%
  filter(grepl("resid",sector)) %>%
  separate(sector, c("sector","decile"), sep = "_") %>%
  mutate(decile = gsub("d","D",decile)) %>%
  mutate(sector = gsub("resid ","",sector)) %>%
  separate(sector, c("sector","type"), sep = " ") %>%
  group_by(scenario, narrative, region, decile, ghg, year, unit) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(year = as.numeric(year)) %>%
  mutate(value = if_else(ghg != "CO2",value * 1E3, value),
         unit = if_else(ghg != "CO2","Gg", "MTC")) %>%
  spread(narrative, value) %>%
  mutate(min = pmin(SSP1,SSP2,SSP3,SSP4,SSP5,na.rm = TRUE),
         max = pmax(SSP1,SSP2,SSP3,SSP4,SSP5,na.rm = TRUE)) %>%
  select(-SSP1,-SSP3,-SSP4,-SSP5) %>%
  rename(value=SSP2)

resid.nonco2.reg<- nonco2 %>%
  filter(grepl("resid",sector)) %>%
  separate(sector, c("sector","decile"), sep = "_") %>%
  mutate(decile = gsub("d","D",decile)) %>%
  mutate(sector = gsub("resid ","",sector)) %>%
  separate(sector, c("sector","type"), sep = " ") %>%
  group_by(scenario, narrative, region, ghg, year, unit) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(year = as.numeric(year)) %>%
  mutate(value = if_else(ghg != "CO2",value * 1E3, value),
         unit = if_else(ghg != "CO2","Gg", "MTC")) %>%
  spread(narrative, value) %>%
  mutate(min = pmin(SSP1,SSP2,SSP3,SSP4,SSP5,na.rm = TRUE),
         max = pmax(SSP1,SSP2,SSP3,SSP4,SSP5,na.rm = TRUE)) %>%
  select(-SSP1,-SSP3,-SSP4,-SSP5) %>%
  rename(value=SSP2)

resid.nonco2_pc<- nonco2 %>%
  filter(grepl("resid",sector)) %>%
  separate(sector, c("sector","decile"), sep = "_") %>%
  mutate(decile = gsub("d","D",decile)) %>%
  mutate(sector = gsub("resid ","",sector)) %>%
  separate(sector, c("sector","type"), sep = " ") %>%
  group_by(scenario, narrative, region, decile, ghg, year, unit) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(year = as.numeric(year)) %>%
  mutate(value = if_else(ghg != "CO2",value * 1E9, value * 1E6),
         unit = if_else(ghg != "CO2","kg", "TC")) %>%
  left_join_error_no_match(pop, by = c("narrative","region", "decile", "year")) %>%
  mutate(value_pc = value / pop_thous) %>%
  mutate(unit = if_else(ghg != "CO2","kg/thous_pers", "TC/thous_pers")) %>%
  select(-value, -pop_thous) %>%
  spread(narrative, value_pc) %>%
  mutate(min = pmin(SSP1,SSP2,SSP3,SSP4,SSP5,na.rm = TRUE),
         max = pmax(SSP1,SSP2,SSP3,SSP4,SSP5,na.rm = TRUE)) %>%
  select(-SSP1,-SSP3,-SSP4,-SSP5) %>%
  rename(value_pc=SSP2)

agg.nonco2<- nonco2 %>%
  group_by(scenario, narrative, region, ghg, year, unit) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  spread(narrative, value) %>%
  mutate(min = pmin(SSP1,SSP2,SSP3,SSP4,SSP5,na.rm = TRUE),
         max = pmax(SSP1,SSP2,SSP3,SSP4,SSP5,na.rm = TRUE)) %>%
  select(-SSP1,-SSP3,-SSP4,-SSP5) %>%
  rename(value=SSP2) %>%
  mutate(year = as.numeric(year))

#----------------------------------------
# Expenditures
price<-getQuery(prj, "prices of all markets") %>%
  mutate(region = if_else(grepl("Africa_Eastern",market),"Africa_Eastern","a"),
         region = if_else(grepl("Africa_Northern",market),"Africa_Northern",region),
         region = if_else(grepl("Africa_Southern",market),"Africa_Southern",region),
         region = if_else(grepl("Africa_Western",market),"Africa_Western",region),
         region = if_else(grepl("Australia_NZ",market),"Australia_NZ",region),
         region = if_else(grepl("USA",market),"USA",region),
         region = if_else(grepl("Brazil",market),"Brazil",region),
         region = if_else(grepl("Canada",market),"Canada",region),
         region = if_else(grepl("Central America and Caribbean",market),"Central America and Caribbean",region),
         region = if_else(grepl("Central Asia",market),"Central Asia",region),
         region = if_else(grepl("China",market),"China",region),
         region = if_else(grepl("EU-12",market),"EU-12",region),
         region = if_else(grepl("EU-15",market),"EU-15",region),
         region = if_else(grepl("Europe_Eastern",market),"Europe_Eastern",region),
         region = if_else(grepl("Europe_Non_EU",market),"Europe_Non_EU",region),
         region = if_else(grepl("European Free Trade Association",market),"European Free Trade Association",region),
         region = if_else(grepl("India",market),"India",region),
         region = if_else(grepl("Indonesia",market),"Indonesia",region),
         region = if_else(grepl("Japan",market),"Japan",region),
         region = if_else(grepl("Mexico",market),"Mexico",region),
         region = if_else(grepl("Middle East",market),"Middle East",region),
         region = if_else(grepl("Pakistan",market),"Pakistan",region),
         region = if_else(grepl("Russia",market),"Russia",region),
         region = if_else(grepl("South Africa",market),"South Africa",region),
         region = if_else(grepl("South America_Northern",market),"South America_Northern",region),
         region = if_else(grepl("South America_Southern",market),"South America_Southern",region),
         region = if_else(grepl("South Asia",market),"South Asia",region),
         region = if_else(grepl("South Korea",market),"South Korea",region),
         region = if_else(grepl("Southeast Asia",market),"Southeast Asia",region),
         region = if_else(grepl("Taiwan",market),"Taiwan",region),
         region = if_else(grepl("Colombia",market),"Colombia",region),
         region = if_else(grepl("Argentina",market),"Argentina",region),) %>%
  mutate(market = gsub("Africa_Eastern","",market),
         market = gsub("Africa_Northern","",market),
         market = gsub("Africa_Southern","",market),
         market = gsub("Africa_Western","",market),
         market = gsub("Australia_NZ","",market),
         market = gsub("USA","",market),
         market = gsub("Brazil","",market),
         market = gsub("Canada","",market),
         market = gsub("Central America and Caribbean","",market),
         market = gsub("Central Asia","",market),
         market = gsub("China","",market),
         market = gsub("EU-12","",market),
         market = gsub("EU-15","",market),
         market = gsub("Europe_Eastern","",market),
         market = gsub("Europe_Non_EU","",market),
         market = gsub("European Free Trade Association","",market),
         market = gsub("India","",market),
         market = gsub("Indonesia","",market),
         market = gsub("Japan","",market),
         market = gsub("Mexico","",market),
         market = gsub("Middle East","",market),
         market = gsub("Pakistan","",market),
         market = gsub("Russia","",market),
         market = gsub("South Africa","",market),
         market = gsub("South America_Northern","",market),
         market = gsub("South America_Southern","",market),
         market = gsub("South Asia","",market),
         market = gsub("South Korea","",market),
         market = gsub("Southeast Asia","",market),
         market = gsub("Taiwan","",market),
         market = gsub("Argentina","",market),
         market = gsub("Colombia","",market)) %>%
  mutate(scenario = gsub("_2p6","-2p6",scenario)) %>%
  separate(scenario,c("model","narrative"),sep = "_") %>%
  mutate(scenario = if_else(grepl("2p6",narrative),"RCP2p6","Baseline")) %>%
  mutate(narrative = gsub("-2p6","",narrative)) %>%
  rename(price = value)


expen<-getQuery(prj,"building final energy by service and fuel") %>%
  filter(grepl("resid",sector)) %>%
  separate(sector, c("sector","decile"), sep = "_") %>%
  mutate(decile = gsub("d","D",decile)) %>%
  filter(year >= 2015, year <= 2050) %>%
  mutate(scenario = gsub("_2p6","-2p6",scenario)) %>%
  separate(scenario,c("model","narrative"),sep = "_") %>%
  mutate(scenario = if_else(grepl("2p6",narrative),"RCP2p6","Baseline")) %>%
  mutate(narrative = gsub("-2p6","",narrative)) %>%
  group_by(scenario, narrative, region, decile, input, year) %>%
  summarise(value_ej = sum(value)) %>%
  ungroup() %>%
  rename(market = input) %>%
  left_join_error_no_match(price, by = c("scenario", "narrative", "year","market","region")) %>%
  mutate(expen_dolar = value_ej * 1E9 * price * gdp_deflator(2015, 1975)) %>%
  group_by(scenario, narrative, region, decile, year) %>%
  summarise(expen_dolar = sum(expen_dolar)) %>%
  ungroup() %>%
  filter(region %in% selected_regions) %>%
  left_join_error_no_match(pop, by = c("narrative", "region", "decile", "year")) %>%
  mutate(expen_pc = expen_dolar / (pop_thous * 1E3)) %>%
  left_join_error_no_match(gdppc %>% rename(decile = `gcam-consumer`) %>% select(-scenario),
                           by = c("region","year","decile","narrative")) %>%
  mutate(gdppc_dolar = value * 1E3,
         perc_exp_residEn = expen_pc / gdppc_dolar) %>%
  select(scenario, narrative, region, decile, year, perc_exp_residEn) %>%
  left_join_error_no_match(agg.serv2 %>%
                             group_by(scenario, region, decile, year, narrative) %>%
                             summarise(value = sum(value)) %>%
                             ungroup(),
                           by = c("scenario","region","decile","year","narrative")) %>%
  left_join_error_no_match(pop, by = c("narrative", "region", "decile", "year")) %>%
  left_join_error_no_match(flsp, by = c("narrative", "region", "decile", "year")) %>%
  mutate(flsp_m2 = flsp_bm2 * 1E9) %>%
  mutate(value_pc = (value * 1E9) / (pop_thous * 1E3))

# =======================================================================

# =======================================================================
# =======================================================================

#-------------FIGURES-------------
#-------------------------------------------
#-------------------------------------------
# Figure 1: Region-level service output by period, sector, and scenario
plot_serv_reg_agg<- ggplot(serv.reg.fuel.agg,
                           aes(x=year,
                               y=value,
                               color=scenario,
                               fill=scenario)) +
  geom_line(size=.5) +
  geom_point(size =.5) +
  geom_ribbon(aes(ymin=min, ymax=max),
              alpha=0.3, linetype = 0) +
  facet_grid(sector~region, scales = "free") +
  theme_classic() +
  labs(x="",y="Service output (EJ)")+
  theme(legend.title = element_blank(),
        axis.text.y = element_text(size=11),
        axis.text.x = element_text(size=9),
        axis.title = element_text(size=14),
        strip.text =  element_text(size=11),
        legend.position = "bottom",
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 11))+
  scale_color_manual(values = my_pal_scen) + 
  scale_fill_manual(values = my_pal_scen) 

plot_serv_reg_agg
ggsave("./figures/ServOut_reg_agg_scen.tiff",plot_serv_reg_agg,"tiff",dpi=200)

#-------------------------------------------
#-------------------------------------------
# Figure 2: Service output demand by region, period, scenario, narrative, service, and fuel
plot_tot_serv_fuel_base<- ggplot(serv.reg.fuel %>% filter(scenario == "Baseline"),
                            aes(x = year,
                                y = value,
                                color = subsector,
                                fill = subsector)) +
  geom_line() +
  geom_ribbon(data = serv.reg.fuel %>% filter(scenario == "Baseline"),
              aes(ymin=min, ymax=max),
              alpha=0.3, 
              linetype = 0) +
  #geom_vline(xintercept = 2015, linetype="dashed", colour="black") +
  facet_grid(sector~region, scales = "free") +
  theme_classic() +
  labs(x="",y="Service output (EJ)")+
  theme(legend.title = element_blank(),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=9),
        axis.title = element_text(size=13),
        strip.text =  element_text(size=12),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = my_pal_energy) + 
  scale_fill_manual(values = my_pal_energy) + 
  ggtitle("Baseline")


plot_tot_serv_fuel_base
ggsave("./figures/AggServOutFuel_Baseline.tiff",plot_tot_serv_fuel_base,"tiff",dpi=200)

plot_tot_serv_fuel_rcp<- ggplot(serv.reg.fuel %>% filter(scenario == "RCP2p6"),
                                 aes(x = year,
                                     y = value,
                                     color = subsector,
                                     fill = subsector)) +
  geom_line() +
  geom_ribbon(data = serv.reg.fuel %>% filter(scenario == "RCP2p6"),
              aes(ymin=min, ymax=max),
              alpha=0.3, 
              linetype = 0) +
  facet_grid(sector~region, scales = "free") +
  theme_classic() +
  labs(x="",y="Service output (EJ)")+
  theme(legend.title = element_blank(),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=9),
        axis.title = element_text(size=13),
        strip.text =  element_text(size=12),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = my_pal_energy) + 
  scale_fill_manual(values = my_pal_energy) + 
  ggtitle("RCP2p6")


plot_tot_serv_fuel_rcp
ggsave("./figures/AggServOutFuel_rcp.tiff",plot_tot_serv_fuel_rcp,"tiff",dpi=200)

#-------------------------------------------
#-------------------------------------------
# Figure 3: Service output demand by region, period, scenario, narrative, service, and decile
plot_agg_serv_ssp<- ggplot(agg.serv.mod %>% filter(scenario == "Baseline"),
                           aes(x=year,
                               y=value,
                               color=factor(decile,levels = c("D1","D2","D3","D4","D5","D6","D7","D8","D9","D10")),
                               fill=factor(decile,levels = c("D1","D2","D3","D4","D5","D6","D7","D8","D9","D10")))) +
  geom_line(size=.5) +
  geom_point(size =.5) +
  geom_ribbon(aes(ymin=min, ymax=max),
              alpha=0.3, linetype = 0) +
  facet_grid(sector~region, scales = "free") +
  theme_classic() +
  labs(x="",y="Service output (EJ)")+
  theme(legend.title = element_blank(),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=9),
        axis.title = element_text(size=13),
        strip.text =  element_text(size=11),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = my_pal) + 
  scale_fill_manual(values = my_pal) 

plot_agg_serv_ssp
ggsave("./figures/ServOut_ssp_baseline.tiff",plot_agg_serv_ssp,"tiff",dpi=200)

#-------------------------------------------
#-------------------------------------------
# Figure 5: Palma ratio in 2050 by scenario, region and service (narrative = SSP2)
m1<-rmap::map(data = modern.palma.serv.plot %>% 
            rename(sector = class,
                   policy = scenario) %>%
            filter(narrative == "SSP2") %>%
            mutate(subRegion = if_else(subRegion == "EU-12","EU_12",subRegion),
                   subRegion = if_else(subRegion == "EU-15","EU_15",subRegion)),
          row = "sector",
          col = "policy",
          folder ="./figures",
          legendType = "kmeans",
          background  = T,
          theme_classic(),
          title = "") 

map.fin<-m1$map_param_KMEANS + 
  ggplot2::theme(strip.text = element_text(size = 11),
                 legend.text = element_text(size=8))
  
ggplot2::ggsave("./figures/Palma_map_2050_ssp2.tiff",map.fin,"tiff",dpi=200)


#-------------------------------------------
#-------------------------------------------
# Figure 6: GHG and air pollutant emissions per period, region, narrative, and gas in the Baseline scenario
plot_resid_nonco2_base<- ggplot(resid.nonco2 %>% filter(scenario == "Baseline", ghg != "CH4"),
                           aes(x=year,
                               y=value,
                               color=factor(decile,levels = c("D1","D2","D3","D4","D5","D6","D7","D8","D9","D10")),
                               fill=factor(decile,levels = c("D1","D2","D3","D4","D5","D6","D7","D8","D9","D10")))) +
  geom_line(size=.5) +
  geom_point(size =.5) +
  geom_ribbon(aes(ymin=min, ymax=max),
              alpha=0.3, linetype = 0) +
  facet_grid(factor(ghg, levels = c("CO2", "HFCs", "BC", "NOx","OC","SO2"))~region, scales = "free") +
  theme_classic() +
  labs(x="",y="Emissions")+
  theme(legend.title = element_blank(),
        axis.text.y = element_text(size=7),
        axis.text.x = element_text(size=7),
        axis.title = element_text(size=11),
        strip.text.y =  element_text(size=10),
        legend.position = "bottom",
        plot.title = element_text(size = 12, face="bold"),
        plot.subtitle = element_text(size = 11))+
  scale_color_manual(values = my_pal) + 
  scale_fill_manual(values = my_pal)

plot_resid_nonco2_base
ggsave("./figures/Resid_Emissions_Baseline.tiff",plot_resid_nonco2_base,"tiff",dpi=200)

#-------------------------------------------
#-------------------------------------------
# Figure 7: Expenditures as percentage of income per scenario, narrative, region, and service in 2050
plot_expen_ssp<- ggplot(expen %>% filter(year %in% c(2050)),
                     aes(x = value_pc,
                         y = perc_exp_residEn,
                         color = factor(decile,levels = c("D1","D2","D3","D4","D5","D6","D7","D8","D9","D10")),
                         fill = factor(decile,levels = c("D1","D2","D3","D4","D5","D6","D7","D8","D9","D10")),
                         shape = scenario)) +
  geom_point(size = 2) +
  facet_grid(region~narrative) +
  theme_classic() +
  labs(x="Per capita service output (GJ/pers)", y="Share of energy expenditures (%)")+
  theme(legend.title = element_blank(),
        axis.text.y = element_text(size=9),
        axis.text.x = element_text(size=9),
        axis.title = element_text(size=11),
        strip.text.y =  element_text(size=8),
        legend.position = "bottom") +
  scale_fill_manual(values = my_pal) +
  scale_color_manual(values = my_pal) + 
  scale_shape_manual(values = c(19,1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))


plot_expen_ssp
ggsave("./figures/Expen2050_facetSSP.tiff",plot_expen_ssp,"tiff",dpi=200)

# =======================================================================
# =======================================================================

#-------------RESULTS SUPLLEMENT-------------
#-------------------------------------------
# SOCIOECONOMICS
#-------------------------------------------
#-------------------------------------------
#-------------------------------------------
# Regional GDP
plot_regGDP<-ggplot(gdp %>% filter(year >= 2015, year <= 2050, region %in% selected_regions) %>% mutate(value = value * gdp_deflator(2015,1990) *1E-6),
                    aes(x=year,y=value, color=narrative))+
  geom_line()+
  geom_point(size = 1)+
  facet_wrap(~region)+
  theme_classic()+
  labs(x="",y="$Trillion")+
  theme(legend.title = element_blank(),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title = element_text(size=12),
        strip.text =  element_text(size=11),
        legend.position = "bottom") +
  scale_color_manual(values = my_pal_ssp) 


plot_regGDP
ggsave("./figures/SM/regGDP.tiff",plot_regGDP,"tiff",dpi=200)

#-------------------------------------------
# Population
plot_pop<-ggplot(pop %>% filter(year >= 2015, year <= 2050 , region %in% selected_regions, decile == "D1") %>% mutate(pop = pop_thous * 1E3 * 1E-6),
                 aes(x=year,y=pop, color=narrative))+
  geom_line()+
  geom_point(size = 1)+
  facet_wrap(~region)+
  theme_classic()+
  labs(x="",y="Population (Million)")+
  theme(legend.title = element_blank(),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title = element_text(size=12),
        strip.text =  element_text(size=11),
        legend.position = "bottom") +
  scale_color_manual(values = my_pal_ssp) 


plot_pop
ggsave("./figures/SM/pop_decile.tiff",plot_pop,"tiff",dpi=200)

#-------------------------------------------
# Income shares
inc_share<- read.csv("./data/income_shares.csv") %>%
  filter(region %in% selected_regions,
         year %in% c(2015,2020,2030,2040,2050),
         model %in% c("Historical data", "PCA algorithm (Two Components)")) %>%
  mutate(category = gsub("d","D", category)) 

plot_inc_share<-ggplot(inc_share,
                 aes(x=factor(category,levels = c("D1","D2","D3","D4","D5",
                                                         "D6","D7","D8","D9","D10")),y=shares,color=sce))+
  geom_line() +
  geom_point(size = 1)+
  facet_grid(region~year) +
  theme_classic()+
  labs(x="",y="Income share (%)")+
  theme(legend.title = element_blank(),
        axis.text.y = element_text(size=9),
        axis.text.x = element_text(size=6),
        axis.title = element_text(size=11),
        strip.text.y =  element_text(size=8),
        legend.position = "bottom") +
  scale_color_manual(values = c("darkorchid3",my_pal_ssp))  


plot_inc_share
ggsave("./figures/SM/IncShares.tiff",plot_inc_share,"tiff",dpi=200)


#-------------------------------------------
# Per capita income
plot_inc<-ggplot(gdppc %>% filter(year >= 2015, region %in% selected_regions),
                 aes(x=year,y=value,color=factor(`gcam-consumer`,levels = c("D1","D2","D3","D4","D5",
                                                                            "D6","D7","D8","D9","D10"))))+
  geom_line()+
  geom_point(size = 1)+
  facet_grid(region~narrative, scales = "free")+
  theme_classic()+
  labs(x="",y="Per capita income (Thous$/pers)")+
  theme(legend.title = element_blank(),
        axis.text.y = element_text(size=9),
        axis.text.x = element_text(size=7),
        axis.title = element_text(size=11),
        strip.text.y =  element_text(size=8),
        legend.position = "bottom") +
  scale_color_manual(values = my_pal)  


plot_inc
ggsave("./figures/SM/GDPpc.tiff",plot_inc,"tiff",dpi=200)
#-------------------------------------------
#-------------------------------------------
# FLOORSPACE
#-------------------------------------------
# Residential floorspace per region, period, and decile in the Baseline scenario
plot_flsp_agg<- ggplot(flsp_plot_agg,
                       aes(x = year,
                           y = flsp_bm2,
                           color = scenario,
                           fill = scenario)) +
  geom_line(size=.5) +
  geom_point(size =.5) +
  geom_ribbon(aes(ymin=min, ymax=max),
              alpha=0.3, 
              linetype = 0) +
  facet_wrap(~region) +
  theme_classic() +
  labs(x="",y="Floorspace (BM2)")+
  theme(legend.title = element_blank(),
        axis.text.y = element_text(size=11),
        axis.text.x = element_text(size=11),
        axis.title = element_text(size=12),
        strip.text =  element_text(size=12),
        legend.position = "bottom",
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 11))+
  scale_color_manual(values = my_pal_scen) +
  scale_fill_manual(values = my_pal_scen) 

plot_flsp_agg
ggsave("./figures/SM/flsp_ssp_agg.tiff",plot_flsp_agg,"tiff",dpi=200)

#-------------------------------------------
# Residential floorspace per region, period, and decile in the Baseline scenario
plot_flsp_base<- ggplot(flsp_plot %>% filter(scenario == "Baseline"),
                   aes(x = year,
                       y = flsp_bm2,
                       color = factor(decile,levels = c("D1","D2","D3","D4","D5",
                                                        "D6","D7","D8","D9","D10")),
                       fill = factor(decile,levels = c("D1","D2","D3","D4","D5",
                                                       "D6","D7","D8","D9","D10")))) +
  geom_line(size=.5) +
  geom_point(size =.5) +
  geom_ribbon(aes(ymin=min, ymax=max),
              alpha=0.3, 
              linetype = 0) +
  facet_wrap(~region, scales = "free") +
  theme_classic() +
  labs(x="",y="Floorspace (BM2)")+
  theme(legend.title = element_blank(),
        axis.text.y = element_text(size=9),
        axis.text.x = element_text(size=9),
        axis.title = element_text(size=11),
        strip.text.y =  element_text(size=10),
        legend.position = "bottom",
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 11))+
  scale_color_manual(values = my_pal) +
  scale_fill_manual(values = my_pal) 

plot_flsp_base
ggsave("./figures/SM/flsp_ssp_base.tiff",plot_flsp_base,"tiff",dpi=200)

#-------------------------------------------
#-------------------------------------------
# SERVICE OUTPUT
#-------------------------------------------
# Service output by period, sector, scenario, and decile
plot_agg_serv_ssp_rcp<- ggplot(agg.serv %>% filter(scenario == "RCP2p6"),
                               aes(x=year,
                                   y=value,
                                   color=factor(decile,levels = c("D1","D2","D3","D4","D5","D6","D7","D8","D9","D10")),
                                   fill=factor(decile,levels = c("D1","D2","D3","D4","D5","D6","D7","D8","D9","D10")))) +
  geom_line(size=.5) +
  geom_point(size =.5) +
  geom_ribbon(aes(ymin=min, ymax=max),
              alpha=0.3, linetype = 0) +
  facet_grid(sector~region, scales = "free") +
  theme_classic() +
  labs(x="",y="Service output (EJ)")+
  theme(legend.title = element_blank(),
        axis.text.y = element_text(size=9),
        axis.text.x = element_text(size=8),
        axis.title = element_text(size=11),
        strip.text.y =  element_text(size=10),
        legend.position = "bottom",
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 11))+
  scale_color_manual(values = my_pal) + 
  scale_fill_manual(values = my_pal)

plot_agg_serv_ssp_rcp
ggsave("./figures/SM/ServOut_ssp_rcp.tiff",plot_agg_serv_ssp_rcp,"tiff",dpi=200)

#-------------------------------------------
# Per capita service output in 2050 per scenario, region, service, decile, and fuel in the SSP2 narrative
PcservList<- serv %>% filter(narrative == "SSP2")  %>%  mutate(subsector = if_else(technology == "hydrogen", "hydrogen",subsector))

serv_pol_list<-split(PcservList,PcservList$scenario)

plot_serv_pol<- function(df) {
  a<-ggplot(df %>% filter(year == 2050),
            aes(x=factor(decile,levels = c("D1","D2","D3","D4","D5","D6","D7","D8","D9","D10")),
                y=value_pc,
                fill=subsector)) +
    geom_bar(stat = "identity", position = "stack") +
    facet_grid(sector~region) +
    theme_classic() +
    labs(x="",y="Service output per capita (GJ/pers)")+
    theme(legend.title = element_blank(),
          axis.text.y = element_text(size=9),
          axis.text.x = element_text(size=7, angle = 90, vjust = .5),
          axis.title = element_text(size=11),
          strip.text.y =  element_text(size=10),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5))+
    scale_fill_manual(values = my_pal_energy) +
    scale_y_continuous(limits = c(0,40)) + 
    ggtitle(unique(df$scenario))
  
  ggsave(paste0("./figures/SM/SSP2_ServOutPc_susbsector_2050_",unique(df$scenario), ".tiff"),a,"tiff",dpi=200 )
  
}

lapply(serv_pol_list, plot_serv_pol)

#-------------------------------------------
# Palma ratio for alternative narratives --  Baseline
map_base<-rmap::map(data = modern.palma.serv.plot.sens %>% 
                rename(sector = class) %>%
                filter(scenario %in% c("2015","Baseline") ) %>%
                mutate(subRegion = if_else(subRegion == "EU-12","EU_12",subRegion),
                       subRegion = if_else(subRegion == "EU-15","EU_15",subRegion)) %>%
                select(-scenario),
              row = "sector",
              col = "narrative",
              folder ="./figures",
              legendType = "kmeans",
              background  = T,
              theme_classic(),
              title = "Baseline") 

map_base_fin<-map_base$map_param_KMEANS + 
  ggplot2::theme(strip.text = element_text(size = 9),
                 legend.text = element_text(size=7),
                 plot.title = element_text(hjust = 0.5))

ggplot2::ggsave("./figures/SM/Palma_map_2050_base.tiff",map_base_fin,"tiff",dpi=200)

#-------------------------------------------
# Palma ratio for alternative narratives --  RCP2.6
map_rcp<-rmap::map(data = modern.palma.serv.plot.sens %>% 
                      rename(sector = class) %>%
                      filter(scenario %in% c("2015","RCP2p6") ) %>%
                      mutate(subRegion = if_else(subRegion == "EU-12","EU_12",subRegion),
                             subRegion = if_else(subRegion == "EU-15","EU_15",subRegion)) %>%
                      select(-scenario),
                    row = "sector",
                    col = "narrative",
                    folder ="./figures",
                    background  = T,
                    theme_classic(),
                    title = "RCP2.6") 

map_rcp_fin<-map_rcp$map_param_KMEANS + 
  ggplot2::theme(strip.text = element_text(size = 9),
                 legend.text = element_text(size=7),
                 plot.title = element_text(hjust = 0.5))

ggplot2::ggsave("./figures/SM/Palma_map_2050_rcp.tiff",map_rcp_fin,"tiff",dpi=200)

#-------------------------------------------
# Gini in 2050 by scenario, region and service (narrative = SSP2)
m1<-rmap::map(data = modern.gini.serv.plot %>% 
                rename(sector = class,
                       policy = scenario) %>%
                filter(narrative == "SSP2") %>%
                mutate(subRegion = if_else(subRegion == "EU-12","EU_12",subRegion),
                       subRegion = if_else(subRegion == "EU-15","EU_15",subRegion)),
              row = "sector",
              col = "policy",
              folder ="./figures",
              legendType = "pretty",
              background  = T,
              theme_classic(),
              title = "") 

map.fin<-m1$map_param_PRETTY + 
  ggplot2::theme(strip.text.y = element_text(size = 9),
                 strip.text.x = element_text(size = 10),
                 legend.text = element_text(size=8))

ggplot2::ggsave("./figures/Gini_map_2050_ssp2.tiff",map.fin,"tiff",dpi=200)
#-------------------------------------------
# Gini for alternative narratives --  Baseline
map_base<-rmap::map(data = modern.gini.serv.plot.sens %>% 
                      rename(sector = class) %>%
                      filter(scenario %in% c("2015","Baseline") ) %>%
                      mutate(subRegion = if_else(subRegion == "EU-12","EU_12",subRegion),
                             subRegion = if_else(subRegion == "EU-15","EU_15",subRegion)) %>%
                      select(-scenario),
                    row = "sector",
                    col = "narrative",
                    folder ="./figures",
                    legendType = "pretty",
                    background  = T,
                    theme_classic(),
                    title = "Baseline") 

map_base_fin<-map_base$map_param_PRETTY + 
  ggplot2::theme(strip.text.y = element_text(size = 6),
                 strip.text.x = element_text(size = 8),
                 legend.text = element_text(size = 7),
                 plot.title = element_text(hjust = 0.5))

ggplot2::ggsave("./figures/SM/Gini_2050_base.tiff",map_base_fin,"tiff", width = 10, height = 5)

#-------------------------------------------
# Gini for alternative narratives --  RCP2.6
map_rcp<-rmap::map(data = modern.gini.serv.plot.sens %>% 
                     rename(sector = class) %>%
                     filter(scenario %in% c("2015","RCP2p6") ) %>%
                     mutate(subRegion = if_else(subRegion == "EU-12","EU_12",subRegion),
                            subRegion = if_else(subRegion == "EU-15","EU_15",subRegion)) %>%
                     select(-scenario),
                   row = "sector",
                   col = "narrative",
                   folder ="./figures",
                   background  = T,
                   legendType = "pretty",
                   theme_classic(),
                   title = "RCP2.6") 

map_rcp_fin<-map_rcp$map_param_PRETTY + 
  ggplot2::theme(strip.text.y = element_text(size = 6),
                 strip.text.x = element_text(size = 8),
                 legend.text = element_text(size = 7),
                 plot.title = element_text(hjust = 0.5))

ggplot2::ggsave("./figures/SM/Gini_map_2050_rcp.tiff",map_rcp_fin,"tiff", width = 10, height = 5)

#-------------------------------------------
#-------------------------------------------
# EMISSIONS
#-------------------------------------------
# Economywide aggregated emissions
plot_agg_nonco2<- ggplot(agg.nonco2,
                         aes(x=year,
                             y=value,
                             color = scenario,
                             fill = scenario)) +
  geom_line(size=.5) +
  geom_point(size =.5) +
  geom_ribbon(aes(ymin=min, ymax=max),
              alpha=0.2, linetype = 0) +
  facet_grid(ghg~region, scales = "free") +
  theme_classic() +
  labs(x="",y="Emissions")+
  theme(legend.title = element_blank(),
        axis.text.y = element_text(size=9),
        axis.text.x = element_text(size=7),
        axis.title = element_text(size=11),
        strip.text.y =  element_text(size=10),
        legend.position = "bottom")+ 
  scale_color_manual(values = my_pal_scen) + 
  scale_fill_manual(values = my_pal_scen) 

plot_agg_nonco2
ggsave("./figures/SM/AggNonCO2.tiff",plot_agg_nonco2,"tiff",dpi=200)

#-------------------------------------------
# Aggregated emissions from the residential sector
plot_agg_nonco2_resid<- ggplot(resid.nonco2.reg,
                         aes(x=year,
                             y=value,
                             color = scenario,
                             fill = scenario)) +
  geom_line(size=.5) +
  geom_point(size =.5) +
  geom_ribbon(aes(ymin=min, ymax=max),
              alpha=0.2, linetype = 0) +
  facet_grid(ghg~region, scales = "free") +
  theme_classic() +
  labs(x="",y="Emissions")+
  theme(legend.title = element_blank(),
        axis.text.y = element_text(size=9),
        axis.text.x = element_text(size=7),
        axis.title = element_text(size=11),
        strip.text.y =  element_text(size=10),
        legend.position = "bottom")+ 
  scale_color_manual(values = my_pal_scen) + 
  scale_fill_manual(values = my_pal_scen) 


plot_agg_nonco2_resid
ggsave("./figures/SM/AggNonCO2_resid.tiff",plot_agg_nonco2_resid,"tiff",dpi=200)

#-------------------------------------------
# GHG and air pollutant emissions per period, region, narrative, and gas in the RCP2.6 scenario
plot_resid_nonco2_rcp<- ggplot(resid.nonco2 %>% filter(scenario == "RCP2p6", ghg != "CH4"),
                                aes(x=year,
                                    y=value,
                                    color=factor(decile,levels = c("D1","D2","D3","D4","D5","D6","D7","D8","D9","D10")),
                                    fill=factor(decile,levels = c("D1","D2","D3","D4","D5","D6","D7","D8","D9","D10")))) +
  geom_line(size=.5) +
  geom_point(size =.5) +
  geom_ribbon(aes(ymin=min, ymax=max),
              alpha=0.3, linetype = 0) +
 # geom_vline(xintercept = 2015, linetype="dashed", colour="black")+
  facet_grid(factor(ghg, levels = c("CO2", "HFCs", "BC", "NOx","OC","SO2"))~region, scales = "free") +
  theme_classic() +
  labs(x="",y="Emissions")+
  theme(legend.title = element_blank(),
        axis.text.y = element_text(size=7),
        axis.text.x = element_text(size=7),
        axis.title = element_text(size=11),
        strip.text.y =  element_text(size=10),
        legend.position = "bottom",
        plot.title = element_text(size = 12, face="bold"),
        plot.subtitle = element_text(size = 11))+
  scale_color_manual(values = my_pal) + 
  scale_fill_manual(values = my_pal)
#ggtitle("Residential emissions by period, region, and gas in the Baseline scenario",
#        subtitle = "Shaded areas represent uncertainty ranges for the five Shared Socioeconomic Pathways (SSP1-SSP5)")

plot_resid_nonco2_rcp
ggsave("./figures/SM/Emissions_rcp.tiff",plot_resid_nonco2_rcp,"tiff",dpi=200)

#-------------------------------------------
#-------------------------------------------
# EXPENDITURES
#-------------------------------------------
# Alternative figure for expenditures: All scenarios
plot_expen<- ggplot(expen %>% filter(year %in% c(2050)),
                    aes(x = value_pc,
                        y = perc_exp_residEn,
                        color = factor(decile,levels = c("D1","D2","D3","D4","D5","D6","D7","D8","D9","D10")),
                        fill = scenario,
                        shape = narrative)) +
  geom_point(size = 2) +
  facet_wrap(~region, ncol = 1) +
  theme_classic() +
  labs(x="Per capita service output (GJ/pers)", y="Share of energy expenditures")+
  theme(legend.title = element_blank(),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title = element_text(size=12),
        strip.text =  element_text(size=11),
        legend.position = "bottom",
        legend.text = element_text(size = 9),
        legend.key.size = unit(0.2, "cm")) +
  scale_fill_manual(values = c("grey80","white")) + 
  scale_color_manual(values = my_pal) + 
  scale_shape_manual(values = c(21,22,23,24,25)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  guides(fill = guide_legend(override.aes = list(shape = 21)))


plot_expen
ggsave("./figures/SM/Expen2050.tiff",plot_expen,"tiff",dpi=200)
