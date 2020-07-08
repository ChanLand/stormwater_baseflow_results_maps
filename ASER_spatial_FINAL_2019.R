# ASER Spatial maps/plots with boxplot summaries
# These plots are replacing the dRio plots from last year
# AMC, 2019-05
# The original tests of these plots are in dRioPlotsWater_2018_V3.R
# This is the clean version
# V2 modifies the plots to only include 2018 data and no NMWQSTDs
# V3 revises the plots again... almost there...
# FINAL version includes baseflow
# 2019: Now into the next reporting year... making plots for 2019 report

library(tidyr)
library(dplyr)
library(lubridate)
library(readr)
library(cowplot)
library(ggspatial)
library(rgdal)
library(ggsn)
library(ggpubr)

# Import files

# IP DATA

# Data thru 2019
ipdata <- read_csv("Data/ASER_IP_thru_2019.csv",
                   col_types = list('sample_date' = col_date("%m/%d/%Y"))) 
names(ipdata) <- make.names(names(ipdata), unique = TRUE)

ipdata <- ipdata %>%
  select(location_id_alias, northing, easting, parameter_name, field_preparation_code, detect_flag,
         sample_date, report_result, sample_purpose) 


# Gage Data
gagedata <- read_csv("Data/ASER_GSM_LAP_WT_WS_WP_thru_2019.csv",
                     col_types = list("sample_date" = col_date("%m/%d/%Y")))
names(gagedata)<-make.names(names(gagedata),unique = TRUE)

gagedata <- gagedata %>%
  select(location_id, location_id_alias, northing, easting, parameter_name, field_preparation_code, detect_flag,
         sample_date, report_result, sample_purpose)

# Base flow data
WS_data <-read_csv("Data/ASER_baseflow_thru_2019.csv",
         col_types = list(`Sample Date` = col_date("%m/%d/%Y"))) 
names(WS_data) <- make.names(names(WS_data), unique = TRUE)

WS_data <- WS_data %>%
  select(Location.Alias, Location.ID, northing, easting, Parameter.Name, Field.Preparation.Code, Detected,
         Sample.Date, Report.Result, Sample.Purpose) %>%
  rename(location_id_alias = Location.Alias, location_id = Location.ID, 
         parameter_name = Parameter.Name,
         field_preparation_code = Field.Preparation.Code, detect_flag = Detected,
         sample_date = Sample.Date, report_result = Report.Result, sample_purpose = Sample.Purpose)


# Distance to Rio and NM water quality standards
driosmas <- read_csv("Data/2017 Data/dRioSMAs.csv")

driogages <- read_csv("Data/2017 Data/dRioGageStations.csv")

drioconf <- read_csv("Data/2017 Data/dRioConfluencesLandmarks.csv")

driobase <- read_csv("Data/2017 Data/drioBaseflow.csv")

wqstds <- read_csv("Data/2017 Data/NMAC_WQSTDS.csv")

####################################################
# Join distance data to analytical data
ipdata <- ipdata %>%
  left_join(driosmas, by = "location_id_alias") %>%
  filter(sample_purpose == "REG") %>%
  select(location_id_alias, northing, easting, parameter_name, field_preparation_code, 
         detect_flag, watershed, 
         subwatershed, sample_date, report_result) 

gagedata <- gagedata %>%
  left_join(driogages, by = "location_id") %>%
  filter(sample_purpose == "REG") %>%
  select(location_id_alias, northing, easting, parameter_name, field_preparation_code, 
         detect_flag, watershed, 
         subwatershed, sample_date, report_result)

WS_data <- WS_data %>%
  left_join(driobase, by = "location_id") %>%
  filter(sample_purpose == "REG") %>%
  select(location_id_alias.x, northing, easting, parameter_name, field_preparation_code, 
         detect_flag, watershed, 
         subwatershed, sample_date, report_result) %>%
  rename(location_id_alias = location_id_alias.x)

#######################################################
# Load in Spatial data

# Subwatershed shapefile
subwatersheds <- readOGR(dsn = "Data/GIS Data", layer = "Subwatersheds")
head(subwatersheds@data, n = 2)

# Drainage (including Rio Grande) shapefile
drainage <- readOGR(dsn = "Data/GIS Data", layer = "correct_drainage")
head(drainage@data, n = 2)

# fortify data so that it can be plotted in ggplot
subwatersheds_f <- fortify(subwatersheds)
subwatersheds$id <- row.names(subwatersheds) # add an id column that will be used to join dataframes
subwatersheds_f <- left_join(subwatersheds_f, subwatersheds@data)

drainage_f <- fortify(drainage)
drainage$id <- row.names(drainage)
drainage_f <- left_join(drainage_f, drainage@data)

#################################################
# Filter data and make plots

# Los Alamos/Pueblo
### PCBs ##########
###################

# IP
ipdata_lap <- ipdata %>%
  filter(parameter_name == "Total PCB" & field_preparation_code == "UF" & 
           (watershed == "Los Alamos" | watershed == "Pueblo")) %>%
  mutate(datasource = "IP") %>%
  mutate(DetData = paste(detect_flag, datasource))

# Gage
gagedata_lap <- gagedata %>%
  filter(parameter_name == "Total PCB" & field_preparation_code == "UF" & 
           (watershed == "Los Alamos" | watershed == "Pueblo") &
           sample_date >= "2005-01-01") %>%
  mutate(datasource = "Gage") %>%
  mutate(DetData = paste(detect_flag, datasource)) 

# Base
base_lap <- WS_data %>%
  filter(parameter_name == "Total PCB" & field_preparation_code == "UF" & 
  (watershed == "Los Alamos" | watershed == "Pueblo" | watershed == "Rio Grande") &
  sample_date >= "2005-01-01") %>%
  mutate(datasource = "Base") %>%
  mutate(DetData = paste(detect_flag, datasource)) 

# Bind dataframes
lap_data <- bind_rows(ipdata_lap, gagedata_lap, base_lap)
rm(ipdata_lap, gagedata_lap, base_lap)

# Percentile
percentiles <- lap_data %>%
  filter(detect_flag == "Y") %>%
  summarize(result = list(enframe(quantile(report_result, probs = c(0.1, 0.5, 0.9))))) %>%
  unnest()

plotdat <- lap_data %>%
  filter(detect_flag == "Y") %>%
  group_by(location_id_alias, northing, easting) %>%
  summarize(median = median(report_result), sd = sd(report_result)) %>%
  filter(!is.na(sd)) %>%
  mutate(percentile = 
           case_when(
             median < percentiles$value[1] ~ "0 - 0.01 (ug/L), Below 10th percentile",
             median >= percentiles$value[1] & median < percentiles$value[2] ~ "0.01 - 0.05 (ug/L), 10-50th percentile",
             median >= percentiles$value[2] & median < percentiles$value[3] ~ "0.05 - 0.48 (ug/L), 50-90th percentile", 
             median > percentiles$value[3] ~ "0.48 - 15.1 (ug/L), Above 90th percentile"
           ))


plotdat$percentile <- factor(plotdat$percentile, levels = c("0 - 0.01 (ug/L), Below 10th percentile",
                                                            "0.01 - 0.05 (ug/L), 10-50th percentile",
                                                            "0.05 - 0.48 (ug/L), 50-90th percentile",
                                                            "0.48 - 15.1 (ug/L), Above 90th percentile"))

# Select subwatersheds to plot
lap_subwatersheds <- subwatersheds_f %>%
  filter(NAME %in% c("Acid Canyon", "Bayo Canyon", "DP Canyon", "Graduation Canyon", 
                     "Guaje Canyon", "Los Alamos Canyon", "Pueblo Canyon"))

# Percentile - FINAL DECISION!
lap_pcb_map <- ggplot() +
  geom_polygon(data = lap_subwatersheds, aes(long, lat, group = group, fill = NAME), color = "black") + 
  scale_fill_manual(values = c("Acid Canyon" = "grey100", "Bayo Canyon" = "grey90", "DP Canyon" = "grey80", "Graduation Canyon" = "grey65",
                               "Guaje Canyon" = "grey70", "Los Alamos Canyon" = "grey40", "Pueblo Canyon" = "grey25")) + 
  guides(fill = FALSE) + # prevents there from being a legend for subwatershed
  geom_path(data = drainage_f, aes(long, lat, group = group), color = "dodgerblue") + 
  geom_point(data = plotdat, aes(easting, northing, color = percentile), size = 3) + 
  scale_color_manual(values = c("0 - 0.01 (ug/L), Below 10th percentile" = "#8bbef3", "0.01 - 0.05 (ug/L), 10-50th percentile" = "#67b44c", 
                                "0.05 - 0.48 (ug/L), 50-90th percentile" = "#fdd422",
                                "0.48 - 15.1 (ug/L), Above 90th percentile" = "#fa5c18"), drop = FALSE) +
  geom_point(data = plotdat, aes(easting, northing), shape = 1, color = "black", size = 3) +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  coord_equal(xlim = c(min(plotdat$easting), 1673000), 
              ylim = c(min(plotdat$northing) - 2800, max(plotdat$northing + 700))) +
  ylab(NULL) + 
  xlab(NULL) +
  annotation_scale(location = "br", width_hint = 0.2, plot_unit = "ft", unit_category = "imperial") +
  annotation_north_arrow(location = "bl", which_north = "true", style = north_arrow_fancy_orienteering) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), axis.text = element_blank(),
        legend.position = "top", legend.box = "vertical",
        legend.key.size = unit(0.4, 'cm'),
        legend.margin = margin(0,0,-0.25,0, unit = 'cm'),
        plot.margin = margin(0,0.2,0,0.2, "cm"))

# Los Alamos Canyon PCB boxplot
la_pcb_boxplot <- lap_data %>%
  filter(year(sample_date) != 2009, watershed == "Los Alamos") %>% # there is only one sample in 2009 - not worth including on plot
  ggplot(aes(x = factor(year(sample_date)), y = report_result)) +
  geom_boxplot(outlier.shape = NA, fill = "#40e0d0", fatten = 1) + 
  ylim(0, 0.5) + 
  xlab(NULL) + 
  ylab(expression(atop("Total"~"PCB",~"Concentration"~"("*mu*"g/L)"))) +
  ggtitle("Los Alamos watershed") + 
  theme_bw() + 
  theme(plot.margin = margin(0,0.2,0.2,0.2, "cm"))

# Pueblo Canyon PCB boxplot
pueblo_pcb_boxplot <- lap_data %>%
  filter(year(sample_date) != 2009, watershed == "Pueblo") %>% # there is only one sample in 2009 - not worth including on plot
  ggplot(aes(x = factor(year(sample_date)), y = report_result)) +
  geom_boxplot(outlier.shape = NA, fill = "#40e0d0", fatten = 1) + 
  ylim(0, 0.5) + 
  xlab(NULL) + 
  ylab(expression(atop("Total"~"PCB",~"Concentration"~"("*mu*"g/L)"))) +
  ggtitle("Pueblo watershed") + 
  theme_bw() + 
  theme(plot.margin = margin(0,0.2,0.2,0.2, "cm"))

# Combine map and boxplot as a two-panel plot and save 
combo_lap_pcb <- plot_grid(lap_pcb_map, la_pcb_boxplot, pueblo_pcb_boxplot, nrow = 3, align = 'v', rel_heights = c(1.2, 0.7, 0.7))

ggsave(combo_lap_pcb, filename = "Figures/LAP_PCB_combo_wWS.pdf", height = 7, width = 12, useDingbats = FALSE)

rm(combo_lap_pcb, la_pcb_boxplot, pueblo_pcb_boxplot, lap_pcb_map, lap_data, plotdat, lap_subwatersheds)

#############################################
# Los Alamos/Pueblo
### Copper ##########
###################

# IP
ipdata_lap <- ipdata %>%
  filter(parameter_name == "Copper" & field_preparation_code == "F" & 
           (watershed == "Los Alamos" | watershed == "Pueblo")) %>%
  mutate(datasource = "IP") %>%
  mutate(DetData = paste(detect_flag, datasource))

# Gage
gagedata_lap <- gagedata %>%
  filter(parameter_name == "Copper" & field_preparation_code == "F" & 
           (watershed == "Los Alamos" | watershed == "Pueblo") &
           sample_date >= "2005-01-01") %>%
  mutate(datasource = "Gage") %>%
  mutate(DetData = paste(detect_flag, datasource)) 

# Base
base_lap <- WS_data %>%
  filter(parameter_name == "Copper" & field_preparation_code == "F" & 
           (watershed == "Los Alamos" | watershed == "Pueblo" | watershed == "Rio Grande") &
           sample_date >= "2005-01-01") %>%
  mutate(datasource = "Base") %>%
  mutate(DetData = paste(detect_flag, datasource)) 

# Bind dataframes
lap_data <- bind_rows(ipdata_lap, gagedata_lap, base_lap)
rm(ipdata_lap, gagedata_lap, base_lap)

# Create a new filtered dataframe of data to plot
# Percentile
percentiles <- lap_data %>%
  filter(detect_flag == "Y") %>%
  summarize(result = list(enframe(quantile(report_result, probs = c(0.1, 0.5, 0.9))))) %>%
  unnest()

plotdat <- lap_data %>%
  filter(detect_flag == "Y") %>%
  group_by(location_id_alias, northing, easting) %>%
  summarize(median = median(report_result), sd = sd(report_result)) %>%
  filter(!is.na(sd)) %>%
  mutate(percentile = 
           case_when(
             median < percentiles$value[1] ~ "0 - 2.1 (ug/L), Below 10th percentile",
             median >= percentiles$value[1] & median < percentiles$value[2] ~ "2.1 - 3.3 (ug/L), 10-50th percentile",
             median >= percentiles$value[2] & median < percentiles$value[3] ~ "3.3 - 6.7 (ug/L), 50-90th percentile", 
             median > percentiles$value[3] ~ "6.7 - 33.0 (ug/L), Above 90th percentile"
           ))


# Add correct levels to water quality standards
plotdat$percentile <- factor(plotdat$percentile, levels = c("0 - 2.1 (ug/L), Below 10th percentile",
                                                            "2.1 - 3.3 (ug/L), 10-50th percentile",
                                                            "3.3 - 6.7 (ug/L), 50-90th percentile",
                                                            "6.7 - 33.0 (ug/L), Above 90th percentile"))
# Select subwatersheds to plot
lap_subwatersheds <- subwatersheds_f %>%
  filter(NAME %in% c("Acid Canyon", "Bayo Canyon", "Barrancas Canyon", "DP Canyon", "Graduation Canyon", 
                     "Guaje Canyon", "Los Alamos Canyon", "Pueblo Canyon", "Rendija Canyon",
                     "School Canyon", "Walnut Canyon"))

# Make the plots!
##########################################

# LAP Copper map
lap_cu_map <- ggplot() +
  geom_polygon(data = lap_subwatersheds, aes(long, lat, group = group, fill = NAME), color = "black") + 
  scale_fill_manual(values = c("Acid Canyon" = "grey100", "Bayo Canyon" = "grey90", "Barrancas Canyon" = "grey45",
                               "DP Canyon" = "grey80", "Graduation Canyon" = "grey65",
                               "Guaje Canyon" = "grey70", "Los Alamos Canyon" = "grey40", "Pueblo Canyon" = "grey25",
                               "Rendija Canyon" = "grey10", "School Canyon" = "grey85", "Walnut Canyon" = "grey50")) + 
  guides(fill = FALSE) + # prevents there from being a legend for subwatershed
  geom_path(data = drainage_f, aes(long, lat, group = group), color = "dodgerblue") + 
  geom_point(data = plotdat, aes(easting, northing, color = percentile), size = 3) + 
  scale_color_manual(values = c("0 - 2.1 (ug/L), Below 10th percentile" = "#8bbef3", 
                                "2.1 - 3.3 (ug/L), 10-50th percentile" = "#67b44c", 
                                "3.3 - 6.7 (ug/L), 50-90th percentile" = "#fdd422",
                                "6.7 - 33.0 (ug/L), Above 90th percentile" = "#fa5c18"), drop = FALSE) +
  geom_point(data = plotdat, aes(easting, northing), shape = 1, color = "black", size = 3) +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  coord_equal(xlim = c(min(plotdat$easting), 1673000), 
              ylim = c(min(plotdat$northing) - 2800, max(plotdat$northing + 700))) +
  ylab(NULL) + 
  xlab(NULL) +
  annotation_scale(location = "br", width_hint = 0.2, plot_unit = "ft", unit_category = "imperial") +
  annotation_north_arrow(location = "bl", which_north = "true", style = north_arrow_fancy_orienteering) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), axis.text = element_blank(),
        legend.position = "top", legend.box = "vertical",
        legend.key.size = unit(0.4, 'cm'),
        legend.margin = margin(0,0,-0.25,0, unit = 'cm'),
        plot.margin = margin(0,0.2,0,0.2, "cm"))

# Los Alamos Canyon PCB boxplot
la_cu_boxplot <- lap_data %>%
  filter(watershed == "Los Alamos") %>% # there is only one sample in 2009 - not worth including on plot
  ggplot(aes(x = factor(year(sample_date)), y = report_result)) +
  geom_boxplot(outlier.shape = NA, fill = "#40e0d0", fatten = 1) + 
  ylim(0, 15) + 
  xlab(NULL) + 
  ylab(expression(atop("Dissolved"~"Copper",~"Concentration"~"("*mu*"g/L)"))) +
  ggtitle("Los Alamos watershed") + 
  theme_bw() + 
  theme(plot.margin = margin(0,0.2,0.2,0.2, "cm"))

# Pueblo Canyon PCB boxplot
pueblo_cu_boxplot <- lap_data %>%
  filter(watershed == "Pueblo") %>% # there is only one sample in 2009 - not worth including on plot
  ggplot(aes(x = factor(year(sample_date)), y = report_result)) +
  geom_boxplot(outlier.shape = NA, fill = "#40e0d0", fatten = 1) + 
  ylim(0, 15) + 
  xlab(NULL) + 
  ylab(expression(atop("Dissolved"~"Copper",~"Concentration"~"("*mu*"g/L)"))) +
  ggtitle("Pueblo watershed") + 
  theme_bw() + 
  theme(plot.margin = margin(0,0.2,0.2,0.2, "cm"))

# Combine map and boxplot as a two-panel plot and save 
combo_lap_cu <- plot_grid(lap_cu_map, la_cu_boxplot, pueblo_cu_boxplot, nrow = 3, align = 'v', rel_heights = c(1.2, 0.7, 0.7))

ggsave(combo_lap_cu, filename = "Figures/LAP_Copper_combo_wWS.pdf", height = 9, width = 12, useDingbats = FALSE)

rm(combo_lap_cu, la_cu_boxplot, pueblo_cu_boxplot, lap_cu_map, lap_data, plotdat, lap_subwatersheds)

####################################################
# Los Alamos/Pueblo
### Lead ##########
###################

# IP
ipdata_lap <- ipdata %>%
  filter(parameter_name == "Lead" & field_preparation_code == "F" & 
           (watershed == "Los Alamos" | watershed == "Pueblo")) %>%
  mutate(datasource = "IP") %>%
  mutate(DetData = paste(detect_flag, datasource))

# Gage
gagedata_lap <- gagedata %>%
  filter(parameter_name == "Lead" & field_preparation_code == "F" & 
           (watershed == "Los Alamos" | watershed == "Pueblo") &
           sample_date >= "2005-01-01") %>%
  mutate(datasource = "Gage") %>%
  mutate(DetData = paste(detect_flag, datasource)) 

# Base
base_lap <- WS_data %>%
  filter(parameter_name == "Lead" & field_preparation_code == "F" & 
           (watershed == "Los Alamos" | watershed == "Pueblo" | watershed == "Rio Grande") &
           sample_date >= "2005-01-01") %>%
  mutate(datasource = "Base") %>%
  mutate(DetData = paste(detect_flag, datasource)) 

# Bind dataframes
lap_data <- bind_rows(ipdata_lap, gagedata_lap, base_lap)
rm(ipdata_lap, gagedata_lap, base_lap)

# Create a new filtered dataframe of data to plot
# Percentile
percentiles <- lap_data %>%
  filter(detect_flag == "Y") %>%
  summarize(result = list(enframe(quantile(report_result, probs = c(0.1, 0.5, 0.9))))) %>%
  unnest()

plotdat <- lap_data %>%
  filter(detect_flag == "Y") %>%
  group_by(location_id_alias, northing, easting) %>%
  summarize(median = median(report_result), sd = sd(report_result)) %>%
  filter(!is.na(sd)) %>%
  mutate(percentile = 
           case_when(
             median < percentiles$value[1] ~ "0 - 0.6 (ug/L), Below 10th percentile",
             median >= percentiles$value[1] & median < percentiles$value[2] ~ "0.6 - 1.0 (ug/L), 10-50th percentile",
             median >= percentiles$value[2] & median < percentiles$value[3] ~ "1.0 - 1.9 (ug/L), 50-90th percentile", 
             median > percentiles$value[3] ~ "1.9 - 10.0 (ug/L), Above 90th percentile"))

# Add correct levels to water quality standards
plotdat$percentile <- factor(plotdat$percentile, levels = c("0 - 0.6 (ug/L), Below 10th percentile",
                                                            "0.6 - 1.0 (ug/L), 10-50th percentile",
                                                            "1.0 - 1.9 (ug/L), 50-90th percentile",
                                                            "1.9 - 10.0 (ug/L), Above 90th percentile"))
# Select subwatersheds to plot
lap_subwatersheds <- subwatersheds_f %>%
  filter(NAME %in% c("Acid Canyon", "Bayo Canyon", "Barrancas Canyon", "DP Canyon", "Graduation Canyon", 
                     "Guaje Canyon", "Los Alamos Canyon", "Pueblo Canyon", "Rendija Canyon",
                     "School Canyon", "Walnut Canyon"))

# Make the plots!
##########################################

# LAP Lead map
lap_pb_map <- ggplot() +
  geom_polygon(data = lap_subwatersheds, aes(long, lat, group = group, fill = NAME), color = "black") + 
  scale_fill_manual(values = c("Acid Canyon" = "grey100", "Bayo Canyon" = "grey90", "Barrancas Canyon" = "grey45", 
                               "DP Canyon" = "grey80", "Graduation Canyon" = "grey65",
                               "Guaje Canyon" = "grey70", "Los Alamos Canyon" = "grey40", "Pueblo Canyon" = "grey25",
                               "Rendija Canyon" = "grey10", "School Canyon" = "grey95", "Walnut Canyon" = "grey50")) + 
  guides(fill = FALSE) + # prevents there from being a legend for subwatershed
  geom_path(data = drainage_f, aes(long, lat, group = group), color = "dodgerblue") + 
  geom_point(data = plotdat, aes(easting, northing, color = percentile), size = 3) + 
  scale_color_manual(values = c("0 - 0.6 (ug/L), Below 10th percentile" = "#8bbef3", 
                                "0.6 - 1.0 (ug/L), 10-50th percentile" = "#67b44c", 
                                "1.0 - 1.9 (ug/L), 50-90th percentile" = "#fdd422",
                                "1.9 - 10.0 (ug/L), Above 90th percentile" = "#fa5c18"), drop = FALSE) +
  geom_point(data = plotdat, aes(easting, northing), shape = 1, color = "black", size = 3) +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  coord_equal(xlim = c(min(plotdat$easting), 1673000), 
              ylim = c(min(plotdat$northing) - 2800, max(plotdat$northing + 700))) +
  ylab(NULL) + 
  xlab(NULL) +
  annotation_scale(location = "br", width_hint = 0.2, plot_unit = "ft", unit_category = "imperial") +
  annotation_north_arrow(location = "bl", which_north = "true", style = north_arrow_fancy_orienteering) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), axis.text = element_blank(),
        legend.position = "top", legend.box = "vertical",
        legend.key.size = unit(0.4, 'cm'),
        legend.margin = margin(0,0,-0.25,0, unit = 'cm'),
        plot.margin = margin(0,0.2,0,0.2, "cm"))

# Los Alamos Canyon PCB boxplot
la_pb_boxplot <- lap_data %>%
  filter(watershed == "Los Alamos") %>% # there is only one sample in 2009 - not worth including on plot
  ggplot(aes(x = factor(year(sample_date)), y = report_result)) +
  geom_boxplot(outlier.shape = NA, fill = "#40e0d0", fatten = 1) + 
  ylim(0, 4) + 
  xlab(NULL) + 
  ylab(expression(atop("Dissolved"~"Lead",~"Concentration"~"("*mu*"g/L)"))) +
  ggtitle("Los Alamos watershed") + 
  theme_bw() + 
  theme(plot.margin = margin(0,0.2,0.2,0.2, "cm"))

# Pueblo Canyon PCB boxplot
pueblo_pb_boxplot <- lap_data %>%
  filter(watershed == "Pueblo") %>% # there is only one sample in 2009 - not worth including on plot
  ggplot(aes(x = factor(year(sample_date)), y = report_result)) +
  geom_boxplot(outlier.shape = NA, fill = "#40e0d0", fatten = 1) + 
  ylim(0, 4) + 
  xlab(NULL) + 
  ylab(expression(atop("Dissolved"~"Lead",~"Concentration"~"("*mu*"g/L)"))) +
  ggtitle("Pueblo watershed") + 
  theme_bw() + 
  theme(plot.margin = margin(0,0.2,0.2,0.2, "cm"))

# Combine map and boxplot as a two-panel plot and save 
combo_lap_pb <- plot_grid(lap_pb_map, la_pb_boxplot, pueblo_pb_boxplot, nrow = 3, align = 'v', rel_heights = c(1.2, 0.7, 0.7))

ggsave(combo_lap_pb, filename = "Figures/LAP_Lead_combo_wWS.pdf", height = 9, width = 12, useDingbats = FALSE)

rm(combo_lap_pb, la_pb_boxplot, pueblo_pb_boxplot, lap_pb_map, lap_data, plotdat, lap_subwatersheds)

################################################################
####################################
####################################
# Sandia/Mortandad
####################################

# Total PCB

# IP
ipdata_san <- ipdata %>%
  filter(parameter_name == "Total PCB" & field_preparation_code == "UF" & 
           (watershed == "Sandia" | watershed == "Mortandad") &
           (sample_date >= "2005-01-01" & sample_date <= "2018-12-31")) %>%
  mutate(datasource = "IP") %>%
  mutate(DetData = paste(detect_flag, datasource))

# Gage
gagedata_san <- gagedata %>%
  filter(parameter_name == "Total PCB" & field_preparation_code == "UF" & 
           (watershed == "Sandia" | watershed == "Mortandad") &
           (sample_date >= "2005-01-01" & sample_date <= "2018-12-31")) %>%
  mutate(datasource = "Gage") %>%
  mutate(DetData = paste(detect_flag, datasource)) 

# Base
base_san <- WS_data %>%
  filter(parameter_name == "Total PCB" & field_preparation_code == "UF" & 
           (watershed == "Sandia" | watershed == "Mortandad" | watershed == "Rio Grande") &
           (sample_date >= "2005-01-01" & sample_date <= "2018-12-31")) %>%
  mutate(datasource = "Base") %>%
  mutate(DetData = paste(detect_flag, datasource)) 

# Bind dataframes
san_data <- bind_rows(ipdata_san, gagedata_san, base_san)
rm(ipdata_san, gagedata_san, base_san)

# Create a new filtered dataframe of data to plot
# Percentile
percentiles <- san_data %>%
  filter(detect_flag == "Y") %>%
  summarize(result = list(enframe(quantile(report_result, probs = c(0.1, 0.5, 0.9))))) %>%
  unnest()

plotdat <- san_data %>%
  filter(detect_flag == "Y") %>%
  group_by(location_id_alias, northing, easting) %>%
  summarize(median = median(report_result), sd = sd(report_result)) %>%
  filter(!is.na(sd)) %>%
  mutate(percentile = 
           case_when(
             median < percentiles$value[1] ~ "0 - 0.003 (ug/L), Below 10th percentile",
             median >= percentiles$value[1] & median < percentiles$value[2] ~ "0.003 - 0.05 (ug/L), 10-50th percentile",
             median >= percentiles$value[2] & median < percentiles$value[3] ~ "0.05 - 0.3 (ug/L), 50-90th percentile", 
             median > percentiles$value[3] ~ "0.3 - 0.5 (ug/L), Above 90th percentile"))

# Add correct levels to water quality standards
plotdat$percentile <- factor(plotdat$percentile, levels = c("0 - 0.003 (ug/L), Below 10th percentile",
                                                            "0.003 - 0.05 (ug/L), 10-50th percentile",
                                                            "0.05 - 0.3 (ug/L), 50-90th percentile",
                                                            "0.3 - 0.5 (ug/L), Above 90th percentile"))
# Select subwatersheds to plot
san_subwatersheds <- subwatersheds_f %>%
  filter(NAME %in% c("Sandia Canyon", "Canada del Buey", "Mortandad Canyon", "Ten-Site Canyon", "Cedro Canyon"))

# Make the plots!
##########################################

# Sandia/Mort PCB map
san_pcb_map <- ggplot() +
  geom_polygon(data = san_subwatersheds, aes(long, lat, group = group, fill = NAME), color = "black") + 
  scale_fill_manual(values = c("Sandia Canyon" = "grey80", "Canada del Buey" = "grey55",
                               "Mortandad Canyon" = "grey35", "Ten-Site Canyon" = "grey20",
                               "Cedro Canyon" = "grey90")) + 
  guides(fill = FALSE) + # prevents there from being a legend for subwatershed
  geom_path(data = drainage_f, aes(long, lat, group = group), color = "dodgerblue") + 
  geom_point(data = plotdat, aes(easting, northing, color = percentile), size = 3) + 
  scale_color_manual(values = c("0 - 0.003 (ug/L), Below 10th percentile" = "#8bbef3", 
                                "0.003 - 0.05 (ug/L), 10-50th percentile" = "#67b44c", 
                                "0.05 - 0.3 (ug/L), 50-90th percentile" = "#fdd422",
                                "0.3 - 0.5 (ug/L), Above 90th percentile" = "#fa5c18"), drop = FALSE) +
  geom_point(data = plotdat, aes(easting, northing), shape = 1, color = "black", size = 3) +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  coord_equal(xlim = c(min(plotdat$easting), 1665000), 
              ylim = c(min(plotdat$northing) - 2800, max(plotdat$northing + 700))) +
  ylab(NULL) + 
  xlab(NULL) +
  annotation_scale(location = "br", width_hint = 0.4, plot_unit = "ft", unit_category = "imperial") +
  annotation_north_arrow(location = "bl", which_north = "true", style = north_arrow_fancy_orienteering) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), axis.text = element_blank(),
        legend.position = "top", legend.box = "vertical",
        legend.key.size = unit(0.4, 'cm'),
        legend.margin = margin(0,0,-0.25,0, unit = 'cm'),
        plot.margin = margin(0,0.2,0,0.2, "cm"))

# Sandia Canyon PCB boxplot
san_pcb_boxplot <- san_data %>%
  filter(year(sample_date) != 2009, watershed == "Sandia") %>% # there is only one sample in 2009 - not worth including on plot
  ggplot(aes(x = factor(year(sample_date)), y = report_result)) +
  geom_boxplot(outlier.shape = NA, fill = "#40e0d0", fatten = 1) + 
  ylim(0, 1.1) + 
  xlab(NULL) + 
  ylab(expression(atop("Total"~"PCB",~"Concentration"~"("*mu*"g/L)"))) +
  ggtitle("Sandia watershed") + 
  theme_bw() + 
  theme(plot.margin = margin(0,0.2,0.2,0.2, "cm"))

# Mortandad Canyon PCB boxplot
mortandad_pcb_boxplot <- san_data %>%
  filter(year(sample_date) != 2009, watershed == "Mortandad") %>% # there is only one sample in 2009 - not worth including on plot
  ggplot(aes(x = factor(year(sample_date)), y = report_result)) +
  geom_boxplot(outlier.shape = NA, fill = "#40e0d0", fatten = 1) + 
  ylim(0, 1.1) + 
  xlab(NULL) + 
  ylab(expression(atop("Total"~"PCB",~"Concentration"~"("*mu*"g/L)"))) +
  ggtitle("Mortandad watershed") + 
  theme_bw() + 
  theme(plot.margin = margin(0,0.2,0.2,0.2, "cm"))

# Combine map and boxplot as a two-panel plot and save 
combo_san_pcb <- plot_grid(san_pcb_map, san_pcb_boxplot, mortandad_pcb_boxplot, nrow = 3, align = 'v', rel_heights = c(1.2, 0.7, 0.7))

ggsave(combo_san_pcb, filename = "Figures/San_PCB_combo_wWS.pdf", height = 11, width = 11.2, useDingbats = FALSE)

rm(combo_san_pcb, san_pcb_boxplot, mortandad_pcb_boxplot, san_pcb_map, san_data, plotdat, san_subwatersheds)

####################################################

# Sandia Copper
#######################################

# IP
ipdata_san <- ipdata %>%
  filter(parameter_name == "Copper" & field_preparation_code == "F" & 
           (watershed == "Sandia" | watershed == "Mortandad") &
           (sample_date >= "2005-01-01" & sample_date <= "2018-12-31")) %>%
  mutate(datasource = "IP") %>%
  mutate(DetData = paste(detect_flag, datasource))

# Gage
gagedata_san <- gagedata %>%
  filter(parameter_name == "Copper" & field_preparation_code == "F" & 
           (watershed == "Sandia" | watershed == "Mortandad") &
           (sample_date >= "2005-01-01" & sample_date <= "2018-12-31")) %>%
  mutate(datasource = "Gage") %>%
  mutate(DetData = paste(detect_flag, datasource)) 

# Base
base_san <- WS_data %>%
  filter(parameter_name == "Copper" & field_preparation_code == "F" & 
           (watershed == "Sandia" | watershed == "Mortandad" | watershed == "Rio Grande") &
           (sample_date >= "2005-01-01" & sample_date <= "2018-12-31")) %>%
  mutate(datasource = "Base") %>%
  mutate(DetData = paste(detect_flag, datasource)) 

# Bind dataframes
san_data <- bind_rows(ipdata_san, gagedata_san, base_san)
rm(ipdata_san, gagedata_san, base_san)

# Create a new filtered dataframe of data to plot
# Percentile
percentiles <- san_data %>%
  filter(detect_flag == "Y") %>%
  summarize(result = list(enframe(quantile(report_result, probs = c(0.1, 0.5, 0.9))))) %>%
  unnest()

plotdat <- san_data %>%
  filter(detect_flag == "Y") %>%
  group_by(location_id_alias, northing, easting) %>%
  summarize(median = median(report_result), sd = sd(report_result)) %>%
  filter(!is.na(sd)) %>%
  mutate(percentile = 
           case_when(
             median < percentiles$value[1] ~ "0 - 3.0 (ug/L), Below 10th percentile",
             median >= percentiles$value[1] & median < percentiles$value[2] ~ "3.0 - 4.8 (ug/L), 10-50th percentile",
             median >= percentiles$value[2] & median < percentiles$value[3] ~ "4.8 - 11.5 (ug/L), 50-90th percentile", 
             median > percentiles$value[3] ~ "11.5 - 46.7 (ug/L), Above 90th percentile"))

# Add correct levels to water quality standards
plotdat$percentile <- factor(plotdat$percentile, levels = c("0 - 3.0 (ug/L), Below 10th percentile",
                                                            "3.0 - 4.8 (ug/L), 10-50th percentile",
                                                            "4.8 - 11.5 (ug/L), 50-90th percentile",
                                                            "11.5 - 46.7 (ug/L), Above 90th percentile"))
# Select subwatersheds to plot
san_subwatersheds <- subwatersheds_f %>%
  filter(NAME %in% c("Sandia Canyon", "Canada del Buey", "Mortandad Canyon", "Ten-Site Canyon", "Cedro Canyon"))

# Make the plots!
##########################################

# Sandia/Mort Copper map
san_cu_map <- ggplot() +
  geom_polygon(data = san_subwatersheds, aes(long, lat, group = group, fill = NAME), color = "black") + 
  scale_fill_manual(values = c("Sandia Canyon" = "grey80", "Canada del Buey" = "grey55",
                               "Mortandad Canyon" = "grey35", "Ten-Site Canyon" = "grey20",
                               "Cedro Canyon" = "grey90")) + 
  guides(fill = FALSE) + # prevents there from being a legend for subwatershed
  geom_path(data = drainage_f, aes(long, lat, group = group), color = "dodgerblue") + 
  geom_point(data = plotdat, aes(easting, northing, color = percentile), size = 3) + 
  scale_color_manual(values = c("0 - 3.0 (ug/L), Below 10th percentile" = "#8bbef3", 
                                "3.0 - 4.8 (ug/L), 10-50th percentile" = "#67b44c", 
                                "4.8 - 11.5 (ug/L), 50-90th percentile" = "#fdd422",
                                "11.5 - 46.7 (ug/L), Above 90th percentile" = "#fa5c18"), drop = FALSE) +
  geom_point(data = plotdat, aes(easting, northing), shape = 1, color = "black", size = 3) +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  coord_equal(xlim = c(min(plotdat$easting), 1665000), 
              ylim = c(min(plotdat$northing) - 2800, max(plotdat$northing + 700))) +
  ylab(NULL) + 
  xlab(NULL) +
  annotation_scale(location = "br", width_hint = 0.4, plot_unit = "ft", unit_category = "imperial") +
  annotation_north_arrow(location = "bl", which_north = "true", style = north_arrow_fancy_orienteering) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), axis.text = element_blank(),
        legend.position = "top", legend.box = "vertical",
        legend.key.size = unit(0.4, 'cm'),
        legend.margin = margin(0,0,-0.25,0, unit = 'cm'),
        plot.margin = margin(0,0.2,0,0.2, "cm"))

# Sandia Canyon PCB boxplot
san_cu_boxplot <- san_data %>%
  filter(watershed == "Sandia") %>% # there is only one sample in 2009 - not worth including on plot
  ggplot(aes(x = factor(year(sample_date)), y = report_result)) +
  geom_boxplot(outlier.shape = NA, fill = "#40e0d0", fatten = 1) + 
  ylim(0, 26) + 
  xlab(NULL) + 
  ylab(expression(atop("Dissolved"~"Copper",~"Concentration"~"("*mu*"g/L)"))) +
  ggtitle("Sandia watershed") + 
  theme_bw() + 
  theme(plot.margin = margin(0,0.2,0.2,0.2, "cm"))

# Mortandad Canyon PCB boxplot
mortandad_cu_boxplot <- san_data %>%
  filter(watershed == "Mortandad") %>% # there is only one sample in 2009 - not worth including on plot
  ggplot(aes(x = factor(year(sample_date)), y = report_result)) +
  geom_boxplot(outlier.shape = NA, fill = "#40e0d0", fatten = 1) + 
  ylim(0, 26) + 
  xlab(NULL) + 
  ylab(expression(atop("Dissolved"~"Copper",~"Concentration"~"("*mu*"g/L)"))) +
  ggtitle("Mortandad watershed") + 
  theme_bw() + 
  theme(plot.margin = margin(0,0.2,0.2,0.2, "cm"))

# Combine map and boxplot as a two-panel plot and save 
combo_san_cu <- plot_grid(san_cu_map, san_cu_boxplot, mortandad_cu_boxplot, nrow = 3, align = 'v', rel_heights = c(1.2, 0.7, 0.7))

ggsave(combo_san_cu, filename = "Figures/San_Copper_combo_wWS.pdf", height = 11, width = 11.2, useDingbats = FALSE)

rm(combo_san_cu, san_cu_boxplot, mortandad_cu_boxplot, san_cu_map, san_data, plotdat, san_subwatersheds)

##################################################
# Sandia Lead
#######################################

# IP
ipdata_san <- ipdata %>%
  filter(parameter_name == "Lead" & field_preparation_code == "F" & 
           (watershed == "Sandia" | watershed == "Mortandad") & 
           (sample_date >= "2005-01-01" & sample_date <= "2018-12-31")) %>%
  mutate(datasource = "IP") %>%
  mutate(DetData = paste(detect_flag, datasource))

# Gage
gagedata_san <- gagedata %>%
  filter(parameter_name == "Lead" & field_preparation_code == "F" & 
           (watershed == "Sandia" | watershed == "Mortandad") &
           (sample_date >= "2005-01-01" & sample_date <= "2018-12-31")) %>%
  mutate(datasource = "Gage") %>%
  mutate(DetData = paste(detect_flag, datasource)) 

# Base
base_san <- WS_data %>%
  filter(parameter_name == "Lead" & field_preparation_code == "F" & 
           (watershed == "Sandia" | watershed == "Mortandad" | watershed == "Rio Grande") &
           (sample_date >= "2005-01-01" & sample_date <= "2018-12-31")) %>%
  mutate(datasource = "Base") %>%
  mutate(DetData = paste(detect_flag, datasource)) 

# Bind dataframes
san_data <- bind_rows(ipdata_san, gagedata_san, base_san)
rm(ipdata_san, gagedata_san, base_san)

# Create a new filtered dataframe of data to plot
# Percentile
percentiles <- san_data %>%
  filter(detect_flag == "Y") %>%
  summarize(result = list(enframe(quantile(report_result, probs = c(0.1, 0.5, 0.9))))) %>%
  unnest()

plotdat <- san_data %>%
  filter(detect_flag == "Y") %>%
  group_by(location_id_alias, northing, easting) %>%
  summarize(median = median(report_result), sd = sd(report_result)) %>%
  filter(!is.na(sd)) %>%
  mutate(percentile = 
           case_when(
             median < percentiles$value[1] ~ "0 - 0.5 (ug/L), Below 10th percentile",
             median >= percentiles$value[1] & median < percentiles$value[2] ~ "0.5 - 0.7 (ug/L), 10-50th percentile",
             median >= percentiles$value[2] & median < percentiles$value[3] ~ "0.7 - 1.5 (ug/L), 50-90th percentile", 
             median >= percentiles$value[3] ~ "1.5 - 82.8 (ug/L), Above 90th percentile"))

# Add correct levels to water quality standards
plotdat$percentile <- factor(plotdat$percentile, levels = c("0 - 0.5 (ug/L), Below 10th percentile",
                                                            "0.5 - 0.7 (ug/L), 10-50th percentile",
                                                            "0.7 - 1.5 (ug/L), 50-90th percentile",
                                                            "1.5 - 82.8 (ug/L), Above 90th percentile"))
# Select subwatersheds to plot
san_subwatersheds <- subwatersheds_f %>%
  filter(NAME %in% c("Sandia Canyon", "Canada del Buey", "Mortandad Canyon", "Ten-Site Canyon", "Cedro Canyon"))

# Make the plots!
##########################################

# Sandia/Mort Copper map
san_pb_map <- ggplot() +
  geom_polygon(data = san_subwatersheds, aes(long, lat, group = group, fill = NAME), color = "black") + 
  scale_fill_manual(values = c("Sandia Canyon" = "grey80", "Canada del Buey" = "grey55",
                               "Mortandad Canyon" = "grey35", "Ten-Site Canyon" = "grey20",
                               "Cedro Canyon" = "grey90")) + 
  guides(fill = FALSE) + # prevents there from being a legend for subwatershed
  geom_path(data = drainage_f, aes(long, lat, group = group), color = "dodgerblue") + 
  geom_point(data = plotdat, aes(easting, northing, color = percentile), size = 3) + 
  scale_color_manual(values = c("0 - 0.5 (ug/L), Below 10th percentile" = "#8bbef3", 
                                "0.5 - 0.7 (ug/L), 10-50th percentile" = "#67b44c", 
                                "0.7 - 1.5 (ug/L), 50-90th percentile" = "#fdd422",
                                "1.5 - 82.8 (ug/L), Above 90th percentile" = "#fa5c18"), drop = FALSE) +
  geom_point(data = plotdat, aes(easting, northing), shape = 1, color = "black", size = 3) +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  coord_equal(xlim = c(min(plotdat$easting), 1665000), 
              ylim = c(min(plotdat$northing) - 13500, max(plotdat$northing + 700))) +
  ylab(NULL) + 
  xlab(NULL) +
  annotation_scale(location = "br", width_hint = 0.4, plot_unit = "ft", unit_category = "imperial") +
  annotation_north_arrow(location = "bl", which_north = "true", style = north_arrow_fancy_orienteering) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), axis.text = element_blank(),
        legend.position = "top", legend.box = "vertical",
        legend.key.size = unit(0.4, 'cm'),
        legend.margin = margin(0,0,-0.25,0, unit = 'cm'),
        plot.margin = margin(0,0.2,0,0.2, "cm"))

# Los Alamos Canyon lead boxplot
san_pb_boxplot <- san_data %>%
  filter(watershed == "Sandia") %>% # there is only one sample in 2009 - not worth including on plot
  ggplot(aes(x = factor(year(sample_date)), y = report_result)) +
  geom_boxplot(outlier.shape = NA, fill = "#40e0d0", fatten = 1) + 
  ylim(0, 4) + 
  xlab(NULL) + 
  ylab(expression(atop("Dissolved"~"Lead",~"Concentration"~"("*mu*"g/L)"))) +
  ggtitle("Sandia watershed") + 
  theme_bw() + 
  theme(plot.margin = margin(0,0.2,0.2,0.2, "cm"))

# Pueblo Canyon lead boxplot
mortandad_pb_boxplot <- san_data %>%
  filter(watershed == "Mortandad") %>% # there is only one sample in 2009 - not worth including on plot
  ggplot(aes(x = factor(year(sample_date)), y = report_result)) +
  geom_boxplot(outlier.shape = NA, fill = "#40e0d0", fatten = 1) + 
  ylim(0, 4) + 
  xlab(NULL) + 
  ylab(expression(atop("Dissolved"~"Lead",~"Concentration"~"("*mu*"g/L)"))) +
  ggtitle("Mortandad watershed") + 
  theme_bw() + 
  theme(plot.margin = margin(0,0.2,0.2,0.2, "cm"))

# Combine map and boxplot as a two-panel plot and save 
combo_san_pb <- plot_grid(san_pb_map, san_pb_boxplot, mortandad_pb_boxplot, nrow = 3, align = 'v', rel_heights = c(1.2, 0.7, 0.7))

ggsave(combo_san_pb, filename = "Figures/San_Lead_combo_wWS.pdf", height = 12, width = 11.2, useDingbats = FALSE)

rm(combo_san_pb, san_pb_boxplot, mortandad_pb_boxplot, san_pb_map, san_data, plotdat, san_subwatersheds)

####################################################

# Sandia Zinc
################################

# IP
ipdata_san <- ipdata %>%
  filter(parameter_name == "Zinc" & field_preparation_code == "F" & 
           (watershed == "Sandia" | watershed == "Mortandad") & 
           (sample_date >= "2015-01-01" & sample_date <= "2018-12-31")) %>%
  mutate(datasource = "IP") %>%
  mutate(DetData = paste(detect_flag, datasource))

# Gage
gagedata_san <- gagedata %>%
  filter(parameter_name == "Zinc" & field_preparation_code == "F" & 
           (watershed == "Sandia" | watershed == "Mortandad") &
           (sample_date >= "2005-01-01" & sample_date <= "2018-12-31")) %>%
  mutate(datasource = "Gage") %>%
  mutate(DetData = paste(detect_flag, datasource)) 

# Base
base_san <- WS_data %>%
  filter(parameter_name == "Zinc" & field_preparation_code == "F" & 
           (watershed == "Sandia" | watershed == "Mortandad" | watershed == "Rio Grande") &
           (sample_date >= "2005-01-01" & sample_date <= "2018-12-31")) %>%
  mutate(datasource = "Base") %>%
  mutate(DetData = paste(detect_flag, datasource)) 

# Bind dataframes
san_data <- bind_rows(ipdata_san, gagedata_san, base_san)
rm(ipdata_san, gagedata_san, base_san)

# Create a new filtered dataframe of data to plot
# Percentile
percentiles <- san_data %>%
  filter(detect_flag == "Y") %>%
  summarize(result = list(enframe(quantile(report_result, probs = c(0.1, 0.5, 0.9))))) %>%
  unnest()

plotdat <- san_data %>%
  filter(detect_flag == "Y") %>%
  group_by(location_id_alias, northing, easting) %>%
  summarize(median = median(report_result), sd = sd(report_result)) %>%
  filter(!is.na(sd)) %>%
  mutate(percentile = 
           case_when(
             median < percentiles$value[1] ~ "0 - 7.3 (ug/L), Below 10th percentile",
             median >= percentiles$value[1] & median < percentiles$value[2] ~ "7.3 - 18.7 (ug/L), 10-50th percentile",
             median >= percentiles$value[2] & median < percentiles$value[3] ~ "18.7 - 47.5 (ug/L), 50-90th percentile", 
             median >= percentiles$value[3] ~ "47.5 - 76.6 (ug/L), Above 90th percentile"))

# Add correct levels to water quality standards
plotdat$percentile <- factor(plotdat$percentile, levels = c("0 - 7.3 (ug/L), Below 10th percentile",
                                                            "7.3 - 18.7 (ug/L), 10-50th percentile",
                                                            "18.7 - 47.5 (ug/L), 50-90th percentile",
                                                            "47.5 - 76.6 (ug/L), Above 90th percentile"))
# Select subwatersheds to plot
san_subwatersheds <- subwatersheds_f %>%
  filter(NAME %in% c("Sandia Canyon", "Canada del Buey", "Mortandad Canyon", "Ten-Site Canyon", "Cedro Canyon"))

# Make the plots!
##########################################

# Sandia/Mort Zinc map
san_zn_map <- ggplot() +
  geom_polygon(data = san_subwatersheds, aes(long, lat, group = group, fill = NAME), color = "black") + 
  scale_fill_manual(values = c("Sandia Canyon" = "grey80", "Canada del Buey" = "grey55",
                               "Mortandad Canyon" = "grey35", "Ten-Site Canyon" = "grey20",
                               "Cedro Canyon" = "grey90")) + 
  guides(fill = FALSE) + # prevents there from being a legend for subwatershed
  geom_path(data = drainage_f, aes(long, lat, group = group), color = "dodgerblue") + 
  geom_point(data = plotdat, aes(easting, northing, color = percentile), size = 3) + 
  scale_color_manual(values = c("0 - 7.3 (ug/L), Below 10th percentile" = "#8bbef3", 
                                "7.3 - 18.7 (ug/L), 10-50th percentile" = "#67b44c", 
                                "18.7 - 47.5 (ug/L), 50-90th percentile" = "#fdd422",
                                "47.5 - 76.6 (ug/L), Above 90th percentile" = "#fa5c18"), drop = FALSE) +
  geom_point(data = plotdat, aes(easting, northing), shape = 1, color = "black", size = 3) +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  coord_equal(xlim = c(min(plotdat$easting), 1665000), 
              ylim = c(min(plotdat$northing) +15500, max(plotdat$northing + 700))) +
  ylab(NULL) + 
  xlab(NULL) +
  annotation_scale(location = "br", width_hint = 0.4, plot_unit = "ft", unit_category = "imperial") +
  annotation_north_arrow(location = "bl", which_north = "true", style = north_arrow_fancy_orienteering) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), axis.text = element_blank(),
        legend.position = "top", legend.box = "vertical",
        legend.key.size = unit(0.4, 'cm'),
        legend.margin = margin(0,0,-0.25,0, unit = 'cm'),
        plot.margin = margin(0,0.2,0,0.2, "cm"))

# Sandia Zinc boxplot
san_zn_boxplot <- san_data %>%
  filter(watershed == "Sandia") %>% # there is only one sample in 2009 - not worth including on plot
  ggplot(aes(x = factor(year(sample_date)), y = report_result)) +
  geom_boxplot(outlier.shape = NA, fill = "#40e0d0", fatten = 1) + 
  ylim(0, 80) + 
  xlab(NULL) + 
  ylab(expression(atop("Dissolved"~"Zinc",~"Concentration"~"("*mu*"g/L)"))) +
  ggtitle("Sandia watershed") + 
  theme_bw() + 
  theme(plot.margin = margin(0,0.2,0.2,0.2, "cm"))

# Mortandad Zinc boxplot
mortandad_zn_boxplot <- san_data %>%
  filter(watershed == "Mortandad") %>% # there is only one sample in 2009 - not worth including on plot
  ggplot(aes(x = factor(year(sample_date)), y = report_result)) +
  geom_boxplot(outlier.shape = NA, fill = "#40e0d0", fatten = 1) + 
  ylim(0, 80) + 
  xlab(NULL) + 
  ylab(expression(atop("Dissolved"~"Zinc",~"Concentration"~"("*mu*"g/L)"))) +
  ggtitle("Mortandad watershed") + 
  theme_bw() + 
  theme(plot.margin = margin(0,0.2,0.2,0.2, "cm"))

# Combine map and boxplot as a two-panel plot and save 
combo_san_zn <- plot_grid(san_zn_map, san_zn_boxplot, mortandad_zn_boxplot, nrow = 3, align = 'v', rel_heights = c(1.2, 0.7, 0.7))

ggsave(combo_san_zn, filename = "Figures/San_Zinc_combo_wWS.pdf", height = 12, width = 11.2, useDingbats = FALSE)

rm(combo_san_zn, san_zn_boxplot, mortandad_zn_boxplot, san_zn_map, san_data, plotdat, san_subwatersheds)

############################################
#############################################
# Ancho Canyon
#####################################

# Ancho Total PCB

# IP
ipdata_anc <- ipdata %>%
  filter(parameter_name == "Total PCB" & field_preparation_code == "UF" & 
           (watershed == "Ancho")) %>%
  mutate(datasource = "IP") %>%
  mutate(DetData = paste(detect_flag, datasource))

# Gage
gagedata_anc<- gagedata %>%
  filter(parameter_name == "Total PCB" & field_preparation_code == "UF" & 
           (watershed == "Ancho") &
           sample_date >= "2005-01-01") %>%
  mutate(datasource = "Gage") %>%
  mutate(DetData = paste(detect_flag, datasource)) 

# Base
base_anc <- WS_data %>%
  filter(parameter_name == "Total PCB" & field_preparation_code == "UF" & 
           (watershed == "Ancho" | watershed == "Rio Grande") &
           sample_date >= "2005-01-01") %>%
  mutate(datasource = "Base") %>%
  mutate(DetData = paste(detect_flag, datasource)) 

# Bind dataframes
anc_data <- bind_rows(ipdata_anc, gagedata_anc, base_anc)
rm(ipdata_anc, gagedata_anc, base_anc)

# Create a new filtered dataframe of data to plot
# Percentile
percentiles <- anc_data %>%
  filter(detect_flag == "Y") %>%
  summarize(result = list(enframe(quantile(report_result, probs = c(0.1, 0.5, 0.9))))) %>%
  unnest()

plotdat <- anc_data %>%
  filter(detect_flag == "Y") %>%
  group_by(location_id_alias, northing, easting) %>%
  summarize(median = median(report_result), sd = sd(report_result)) %>%
  filter(!is.na(sd)) %>%
  mutate(percentile = 
           case_when(
             median < percentiles$value[1] ~ "0 - 0.0006 (ug/L), Below 10th percentile",
             median >= percentiles$value[1] & median < percentiles$value[2] ~ "0.0006 - 0.003 (ug/L), 10-50th percentile",
             median >= percentiles$value[2] & median < percentiles$value[3] ~ "0.003 - 3.16 (ug/L), 50-90th percentile", 
             median >= percentiles$value[3] ~ "3.16 - 3.23 (ug/L), Above 90th percentile"))

# Add correct levels to water quality standards
plotdat$percentile <- factor(plotdat$percentile, levels = c("0 - 0.0006 (ug/L), Below 10th percentile",
                                                            "0.0006 - 0.003 (ug/L), 10-50th percentile",
                                                            "0.003 - 3.16 (ug/L), 50-90th percentile",
                                                            "3.16 - 3.23 (ug/L), Above 90th percentile"))
# Select subwatersheds to plot
anc_subwatersheds <- subwatersheds_f %>%
  filter(NAME %in% c("Ancho Canyon", "North Ancho Canyon"))


# Make the plots!
##########################################

# Ancho PCB map
anc_pcb_map <- ggplot() +
  geom_polygon(data = anc_subwatersheds, aes(long, lat, group = group, fill = NAME), color = "black") + 
  scale_fill_manual(values = c("Ancho Canyon" = "grey80", "North Ancho Canyon" = "grey55")) + 
  guides(fill = FALSE) + # prevents there from being a legend for subwatershed
  geom_path(data = drainage_f, aes(long, lat, group = group), color = "dodgerblue") + 
  geom_point(data = plotdat, aes(easting, northing, color = percentile), size = 3) + 
  scale_color_manual(values = c("0 - 0.0006 (ug/L), Below 10th percentile" = "#8bbef3", 
                                "0.0006 - 0.003 (ug/L), 10-50th percentile" = "#67b44c", 
                                "0.003 - 3.16 (ug/L), 50-90th percentile" = "#fdd422",
                                "3.16 - 3.23 (ug/L), Above 90th percentile" = "#fa5c18"), drop = FALSE) +
  geom_point(data = plotdat, aes(easting, northing), shape = 1, color = "black", size = 3) +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  coord_equal(xlim = c(min(plotdat$easting) - 2000, 1650000), 
              ylim = c(min(plotdat$northing) - 4200, max(plotdat$northing + 700))) +
  ylab(NULL) + 
  xlab(NULL) +
  annotation_scale(location = "tr", width_hint = 0.4, plot_unit = "ft", unit_category = "imperial") +
  annotation_north_arrow(location = "bl", which_north = "true", style = north_arrow_fancy_orienteering) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), axis.text = element_blank(),
        legend.position = "top", legend.box = "vertical",
        legend.key.size = unit(0.4, 'cm'),
        legend.margin = margin(0,0,-0.25,0, unit = 'cm'),
        plot.margin = margin(0,0.2,0,0.2, "cm"))

# Ancho PCB boxplot
anc_pcb_boxplot <- anc_data %>%
  ggplot(aes(x = factor(year(sample_date)), y = report_result)) +
  geom_boxplot(outlier.shape = NA, fill = "#40e0d0", fatten = 1) + 
  ylim(0, 4) + 
  xlab(NULL) + 
  ylab(expression(atop("Total"~"PCB",~"Concentration"~"("*mu*"g/L)"))) +
  ggtitle("Ancho watershed") + 
  theme_bw() + 
  theme(plot.margin = margin(0,0.2,0.2,0.2, "cm"))


# Combine map and boxplot as a two-panel plot and save 
combo_anc_pcb <- plot_grid(anc_pcb_map, anc_pcb_boxplot, nrow = 2, align = 'v', rel_heights = c(1.2, 0.5))

ggsave(combo_anc_pcb, filename = "Figures/Ancho_PCB_combo_wWS.pdf", height = 9, width = 8, useDingbats = FALSE)

rm(combo_anc_pcb, anc_pcb_boxplot, anc_pcb_map, anc_data, plotdat, anc_subwatersheds)


##################################################
#################################################

# Ancho Mercury

# IP
ipdata_anc <- ipdata %>%
  filter(parameter_name == "Mercury" & field_preparation_code == "UF" & 
           (watershed == "Ancho")) %>%
  mutate(datasource = "IP") %>%
  mutate(DetData = paste(detect_flag, datasource))

# Gage
gagedata_anc<- gagedata %>%
  filter(parameter_name == "Mercury" & field_preparation_code == "UF" & 
           (watershed == "Ancho") &
           sample_date >= "2005-01-01") %>%
  mutate(datasource = "Gage") %>%
  mutate(DetData = paste(detect_flag, datasource)) 

# Base
base_anc <- WS_data %>%
  filter(parameter_name == "Mercury" & field_preparation_code == "UF" & 
           (watershed == "Ancho" | watershed == "Rio Grande") &
           sample_date >= "2005-01-01") %>%
  mutate(datasource = "Base") %>%
  mutate(DetData = paste(detect_flag, datasource)) 

# Bind dataframes
anc_data <- bind_rows(ipdata_anc, gagedata_anc, base_anc)
rm(ipdata_anc, gagedata_anc, base_anc)

# Create a new filtered dataframe of data to plot
# Percentile
percentiles <- anc_data %>%
  filter(detect_flag == "Y") %>%
  summarize(result = list(enframe(quantile(report_result, probs = c(0.1, 0.5, 0.9))))) %>%
  unnest()

plotdat <- anc_data %>%
  filter(detect_flag == "Y") %>%
  group_by(location_id_alias, northing, easting) %>%
  summarize(median = median(report_result), sd = sd(report_result)) %>%
  filter(!is.na(sd)) %>%
  mutate(percentile = 
           case_when(
             median < percentiles$value[1] ~ "0 - 0.2 (ug/L), Below 10th percentile",
             median >= percentiles$value[1] & median < percentiles$value[2] ~ "0.2 - 0.4 (ug/L), 10-50th percentile",
             median >= percentiles$value[2] & median < percentiles$value[3] ~ "0.4 - 1.3 (ug/L), 50-90th percentile", 
             median >= percentiles$value[3] ~ "> 1.3 (ug/L), Above 90th percentile"))

# Add correct levels to water quality standards
plotdat$percentile <- factor(plotdat$percentile, levels = c("0 - 0.2 (ug/L), Below 10th percentile",
                                                            "0.2 - 0.4 (ug/L), 10-50th percentile",
                                                            "0.4 - 1.3 (ug/L), 50-90th percentile",
                                                            "> 1.3 (ug/L), Above 90th percentile"))
# Select subwatersheds to plot
anc_subwatersheds <- subwatersheds_f %>%
  filter(NAME %in% c("Ancho Canyon", "Chaquehui Canyon"))

# Make the plots!
##########################################

# Ancho Mercury map
anc_hg_map <- ggplot() +
  geom_polygon(data = anc_subwatersheds, aes(long, lat, group = group, fill = NAME), color = "black") + 
  scale_fill_manual(values = c("Ancho Canyon" = "grey80", "Chaquehui Canyon" = "grey55")) + 
  guides(fill = FALSE) + # prevents there from being a legend for subwatershed
  geom_path(data = drainage_f, aes(long, lat, group = group), color = "dodgerblue") + 
  geom_point(data = plotdat, aes(easting, northing, color = percentile), size = 3) + 
  scale_color_manual(values = c("0 - 0.2 (ug/L), Below 10th percentile" = "#8bbef3", 
                                "0.2 - 0.4 (ug/L), 10-50th percentile" = "#67b44c", 
                                "0.4 - 1.3 (ug/L), 50-90th percentile" = "#fdd422",
                                "> 1.3 (ug/L), Above 90th percentile" = "#fa5c18"), drop = FALSE) +
  geom_point(data = plotdat, aes(easting, northing), shape = 1, color = "black", size = 3) +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  coord_equal(xlim = c(min(plotdat$easting) - 2000, 1650000), 
              ylim = c(min(plotdat$northing) - 3000, max(plotdat$northing + 700))) +
  ylab(NULL) + 
  xlab(NULL) +
  annotation_scale(location = "tr", width_hint = 0.5, plot_unit = "ft", unit_category = "imperial") +
  annotation_north_arrow(location = "br", which_north = "true", style = north_arrow_fancy_orienteering) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), axis.text = element_blank(),
        legend.position = "top", legend.box = "vertical",
        legend.key.size = unit(0.4, 'cm'),
        legend.margin = margin(0,0,-0.25,0, unit = 'cm'),
        plot.margin = margin(0,0.2,0,0.2, "cm"))


# Ancho PCB boxplot
anc_hg_boxplot <- anc_data %>%
  ggplot(aes(x = factor(year(sample_date)), y = report_result)) +
  geom_boxplot(outlier.shape = NA, fill = "#40e0d0", fatten = 1) + 
  ylim(0, 1.5) + 
  xlab(NULL) + 
  ylab(expression(atop("Total"~"Mercury",~"Concentration"~"("*mu*"g/L)"))) +
  ggtitle("Ancho watershed") + 
  theme_bw() + 
  theme(plot.margin = margin(0,0.2,0.2,0.2, "cm"))


# Combine map and boxplot as a two-panel plot and save 
combo_anc_hg <- plot_grid(anc_hg_map, anc_hg_boxplot, nrow = 2, align = 'v', rel_heights = c(1.2, 0.5))

ggsave(combo_anc_hg, filename = "Figures/Ancho_Mercury_combo_wWS.pdf", height = 9, width = 9, useDingbats = FALSE)

rm(combo_anc_hg, anc_hg_boxplot, anc_hg_map, anc_data, plotdat, anc_subwatersheds)
