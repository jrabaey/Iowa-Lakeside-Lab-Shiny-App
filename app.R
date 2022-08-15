#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#'
#'This Shiny App will display buoy data for Lake Okoboji 2015-2021, and Big Spirit Lake 
#'for 2020-2021. 

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(mosaic)
library(rLakeAnalyzer)
library(bslib)
library(showtext)
library(thematic)
library(grid)
library(rvest)
library(httr)
source("oxygen_heatmap.R")


# Setup the bslib theme object
my_theme <- bs_theme(bootswatch = "cerulean")

thematic_shiny(font = "auto")

#' Read in Data
#Live Data
my_session <- session("https://wqdatalive.com/login")
log_in_form <- html_form(my_session)[[1]]
fill_form <- html_form_set(log_in_form,
                        `_username` = "matthew-fairchild@uiowa.edu",
                        `_password` = "Itsbuoytime")
fill_form$fields[[4]]$name <- "button"
session_submit(my_session, fill_form)

##Okoboji
data_live_ok <- session_jump_to(my_session,
                                "https://wqdatalive.com/project/470/dashboard/downloaddata?paramIds%5B%5D=72291&paramIds%5B%5D=72292&paramIds%5B%5D=72293&paramIds%5B%5D=72294&paramIds%5B%5D=72295&paramIds%5B%5D=72296&paramIds%5B%5D=72297&paramIds%5B%5D=72298&paramIds%5B%5D=72299&paramIds%5B%5D=72300&paramIds%5B%5D=72301&paramIds%5B%5D=72302&paramIds%5B%5D=72303&paramIds%5B%5D=72304&paramIds%5B%5D=72305&paramIds%5B%5D=72306&paramIds%5B%5D=72307&paramIds%5B%5D=72308&paramIds%5B%5D=72309&paramIds%5B%5D=72310&paramIds%5B%5D=72311&paramIds%5B%5D=72312&paramIds%5B%5D=72313&paramIds%5B%5D=72314&paramIds%5B%5D=72315&paramIds%5B%5D=72316&paramIds%5B%5D=72317&paramIds%5B%5D=72318&paramIds%5B%5D=72319&paramIds%5B%5D=72320&paramIds%5B%5D=72321&paramIds%5B%5D=72322&paramIds%5B%5D=72323&paramIds%5B%5D=72324&paramIds%5B%5D=72325&paramIds%5B%5D=72326&paramIds%5B%5D=72328&paramIds%5B%5D=72329&paramIds%5B%5D=72330&paramIds%5B%5D=72331&paramIds%5B%5D=72332&paramIds%5B%5D=72333&paramIds%5B%5D=72663&paramIds%5B%5D=72664&paramIds%5B%5D=72667&paramIds%5B%5D=72669&downloadOption=data&timeRange=last_month"
)

data_live_ok <- content(data_live_ok$response)
names(data_live_ok) <- data_live_ok[2,]
data_live_ok <- data_live_ok[-c(1,2),] %>% janitor::clean_names() %>% rename(
  do_25 = "do_at_25m_mg_l", temp_25 = "temperature_f", dosat_25 = "do_sat_percent",
  do_23 = "do_at_23m_mg_l", temp_23 = "temperature_f_2", dosat_23 = "do_sat_percent_2",
  do_21 = "do_at_21m_mg_l", temp_21 = "temperature_f_3", dosat_21 = "do_sat_percent_3",
  do_19 = "do_at_19m_mg_l", temp_19 = "temperature_f_4", dosat_19 = "do_sat_percent_4",
  do_17 = "do_at_17m_mg_l", temp_17 = "temperature_f_5", dosat_17 = "do_sat_percent_5",
  do_15 = "do_at_15m_mg_l", temp_15 = "temperature_f_6", dosat_15 = "do_sat_percent_6",
  do_13 = "do_at_13m_mg_l", temp_13 = "temperature_f_7", dosat_13 = "do_sat_percent_7",
  do_11 = "do_at_11m_mg_l", temp_11 = "temperature_f_8", dosat_11 = "do_sat_percent_8",
  do_9 = "do_at_9m_mg_l", temp_9 = "temperature_f_9", dosat_9 = "do_sat_percent_9",
  do_7 = "do_at_7m_mg_l", temp_7 = "temperature_f_10", dosat_7 = "do_sat_percent_10",
  do_5 = "do_at_5m_mg_l", temp_5 = "temperature_f_11", dosat_5 = "do_sat_percent_11",
  do_3 = "do_at_3m_mg_l", temp_3 = "temperature_f_12", dosat_3 = "do_sat_percent_12",
  do_1 = "odo_at_surface_mg_l", temp_1 = "temperature_at_surface_f", dosat_1 = "odo_sat_at_surface_percent",
  temp_air = "air_temperature_f", datetime = "parameter_timestamp"
) %>% mutate(datetime = as.POSIXct(datetime, format = "%m-%d-%Y %H:%M:%S"))
data_live_ok[,-1] <- sapply(data_live_ok[,-1], as.numeric )
###change depth temps to celcius
data_live_ok[,grepl("temp_.+", colnames(data_live_ok))] <- lapply(
  data_live_ok[,grepl("temp_.+", colnames(data_live_ok))], 
  function(x) (x-32)*5/9)
### make temp and do data long
data_live_ok <- data_live_ok %>% select(-c(wind_speed_mph, wind_direction_deg, temp_air, temperature_f_13,
  rel_barometric_pressure_in_hg, p_h_at_surface, sp_cond_at_surface_u_s_cm)) %>% pivot_longer(cols = !datetime,
  names_to = c("Var", "Depth_m"),
  names_sep = "_") %>% pivot_wider(names_from = "Var", values_from = "value") %>% mutate(Depth_m = as.numeric(Depth_m)) %>%
  merge(mutate(data_live_ok[,c("datetime", "wind_speed_mph", "wind_direction_deg", "temp_air",
                        "rel_barometric_pressure_in_hg", "p_h_at_surface", "sp_cond_at_surface_u_s_cm")],
               Depth_m = 1), all = TRUE) %>%
  rename(
    do_sat = "dosat", air_temp = "temp_air", ph = "p_h_at_surface", barometric_pressure = "rel_barometric_pressure_in_hg",
    sp_cond = "sp_cond_at_surface_u_s_cm", wind_speed = "wind_speed_mph", wind_direction = "wind_direction_deg"
  ) %>% mutate(
    wind_speed = wind_speed*0.44704, barometric_pressure = barometric_pressure*33.86
  )
data_live_ok <- data_live_ok[with(data_live_ok, order(datetime, Depth_m)), ]

## Big Spirit
data_live_bs <- session_jump_to(my_session,
                                "https://wqdatalive.com/project/470/dashboard/downloaddata?paramIds%5B%5D=35467&paramIds%5B%5D=35468&paramIds%5B%5D=35469&paramIds%5B%5D=35470&paramIds%5B%5D=35471&paramIds%5B%5D=35479&paramIds%5B%5D=35480&paramIds%5B%5D=35484&paramIds%5B%5D=35485&paramIds%5B%5D=37836&paramIds%5B%5D=37837&paramIds%5B%5D=37838&paramIds%5B%5D=37839&paramIds%5B%5D=41338&paramIds%5B%5D=41413&paramIds%5B%5D=41414&paramIds%5B%5D=41415&paramIds%5B%5D=41416&paramIds%5B%5D=41417&paramIds%5B%5D=41418&paramIds%5B%5D=41419&paramIds%5B%5D=41420&paramIds%5B%5D=41421&paramIds%5B%5D=41422&paramIds%5B%5D=41423&paramIds%5B%5D=41424&paramIds%5B%5D=41425&paramIds%5B%5D=41426&paramIds%5B%5D=41427&paramIds%5B%5D=50320&paramIds%5B%5D=50321&paramIds%5B%5D=50322&paramIds%5B%5D=50323&paramIds%5B%5D=50324&paramIds%5B%5D=50325&paramIds%5B%5D=50326&paramIds%5B%5D=50327&paramIds%5B%5D=50328&downloadOption=data&timeRange=last_month"
)

data_live_bs <- content(data_live_bs$response)
names(data_live_bs) <- data_live_bs[2,]
data_live_bs <- data_live_bs[-c(1,2),] %>% janitor::clean_names() %>% rename(
  do_6 = "do_at_20_ft_mg_l", temp_6 = "temperature_at_20_ft_f", dosat_6 = "do_sat_at_20_ft_percent",
  do_5 = "do_at_16_ft_mg_l", temp_5 = "temperature_at_16_ft_f", dosat_5 = "do_sat_at_16_ft_percent",
  do_4 = "do_at_13_ft_mg_l", temp_4 = "temperature_at_13_ft_f", dosat_4 = "do_sat_at_13_ft_percent",
  do_3 = "do_at_10_ft_mg_l", temp_3 = "temperature_at_10_ft_f", dosat_3 = "do_sat_at_10_ft_percent",
  do_2 = "do_at_6_ft_mg_l", temp_2 = "temperature_at_6_ft_f", dosat_2 = "do_sat_at_6_ft_percent",
  do_1 = "odo_at_surface_mg_l", temp_1 = "water_temperature_at_surface_f", dosat_1 = "odo_sat_at_surface_percent",
  temp_air = "air_temperature_f", datetime = "parameter_timestamp"
) %>% mutate(datetime = as.POSIXct(datetime, format = "%m-%d-%Y %H:%M:%S"))
data_live_bs[,-1] <- sapply(data_live_bs[,-1], as.numeric )
### change depth temps to celcius
data_live_bs[,grepl("temp_.+", colnames(data_live_bs))] <- lapply(
  data_live_bs[,grepl("temp_.+", colnames(data_live_bs))], 
  function(x) (x-32)*5/9)
### make temp and do data long
data_live_bs <- data_live_bs %>% select(-c(sp_cond_at_surface_u_s_cm, p_h_at_surface, rel_barometric_pressure_in_hg,
  temp_air, wind_direction_deg, wind_speed_mph, bga_phycocyanin_rfu_at_surface_rfu,
  bga_pc_at_surface_ug_l, chlorophyll_rfu_at_surface_rfu, chlorophyll_at_surface_ug_l,
  photosynthetically_active_radiation_above_surface_umol_s_m2, heading_deg,
  tp_dpd_wave_period_sec, dominant_wave_direction_deg, mean_wave_direction_mwd_deg,
  hmax_wave_height_m, h10_wave_height_m, roll_deg, pitch_deg, hs_wave_height_m)) %>% 
  pivot_longer(cols = !datetime,
    names_to = c("Var", "Depth_m"),
    names_sep = "_") %>% pivot_wider(names_from = "Var", values_from = "value") %>%
  merge(mutate(data_live_bs[,c("datetime", "sp_cond_at_surface_u_s_cm", "p_h_at_surface", "rel_barometric_pressure_in_hg",
    "temp_air", "wind_direction_deg", "wind_speed_mph", "bga_phycocyanin_rfu_at_surface_rfu",
    "bga_pc_at_surface_ug_l", "chlorophyll_rfu_at_surface_rfu", "chlorophyll_at_surface_ug_l",
    "photosynthetically_active_radiation_above_surface_umol_s_m2", "heading_deg",
    "tp_dpd_wave_period_sec", "dominant_wave_direction_deg", "mean_wave_direction_mwd_deg",
    "hmax_wave_height_m", "h10_wave_height_m", "roll_deg", "pitch_deg", "hs_wave_height_m")],
    Depth_m = 1), all = TRUE) %>%
  rename(
    do_sat = "dosat", air_temp = "temp_air", ph = "p_h_at_surface", 
    par = "photosynthetically_active_radiation_above_surface_umol_s_m2",
    sp_cond = "sp_cond_at_surface_u_s_cm", bga_pc_rfu = "bga_phycocyanin_rfu_at_surface_rfu",
    bga_pc = "bga_pc_at_surface_ug_l", chl = "chlorophyll_at_surface_ug_l",
    chl_rfu = "chlorophyll_rfu_at_surface_rfu", wind_direction = "wind_direction_deg",
    wind_speed = "wind_speed_mph", hs_wave_height = "hs_wave_height_m",
    hmax_wave_height = "hmax_wave_height_m", mean_wave_direction = "mean_wave_direction_mwd_deg",
    wave_period = "tp_dpd_wave_period_sec"
  ) %>% mutate(
    wind_speed = wind_speed*0.44704
  )

data_live <- list(ok = data_live_ok,
                  bs = data_live_bs)

# Past Data
ok <- rbind(read_csv("Okoboji_Data_1.csv"),
  read_csv("Okoboji_Data_2.csv"),
  read_csv("Okoboji_Data_3.csv"),
  read_csv("Okoboji_Data_4.csv")) %>% 
  mutate(datetime = as.POSIXct(datetime))

data <- list(ok = ok,
             bs = read_csv("Big_Spirit_Data.csv") %>% 
               mutate(datetime = as.POSIXct(datetime)))

# Yearly Summary for Okoboji
summer <- read_csv("yearly_summary.csv")

# Lists for variable choices for dropdown menus
variable_choices <- list(ok = c("Dissolved Oxygen (mg/L)" = "do",
                                "Oxygen Saturation (%)" = "do_sat",
                                "Temperature (C)" = "temp",
                                "Air Temperature (C)" = "air_temp",
                                "Barometric Pressure (mbar)" = "barometric_pressure",
                                "Relative Humidity (%)" = "humidity",
                                "pH" = "ph",
                                "Daily Rain (mm)" = "rain_daily",
                                "Rain Intesity (mm/hr)" = "rain_inten",
                                "Specific Conductivity (uS/cm)" = "sp_cond",
                                "Wet Bulb Temperature (C)" = "wet_bulb_temp",
                                "Max Wind Speed (m/s)" = "max_wind_speed",
                                "Wind Direction (Degrees)" = "wind_dir",
                                "Wind Speed (m/s)" = "wind_speed"),
                         bs = c("Dissolved Oxygen (mg/L)" = "do",
                                "Oxygen Saturation (%)" = "do_sat",
                                "Temperature (C)" = "temp",
                                "Air Temperature (C)" = "air_temp",
                                "pH" = "ph",
                                "PAR" = "par",
                                "Specific Conductivity" = "sp_cond",
                                "Blue-Green Algae (Relative Fluorescence)" = "bga_pc_rfu",
                                "Blue-Green Algae (ug/L)" = "bga_pc",
                                "Chlorophyll (ug/L)" = "chl",
                                "Chlorophyll (Relative Fluorescence)" = "chl_rfu",
                                "Wind Direction (Degrees)" = "wind_direction",
                                "Wind Speed (m/s)" = "wind_speed",
                                "Wave Height (m)" = "hs_wave_height",
                                "Max Wave Height (m)" = "hmax_wave_height",
                                "Wave Direction (Degrees)" = "mean_wave_direction",
                                "Wave Period (s)" = "wave_period"))
wind_temp_choice <- c("Air Temp (C)" = "air_temp", "Wind Speed (m/s)" = "wind_speed")
depletion_var_choice <- c("Anoxia Duration (Days)" = "anoxia_duration",
                          "Average Spring Air Temp (C)" = "mean_spring_temp"
                          )


# Define UI for application
ui <- navbarPage(
  title = strong("Lakeside Lab: Monitoring Buoy Data"),
  theme = my_theme,
  footer = p(strong("Contact:"), "Iowa Lakeside Laboratory (lakesidelab@uiowa.edu) or visit the", a("Lakeside homepage.",
    href = "https://iowalakesidelab.org/"), "for more information.",
    hr()),
  tabPanel("Home",
    fluidRow(
      headerPanel(div(
        style = "margin-left:-30px;margin-top:-30px;",img(src='okoboji_pic.jpg', height=100, width=1199)
      )),
      h1("Monitoring Buoys at the Iowa Lakeside Laboratory",
         align = "center")
    ),
    br(),
    fluidRow(
      column(3,
        br(),
        br(),
        wellPanel(h5("Want to see live data?"),
          br(),
          p("Check out the",
            a("WQ Data Live",
              href = "https://wqdatalive.com/public/470"),
            "site for live streaming data from West Okoboji and Big Spirit Lakes, or check out the",
            strong('Live Data Stream'), "tab.")
        )
      ),
      column(9,
        h3(div("Water Quality Monitoring for West Okoboji and Big Spirit Lakes", style = "color:black")),
        br(),
        p("In 2015, members of the Iowa Lakeside Laboratory deployed a buoy on West Okoboji Lake, fully equipped with instruments to gather near-constant water quality
          and weather data. As part of the", a("GLEON", href = "https://gleon.org/"),
          "(Global Lake Ecological Network) organization, this data adds to a growing number of similar monitoring stations
           around the globe, helping scientists understand how to best preserve our natural aquatic resources.
           In 2020, a second buoy was placed on Big Spirit Lake. With hundreds of new data points every 10 minutes,
           the buoys provide valuable data for not only scientists and researchers, but also fishermen and recreational lake users.
           With this interactive dashboard you can explore all of the collected buoy data from both West Okoboji and Big Spirit. Check out the",
          strong("'Past Data Explorer'"), "tab to explore data back to 2015, or the", strong("'Using the Data'"), "tabs to learn how some of the aquatic data is used."),
        p("For more information on other projects and events happening at the Iowa Lakeside lab, check out the",
          a("Lakeside homepage.", 
            href = "https://iowalakesidelab.org/"))
      )
    ),
    fluidRow(
      column(9,
        h3(div("About the Buoys", style = "color:black")),
        p("Instrumented monitoring buoys are becoming mainstream research tools for aquatic scientists. From small lakes to the ocean,
           monitoring buoys can take continuous measurements of numerous variables, leading to valuable long-term datasets.
           The buoys deployed on Big Spirit and West Okoboji are powered by a NexSens CB-450 monitoring platform, with a data logger equipped with cellular telemetry,
           to transmit live data to the web. Each buoy is equipped with a weather station to monitor wind, rain, and air temperature. beneath the buoy are multiple
           sensors that monitor parameters in the surface water, including pH, conductivity, and chlorophyll concentrations. Dissolved Oxygen 
          and temperature are monitored throughout the water column from the surface to the bottom of each lake, with sensors hung on a chain beneath the buoy.
           The buoy on Big Spirit Lake is also equipped with a wave sensor, to record the direction, height, and frequency of waves on the lake.
           All these sensors lead to hundreds of data points being collected every 10 minutes, giving researchers a powerful tool to study physical, chemical, and biological processes happening withing the lakes.")
      ),
      column(3,
        br(),
        br(),
        img(src='buoy_pic.jpg', width=275, align="right")
      )
    ),
    fluidRow(
      column(4,
        br(),
        img(src='lake_map.jpg', width=350, align="left")
      ),
      column(8,
        h3(div("About the Iowa Great Lakes", style = "color:black")),
        p("The Iowa Great Lakes are a group of natural lakes in northwestern Iowa. 
        A well loved natural resource, there are seven lakes in the region totaling over 12,000 acres! The three largest lakes of the group are Big Spirit Lake, 
        East Okoboji Lake, and the pristine West Okoboji Lake. They are the largest natural lakes in the state of Iowa, with the largest, Big Spirit Lake, over 5,600 acres in size.
        These glacial lakes were formed over 13,500 years ago as glaciers pressed south into Iowa. 
        Despite their close proximity, Big Spirit Lake and West Okoboji are two very different ecosystems, and by monitoring both we can see how these lakes function differently.
        Big Spirit Lake is a large, shallow lake with a maximum depth of 22 ft (7m), and an average depth of just 15 ft
        (see", a("Big Spirit Map", href = "https://www.iowadnr.gov/Portals/idnr/uploads/fish/maps/SPL30.pdf"),
        "for a detailed view). In contrast, West Okoboji has a maximum depth of 136 feet (41 m), and is characterized by clean and clear water (",
        a("Okoboji Map", href = "https://www.iowadnr.gov/Portals/idnr/uploads/fish/maps/wok30.pdf"),
        "). The physical differences between these two lakes leads to many differences in everything from water chemistry to fish habitat. Check out the buoy data to see how many differences you can find!")
      )
    ),
    hr(),
  ),
  
  # Data exploration tab
  tabPanel("Past Data Explorer",
    fluidRow(h2("Explore Past Data", align = "center"),
      br(),
      br(),
      p("Explore past buoy data from West Okoboji (2015-2021) and Big Spirit Lake (2020-2021). 
        Use the inputs below to select which lake, variable and time period you want to view.
        For Dissolved Oxygen, Oxygen Saturation, and Water Temperature, you can select which depth
        (in meters) to view. To add a smoothed trendline to the data, check the trendline box.")
    ),
    br(),
    hr(),
    #Dropdown Menus to select Lake, Year, variable
    fluidRow(
      column(4,
        #Lake
        selectInput("lake", "Lake:",
          choices = c("West Okoboji" = "ok",
            "Big Spirit" = "bs"),
          selected = "West Okoboji"
        ),
        #Variable
        selectInput(
          "var", "Variable:", choices = variable_choices[["ok"]]
        )
      ),
      column(4,
        #Depth
        selectInput(
          "depth", "Depth (m) (If Oxygen or Temp):",
          choices = sort(unique(data[["ok"]]$depth))
        ),
        #Year
        selectInput(
          "year", "Year:",
          choices = c(unique(data[["ok"]]$year))
        )
      ),
      column(4,
        # Date Range     
        dateRangeInput(
          "dates", "Select Dates (default is whole year):"
        ),
        checkboxInput(
          "line", "Add Smoothed Trendline", value = FALSE
        )
      ),
      
      hr(),
      
      # Show a plot of the timeseries
      fluidRow(
        plotOutput("timeseries")
      ),
      hr()
    )
  ),
  navbarMenu("Using the Data",
    # Lake stratification Tab
    tabPanel("Stratification",
      fluidRow(
        h2("Lake Stratification", align = "center")
      ),
      br(),
      fluidRow(column(8,
        p("After ice melts in the spring, warming air temperatures and the increasing sunshine heat
           up the surfaces of lakes. Since warmer water is less dense than cold water, the warm surface water
           soon starts to separate and not mix with the colder water beneath. This results in two distinct layers in deep lakes during the summer months:
           a warm surface layer, followed by a transition layer to a cold bottom layer. If you have ever jumped in a lake and gotten down deep enough to feel some frigid water, 
          then you have experienced this! This separation of layers is called", em("stratification."), "This stratification makes lakes a unique environment, as the cold deep layer is
           shut off from the atmosphere above, and soon becomes a different habitat compared to the surface layer. The bottom layer can be drained of oxygen throughout the summer, as oxygen does not mix in from the surface layer.
          Can you see the difference in the two layers with the temperature and oxygen data below? Are there differences between the shallow Big Spirit Lake and the deeper West Okoboji Lake? Select a lake and year to create
          a heat map of either temperature or oxygen.")
        ),
        column(4,
          fluidRow(
            img(src='lake_strat.jpg', width=350, height = 200, align="right")
          ),
          fluidRow(
            p("Image source:", em("International Institute for Sustainable Development"))
          )
        )
      ),
      #Dropdown Menus to select Lake, Year, variable
      hr(),
      fluidRow(h5("Temperature"),
        column(3,
          #Lake
          selectInput("lake_temp", "Lake:",
            choices = c("West Okoboji" = "ok",
              "Big Spirit" = "bs"),
            selected = "West Okoboji"
          ),
          #Year
          selectInput(
            "year_temp", "Year:",
            choices = c(unique(data[["ok"]]$year))
          ),
          # Date Range     
          dateRangeInput(
            "dates_temp", "Select Dates (default is whole year):"
          ),
          # Check bok for thermocline
          checkboxInput(
            "line_temp", "Add Thermocline", value = FALSE
          )
        ),
        column(9,
          plotOutput("temp_strat")
        )
      ),
      hr(),
      fluidRow(h5("Oxygen"),
        column(3,
          #Lake
          selectInput("lake_ox", "Lake:",
            choices = c("West Okoboji" = "ok",
              "Big Spirit" = "bs"),
            selected = "West Okoboji"
          ),
          #Year
          selectInput(
            "year_ox", "Year:",
            choices = c(unique(data[["ok"]]$year))
          ),
          # Date Range     
          dateRangeInput(
            "dates_ox", "Select Dates (default is whole year):"
          ),
          # Check bok for thermocline
          checkboxInput(
            "line_ox", "Add Thermocline", value = FALSE
          )
        ),
        column(9,
               plotOutput("ox_strat")
        )
      ),
      hr(),
      fluidRow(h5("Strength of Stratification"),
        p("The strength of stratification increases as the density difference (temperature difference) between layers increases.
          We can quantify the strength of stratification with metrics like", em("Schmidt Stability"),
          "How does the stratification strength change throughout the summer? Also, how do factors such as air temperature and wind play a part?")
      ),
      fluidRow(
        column(3,
          #Lake
          selectInput("lake_schmidt", "Lake:",
            choices = c("West Okoboji" = "ok",
              "Big Spirit" = "bs"),
              selected = "West Okoboji"
          ),
          #Year
          selectInput(
            "year_schmidt", "Year:",
            choices = c(unique(data[["ok"]]$year))
          ),
          br(),
          #Variable
          selectInput(
            "var_schmidt", "Driver Variable:",
            choices = wind_temp_choice
          )
        ),
        column(9, 
          splitLayout(
            plotOutput("schmidt"),
            plotOutput("schmidt_temp"),
            cellWidths = "50%"
          )
        )
      ),
      hr()
    ),
    tabPanel("Oxygen Depletion",
      fluidRow(h2("Summer Oxygen Depletion", align = "center"),
        br(),
        p("Deep stratified lakes like West Okoboji can lose oxygen in the deep waters 
          throughout the summer. This deep water is cut off from the surface, and oxygen 
          lost cannot be replenished (To look at stratification data, check out the", strong("'Stratification'"),
          "tab). When oxygen in the deep water layer eventually runs out, the lack of oxygen is called",
          em("Anoxia"), ", meaning the absence of oxygen. Most fish species need at least 5 mg of oxygen per liter of water, and the lack of oxygen
          can lead to fish moving up into warmer water, or even in some cases fish kills. Select a year below to see the rate of oxygen depleting and the
          duration of the anoxic period in the bottom waters of West Okoboji Lake.")
      ),
      hr(),
      fluidRow(
        column(2,
          #Year
          selectInput(
            "year_ox_dep", "Year:",
            choices = c("2016" = "2016",
                        "2017" = "2017",
                        "2019" = "2019",
                        "2020" = "2020",
                        "2021" = "2021")
          )
        ),
        column(5,
          plotOutput("oxygen_depletion")
        ),
        column(5,
          plotOutput("anoxia_duration")  
        )
      ),
      hr(),
      fluidRow(
        column(4,
          p("The duration of stratification during the summer can influence the duration of anoxia, and can also be heavily influenced
            by spring and summer temperatures. See how these variables relate across different years below."),
          #Variable
          selectInput(
            "var_depletion_x", "Variable:",
            choices = depletion_var_choice
          )
          
        ),
        column(5,
          plotOutput("depletion_regression")  
        )
      ),
      hr()
    )
  ),
  tabPanel("Live Data Stream",
    fluidRow(
      h2("Live Data", align = "center"),
      br(),
      p("View live data from both the West Okoboji and Big Spirit buoys (up to one month ago though current).
        Oxygen and Temperature data are shown as heatmaps across all depths."),
      p("Uncertainty and potential for error can be associated with environmental monitoring data. This data is not cleaned or filtered, and sensor errors can be common. 
      Data users are cautioned to consider carefully the provisional nature of the information before using it for 
        decisions that concern personal or public safety or the conduct of business that involves substantial monetary or operational consequences. 
        This data is streamed from", a("WQ Data Live", href = "https://wqdatalive.com/public/470"),
        ", check out their site for more information.")
    ),       
    hr(),
    fluidRow(
      column(3,
        #Lake
        selectInput("lake_live_data", "Lake:",
          choices = c("West Okoboji" = "ok",
            "Big Spirit" = "bs"),
          selected = "West Okoboji"
        ),
        #Variable
        selectInput(
          "var_live_data", "Variable:", choices = c("Dissolved Oxygen (mg/L)" = "do",
          "Oxygen Saturation (%)" = "do_sat",
          "Temperature (C)" = "temp",
          "Air Temperature (C)" = "air_temp",
          "Barometric Pressure (mbar)" = "barometric_pressure",
          "pH" = "ph",
          "Specific Conductivity (uS/cm)" = "sp_cond",
          "Wind Speed (m/s)" = "wind_speed")
        ),
        # Date Range     
        dateRangeInput(
          "dates_live_data", "Select Dates (up to the past month):"
        )
      ),
      column(9,
        plotOutput("live_data")
      )
    ),
    hr()
  )
)

# Define server logic 
server <- function(input, output, session) {
  
  # Make drop-down choice of Year, Variable, and Depth dependent upon user input of "Lake"
  observeEvent(input$lake, {
    updateSelectInput(session, "year", 
                      choices = unique(data[[input$lake]]$year)
    )
    updateSelectInput(session,"var", 
                      choices = variable_choices[[input$lake]]
    )
    updateSelectInput(session, "depth", 
                      choices = c(sort(unique(data[[input$lake]]$depth)))
    )
  })
  
  # Update choice based on variable
  observeEvent(input$var, {
    if(input$var != "do" & input$var != "do_sat" & input$var != "temp"){
      updateSelectInput(session,"depth", choices = NA)
    }
    else{
      updateSelectInput(session, "depth", choices = c(unique(data[[input$lake]]$depth)))
    }
  })
  
  #update date choices based on year selected
  observeEvent(input$year, {
    updateDateRangeInput(session, "dates",
      start = as.Date(paste(input$year, "-01-01", sep = "")),
      end = as.Date(paste(input$year, "-12-31", sep = "")),
      min = as.Date(paste(input$year, "-01-01", sep = "")),
      max = as.Date(paste(input$year, "-12-31", sep = ""))
    )
  })
  
  
  output$timeseries <- renderPlot({
    
    #generate time series plot based on selected variables
    data <- data[[input$lake]]
    data <- filter(data, datetime >= input$dates[1] & datetime <= input$dates[2])
    if(input$var == "do" | input$var == "do_sat" | input$var == "temp"){
      data <- filter(data, depth == input$depth)
    }
    
    data <- data %>% select("datetime", input$var) %>% na.omit() 
    x <- data[["datetime"]]
    y <- data[[input$var]]
    
    # draw the plot
    ggplot(data = data, aes(x = datetime, y = y)) + geom_point() + 
      labs(x = "Date", y = 
             names(
               variable_choices[[input$lake]][which(variable_choices[[input$lake]] == input$var)])
      ) + theme_minimal(base_size = 23) + theme(axis.text = element_text(size = 17)) + 
      scale_x_datetime(breaks = scales::breaks_pretty(10)) + 
      if(input$line){
        stat_smooth( size = 1.7)
      }
  })
  
  #Temp Stratification
  # Make drop-down choice of Year, Variable, and Depth dependent upon user input of "Lake"
  observeEvent(input$lake_temp, {
    updateSelectInput(session, "year_temp", 
                      choices = unique(data[[input$lake_temp]]$year)
    )
  })
  #update date choices based on year selected
  observeEvent(input$year_temp, {
    updateDateRangeInput(session, "dates_temp",
                         start = as.Date(paste(input$year_temp, "-01-01", sep = "")),
                         end = as.Date(paste(input$year_temp, "-12-31", sep = "")),
                         min = as.Date(paste(input$year_temp, "-01-01", sep = "")),
                         max = as.Date(paste(input$year_temp, "-12-31", sep = ""))
    )
  })
  
  output$temp_strat <- renderPlot({
    data <- data[[input$lake_temp]]
    data <- filter(data, datetime >= input$dates_temp[1] & datetime <= input$dates_temp[2])
    if(format(data$datetime[1], "%Y") == 2019){
      data <- filter(data, depth != 11)
    }
    if(format(data$datetime[1], "%Y") == 2021){
      data <- filter(data, depth != 7)
    }
    #plot 
    profile_heatmap(data, depth_col = "depth", var_col = "temp", wtr_col = "temp",
      var_label = "Temp (C)", thermo = ifelse(input$line_temp, TRUE, FALSE)) 
  })
  
  #Oxygen Stratification
  # Make drop-down choice of Year dependent upon user input of "Lake"
  observeEvent(input$lake_ox, {
    updateSelectInput(session, "year_ox", 
                      choices = unique(data[[input$lake_ox]]$year)
    )
  })
  #update date choices based on year selected
  observeEvent(input$year_ox, {
    updateDateRangeInput(session, "dates_ox",
                         start = as.Date(paste(input$year_ox, "-01-01", sep = "")),
                         end = as.Date(paste(input$year_ox, "-12-31", sep = "")),
                         min = as.Date(paste(input$year_ox, "-01-01", sep = "")),
                         max = as.Date(paste(input$year_ox, "-12-31", sep = ""))
    )
  })
  
  output$ox_strat <- renderPlot({
    data <- data[[input$lake_ox]]
    data <- filter(data, datetime >= input$dates_ox[1] & datetime <= input$dates_ox[2])
    if(format(data$datetime[1], "%Y") == 2019){
      data <- filter(data, depth != 11)
    }
    if(format(data$datetime[1], "%Y") == 2021){
      data <- filter(data, depth != 7, depth !=1)
    }
    #plot 
    profile_heatmap(data, depth_col = "depth", var_col = "do", wtr_col = "temp",
                    var_label = "Dissolved \nOxygen (mg/L)", thermo = ifelse(input$line_ox, TRUE, FALSE)) 
  })
  
  #Schmidt Stability
  # Make drop-down choice of Year dependent upon user input of "Lake"
  observeEvent(input$lake_schmidt, {
    updateSelectInput(session, "year_schmidt", 
                      choices = unique(data[[input$lake_schmidt]]$year)
    )
  })
  #Plot of Schmidt stability over time
  output$schmidt <- renderPlot({
    data <- data[[input$lake_schmidt]]
    data <- filter(data, year == input$year_schmidt) %>% 
        select(datetime, schmidt.stability) %>% na.omit()
    x <- data$datetime
    y <- data$schmidt.stability
    #plot 
    ggplot(data = data, aes(x = x, y = y)) + geom_line() + theme_classic(base_size = 18) + 
      labs(y = "Stratification Strength (Schmidt Stability)", x = "Date") + 
      theme(axis.text = element_text(size = 14)) + 
      scale_x_datetime(breaks = scales::breaks_pretty(5))
  })
  # linear regression plot
  output$schmidt_temp <- renderPlot({
    data <- data[[input$lake_schmidt]]
    data <- filter(data, year == input$year_schmidt) %>% 
      select(datetime, air_temp, wind_speed, schmidt.stability)
    
    x <- data[[input$var_schmidt]]
    y <- data$schmidt.stability
    #plot 
    ggplot(data = data, aes(x = x, y = y)) + geom_point() + stat_smooth(method = "lm", size = 2) +
      theme_classic(base_size = 18) + labs(y = "Stratification Strength (Schmidt Stability)", x = names(
        wind_temp_choice[which(wind_temp_choice == input$var_schmidt)])
      ) + theme(axis.text = element_text(size = 14))
  })
  
  #Oxygen Depletion
  #Depletion Rate
  output$oxygen_depletion <- renderPlot({
    stop_date <- data[["ok"]] %>% filter(depth == 25, year == input$year_ox_dep) %>% 
      filter(do <= 0.5) %>% summarize(date = min(datetime))
    data <- data[["ok"]] %>% 
      filter(depth == 25, year == input$year_ox_dep, datetime <= stop_date$date[1]) %>% 
      select(datetime, do) %>% na.omit()
    
    x <- data$datetime
    y <- data$do
    #plot 
    ggplot(data = data, aes(x = x, y = y)) + geom_point() + stat_smooth(method = "lm", size = 2) +
      theme_classic(base_size = 18) + labs(y = "Dissolved Oxygen (mg/L)", x = "Date") + 
      theme(axis.text = element_text(size = 14)) + 
      scale_x_datetime(breaks = scales::breaks_pretty(5)) +
      annotation_custom(grobTree(textGrob(
        paste("Rate of Oxygen Depletion:\n ", round(filter(summer, year == input$year_ox_dep)$depletion_rate,3),
          "mg/L/day"), x=0.5,  y=0.87, hjust=0), gp=gpar(fontsize=15)))
  })
  # Anoxia Duration
  output$anoxia_duration <- renderPlot({
    data <- data[["ok"]] %>% 
      filter(depth == 25, year == input$year_ox_dep) %>% 
      select(datetime, do) %>% na.omit()
    
    x <- data$datetime
    y <- data$do
    #plot 
    ggplot(data = data, aes(x = x, y = y)) + geom_point() + 
      theme_classic(base_size = 18) + labs(y = "Dissolved Oxygen (mg/L)", x = "Date") + 
      theme(axis.text = element_text(size = 14)) + 
      geom_vline(xintercept = filter(summer, 
        year == input$year_ox_dep)$anoxia_start, size = 1.3, color = "red") + 
      scale_x_datetime(breaks = scales::breaks_pretty(5)) +
      geom_vline(xintercept = filter(summer, 
        year == input$year_ox_dep)$anoxia_end, size = 1.3, color = "red") +
        annotation_custom(grobTree(textGrob(
        paste("Duration of Summer \nAnoxia:", round(filter(summer, year == input$year_ox_dep)$anoxia_duration),
          "days"), x=0.55,  y=0.7, hjust=0), gp=gpar(fontsize=15)))
  })
  # Yearly Depletion and stratification variables
  output$depletion_regression <- renderPlot({
    x <- summer[[input$var_depletion_x]]
    y <- summer$strat_duration
    #plot 
    ggplot(data = summer, aes(x = x, y = y)) + geom_point(aes(color = as.character(summer$year)), size = 4) +
      stat_smooth(method = "lm", se = FALSE, size = 2) +
      theme_classic(base_size = 20) + 
      labs(y = "Stratification Duration (Days)", 
        x = names(depletion_var_choice[which(depletion_var_choice == input$var_depletion_x)]),
        color = "Year") + 
      theme(axis.text = element_text(size = 14))
  })
  
  # Live Data
  # Make drop-down choice of Variable and Date dependent upon user input of "Lake"
  observeEvent(input$lake_live_data, {
    if(input$lake_live_data == "ok"){
      updateSelectInput(session,"var_live_data", 
        choices = c("Dissolved Oxygen (mg/L)" = "do",
        "Oxygen Saturation (%)" = "do_sat",
        "Temperature (C)" = "temp",
        "Air Temperature (C)" = "air_temp",
        "Barometric Pressure (mbar)" = "barometric_pressure",
        "pH" = "ph",
        "Specific Conductivity (uS/cm)" = "sp_cond",
        "Wind Speed (m/s)" = "wind_speed"))
    } else{
      updateSelectInput(session,"var_live_data", 
                        choices = variable_choices[["bs"]])
    }
    
    updateDateRangeInput(session, "dates_live_data",
      start = as.Date(as.Date(max(data_live[[input$lake_live_data]]$datetime)) - 7),
      end = as.Date(max(data_live[[input$lake_live_data]]$datetime)),
      min = as.Date(min(data_live[[input$lake_live_data]]$datetime)),
      max = as.Date(max(data_live[[input$lake_live_data]]$datetime))
    )
  })
  
  output$live_data <- renderPlot({
    # generate time series plot based on selected variables
    data <- data_live[[input$lake_live_data]]
    data <- filter(data, datetime >= input$dates_live_data[1] & datetime <= input$dates_live_data[2])
    if(input$var_live_data == "do" | input$var_live_data == "do_sat" | input$var_live_data == "temp"){
      profile_heatmap(data, depth_col = "Depth_m", var_col = input$var_live_data, wtr_col = "temp",
        var_label = names(
          variable_choices[[input$lake_live_data]][which(variable_choices[[input$lake_live_data]] == input$var_live_data)]), 
        thermo = TRUE)
    } else{
      data <- data %>% select("datetime", input$var_live_data) %>% na.omit() 
      x <- data[["datetime"]]
      y <- data[[input$var_live_data]]
      
      # draw the plot
      ggplot(data = data, aes(x = datetime, y = y)) + geom_point() + 
        labs(x = "Date", y = 
          names(
            variable_choices[[input$lake_live_data]][which(variable_choices[[input$lake_live_data]] == input$var_live_data)])
        ) + theme_minimal(base_size = 23) + theme(axis.text = element_text(size = 17)) + 
        scale_x_datetime(breaks = scales::breaks_pretty(10))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)