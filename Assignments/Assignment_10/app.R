library(leaflet)
library(shiny)
library(bslib)
library(tidyverse)

df <- read_csv("./1950-2022_all_tornadoes.csv")

torn_names <- c(number_so_far = 'om',
                year = 'yr',
                month = 'mo',
                day = 'dy',
                date = 'date',
                time = 'time',
                timezone = 'tz',
                state_abbr = 'st',
                state_fips = 'stf',
                state_number = 'stn',
                f_scale = 'mag',
                injuries = 'inj',
                fatalities = 'fat',
                property_loss = 'loss',
                crop_loss = 'closs',
                starting_latitude = 'slat',
                starting_longitude = 'slon',
                ending_latitude = 'elat',
                ending_longitude = 'elon',
                length_miles = 'len',
                width_yards = 'wid',
                num_states_effected = 'ns',
                entire_track = 'sn',
                segment_number = 'sg',
                county_1 = 'f1',
                county_2 = 'f2',
                county_3 = 'f3',
                county_4 = 'f4',
                edited_county = 'fc')

df <- rename(df, all_of(torn_names)) %>%
    mutate(row_id = row_number())

# Convert from wide to long format and then back to wide format

df <- df %>%
    pivot_longer(cols = starts_with('county'),
                 names_to = 'county_number',
                 values_to = 'county_fips',
                 names_prefix = "county_") %>%
    filter(county_fips != 0)

getColor <- function(category) {
    case_when(
        category == 0 ~ "blue",
        category == 1 ~ "green3",
        category == 2 ~ "orange",
        category == 3 ~ "darkorange3",
        category == 4 ~ "red3",
        TRUE ~ "gray"
    )
}

choices <- unique(df$year)

shinyApp(
    ui = fluidPage(
        selectInput("year", "Year:",
                    choices=choices),
        leafletOutput("testMap")
    ),
    server = function(input, output) {
        output$testMap = renderLeaflet({
            df_year <- df %>%
                filter(year == as.numeric(input$year))

            m <- leaflet() %>%
                addTiles()

            for (i in 1:nrow(df_year)) {
                m <- m %>%
                    addPolylines(lng=c(
                        df_year$starting_longitude[i],
                        df_year$ending_longitude[i]),
                        lat=c(
                            df_year$starting_latitude[i],
                            df_year$ending_latitude[i]),
                        color = getColor(df_year$f_scale[i]),
                        weight = 2,
                        opacity = 1)
            }

            m
        })
    }
)

