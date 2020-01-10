## ADD STEP, formatting planting data with lubridate
# CHECK data sets 7, 8, 20, 25, 26, 42, 69, 72.
surv_data %>% map(
  function(x) { x %>% mutate(
    date_formated = parse_date_time(`Planting Date`, orders = c("ymd_HMS", "ymd", "dmy"))
  )
  }
)
# we need to load the rainfall data as well
### PLANTING DATES NOT IMPORTING CORRECTLY
rain_data <- read_csv("data/converted/rainfall-data-updated.csv")
rain_data <- rain_data %>% mutate(
  date_formatted = parse_date_time(planting_date, orders = c("dmy_HM", "dmy"))
)
