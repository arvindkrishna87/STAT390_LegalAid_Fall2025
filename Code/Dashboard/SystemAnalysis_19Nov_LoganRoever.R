## Create regression predictive model for queue wait times 

# load necessary data and packages
library(tidyverse)


# USER INPUT REQUIRED - CHANGE TO YOUR PATHWAY 
car_data <- read_csv(file = "/Users/loganroever/Desktop/stat390.nosync/STAT390_LegalAid_Fall2025/Internal work of Teams/Logan/car_data.csv")

# pull calls that had a callback retry
callbackretries <- car_data %>% 
  filter(activity_name == "CallbackRetry") %>% 
  semi_join(car_data, join_by(contact_session_id == contact_session_id))

example1 <- car_data %>% filter(contact_session_id == "02f3875b-ceae-4836-9786-1daf78f2841b")

example2 <- car_data %>% filter(contact_session_id == "0bfab117-b496-45c8-9ead-31a176aa6a95")

example3 <- car_data %>% filter(contact_session_id == "111cf36c-81b0-4c05-9915-2cf503ad064c")

example4 <- car_data %>% filter(contact_session_id == "11d0533f-93f8-4c56-bd15-e6100b62eb24")

example5 <- car_data %>% filter(contact_session_id == "150adf07-4d5e-4057-844b-8a69e10edfbc")
