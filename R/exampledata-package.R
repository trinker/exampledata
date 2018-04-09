#' Example Data for Teaching
#'
#' Small data sets that are optimized for teaching particular constructs.
#' @docType package
#' @name exampledata
#' @aliases exampledata package-exampledata
NULL

#' A Fake Wegmans Grocery
#'
#' A dataset containing fake grocery related data.
#'
#' @details
#' \itemize{
#'   \item department. The department
#'   \item item. The grocery item
#'   \item last_shipment. The last time the item was shipped to the stor
#'   \item weight. The weight of the item
#'   \item wholesale. The price Weagmans pays for the item
#'   \item old_price. The price Weagmans was charging before the price change
#'   \item new_price. The current price
#'   \item popularity. A customer rating of stars (1-5 stars; with 5 being the most popular)
#'   \item organic. A messy character field describing if the item comes in organic ('yes', 'y'), non-organic ('no', 'n'), or both organic and non-organic
#'   \item product_of_usa. A dummy coded (aka, one-hot encoding) variable (0-no, 1-yes) of whether the item is from the U.S. or not
#' }
#'
#' @docType data
#' @keywords datasets
#' @name wegmans
#' @usage data(wegmans)
#' @format A data frame with 30 rows and 10 variables
NULL

#' Select Variables from Carnegie Data Set
#'
#' A dataset containing Carnegie classification for institutions.
#'
#' @details
#' \itemize{
#'   \item UNITID. A unique identifier
#'   \item NAME. The name of the institution
#'   \item CITY. The city of the institution
#'   \item STABBR. State abbreviation for the institution
#'   \item CONTROL. Control type (e.g., public, private)
#'   \item ICLEVEL. Level of institution (number of years)
#'   \item BASIC2015. Basic classification levels
#'   \item LOCALE. Location (e.g., City Large, Rural Distant)
#'   \item IPGRAD2015. Graduate institution classification (e.g., Research Doctoral: STEM-dominant)
#'   \item ASSOCDEG. Count of associate's degrees
#'   \item BACCDEG. Count of bachelor's degrees
#'   \item MASTDEG. Count of master's degrees
#'   \item DOCRSDEG. Count of doctoral degrees - research/scholarship
#'   \item DOCPPDEG. Count of doctoral degrees - professional practice
#'   \item DOCOTHDEG. Count of doctoral degrees - other
#'   \item TOTDEG. Total degrees conferred
#'   \item HBCU. Historically black college or university flag
#'   \item MSI. Minority serving institution flag
#'   \item WOMENS. Women's college flag
#'   \item MEDICAL. Institution grants a medical degree flag
#'   \item ACTCAT. Final ACT category (1=inclusive; 2=selective; 3=more selective)
#'   \item ROOMS. Total dormitory capacity (campus owned-, operated, or affiliated-housing)
#'   \item FALLENR13. Total Fall 2013 enrollment
#'   \item FALLENR14. Total Fall 2014 enrollment
#'   \item SATV25. SAT-Verbal 25th percentile score
#'   \item SATM25. SAT-Math 25th percentile score
#'   \item SATCMB25. Combined SAT-Math and SAT-Verbal 25th percentils scores
#'   \item UGDSFTF14. Undergraduate degree-seeking full-time enrollment, fall 2014
#'   \item UGDSPTF14. Undergraduate degree-seeking part-time enrollment, fall 2014
#'   \item UGNDFT14. Undergraduate non-degree full-time students, fall 2014
#'   \item UGNDPT14. Undergraduate non-degree part-time students, fall 2014
#'   \item GRFTF14. Graduate full-time enrollment, fall 2014
#'   \item GRPTF14. Graduate part-time enrollment, fall 2014
#'   \item UGN1STTMFT14. Undergraduate new first-time full-time students
#'   \item UGN1STTMPT14. Undergraduate degree-seeking part-time enrollment
#'   \item UGNTRFT14. Undergraduate new transfer-in full-time students
#'   \item UGNTRPT14. Undergraduate new transfer-in part-time students
#' }
#'
#' @docType data
#' @keywords datasets
#' @name carnegie
#' @usage data(carnegie)
#' @format A data frame with 4,665 rows and 36 variables
#' @references \url{http://carnegieclassifications.iu.edu}
NULL


#' A Lookup Table of State Abbreviations and Regions
#'
#' A dataset containing Carnegie region classifications and corresponding states.
#'
#' @details
#' \itemize{
#'   \item ID. An identifier for the region + state
#'   \item Region. A region of states
#'   \item State. A state abbreviation
#' }
#'
#' @docType data
#' @keywords datasets
#' @name region
#' @usage data(region)
#' @format A data frame with 60 rows and 3 variables
#' @references \url{http://carnegieclassifications.iu.edu}
NULL






#' Montgomery County Traffic Violations
#' 
#' A dataset containing traffic violation records from January 1, 2015 to 
#' December 31, 2017.  Taken from: https://data.montgomerycountymd.gov/Public-Safety/Traffic-Violations/4mse-ku6q
#' First read about this data set from: https://www.nytimes.com/2018/01/30/upshot/do-fast-and-furious-movies-cause-a-rise-in-speeding.html
#' 
#' @details 
#' \itemize{ 
#'   \item `Date Of Stop`. Date of the traffic violation.
#'   \item `Time Of Stop`. Time of the traffic violation.
#'   \item `Latitude`. Latitude location of the traffic violation.
#'   \item `Longitude`. Longitude location of the traffic violation.
#'   \item `Accident`. If traffic violation involved an accident.
#'   \item `Belts`. If traffic violation involved a seat belt violation.
#'   \item `Personal Injury`. If traffic violation involved Personal Injury.
#'   \item `Property Damage`. If traffic violation involved Property Damage.
#'   \item `Fatal`. If traffic violation involved a fatality.
#'   \item `Commercial License`. If driver holds a Commercial Drivers License.
#'   \item `HAZMAT`. If the traffic violation involved hazardous materials.
#'   \item `Commercial Vehicle`. If the vehicle committing the traffic violation is a commercial vehicle.
#'   \item `Work Zone`. If the traffic violation was in a work zone.
#'   \item `State`. State issuing the vehicle registration.
#'   \item `VehicleType`. Type of vehicle (Examples: Automobile, Station Wagon, Heavy Duty Truck, etc.)
#'   \item `Year`. Year vehicle was made.
#'   \item `Make`. Manufacturer of the vehicle (Examples: Ford, Chevy, Honda, Toyota, etc.)
#'   \item `Model`. Model of the vehicle.
#'   \item `Color`. Color of the vehicle.
#'   \item `Violation Type`. Violation type. (Examples: Warning, Citation, SERO)
#'   \item `Charge`. Numeric code for the specific charge.
#'   \item `Contributed To Accident`. If the traffic violation was a contributing factor in an accident.
#'   \item `Race`. Race of the driver. (Example: Asian, Black, White, Other, etc.)
#'   \item `Gender`. Gender of the driver (F = Female, M = Male)
#'   \item `Driver City`. City of the driver’s home address.
#'   \item `Driver State`. State of the driver’s home address.
#'   \item `Arrest Type`. Type of Arrest (A = Marked, B = Unmarked, etc.)
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name traffic_violations 
#' @usage data(traffic_violations) 
#' @format A data frame with 1,276,580 rows and 35 variables 
#' @references https://data.montgomerycountymd.gov/Public-Safety/Traffic-Violations/4mse-ku6q
#' @examples 
#' \dontrun{
#' # https://www.nytimes.com/2018/01/30/upshot/do-fast-and-furious-movies-cause-a-rise-in-speeding.html
#' # https://data.montgomerycountymd.gov/Public-Safety/Traffic-Violations/4mse-ku6q
#' 
#' data(traffic_violations)
#' 
#' library(tidyverse)
#' library(chron)
#' library(numform)
#' library(ggrepel)
#' 
#' traffic_violations_clean <- traffic_violations %>%
#'     mutate(
#'         `Date Of Stop` = as.Date(`Date Of Stop`, format = '%m/%d/%Y'),
#'         year_of_stop = format(`Time Of Stop`, format="%Y"),
#'         hour_of_day = as.numeric(format(strptime(`Time Of Stop`, format="%H:%M:%S"), format = '%H')),
#'         day_of_week = factor(weekdays(`Date Of Stop`), levels = numform::constant_weekdays)
#'     ) %>%
#'     mutate_at(
#'         vars(Accident:`Work Zone`, `Contributed To Accident`), 
#'         function(x) {
#'             case_when(
#'                 x == 'No' ~ FALSE,
#'                 x == 'Yes' ~ TRUE,
#'                 TRUE ~ NA
#'             )
#'         }
#'     ) %>%
#'     select(-Geolocation) %>%
#'     extract(VehicleType, c('Vehicle_Type_Code', 'Vehicle_Type'), '(\\d+)\\s+-\\s+([A-M].+)\\s*')
#' 
#' traffic_violations_clean %>%
#'     group_by(day_of_week, hour_of_day) %>%
#'     summarize(Count = n()) %>%
#'     ggplot(aes(hour_of_day, Count, group = 1)) +
#'         geom_line() +
#'         facet_wrap(~day_of_week, ncol = 2) +
#'         scale_x_continuous(breaks = seq(0, 23, by = 2), 
#'             labels = function(x) numform::f_12_hour(x, format = "%I %p")
#'         ) +
#'         scale_y_continuous(labels = f_denom)
#' 
#' 
#' 
#' traffic_violations_clean %>%
#'     group_by(day_of_week, hour_of_day) %>%
#'     summarize(Count = n()) %>%
#'     ungroup() %>%
#'     mutate(weekend = day_of_week %in% c('Sunday', 'Saturday', 'Friday')) %>%
#'     ggplot(aes(hour_of_day, Count, group = day_of_week, color = day_of_week)) +
#'         geom_line(aes(linetype = weekend)) +
#'         scale_x_continuous(
#'             limits = c(-2, 23),
#'             breaks = seq(0, 23, by = 2), 
#'             labels = function(x) numform::f_12_hour(x, format = "%I %p")
#'         ) +
#'         scale_y_continuous(labels = f_denom) +
#'         geom_text_repel(
#'             data = traffic_violations_clean %>%
#'                 count(day_of_week, hour_of_day) %>%
#'                 rename(Count = n) %>%
#'                 filter(hour_of_day == 0),
#'             aes(label = day_of_week),
#'             size = 4,
#'             nudge_x = -1,
#'             segment.color = 'grey80'
#'           ) +
#'           theme(legend.position = 'none')
#' 
#' 
#' traffic_violations_clean %>%
#'     group_by(day_of_week, hour_of_day) %>%
#'     summarize(Count = n()) %>%
#'     ggplot(aes(hour_of_day, Count, group = day_of_week, color = day_of_week)) +
#'         geom_line() +
#'         scale_x_continuous(breaks = seq(0, 23, by = 2), 
#'             labels = function(x) numform::f_12_hour(x, format = "%I %p")
#'         ) +
#'         scale_y_continuous(labels = f_denom) +
#'         coord_polar()
#' 
#' ## Indicates this sample doesn't typically contain DUI/DWI
#' traffic_violations_clean  %>%
#'     filter(Alcohol) %>%
#'     group_by(day_of_week, hour_of_day) %>%
#'     summarize(Count = n()) %>%
#'     ungroup() %>%
#'     mutate(weekend = day_of_week %in% c('Sunday', 'Saturday', 'Friday')) %>%
#'     ggplot(aes(hour_of_day, Count, group = day_of_week, color = day_of_week)) +
#'         geom_line(aes(linetype = weekend)) +
#'         scale_x_continuous(
#'             limits = c(-2, 23),
#'             breaks = seq(0, 23, by = 2), 
#'             labels = function(x) numform::f_12_hour(x, format = "%I %p")
#'         ) +
#'         scale_y_continuous(labels = f_denom) +
#'         geom_text_repel(
#'             data = traffic_violations_clean  %>%
#'                 filter(Alcohol) %>%
#'                 count(day_of_week, hour_of_day) %>%
#'                 rename(Count = n) %>%
#'                 filter(hour_of_day == 0),
#'             aes(label = day_of_week),
#'             size = 4,
#'             nudge_x = -1,
#'             segment.color = 'grey80'
#'           ) +
#'           theme(legend.position = 'none')
#' }
NULL 


#' Historic Snowfall by Month for Buffalo
#' 
#' A dataset containing historical monthly snowfall records.  Columns are in 
#' character because of T cell values.  The user will want to determine what T
#' means, replace it with an appropriate value, and convert the columns to
#' numeric.
#' 
#' @details 
#' \itemize{ 
#'   \item SEASON. Year span for winter season
#'   \item JUL. Snow fall in this month
#'   \item AUG. Snow fall in this month
#'   \item SEP. Snow fall in this month
#'   \item OCT. Snow fall in this month
#'   \item NOV. Snow fall in this month
#'   \item DEC. Snow fall in this month
#'   \item JAN. Snow fall in this month
#'   \item FEB. Snow fall in this month
#'   \item MAR. Snow fall in this month
#'   \item APR. Snow fall in this month
#'   \item MAY. Snow fall in this month
#'   \item JUN. Snow fall in this month
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name buffalo_snow 
#' @usage data(buffalo_snow) 
#' @format A data frame with 78 rows and 13 variables 
#' @references https://www.weather.gov/buf/BuffaloSnow
#' @examples 
#' \dontrun{
#' library(tidyverse)
#' library(exampledata)
#' library(viridis)
#' 
#' bufsnow <- buffalo_snow %>%
#'     gather(Month, Snow, - SEASON)  %>%
#'     extract(SEASON, c('Start'), '(\\d{4})-\\d{2}', remove = FALSE) %>%
#'     filter(!is.na(Snow)) %>%
#'     mutate(
#'         Month = gsub('(^.)(.+$)', '\\U\\1\\L\\2', Month, perl = TRUE)  %>%
#'             factor(levels = month.abb),
#'         Snow = replace_na(as.numeric(Snow), .01),
#'         Start = as.integer(Start),
#'         Year = case_when(as.integer(Month) %in% 1:6 ~ as.integer(Start + 1), TRUE ~ Start)
#'     ) %>%  
#'     rename(Season = SEASON) %>%
#'     arrange(Season, Year, Month) %>% 
#'     select(Season, Year, Month, Snow)
#' 
#' 
#' bufsnow %>%
#'     ggplot(aes(x = Month, y = Snow, color = as.factor(Year), group = Year)) +
#'         geom_line() +
#'         coord_polar()
#' 
#' 
#' bufsnow %>%
#'     mutate(Month = factor(Month, levels = c(month.abb[c(7:12, 1:6)]))) %>%
#'     ggplot(aes(x = Month, y = Season, fill= Snow)) +
#'     geom_tile() +
#'     scale_fill_viridis()
#' 
#' 
#' bufsnow %>%
#'     group_by(Season) %>%
#'     summarize(total = sum(Snow)) %>%
#'     ggplot(aes(y=total, x=Season, group = 1)) +
#'         geom_line(size = 1) +
#'         geom_point() +
#'         theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust =1))
#' 
#' }
NULL 
