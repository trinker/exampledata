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
#'   \item wholesale. The price Wegmans pays for the item
#'   \item old_price. The price Wegmans was charging before the price change
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
#'   \item REGIONID. Region code
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
#' @format A data frame with 4,665 rows and 37 variables
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
#'   \item `Driver City`. City of the driver's home address.
#'   \item `Driver State`. State of the driver's home address.
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
#'         hour_of_day = `Time Of Stop` %>%
#'             strptime(format="%H:%M:%S") %>%
#'             format(format = '%H') %>%
#'             as.numeric(),
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


#' Cereal Nutrition
#' 
#' A dataset containing nutritional information on popular cereals.
#' 
#' @details 
#' \itemize{ 
#'   \item name. Name of the cereal
#'   \item mfr. Manufacturer of cereal \code{list(A = 'American Home Food Products', 
#'   G = 'General Mills', K = 'Kelloggs', N = 'Nabisco', P = 'Post', Q = 'Quaker Oats',
#'   R = 'Ralston Purina')}
#'   \item type. Cereal type; hot (h) or cold (c)
#'   \item calories. Calories per serving
#'   \item protein. Grams of protei
#'   \item fat. Grams of fat
#'   \item sodium. Milligrams of sodium 
#'   \item fiber. Grams of dietary fiber
#'   \item carbo. Grams of complex carbohydrates
#'   \item sugars. Grams of sugars
#'   \item potass. Milligrams of potassium
#'   \item vitamins. Vitamins and minerals: One of 0, 25, or 100, indicating the typical percentage of FDA recommended
#'   \item shelf. Display shelf (1, 2, or 3, counting from the floor)
#'   \item weight. Weight in ounces of one serving
#'   \item cups. Number of cups in one serving
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name cereal 
#' @author Petra Isenberg, Pierre Dragicevic and Yvonne Jansen
#' @usage data(cereal) 
#' @format A data frame with 77 rows and 15 variables 
#' @references https://www.kaggle.com/crawford/80-cereals
NULL 


#' Joy of Painting Objects
#' 
#' A dataset containing an indicator of which episodes contained various tagged 
#' objects.  Originally used by fivethirtyeight.com and analyzed 
#' https://fivethirtyeight.com/features/a-statistical-analysis-of-the-work-of-bob-ross
#' 
#' @details 
#' \itemize{ 
#'   \item Season. The season number
#'   \item Episode. The episode number
#'   \item Aired. Sate episode aired
#'   \item Title. The title of the episode
#'   \item aurora_borealis. An indicator tag of this event
#'   \item barn. An indicator tag of this event
#'   \item beach. An indicator tag of this event
#'   \item boat. An indicator tag of this event
#'   \item bridge. An indicator tag of this event
#'   \item building. An indicator tag of this event
#'   \item bushes. An indicator tag of this event
#'   \item cabin. An indicator tag of this event
#'   \item cactus. An indicator tag of this event
#'   \item cirrus. An indicator tag of this event
#'   \item cliff. An indicator tag of this event
#'   \item clouds. An indicator tag of this event
#'   \item conifer. An indicator tag of this event
#'   \item cumulus. An indicator tag of this event
#'   \item deciduous. An indicator tag of this event
#'   \item diane_andre. An indicator tag of this event
#'   \item dock. An indicator tag of this event
#'   \item farm. An indicator tag of this event
#'   \item fence. An indicator tag of this event
#'   \item fire. An indicator tag of this event
#'   \item flowers. An indicator tag of this event
#'   \item fog. An indicator tag of this event
#'   \item framed. An indicator tag of this event
#'   \item grass. An indicator tag of this event
#'   \item guest. An indicator tag of this event
#'   \item hills. An indicator tag of this event
#'   \item lake. An indicator tag of this event
#'   \item lakes. An indicator tag of this event
#'   \item lighthouse. An indicator tag of this event
#'   \item mill. An indicator tag of this event
#'   \item moon. An indicator tag of this event
#'   \item mountain. An indicator tag of this event
#'   \item mountains. An indicator tag of this event
#'   \item night. An indicator tag of this event
#'   \item ocean. An indicator tag of this event
#'   \item palm_trees. An indicator tag of this event
#'   \item path. An indicator tag of this event
#'   \item person. An indicator tag of this event
#'   \item portrait. An indicator tag of this event
#'   \item river. An indicator tag of this event
#'   \item rocks. An indicator tag of this event
#'   \item snow. An indicator tag of this event
#'   \item snowy_mountain. An indicator tag of this event
#'   \item steve_ross. An indicator tag of this event
#'   \item structure. An indicator tag of this event
#'   \item sun. An indicator tag of this event
#'   \item tree. An indicator tag of this event
#'   \item trees. An indicator tag of this event
#'   \item waterfall. An indicator tag of this event
#'   \item waves. An indicator tag of this event
#'   \item windmill. An indicator tag of this event
#'   \item winter. An indicator tag of this event
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name bob_ross 
#' @author Walt Hickey
#' @usage data(bob_ross) 
#' @format A data frame with 403 rows and 56 variables 
#' @references https://github.com/fivethirtyeight/data/tree/master/bob-ross
NULL 


#' Tidy Version of Historic Snowfall by Month for Buffalo 
#' 
#' A dataset containing historical monthly snowfall records.  
#' 
#' @details 
#' \itemize{ 
#'   \item Season. The snow season (spans across 2 years)
#'   \item Year. The year within the season
#'   \item Month. The month within the year
#'   \item Decade. The decade attached to the year
#'   \item Snow. The amount of snow in inches that fell
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name buffalo_snow_tidy 
#' @usage data(buffalo_snow_tidy) 
#' @format A data frame with 932 rows and 4 variables 
#' @references https://www.weather.gov/buf/BuffaloSnow
NULL 


#' Dogs of NYC Project 
#' 
#' A dataset containing New York City dog data.
#' 
#' @details 
#' \itemize{ 
#'   \item dog_name. Name of dog
#'   \item gender. Gender of dog
#'   \item breed. Breed of dog
#'   \item birth. Birth date of dog
#'   \item birth_year. Birth year of dog
#'   \item birth_month. Birth month of dog
#'   \item dominant_color. Primary fur color
#'   \item secondary_color. Secondary fur color
#'   \item third_color. Third fur color
#'   \item spayed_or_neutered. Logical indicating if dog has been fixed
#'   \item guard_or_trained. Logical indicating if the dog is trained for gaurd duty or or special area
#'   \item borough. Borough of NYC dog is located in
#'   \item zip_code. Zip code of NYC dog is located in
#'   \item latitude. Latitude of NYC dog is located in
#'   \item longitude. Longitude of NYC dog is located in
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name nyc_dogs 
#' @usage data(nyc_dogs) 
#' @format A data frame with 81,542 rows and 15 variables 
#' @references https://fusiontables.google.com/data?docid=1pKcxc8kzJbBVzLu_kgzoAMzqYhZyUhtScXjB0BQ#rows:id=1
NULL 


#' Cross Tab of Top Breeds and Names from nyc_dogs Data Set
#' 
#' A dataset containing a cross tab of breeds and dog names.
#' 
#' @details 
#' \itemize{ 
#'   \item breed. The breed of the dog
#'   \item Bella. A name
#'   \item Buddy. A name 
#'   \item Charlie. A name 
#'   \item Coco. A name 
#'   \item Daisy. A name 
#'   \item Lola. A name 
#'   \item Lucky. A name 
#'   \item Lucy. A name 
#'   \item Max. A name 
#'   \item Molly. A name 
#'   \item Princess. A name 
#'   \item Rocky. A name 
#'   \item n. Total count of that breed for the \code{nyc_dogs} data set. 
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name breed_name 
#' @usage data(breed_name) 
#' @format A data frame with 10 rows and 14 variables 
#' @references https://fusiontables.google.com/data?docid=1pKcxc8kzJbBVzLu_kgzoAMzqYhZyUhtScXjB0BQ#rows:id=1
NULL 


#' National School Lunch Program (NSLP) 
#' 
#' A dataset containing information about free, reduced and full priced lunches.
#' 
#' @details 
#' \itemize{ 
#'   \item year. Year 
#'   \item free. Number of students getting free lunch
#'   \item reduced. Number of students getting reduced price lunch 
#'   \item full. Number of students paying full price for lunch 
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name lunches 
#' @usage data(lunches) 
#' @format A data frame with 47 rows and 4 variables 
#' @references https://catalog.data.gov/dataset/national-school-lunch-assistance-program-participation-and-meals-served-data
#' @examples \dontrun{
#' library(tidyverse)
#' library(ggrepel)
#' 
#' lunches %>%
#'     gather(type, students, -year) %>%
#'     group_by(year) %>%
#'     mutate(prop = students/sum(students)) %>%
#'     ggplot(aes(year, prop, color = type, group = type)) +
#'         geom_text(
#'             data = lunches %>%
#'                 gather(type, students, -year) %>%
#'                 group_by(year) %>%
#'                 mutate(prop = students/sum(students)) %>%                
#'                 filter(year == 2017),
#'             aes(label = type),
#'             hjust = 0, size = 5
#'         ) +
#'         geom_line(size = 1) +
#'         theme_minimal() +
#'         theme(legend.position = 'none') +
#'         scale_x_continuous(limits = c(1970, 2023), breaks = seq(1970, 2010, by = 10)) +
#'         scale_y_continuous(labels = scales::percent)
#' }
NULL 





#' Join Practice: Sex Based Health Info
#' 
#' A dataset containing the sex based health information for practice with
#' joins.  Tables that work together for join practice include: \code{jp_health},
#' \code{jp_publishers}, & \code{superheroes}.
#' 
#' @details 
#' \itemize{ 
#'   \item sex. male/female
#'   \item life_expectancy. Average life expectancy in 2016 in the U.S.
#'   \item daily_calories. Recommended daily caloric intake
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name jp_health 
#' @usage data(jp_health) 
#' @format A data frame with 2 rows and 3 variables 
NULL 


#' Join Practice: Publisher Info
#' 
#' A dataset containing superhero comic book publisher founding years for 
#' practice with joins.  Tables that work together for join practice include: 
#' \code{jp_health}, \code{jp_publishers}, & \code{superheroes}.
#' 
#' @details 
#' \itemize{ 
#'   \item publisher. The name of the comic book publisher
#'   \item yr_founded. Year the publishing company was founded
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name jp_publishers 
#' @usage data(jp_publishers) 
#' @format A data frame with 3 rows and 2 variables 
#' @author Jenny Bryan
#' @references http://stat545.com/bit001_dplyr-cheatsheet.html
NULL 


#' Join Practice: Superhero Characters
#' 
#' A dataset containing the superhero character information for practice with
#' joins.  Tables that work together for join practice include: \code{jp_health},
#' \code{jp_publishers}, & \code{superheroes}.
#' 
#' @details 
#' \itemize{ 
#'   \item name. The name of the character
#'   \item alignment. Does the character  align with good or bad more often?
#'   \item sex. The sex of the character
#'   \item publisher. The publisher who owns the rights to the character
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name jp_superheroes 
#' @usage data(jp_superheroes) 
#' @format A data frame with 7 rows and 4 variables 
#' @author Jenny Bryan
#' @references http://stat545.com/bit001_dplyr-cheatsheet.html
NULL 



#' Row Bind Practice: 8 Cylinders
#' 
#' A dataset containing a cars data set that can be used for practicing row
#' binding.  A resplit of the \code{mtcars} data set by cylinders with select 
#' columns removed.
#' 
#' @details 
#' \itemize{ 
#'   \item car. The name of the car
#'   \item mpg. Miles per gallon
#'   \item disp. Displacement (cu.in.)
#'   \item hp. Horsepower
#'   \item drat. Rear axle ratio
#'   \item wt. Weight (1000 lbs)
#'   \item am. Transmission (0 = automatic, 1 = manual)
#'   \item gear. Number of forward gears
#'   \item carb. Number of carburetors
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name bp_cyl_8 
#' @usage data(bp_cyl_8) 
#' @format A data frame with 14 rows and 9 variables 
NULL 


#' Row Bind Practice: 6 Cylinders
#' 
#' A dataset containing a cars data set that can be used for practicing row
#' binding.  A resplit of the \code{mtcars} data set by cylinders with select 
#' columns removed.
#' 
#' @details 
#' \itemize{ 
#'   \item car. The name of the car
#'   \item disp. Displacement (cu.in.)
#'   \item hp. Horsepower
#'   \item drat. Rear axle ratio
#'   \item wt. Weight (1000 lbs)
#'   \item vs. V/S
#'   \item am. Transmission (0 = automatic, 1 = manual)
#'   \item gear. Number of forward gears
#'   \item carb. Number of carburetors
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name bp_cyl_6 
#' @usage data(bp_cyl_6) 
#' @format A data frame with 7 rows and 9 variables 
NULL 


#' Row Bind Practice: 4 Cylinders
#' 
#' A dataset containing a cars data set that can be used for practicing row
#' binding.  A resplit of the \code{mtcars} data set by cylinders with select 
#' columns removed.
#' 
#' @details 
#' \itemize{
#'   \item car. The name of the car
#'   \item mpg. Miles per gallon
#'   \item disp. Displacement (cu.in.)
#'   \item hp. Horsepower
#'   \item drat. Rear axle ratio
#'   \item wt. Weight (1000 lbs)
#'   \item qsec 1/4 mile time
#'   \item vs. V/S
#'   \item am. Transmission (0 = automatic, 1 = manual)
#'   \item carb. Number of carburetors
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name bp_cyl_4 
#' @usage data(bp_cyl_4) 
#' @format A data frame with 11 rows and 9 variables 
NULL 

