pacman::p_load(tidyverse, rvest, xml2, zoo)

weather_data <- 2015:2017 %>%
lapply(function(x){

    ## note that there ~60 days that could not be returned from the database
    raw_dat <- sprintf('https://www.wunderground.com/history/airport/KGAI/%s/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=%s&req_city=&req_state=&req_statename=&reqdb.zip=&reqdb.magic=&reqdb.wmo=', x, x) %>%
        read_html()

    raw_dat %>%
        html_table() %>%
        `[[`(2) %>% 
        `[`(, c(1, 3, 20)) %>%
        setNames(c('base', 'temp', 'percp')) %>% #View()
        mutate(
            month2 = gsub('\\d|\\s|[^ -~]', '', base),
            month2 = ifelse(month2 == '', NA, month2),
            month2 = repeat_last(month2),
            year = x
        ) %>%
        rename(day = base) %>%
        as_tibble() %>%
        left_join(data_frame(month2 = substring(month.name, 1, 3), month = factor(month.name, levels = month.name)), by = 'month2') %>%
        mutate_at(vars(temp, percp), as.numeric) %>%
        select(-month2) %>%
        dplyr::filter(grepl('^\\d{1,2}$', day)) %>%
        mutate(
            date = paste(
                year, 
                stringi::stri_pad_left(as.integer(month), 2, '0'), 
                stringi::stri_pad_left(day, 2, '0'), 
                sep = '-'
            ) %>%
                as.Date()
        ) 

}) %>%
     bind_rows()

range(weather_data$date) %>% diff()

repeat_last <- function(x, forward = TRUE, maxgap = Inf, na.rm = FALSE) {
    if (!forward) x = rev(x)           # reverse x twice if carrying backward
    ind = which(!is.na(x))             # get positions of nonmissing values
    if (is.na(x[1]) && !na.rm)         # if it begins with NA
        ind = c(1,ind)                 # add first pos
    rep_times = diff(                  # diffing the indices + length yields how often
        c(ind, length(x) + 1) )          # they need to be repeated
    if (maxgap < Inf) {
        exceed = rep_times - 1 > maxgap  # exceeding maxgap
        if (any(exceed)) {               # any exceed?
            ind = sort(c(ind[exceed] + 1, ind))      # add NA in gaps
            rep_times = diff(c(ind, length(x) + 1) ) # diff again
        }
    }
    x = rep(x[ind], times = rep_times) # repeat the values at these indices
    if (!forward) x = rev(x)           # second reversion
    x
}

saveRDS(weather_data, 'Montgomery_County_Weather.RDS')


