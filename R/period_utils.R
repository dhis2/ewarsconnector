get_iso_week_label <- function(date) {
  paste0(lubridate::isoyear(date),"W",sprintf("%02d",lubridate::isoweek(date)))
}

get_month_label <- function(date) {
  paste0(lubridate::year(date),sprintf("%02d",lubridate::month(date)))
}

get_iso_week_series <- function(start_date, end_date) {
  wks <- seq(start_date, end_date, by = '1 week')
  get_iso_week_label(wks)
}

get_month_series <- function(start_date, end_date) {
  mnths <- seq(start_date, end_date, by = '1 month')
  get_month_label(mnths)
}

get_year_series <- function(start_date, end_date){
  lubridate::year(seq(start_date, end_date, by = '1 year'))
}

get_period_series <- function(start_date, end_date, period_type) {
   switch(period_type,
          "yearly" = get_year_series(start_date, end_date),
          "monthly" = get_month_series(start_date, end_date),
          "weekly" = get_iso_week_series(start_date, end_date),
          stop("That period type is not supported."))
}

get_start_date_from_end_date <- function(end_date = lubridate::today(), previous_years) {
  end_date - lubridate::years(previous_years)
}
