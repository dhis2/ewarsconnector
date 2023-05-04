#' Title
#'
#' @param filepath
#'
#' @return
#' @export
#'
ewars_parse_config <- function(filepath = "config.yaml") {
  yaml::yaml.load_file(filepath)
}


#' Title
#'
#' @param config_path
#' @param config_path_level
#' @param username
#' @param password
#' @param base_url
#' @param d2_session_name
#' @param d2_session_envir
#'
#' @return
#' @export
#'
ewars_login_dhis2 <- function(
    config_path = NULL,
    config_path_level = "dhis",
    username = NULL,
    password = NULL,
    base_url = NULL,
    d2_session_name = "d2_default_session",
    d2_session_envir = parent.frame()) {

  datimutils::loginToDATIM(
    config_path = config_path,
    config_path_level = config_path_level,
    username = username,
    password = password,
    base_url = base_url,
    d2_session_name = d2_session_name,
    d2_session_envir = d2_session_envir)
}


ewars_get_org_units <- function(ou_level,
                                d2_session = dynGet("d2_default_session")) {
  url <- paste0(d2_session$base_url,
                "api/organisationUnits?filter=level:eq:",
                ou_level,
                "&fields=id,name,code&paging=false")

  r <- httr::GET(url,
                 handle = d2_session$handle)

  if (r$status_code == 200L) {
    r %>%
      httr::content("text") %>%
      jsonlite::fromJSON(r) %>%
      purrr::pluck("organisationUnits") %>%
      purrr::map_dfr(tibble::as_tibble) %>%
      tidyr::drop_na()
  }
}

ewars_get_analytics <- function(params,
                            root_ou,
                            hierarchy_level,
                            start_date,
                            end_date,
                            d2_session = dynGet("d2_default_session")) {

  data_label <- names(params)
  this_period_type <- params[[data_label]][["frequency"]]
  this_uid <- params[[data_label]][["uid"]]

  this_period_series <-
    get_period_series(start_date, end_date, this_period_type)

  r <- datimutils::getAnalytics(dx = params[[data_label]][["uid"]],
                           pe = paste(this_period_series, sep = "", collapse = ";"),
                           paste0("dimension=ou:",root_ou, ";LEVEL-", hierarchy_level),
                          d2_session = d2_session)

   r %>% dplyr::mutate(year = substr(Period, 0, 4),
                       ewars_label = names(params),
                       period_type = params[[data_label]][["frequency"]]) %>%
     dplyr::rename(dx = `Data`,
                   period = `Period`,
                   ou_uid = `Organisation unit`,
                   value = `Value`)


}




#' Title
#'
#' @param ewars_config
#' @param start_date
#' @param end_date
#' @param d2_session
#'
#' @importFrom magrittr %>%
#' @return
#' @export
#'

ewars_fetch_data <- function(ewars_config,
                             start_date = NULL,
                             end_date = lubridate::today(),
                             d2_session = dynGet("d2_default_session",
                                                 inherits = TRUE)) {


  if (is.null(end_date)) {
    stop("End date cannot be NULL")
  }

  if (is.null(start_date)) {
    start_date <- get_start_date_from_end_date(end_date, previous_years = ewars_config$previous_years)
  }

  #Get a map of organisation units
  org_units <-
    ewars_get_org_units(ewars_config$hierarchy_level, d2_session = d2_session) %>%
    dplyr::rename(ou_name = name,
                  ou_uid = id,
                  ou_code = code) %>%
    dplyr::mutate(ou_numeric_code = stringr::str_extract(ou_name, "^\\d{2}"),
                  ou_name = stringr::str_extract(ou_name, "(?<=\\s).*$"))

  d <- purrr::map_dfr(ewars_config$data,
                      function(x)
                        ewars_get_analytics(
                          x,
                          root_ou = ewars_config$root_ou,
                          hierarchy_level = ewars_config$hierarchy_level,
                          start_date = start_date,
                          end_date = end_date,
                          d2_session
                        ))

  #Separate out the yearly and weekly data
  d_weekly <- d %>% dplyr::filter(period_type == "weekly")
  all_weeks <- d_weekly %>% dplyr::select(year, period) %>%
    dplyr::distinct()

  d_yearly <- d %>% dplyr::filter(period_type == "yearly") %>%
    dplyr::select(-period) %>%
    dplyr::full_join(all_weeks, by = "year")

  #Recombine and reshape

  d <- dplyr::bind_rows(d_weekly, d_yearly) %>%
    dplyr::select(ou_uid,
                  period,
                  ewars_label,
                  value) %>%
    tidyr::pivot_wider(names_from = ewars_label,
                       values_from = value) %>%
  tidyr::separate(period, into = c("year", "week"), sep="W") %>%
  dplyr::left_join(org_units, by = "ou_uid") %>%
    dplyr::select(year,
                  district = ou_numeric_code,
                  population,
                  week,
                  weekly_hospitalised_cases,
                  rainsum,
                  meantemperature
                  ) %>%
    dplyr::mutate(district = stringr::str_replace(district, "^0", "")) %>%
    dplyr::arrange(year, district, week)

  return(d)
}


