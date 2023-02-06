#' Short-period Environmental Regime (SER)
#'
#' @importFrom purrr map map2 reduce
#' @importFrom tibble tibble
#' @importFrom dplyr %>% mutate select
#' @importFrom tidyr unnest pivot_longer pivot_wider
#' @importFrom lubridate days years interval
#' @importFrom stats lm quantile sd
#' @param env_date a vector containing dates that a environmental variables were measured, make sure it is in date format before passing it to the function
#' @param env_val a vector containing environmental values for corresponding dates in **env_date**,  make sure it is in numeric format
#' @param sample_date a vector containing days for biological sampling date for initiation of the experiment (measurements) should be included as the first element of the vector, and also make sure it is in date format
#' @param days_bf a numeric vector representing N days (months or years) before the sampling date; if days_bf = NULL (by default), then days_bf = days (months/years) between two successive sampling dates
#' @param type a character vector indicating whether the 'days_bf' is "day", "month", or "year" specific
#' @param include_sample.date TRUE or FALSE (by default), indicates whether environmental data at sample.date is included or not during the calculation
#' @param include_successive TRUE or FALSE (by default), indicates whether environmental data between two successive sampling dates is used for calculating 'BetwSamT' environmental regime
#' @param simplify TRUE (by default) or FALSE, indicates whether the simplified result is returned. Set simplify = TRUE if you want to check the intermediate results
#'
#' @return a data frame with the first column as the sampling date(s), whereas the rest columns are calculated indices.
#' @details SER is extremely useful in linking time-series environmental variables to discrete biological responses. Specifically, SER function summaries
#' time-series environmental variables into indices covering different facets i.e., magnitude, frequency, and rate of change of the data in any focused period,
#' which are typically masked by using simple average or median values in conventional way. In total, 11 elementary indices were developed, and users can developed
#' their own environmental regimes by changing the argument **days_bf**. Besides, it is recommended that users carefully read the vignette and check published studies (Guo et al. 2021, 2022; Wu et al. 2022)
#' that used SER to make most out of your own data set with this package.
#'
#' @export
#'
#' @examples
#'
#' # inspect the discharge data
#' str(hydro_df)
#'
#' # inspect the sample date data
#' str(sample_date)
#'
#'#############
#' #day- specific SER
#' # calculate short-period hydrological indices
#' SER(hydro_df$Date, hydro_df$Discharge, sample_date)
#' SER(hydro_df$Date, hydro_df$Discharge, sample_date, days_bf=c(3,7,14))
#' SER(hydro_df$Date, hydro_df$Discharge, sample_date, days_bf=c(3,7,14), include_sample.date = TRUE)
#' SER(hydro_df$Date, hydro_df$Discharge, sample_date, days_bf=c(3,7,14), include_successive = TRUE)
#' SER(hydro_df$Date, hydro_df$Discharge, sample_date, days_bf=c(3,7,14),
#' include_sample.date = TRUE, include_successive = TRUE)
#'
#'##################
#' # month-specific SER
#' # only include the first and last dates in sample_dates
#' test.date.m <- sample_date[c(1, 13)]
#' SER(hydro_df$Date, hydro_df$Discharge, test.date.m, days_bf = NULL, type = "month")
#' SER(hydro_df$Date, hydro_df$Discharge, test.date.m, days_bf = c(2, 4, 6),
#' type = "month", include_sample.date = TRUE)
#' SER(hydro_df$Date, hydro_df$Discharge, test.date.m, days_bf = c(2, 4, 6),
#'  type = "month", include_successive = TRUE)
#' SER(hydro_df$Date, hydro_df$Discharge, test.date.m, days_bf = c(2, 4, 6),
#' type = "month", include_sample.date = TRUE, include_successive = TRUE)
#'
#'##################
#' # year-specific SER
#' # generate data for testing
#' test.df <- data.frame(Date = seq(ymd("2010-1-1"), ymd("2020-1-1"), by="1 day"),
#'                       Env = sample( hydro_df$Discharge, 3653, replace = TRUE))
#' test.date.y <-  seq(ymd("2012-1-1"), ymd("2020-1-1"), by="2 year")
#' SER(test.df$Date, test.df$Env, test.date.y, days_bf = NULL, type = "year")
#' SER(test.df$Date, test.df$Env, test.date.y, days_bf = c(2),
#' type = "year", include_sample.date = TRUE)
#' SER(test.df$Date, test.df$Env, test.date.y, days_bf = c(2),
#' type = "year", include_successive = TRUE)
#' SER(test.df$Date, test.df$Env, test.date.y, days_bf = c(2), type = "year",
#' include_successive = TRUE, include_sample.date = TRUE)
SER <- function(env_date, env_val, sample_date, days_bf=NULL, type = NULL,
                include_sample.date = FALSE,
                include_successive = FALSE,
                simplify = TRUE){

  # should sample.date be included? by default, NO
  # should days between two successive sample dates be included? by default, NO

  #####################
  # step1
  # initialize type (day, month, year)
  type <- ifelse(is.null(type), "day", type)
  # check if type is one of day, month, or year
  if (!(type %in% c("day", "month", "year")) ){
    stop ("type mush be one of 'day', 'month', 'year' ")
  }

  # type name
  type.nm <- switch(type,
                    "day" = "d",
                    "month" = "m",
                    "year" = "y")

  # type function
  # exclude the sample date itself
  type <- switch(type,
                 "day" = "days",
                 "month" = "months",
                 "year" = "years")
  type.fun <- match.fun(type)



  #####################
  #step2
  # initialize days_bf

  ############
  # step2.1, time intervals between two successive sample dates
  # start date
  tmp1 <- sample_date[-length(sample_date)]

  # end date
  tmp2 <- sample_date[-1]

  if (!include_sample.date){
    tmp2 <- tmp2 - type.fun(1)
  }

  # intervals
  default.intv  <- map2(tmp1, tmp2, ~ interval(.x, .y))
  default.intv <- reduce(default.intv, c)

  names(default.intv) <- rep("BetwSamT", length(tmp2))

  default.intv <- tibble(sample_date = tmp2,
                         regime_indice=rep("BetwSamT", length(tmp1)),
                         date_diff = as.numeric(tmp2 - tmp1),
                         start_date=tmp1,
                         end_date=tmp2,
                         intv=default.intv)
  ########
  # step2.2, construct intervals

  if (is.null(days_bf)) {
    sample_interval <- default.intv

  } else {
    sample_interval <- tibble(sample_date = rep(tmp2, each=length(days_bf)),
                              date_diff = rep(days_bf, length(tmp2)),
    ) %>%
      # start date for each of days_bf
      mutate(start_date = map2(sample_date, date_diff, ~ .x - type.fun(.y -1)) %>% reduce(c)) %>%
      # end date for each days_bf
      mutate(end_date = sample_date) %>%
      # time interval
      mutate(intv=map2(start_date, end_date, ~ interval(.x, .y))%>% reduce(c)) %>%
      # regime indice
      mutate(regime_indice = rep(paste0("Bfor_", days_bf, type.nm), length(tmp2)) ) %>%
      select(sample_date, regime_indice, date_diff, start_date, end_date, intv)

    # # shall default.intv be included
    if (include_successive) {
      sample_interval <- rbind(sample_interval, default.intv)
    }

    # convert sample date back to true sample date
    if (!include_sample.date){
      sample_interval <- sample_interval %>%
        mutate(sample_date = sample_date + type.fun(1))
    }

  }

  ###################
  # step 3.0
  # construct env data based on sample_interval
  env_df.ls <- sample_interval %>%
    mutate(date_df=map(intv, ~ env_date[env_date %within% .x]),
           env_df=map(intv, ~ env_val[env_date %within% .x]))

  # step 3.1
  # calculate 10%, 25%, 75% and 95% percentiles of entire Env_data
  percentiles <- quantile(env_val, probs = c(0.1,0.25,0.75,0.9), type = 6, names = F)
  names(percentiles) <- c("10%",  "25%", "75%",  "90%")


  calc_SER <- function (env.vec) {
    meanFlow <- mean(env.vec)
    medFlow <- quantile(env.vec, probs =0.5)
    MA1 <- meanFlow
    MA2 <- medFlow
    MA3 <- sd(env.vec)/MA1
    MA4 <- MA1/MA2

    N=length(env.vec)
    ML1 <- sum(env.vec<percentiles["25%"])
    MH1 <- sum(env.vec>percentiles["75%"])
    EL1 <- sum(env.vec<percentiles["10%"])
    EH1 <- sum(env.vec>percentiles["90%"])

    change.rate <- rep(0,N-1)
    for (i in 2:(N-1)){
      change.rate[i-1] <- env.vec[i]-env.vec[i-1]
    }
    N.t <- 1:N
    RC <- (summary(lm(env.vec~N.t)))$coefficients[2]
    RL1 <- sum(change.rate<0)
    RH1 <- sum(change.rate>0)

    SER_res <-  data.frame(Value = c(MA1,MA2, MA3, MA4, ML1, MH1, EL1, EH1, RC, RL1, RH1))
    SER_res <- t(SER_res) %>% as.data.frame()
    names(SER_res)= c("MA1","MA2", "MA3", "MA4", "ML1", "MH1", "EL1","EH1", "RC", "RL1",  "RH1")
    return(SER_res)

  }

  # step 4
  res_df <- env_df.ls %>%
    mutate(regime_df=map(env_df, ~ calc_SER(.x)))

  # simplified results
  if (simplify){
    res_df <- res_df %>%
      select(sample_date, regime_indice, regime_df) %>%
      unnest(regime_df) %>%
      pivot_longer(cols = 3:13, names_to = "ind", values_to = "val") %>%
      mutate(regime_indice=paste0(regime_indice, ".", ind)) %>%
      select(-ind) %>%
      pivot_wider(id_cols = sample_date, names_from = regime_indice, values_from = val)
  }

  return(res_df)
}




