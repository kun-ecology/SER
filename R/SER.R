#' Short-period Environmental Regime (SER)
#'
#' @param Env_data: time series environmental data, a dataframe with columns Date and Env, make sure Date is in date format while Env is in numeric format.
#' @param sample_date: sampling dates, a vector containing sampling days. Date for initiation of the experiment (measurements) should be included as the first element of the vector.
#' @param days_bf:  N days before the sampling date (not included); by default, days_bf = c(days between two successive sampling dates)
#'
#' @return a data frame
#' @export
#' @examples
#' # inspect the discharge data
#' str(hydro_df)
#'
#' # make sure name of the column with data (in this case Discharge) is "Env"
#' names(hydro_df)[2] <- "Env"
#'
#' # inspect the sample date data
#' str(sample_date)
#'
#' # calculate short-period hydrological indices
#' hydro_ser <- SER(hydro_df,sample_date,days_bf = NULL)
#' str(hydro_ser)
#' head(hydro_ser)
#'
#'

SER <- function(Env_data,sample_date,days_bf=NULL){
  date.vector <- Env_data$Date
  env.vector <- Env_data$Env
  # construct date interval based on different sample date
  sample_interval <-list() # initiate a list for storing interested time period
  if (is.null(days_bf)){
    for (i in 2:length(sample_date)) {
      # default interested time period: days between two successive sampling dates
      sample_interval[[i-1]] <- c(interval(sample_date[i-1],sample_date[i]-days(1))) }
  } else {
    days_bf <- days_bf
    for (i in 2:length(sample_date)) {
      # days between two successive sampling dates are automatically added to argument days_bf
      days_bf.interval <- interval(sample_date[i]-lapply(days_bf,days) %>% reduce(c), sample_date[i]-days(1))
      sample_interval[[i-1]] <- c(days_bf.interval,interval(sample_date[i-1],sample_date[i]-days(1)))  }
  }

  #construct env data based on sample_interval
  Env_df.ls <- lapply(sample_interval,function(x){
    lapply(x,function(i){
      env.vector[date.vector %within% i]
    })
  })

  #set names to Env_df data
  names(Env_df.ls) <- as.character(sample_date[-1])
  if (length(Env_df.ls[[1]])==1){
    Env_df.ls <- lapply(Env_df.ls,function(x)set_names(x,"BetwSamT"))
  } else {
    Env_df.ls <- lapply(Env_df.ls,function(x)set_names(x,c(paste0("Bfor_",days_bf,"d"),"BetwSamT")))
  }

  ####calculate 10%, 25%, 75% and 95% percentiles of entire Env_data
  percentiles <- quantile(env.vector, probs = c(0.1,0.25,0.75,0.9), type = 6, names = F)
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
    row.names(SER_res)= c("MA1","MA2", "MA3", "MA4", "ML1", "MH1", "EL1","EH1", "RC", "RL1",  "RH1")
    SER_res$Value <- round(SER_res$Value , digits = 3)
    return(SER_res)

  }
  SER_res <- lapply(Env_df.ls,function(x)lapply(x,calc_SER)) %>%
    lapply(function(x)do.call(rbind,x)) %>%
    do.call(rbind,.)
  SER_res_f <- mutate(SER_res,SampleDate=stringr::word(row.names(SER_res),1,sep="\\."),
                      SER_Indice=substr(row.names(SER_res),12,nchar(row.names(SER_res))))
  SER_res_f <- SER_res_f[,c(2,3,1)] # rearrange dataframe
  SER_res_f$SER_Indice <- factor(SER_res_f$SER_Indice,levels = unique(SER_res_f$SER_Indice))
  SER_res_f <- spread(SER_res_f, SER_Indice, Value) #convert data to wide data format
  row.names(SER_res_f) <- NULL
  return(SER_res_f)
}


