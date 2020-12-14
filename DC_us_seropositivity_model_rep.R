# Daily Chart: 
# Replication code

# Note:
# IFR = Infection Fatality Ratio

# Load packages
library(readr)
library(ggplot2)
library(lubridate)

# Load data from CDC
cdc <- read_csv("Provisional_COVID-19_Death_Counts_by_Sex__Age__and_Week.csv")

# Reformatting for convenience
cdc$age <- unlist(lapply(cdc$`Age Group`, FUN = function(x) unlist(strsplit(x, " "))[1] ))
cdc$age[cdc$age == "All"] <- "total"
cdc$sex <- ifelse(cdc$Sex == "Female", "f", "m") 
cdc$deaths <- cdc$`COVID-19 Deaths`
cdc$date <- as.Date(cdc$`End Week`, format = "%m/%d/%Y")
cdc$week <- week(cdc$date)
cdc$age[cdc$age == "85"] <- "85+"

# We consider men and women separately as they have different IFRs, and therefore exclude the combined category
cdc <- cdc[cdc$Sex != "All Sex", ]

# We combine deaths in the under 1 category and the 1-4 category because no source provides an IFR for them disaggregated
cdc$age[cdc$age == "Under"] <- "0-4" 
cdc$age[cdc$age == "1-4"] <- "0-4"
cdc$deaths <- ave(cdc$deaths, interaction(cdc$age, cdc$week, cdc$sex), FUN = function(x) sum(x))

# We then take the subset of relevant columns, removing the duplicate 0-4 categories:
cdc <- cdc[!duplicated(interaction(cdc$week, cdc$age, cdc$sex)), c("age", "sex", "week", "date", "deaths")]

# And add placeholder categories for the last weeks of the year:
for(i in 1:10){
temp <- cdc[cdc$week == max(cdc$week), ]
temp$date <- temp$date + 7*i
temp$week <- week(temp$date)
temp$deaths <- 0
cdc <- rbind(cdc, temp)
}


# Load IFR:
load.nature <- T

if(load.nature){
  
  # Load IFR Nature:
  ifr <- read.csv("ifr_by_age_nature.csv")
  ifr$age <- ifr[, 1]
  ifr$ifr_raw <- ifr$ifr
  ifr$ifr <- as.numeric(unlist(lapply(ifr$ifr_raw, FUN = function(x) 
    unlist(strsplit(x, " "))[1])))
  ifr$ifr_low <- as.numeric(unlist(lapply(ifr$ifr_raw, FUN = function(x){
    x <- gsub("\\(", "-", x)
    x <- gsub("\\)", "", x)
    unlist(strsplit(x, "-"))[2]})))
  ifr$ifr_high <- as.numeric(unlist(lapply(ifr$ifr_raw, FUN = function(x){
    x <- gsub("\\(", "-", x)
    x <- gsub("\\)", "", x)
    unlist(strsplit(x, "-"))[3]})))
  ifr <- ifr[, c("age", "sex", "ifr_raw", "ifr", "ifr_low", "ifr_high", "source")]
  head(ifr[, 1:6])
  
  
  # Make categories match and merge with CI:
  cdc$ifr <- NA
  
  ifr$ifr_point <- ifr$ifr
  
  for(p in c("estimate", "low", "high")){
    if(p == "estimate"){
      ifr$ifr <- ifr$ifr_point
    }
    if(p == "low"){ 
      ifr$ifr <- ifr$ifr_low
    }
    if(p == "high"){
      ifr$ifr <- ifr$ifr_high
    }

    for(i in c("m", "f")){
      cdc$ifr[cdc$age == "total" & cdc$sex == i] <- 0.731307345072339 # from age distribution * ifr by age 
      cdc$ifr[cdc$age == "0-4" & cdc$sex == i] <- max(c(mean(c(ifr$ifr[ifr$sex == i & ifr$age %in% c("0-4")])), 0.001))
      cdc$ifr[cdc$age == "5-14" & cdc$sex == i] <- max(c(mean(c(ifr$ifr[ifr$sex == i & ifr$age %in% c("5-9", "10-14")])), 0.001))
      cdc$ifr[cdc$age == "15-24" & cdc$sex == i] <- mean(c(ifr$ifr[ifr$sex == i & ifr$age %in% c("15-19", "20-24")]))
      cdc$ifr[cdc$age == "25-34" & cdc$sex == i] <- mean(c(ifr$ifr[ifr$sex == i & ifr$age %in% c("25-29", "30-34")]))
      cdc$ifr[cdc$age == "35-44" & cdc$sex == i] <- mean(c(ifr$ifr[ifr$sex == i & ifr$age %in% c("35-39", "40-44")]))
      cdc$ifr[cdc$age == "45-54" & cdc$sex == i] <- mean(c(ifr$ifr[ifr$sex == i & ifr$age %in% c("45-49", "50-54")]))
      cdc$ifr[cdc$age == "55-64" & cdc$sex == i] <- mean(c(ifr$ifr[ifr$sex == i & ifr$age %in% c("55-59", "60-64")]))
      cdc$ifr[cdc$age == "65-74" & cdc$sex == i] <- mean(c(ifr$ifr[ifr$sex == i & ifr$age %in% c("65-49", "70-74")]))
      cdc$ifr[cdc$age == "75-84" & cdc$sex == i] <- mean(c(ifr$ifr[ifr$sex == i & ifr$age %in% c("75-79", "80")]), weights = c(2, 1))
      cdc$ifr[cdc$age == "85+" & cdc$sex == i] <- mean(c(ifr$ifr[ifr$sex == i & ifr$age %in% c("80")]))
    }
    cdc[, paste0("ifr_", p)] <- cdc$ifr
}

  cdc$ifr <- cdc$ifr_estimate
  cdc$sero_estimate <- cdc$deaths*(100/cdc$ifr_estimate)
  cdc$sero_estimate_low <- cdc$deaths*(100/cdc$ifr_high) # opposite necessary here
  cdc$sero_estimate_high <- cdc$deaths*(100/cdc$ifr_low) # opposite necessary here
  cdc$date <- as.Date(cdc$date)

}  
  
# Get official US cases / deaths:
# # Load cases and deaths
# dat <- read.csv("C:/Users/sondr/git-projects/corona/output-data/dat.ts.csv")
# dat <- dat[!is.na(dat$date) & !is.na(dat$iso2c) & dat$date <= "2020-12-06", ]
# dat <- dat[dat$iso2c == "US", ]
# 
# write.csv(dat[, c("country.name", "date", "confirmed", "deaths", "new_deaths", "new_cases", "new_cases_7dma", "new_deaths_7dma", "pop.2020")], "official_cases.csv")

dat <- read.csv("official_cases.csv")
dat$date <- as.Date(dat$date)

dat$week <- week(dat$date - 3) # to match CDC reporting weeks

weekly_sum <- function(x){
  x <- na.omit(x)
  if(length(x) == 7){
    return(sum(x))
  } else {
    print(x)
    return(7*sum(x)/length(x))
  }
}
end_of_week <- function(x){
  rev(na.omit(x))[1]*7
}

# We then make and rename a few variables for convenience
dat$new_cases_reported <- ave(dat$new_cases, dat$week, FUN = weekly_sum)
dat$new_deaths_reported <- ave(dat$new_deaths, dat$week, FUN = weekly_sum)

dat$new_cases_reported_7dma <- ave(dat$new_cases_7dma, dat$week, FUN = end_of_week)
dat$new_deaths_reported_7dma <- ave(dat$new_deaths_7dma, dat$week, FUN = end_of_week)

dat$total_deaths <- dat$deaths
dat$total_cases <- dat$confirmed
dat <- rev(dat)

# Merging official and estimated cases
df <- merge(cdc, dat[!duplicated(dat$week), c("country.name", "total_cases", "total_deaths", "new_deaths_reported", "new_cases_reported", "new_deaths_reported_7dma", "new_cases_reported_7dma", "pop.2020", "week")], by = "week", all.y= T)

# Shift to account for lag between infections and death:
# Time lag from cases to death:
# Source: https://www.cdc.gov/coronavirus/2019-ncov/hcp/planning-scenarios.html
death_interval <- 14
pred <- cdc
pred$date <- pred$date - death_interval
pred$week <- pred$week - 2
pred$pred_infections <- pred$sero_estimate
pred$pred_infections_low <- pred$sero_estimate_low
pred$pred_infections_high <- pred$sero_estimate_high

df <- merge(df, pred[, c("week", "age", "sex", 
                         "pred_infections", "pred_infections_low", 
                         "pred_infections_high")], all.x=T)

# Plot 1, no correction for missing data for recent weeks:
ggplot(df[df$age != "total", ], aes(x=as.Date(date), y=pred_infections_low))+
  geom_col(aes(fill = age, col = age, x=as.Date(date)))+
  geom_line(aes(y=new_cases_reported_7dma))+
  geom_line(aes(y=-100*new_deaths_reported_7dma, x = as.Date(date)))+
  geom_line(data = df[df$age == "total", ])+
  theme_minimal()

# We next augment the model by extending data for recent week based on reported cases
for(t in c("pred_infections", "pred_infections_low", "pred_infections_high")){
  df$temp <- df[, t]
  
  df$pred_infections_total_in_week <- NA
  df$pred_infections_total_in_week[df$age != "total"] <- with(df[df$age != "total", ], ave(temp, week, FUN = sum))
  df$pred_infections_total_in_week <- ave(df$pred_infections_total_in_week, df$week, FUN = function(x) mean(x, na.rm = T))
  
  multiplier_in_nov <- with(df[df$age == "total" &
                                 df$week %in% 40:43, ], 
                            sum(pred_infections_total_in_week)/sum(new_cases_reported_7dma))
  
  # Last week of good data == week 45, means our preds end at 43:
  for(j in 44:52){
    total_pred_cases_target <- mean(multiplier_in_nov*df$new_cases_reported_7dma[df$week == j])
    
    for(i in interaction(df$age, df$sex)){
      df$temp[as.character(interaction(df$age, df$sex)) == i 
                         & df$week == j] <- 
        mean(df$temp[df$week %in% 40:43 &
                                  interaction(df$age, df$sex) == i])
    }
    
    pred_cases <- sum(df$temp[df$week == j & df$age != "total"])
    multiplier <- total_pred_cases_target/pred_cases
    
    df$temp[df$week == j & 
                         df$age != "total"] <- df$temp[df$week == j & 
                                                                    df$age != "total"]*multiplier
  }
  
  df[, t] <- df$temp
  }

# Recalculate totals post-extrapolation
df$pred_infections_total_in_week <- NA
df$pred_infections_total_in_week[df$age != "total"] <- with(df[df$age != "total", ], ave(pred_infections, week, FUN = sum))
df$pred_infections_total_in_week <- ave(df$pred_infections_total_in_week, df$week, FUN = function(x) mean(x, na.rm = T))

df$pred_infections_total_in_week_low <- NA
df$pred_infections_total_in_week_low[df$age != "total"] <- with(df[df$age != "total", ], ave(pred_infections_low, week, FUN = sum))
df$pred_infections_total_in_week_low <- ave(df$pred_infections_total_in_week_low, df$week, FUN = function(x) mean(x, na.rm = T))

df$pred_infections_total_in_week_high <- NA
df$pred_infections_total_in_week_high[df$age != "total"] <- with(df[df$age != "total", ], ave(pred_infections_high, week, FUN = sum))
df$pred_infections_total_in_week_high <- ave(df$pred_infections_total_in_week_high, df$week, FUN = function(x) mean(x, na.rm = T))

df <- df[df$week >=10, ]

# Plot 2:
# Shows infections with age in color
ggplot(df[df$age != "total", ], aes(x=as.Date(date), y=pred_infections))+
  geom_col(aes(fill = age, col = age, x=as.Date(date)))+
  geom_line(aes(y=new_cases_reported_7dma))+
  theme_minimal()+xlab("")+ylab("")

# Get cumulative infection rates:
df <- df[order(df$date), ]
df$pred_infections_total <- ave(df$pred_infections, interaction(df$age, df$sex), FUN= function(x) cumsum(x))

# Plot 3: (Panel A)
write.csv(df, "us_covid_model.csv")
df <- read.csv("us_covid_model.csv")
df$date <- as.Date(df$date)

ggplot(df[!duplicated(df$week), ], aes(x=as.Date(date)))+
  geom_area(aes(y=pred_infections_total_in_week, fill = "estimated"))+
  geom_line(aes(y=pred_infections_total_in_week_low), col = "black")+
  geom_line(aes(y=pred_infections_total_in_week_high), col = "black")+
  geom_area(aes(y=new_cases_reported_7dma, fill = "reported"))+
  theme_minimal()+xlab("")+ylab("")+scale_y_continuous(labels = scales::comma)+theme(legend.position = "bottom", legend.title = element_blank())+ggtitle("America's Pandemic\nWeekly totals\n(blue) Reported covid-19 cases, \n(red) Estimated Infections based on The Economist's seropositivity model \n(with 95% confidence interval)")
ggsave("panel_a.png", width = 10, height = 10)

# Plot 4: (Panel B)

ggplot(df[df$age != "total", ], aes(x=date, fill=age, group=age))+
  geom_col(aes(y=100*pred_infections_total/(1000*df$pop.2020[1])))+ylab("%")+theme_minimal()+ggtitle("Estimated cumulative cases by age group, weekly totals")
ggsave("panel_b.png", width = 10, height = 10)


# Numbers in text:
# Estimated infections: Point, High, Low
sum(df$pred_infections[df$age != "total"])
sum(df$pred_infections_high[df$age != "total"])
sum(df$pred_infections_low[df$age != "total"])

# Infections per reported case in March:
sum(df$pred_infections[df$age != "total" & df$date < as.Date("2020-04-01") & df$date >= as.Date("2020-03-01")])/sum(df$new_cases_reported_7dma[df$age == "total" & df$date < as.Date("2020-04-01") & df$date >= as.Date("2020-03-01")])

# Infections per reported case in July:
sum(df$pred_infections[df$age != "total" & df$date < as.Date("2020-08-01") & df$date >= as.Date("2020-07-01")])/sum(df$new_cases_reported_7dma[df$age == "total" & df$date < as.Date("2020-08-01") & df$date >= as.Date("2020-07-01")])

# Infections per reported case in most recent week:
sum(df$pred_infections[df$age != "total" & df$week == max(df$week)])/df$new_cases_reported_7dma[df$age == "total" & df$week == max(df$week)][1]

# Number of deaths at the end of august vs total:
max(df$total_deaths[df$date <= "2020-08-01"])/max(df$total_deaths[df$date >= "2020-08-01"])

