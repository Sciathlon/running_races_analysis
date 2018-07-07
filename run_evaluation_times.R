library(devtools)
#devtools::install_github('fawda123/rStrava') #for installing the package
library(rStrava)
library(ggplot2)
library(curl)
library(sqldf)
#loading the athlete data
athlete_data <- athl_fun('beeep!', trace = FALSE)
app_name <- 'beeep!' # chosen by user
app_client_id  <- 'beeep!' # an integer, assigned by Strava
app_secret <- 'beeeeep!' # an alphanumeric secret, assigned by Strava

# create the authentication token
stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret))
barplot(athlete_data$`21915362`$monthly)
# get activities, get activities by location, plot
my_acts <- get_activity_list(stoken)
#sort the data into a dataframe
elevation_gain <- unlist(lapply(my_acts, function(x) x$total_elevation_gain))
distance <- unlist(lapply(my_acts, function(x) x$distance))
distance <- distance/1000
times <- unlist(lapply(my_acts, function(x) x$moving_time))
times <- round(times/(60), 0)
dates <- unlist(lapply(my_acts, function(x) x$start_date))
dates <- format(as.Date(dates), "%Y-%m")
df <- data.frame(dates, distance, times, elevation_gain)
ggplot(df, aes(x = distance, y=times, col=dates)) + geom_point()
ggplot(df, aes(x = distance, y=times)) + stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm')
ggplot(df, aes(x = distance, y=times, col=dates)) + stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm')
ggplot(df, aes(x = distance, y=dates)) + geom_point()
df_max <- sqldf("select max(distance) as max_dist, dates from df group by dates")
ggplot(df_max, aes(x = dates, y=max_dist, group=1)) + geom_point()  + stat_summary(geom="line")
df_num_amount <- sqldf("select count(distance) as num_run,dates,sum(distance) as sum_dist from df group by dates")
ggplot(df_num_amount, aes(x = dates)) + geom_point(aes(x = dates, y=num_run)) +  geom_point(aes(x = dates, y=sum_dist)) + geom_line(aes(x = dates, y=num_run, group=1,  col="red")) + geom_line(aes(x = dates, y=sum_dist, group=1,  col="blue")) + theme(legend.position="none", axis.title.y=element_blank())
df_round <- df
df_round$distance <- round(df_round$distance, 0)
sub_5 <- sqldf("select count(distance) as dist_count from df where distance < 5")
sub_10 <- sqldf("select count(distance) as dist_count from df where distance > 5 and distance < 10")
sub_15 <- sqldf("select count(distance) as dist_count from df where distance > 10 and distance < 15")
sub_20 <- sqldf("select count(distance) as dist_count from df where distance > 15 and distance < 20")
over_20 <- sqldf("select count(distance) as dist_count from df where distance > 15 and distance < 20")
df_partition <- rbind(sub_5, sub_10, sub_15, sub_20, over_20)
row.names(df_partition) <- c('<5', '<10', '<15', '<20', '>=20')
ggplot(df_partition, aes(x= row.names(df_partition),y = dist_count)) + geom_col() + scale_x_discrete(breaks=c(0, 5, 10, 15, 20)) + scale_x_discrete(labels=c("<5", "<10","<15", "<20", ">=20")) + ggtitle("Number of runs of different lengths") + theme(axis.title.x=element_blank())
ggplot(df, aes(x = dates, y=distance)) + geom_boxplot()
