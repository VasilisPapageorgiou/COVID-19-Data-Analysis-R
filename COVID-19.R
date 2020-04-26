######## Data analysis of COVID-19 through visualizations in R ###################

print('Hello World')
print('Our observations begins at 22/01/2020 and ends at 17/03/2020')


# Installation of useful packages and activation of libraries
if (require('ggplot2')==FALSE){install.packages("ggplot2")}
library('ggplot2')
if (require('dplyr')==FALSE){install.packages("dplyr")}
library('dplyr')
if (require('readr')==FALSE){install.packages("readr")}
library('readr')


############################## Exercise 1 #####################################

# Importing and printing dataset confirmed_cases_worldwide
confirmed_cases_worldwide <- read_csv("confirmed_cases_worldwide.csv")
View(confirmed_cases_worldwide)
str(confirmed_cases_worldwide)
head(confirmed_cases_worldwide, n=10)
tail(confirmed_cases_worldwide, n=10)


############################## Exercise 2 #####################################

# Creating a line graph to visualize the upward trend of the number of cases globally.

ggplot(confirmed_cases_worldwide, aes(x=date, y=cum_cases))+
  geom_line(aes(x=date,y=cum_cases,col='green')) +
  geom_line(col = 'lightblue3', size = 2) +
  ylab('Cumulative confirmed cases')

############################## Exercise 3 #####################################

# Importing and printing dataset confirmed_cases_china_vs_world

confirmed_cases_china_vs_world <- read_csv("confirmed_cases_china_vs_world.csv")
View(confirmed_cases_china_vs_world)
str(confirmed_cases_china_vs_world)

# Our dataset contains the cumulative sum of cases in China and in the rest of the world from 22-1-2020 to 17-3-2020
# Now we create a graph for the two timeseries of cases (China and Rest of the world).

plt_cum_confirmed_cases_china_vs_world <- ggplot(confirmed_cases_china_vs_world) +
  geom_line(aes(x=date, y=cum_cases,group=is_china, col = is_china)) +
  ylab('Cumulative confirmed cases')
plt_cum_confirmed_cases_china_vs_world

# From this graph we can easily observe the difference between the trends of the number of cases for China and for the Rest of the World. 
# Another important observation is that during the last week of February the number of cases every day has become
# constant for China while for the rest of the world, this week marks the beginning of an exponential trend to everyday cases.

############################## Exercise 4 #####################################

# Marking the parts of the graph which represent special events

who_events <- tribble(
  ~ date, ~ event,
  "2020-01-30", "Global health\nemergency declared",
  "2020-03-11", "Pandemic\ndeclared",
  "2020-02-13", "China reporting\nchange"
) %>%
  mutate(date = as.Date(date))

# Adding vertical lines to the previous graph to mark the special events. 

plt_cum_confirmed_cases_china_vs_world +
  geom_vline(data = who_events, aes(xintercept = date), linetype = 'dashed', col = 'darkgreen') +
  geom_text(aes(x=date, label=event), y=100000, data=who_events)

############################## Exercise 5 #####################################

# Isolating the cases in China from 15 of February and then. 
china_after_feb15 <- confirmed_cases_china_vs_world %>%
  filter(date > '2020-02-14' & is_china == 'China')

ggplot(china_after_feb15, aes(x=date, y=cases)) +
  geom_line(col='lightblue2', size = 2) +
  stat_smooth(method = 'lm', se = F, col = 'orange') +
  ylab("Cumulative confirmed cases")

# We observe that from 15 of February and then the trend of the cases is downwards. The linear regression
# seems to fit to our data, maybe polynomial regression would give us better fitting results.  

############################## Exercise 6 #####################################

# Creating a new subset containing dates and cases for the rest of the world.
not_china <- confirmed_cases_china_vs_world %>%
  filter(is_china != 'China')

# Visualazing the timeseries of cumulative cases globally except from china  
plt_not_china_trend_lin <- ggplot(not_china, aes(x=date, y = cum_cases))+
  geom_line(col='darkgreen', size = 1) +
  stat_smooth(method = 'lm', se = F, col = 'red') +
  ylab("Cumulative confirmed cases")
plt_not_china_trend_lin 

# The exponential trend of cumulative cases is obvious. We cannot base any prediction
# on linear regression. This graph shows the severity of this epidemic because the 
# increase of the cases globally seems far from over. 

############################## Exercise 7 #####################################

# Transforming y axis to logarithmic scale 

plt_not_china_trend_lin + 
  scale_y_log10()

# Now, linear regression fits really good to our data. This is another clue that the 
# cumulative sum of cases has an exponential trend. By using logarithmic scale we cancelled 
# out the exponential trend, by transforming an exponential trend (y = e^(ax), a > 0) to linear 
# trend (log(y) = log(e^(ax)) = ax, a > 0).

############################## Exercise 8 #####################################

# Importing dataset confirmed_cases_by_country
confirmed_cases_by_country <- read_csv("confirmed_cases_by_country.csv")
glimpse(confirmed_cases_by_country)

# We group our data according to countries and then we find the maximum cumulative sum of cases. 
top_countries_by_total_cases <- confirmed_cases_by_country %>% 
  group_by(country) %>% 
  summarize(total_cases=max(cum_cases)) %>% 
  top_n(7) 
top_countries_by_total_cases

# Until 17/3/2020 Italy had the most total cases (31.506), which is 2 times the cases of Iran.
# The first signs of flattening of the exponential curve began to appear a month later
# (13/04/2020). Althouth the cumulative number of deaths in Italy continued to increase rapidly. 
# So far (16/04/2020) the country with the most cases and deaths is USA with 641.813 and 28.443 proportionally.

############################## Exercise 9 #####################################

# Importing and printing dataset confirmed_cases_top7_outside_china.
confirmed_cases_top7_outside_china <- read_csv("confirmed_cases_top7_outside_china.csv")

# In the first graph we have the increasing trends of cases for the 7 nations with the most cases.
# The number of cases in these countries grow exponentially, but Italy's trend has the most 
# intense shift. 
ggplot(confirmed_cases_top7_outside_china)+
  geom_line(aes(x=date,y=cum_cases,group=country,col=country)) +
  ylab("Cumulative confirmed cases")

# Another graph for the aforementioned phenomenon.
ggplot(confirmed_cases_top7_outside_china, aes(x=date,y=cum_cases,group=country,fill=country))+
  geom_ribbon(aes(ymax=cum_cases, ymin=0), alpha = 0.3)+
  ylab("Cumulative confirmed cases")

############################################################################################################
