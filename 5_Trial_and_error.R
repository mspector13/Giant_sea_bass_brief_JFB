#Probably a bunch of garbage
view(New_data)


#To look at summary data for month, and hour
Sum_data <- New_data %>% 
  mutate(year = year(date)) %>% 
  mutate(month = month(date)) %>%
  mutate(day = day(date)) %>% 
  mutate(hour = hour(date)) %>% 
  arrange(desc(hour)) %>% 
  arrange(desc(month)) 

grouped_data <- Sum_data %>% 
  group_by(id, loc, year, month, day, hour) %>% 
  summarise(n_detect_all = n())

#Differences across grouping variables and their interactions 
group_aov_all <- aov(n_detect_all~loc*month*hour, data = grouped_data)
summary(group_aov_all)
#something isn't right... df for month and hour is wrong

group_aov_hour <- aov(n_detect_all~hour, data = grouped_data)
summary(group_aov_hour)

#Mixed effects model
#https://ourcodingclub.github.io/tutorials/mixed-models/
library(lme4)
mixed.lmer <- lmer(n_detect_all~loc + factor((1|month)) + factor((1|hour)), data = grouped_data)
summary(mixed.lmer)

plot(mixed.lmer) #no clear pattern

qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer)) #not great, but I wouldn't expect our data to be normal

grouped_data$hour <- factor(grouped_data$hour,levels = c(8,9,10,11,12,13,14,
                                                         15,16,17,18,19,20,
                                                         21,22,23,24,0,1,2,
                                                         3,4,5,6,7))
#WTF INS'T FILTER WORKING FOR A CHARACTER STRING!?
grouped_data %>% 
  filter(month == c("10")) %>%                            
  ggplot(aes(x = hour, y = n_detect_all, fill=month)) +
  facet_wrap(~loc, nrow = 4) +
  geom_bar(stat = "identity")


geom_boxplot(aes(group = cut_width(n_detect_all, 0.25)),
             outlier.alpha = 0.1) +
  theme_classic() +
  theme(panel.spacing = unit(2, "lines"))

#FIGURE:MONHTLY DETECTION BAR PLOT
Sum_data %>% 
  ggplot(aes(x = month, stat = "count")) +
  geom_bar() +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7",
                              "8", "9", "10", "11", "12")) +
  facet_wrap(vars(loc))

#subset by the mound area to look at MONTHLY variation 
Site_monthly_bar <- Sum_data %>% 
  filter(loc %in% c("Mound Inshore", 
                    "Mound Offshore",
                    "SB-1",
                    "SB-10")) %>% 
  ggplot(aes(x = month, stat = "count")) +
  geom_bar() +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7",
                              "8", "9", "10", "11", "12")) +
  facet_wrap(vars(loc))

#subset by the mound area to look at HOUR variation 
Site_hour_bar <- Sum_data %>% 
  filter(loc %in% c("Mound Inshore", 
                    "Mound Offshore",
                    "SB-1",
                    "SB-10",
                    "SB-9",
                    "UW Arch")) %>% 
  ggplot(aes(x = hour, stat = "count"))+
  geom_bar() +
  facet_wrap(vars(loc)) +
  coord_cartesian(ylim = c(0, 3000))

#Path vizualization of island-wide usage by hour
Sum_data %>% 
  ggplot(aes(x = x, y = y, color = hour)) +
  geom_path() +
  facet_wrap(vars(id))

Sum_data %>% 
  group_by(loc, hour) %>% 
  summarise(
    n = n(),
    mean = mean(n_detections),
    sd = sd(n_detections)
  )

view(summary(hour_sum))

library(ggpubr)
ggqqplot(hour_sum$n_detect_hour)

shapiro.test(hour_sum$n_detect_hour) #yikes, data are not normal!

hr_aov <- aov(hour_sum$n_detect_hour~hour_sum$hour)
summary(hr_aov)

ggqqplot(month_sum$n_detect_month) #same as above!
shapiro.test(month_sum$n_detect_month)

month_aov <- aov(month_sum$n_detect_month~month_sum$month)
summary(month_aov)

kruskal.test(n_detect_month ~ month, data = month_sum)
pairwise.wilcox.test(month_sum$n_detect_month, month_sum$month,
                     p.adjust.method = "BH")

kruskal.test(n_detect_hour ~ hour, data = hour_sum)
pairwise.wilcox.test(hour_sum$n_detect_hour, hour_sum$hour,
                     p.adjust.method = "BH")

hour_sum %>% 
  ggplot(aes(hour, n_detect_hour)) +
  geom_boxplot(aes(group = cut_width(hour, 0.25)), outlier.alpha = 0.1)+
  facet_wrap(~loc)

month_sum %>% 
  ggplot(aes(month, n_detect_month)) +
  geom_boxplot(aes(group = cut_width(month, 0.25)), outlier.alpha = 0.1)+
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7",
                              "8", "9", "10", "11", "12"))+
  facet_wrap(~loc)
