library(tidyverse)

GSB_data <- read_csv("1_filtered_detections_all.csv", na="NULL") %>% 
  select(c(ID, Loc, Lat, Lon, Date, Year, Month)) %>% 
  mutate(ID = as.character(ID)) %>% 
  #mutate(Month = as.character(Month)) %>% 
  mutate(Year = as.factor(Year))

#PUT ZEROs IN

# Base R add Season and Diurnal Columns based on Month  and Hour ----------
#Create new object with "Season" place holders in same order as months (numeric)
#Position matters, in this case the object's string of month names HAS to match the order of assignment

Seasons <- c(rep("Winter", 2), 
             rep("Spring", 3), 
             rep("Summer", 3), 
             rep("Fall", 3), 
             "Winter")

#Diurnal <- c(rep("Night",6 ),
           #rep("Day", 12),
           #rep("Night", 6))

#GSB_data$Hour[GSB_data$Hour == 0] <- 24
#GSB_data$Diurnal <- Diurnal[GSB_data$Hour]
GSB_data$Season <- Seasons[as.numeric(GSB_data$Month)]

#Binds new column, returns a vector that has "Seasons" names retrieved by indexes of the "month" numbers
#Effectively, R is "looking" for a position (numeric) and returning a called name (Seasons)


# Tidyverse fix -----------------------------------------------------------
GSB_data <- GSB_data %>% 
  mutate(Diurnal =
           case_when(Hour == 1 ~ "Night",
                     Hour == 2 ~ "Night",
                     Hour == 3 ~ "Night",
                     Hour == 4 ~ "Night",
                     Hour == 5 ~ "Night",
                     Hour == 6 ~ "Day",
                     Hour == 7 ~ "Day",
                     Hour == 8 ~ "Day",
                     Hour == 9 ~ "Day",
                     Hour == 10 ~ "Day",
                     Hour == 11 ~ "Day",
                     Hour == 12 ~ "Day",
                     Hour == 13 ~ "Day",
                     Hour == 14 ~ "Day",
                     Hour == 15 ~ "Day",
                     Hour == 16 ~ "Day",
                     Hour == 17 ~ "Day",
                     Hour == 18 ~ "Day",
                     Hour == 19 ~ "Day",
                     Hour == 20 ~ "Night",
                     Hour == 21 ~ "Night",
                     Hour == 22 ~ "Night",
                     Hour == 23 ~ "Night",
                     Hour == 0 ~ "Night"))


GSB_data <- GSB_data %>% 
  mutate(Season =
           case_when(Month == 12 ~ "Winter",
                     Month == 1 ~ "Winter",
                     Month == 2 ~ "Winter",
                     Month == 3 ~ "Spring",
                     Month == 4 ~ "Spring",
                     Month == 5 ~ "Spring",
                     Month == 6 ~ "Summer",
                     Month == 7 ~ "Summer",
                     Month == 8 ~ "Summer",
                     Month == 9 ~ "Fall",
                     Month == 10 ~ "Fall",
                     Month == 11 ~ "Fall"))

head(GSB_data)


# Data exploration --------------------------------------------------------


Year_data <- GSB_data %>% 
  group_by(ID, Loc, Year) %>% 
  summarise(n_detect_all = n()) 
  
Year_sum <- Year_data %>% 
  group_by(Year) %>% 
  summarise(n_detect_all = sum(n_detect_all))

Year_data %>% 
  ggplot(aes(x = Loc,
             y = n_detect_all)) +
  geom_bar(stat='identity',
           aes(fill = ID),
           position = position_dodge(width = 0.9)) +
  theme(axis.text.x = element_text(angle = 45)) +
  facet_grid(ID ~ Year)

Month_data <- GSB_data %>% 
  group_by(ID, Loc, Month, Year) %>% 
  summarise(n_detect_all = n()) 

Month_sum <- Month_data %>% 
  group_by(Month) %>% 
  summarise(n_detect_all = sum(n_detect_all))

Month_data %>% 
  #filter(Month > 6, 
         #Month < 11,
         #Year > 2018, Year < 2021 ) %>% 
  ggplot(aes(x = Loc,
             y = n_detect_all,
             fill = ID))+
  geom_bar(stat='identity',
           position = position_dodge(width = 0.9)) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme_classic() +
  facet_grid(Month ~ Year)


Season_data <-  GSB_data %>% 
  group_by(ID, Loc, Season, Year) %>% 
  summarise(n_detect_all = n())

# Visualization(s) -----------------------------------------------------
library(rstatix)
library(ggpubr)

head(GSB_data)

GSB_workup <- GSB_data %>%
  #mutate(Month = as.character(Month)) %>% 
  group_by(ID, Loc, Year, Month, Season) %>%
  summarise(n_detect_all = n()) %>% 
  rename(Observations = n_detect_all)

head(GSB_workup)

#GSB_workup$Month <- character(GSB_workup$Month, levels = c(1, 2, 3, 4, 5, 6,
                                                           7, 8, 9, 10, 11, 12))

GSB_workup %>% 
  summarize(Observations)

#We should probably include 2018; lots of obs + the CHU breaks down otherwise
#GSB_workup %>% 
  filter(Year %in% c(2019, 2020)) %>% 
  filter(Month %in% c(6, 7, 8, 9, 10)) %>%
  filter(Loc %in% c("Mound Inshore", "Mound Offshore",
                    "SB-1", "SB-10",
                    "SB-9", "SB-9.5")) %>%
  ggplot(aes(x = Loc, 
             y = obs,
             fill = ID)) +
  geom_boxplot() +
  ylim(0, 250) +
  theme_classic() +
  facet_grid(Month ~ Year)

site_order <- c("SB-3", "SB-2", "SB-1", "Mound Inshore", "Mound Offshore", "SB-10",
                "SB-9.5", "SB-9", "MPA Corner", "SB-8", "SB-7", "Sutil", "SB-6",
                "SB-5", "UW Arch", "WR-W", "WR-C", "WR-E", "SB-4")


GSB_workup <- GSB_workup %>% 
  mutate(Cardinal =
           case_when(Loc == "SB-3" ~ "Northwest",
                     Loc == "SB-2" ~ "Northeast",
                     Loc == "SB-1" ~ "Northeast",
                     Loc == "Mound Inshore" ~ "Northeast",
                     Loc == "Mound Offshore" ~ "Northeast",
                     Loc == "SB-10" ~ "Northeast",
                     Loc == "SB-9.5" ~ "Northeast",
                     Loc == "SB-9" ~ "Southeast",
                     Loc == "MPA Corner" ~ "Southeast",
                     Loc == "SB-8" ~ "Southeast",
                     Loc == "SB-7" ~ "Southeast",
                     Loc == "SB-6" ~ "Southwest",
                     Loc == "Sutil" ~ "Southwest",
                     Loc == "SB-5" ~ "Northwest",
                     Loc == "UW Arch" ~ "Northwest",
                     Loc == "WR-W" ~ "Northwest",
                     Loc == "WR-C" ~ "Northwest",
                     Loc == "WR-E" ~ "Northwest",
                     Loc == "SB-4" ~ "Northwest")) 
#note: this breakdown isn't valid; fish like both kelp and sand (and there're range issues)
  mutate(Habitat =
           case_when(Loc == "SB-3" ~ "Kelp",
                     Loc == "SB-2" ~ "Sand",
                     Loc == "SB-1" ~ "Kelp",
                     Loc == "Mound Inshore" ~ "Kelp",
                     Loc == "Mound Offshore" ~ "Kelp",
                     Loc == "SB-10" ~ "Sand",
                     Loc == "SB-9.5" ~ "Sand",
                     Loc == "SB-9" ~ "Sand",
                     Loc == "MPA Corner" ~ "Kelp",
                     Loc == "SB-8" ~ "Kelp",
                     Loc == "SB-7" ~ "Sand",
                     Loc == "SB-6" ~ "Kelp",
                     Loc == "Sutil" ~ "Kelp",
                     Loc == "SB-5" ~ "Sand",
                     Loc == "UW Arch" ~ "Kelp",
                     Loc == "WR-W" ~ "Kelp",
                     Loc == "WR-C" ~ "Kelp",
                     Loc == "WR-E" ~ "Kelp",
                     Loc == "SB-4" ~ "Kelp"))

head(GSB_workup)

### FIG SITE BOX; deprecated
GSB_workup %>% 
  filter(Year %in% c(2019, 2020)) %>% 
  #filter(Loc %in% c("Mound Inshore", "Mound Offshore",
                    #"SB-1", "SB-10",
                    #"SB-9", "SB-9.5")) %>%
  ggplot(aes(x = factor(Loc, site_order), 
             y = obs)) +
  geom_boxplot(stat = "boxplot",
              position = "dodge2",
              outlier.alpha = 0.1) +
  ylim(0, 250) +
  labs(y = "Number of Observations per Site") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(axis.title.x=element_blank()) +
  facet_wrap(~Cardinal)

### FIG MONTH BOX: USE

season_order <- c("Winter", "Spring", "Summer", "Fall")
month_order <- c("3", "4", "5", "6", "7", "8", "9", "10",
                 "11", "12", "1", "2")
month_names <- lubridate::month(GSB_workup$Month, label = TRUE, abbr = FALSE)

#RUN THIS
#Per reviewer #2's comment, we need to truncate the y-axis (and remove points)
#reviewer 3: log scale? (e.g., scale_y_continuous(trans = log2_trans()),
#scale_y_continuous(trans = sqrt_trans()), or scale_y_continuous(trans = log2_trans())
#We did *not* transform data, this is an artifact for others to use
library(scales)
GSB_workup %>% 
  group_by(ID, Loc, Year, Month, Season, Cardinal, Observations) %>%
  ggplot(aes(x = factor(month_names), 
             y = Observations,
             fill = factor(Season, season_order))) +
  geom_boxplot(stat = "boxplot",
               position = "dodge2",
               outlier.alpha = 0.1) +
  ylim(0,2000) +
  #scale_y_continuous(trans = sqrt_trans()) +
  labs(y = "Number of Detections per Month ") +
  theme_classic() +
  theme(text = element_text(size = 20)) +
  scale_fill_grey(start=0.1, end=0.9) +
  theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  theme(axis.title.x=element_blank()) +
  labs(fill = "Binned Seasons") +
  #scale_fill_discrete(name = "Binned Seasons") +
  facet_wrap(~factor(Cardinal), ncol = 2)

  

# Modeling: ANOVA and Poisson ---------------------------------------------

#Gama distribution; starts at zero and counts up
#Presence ONLY models; should we fill in zeros when absent???

glm <- glm(Observations ~ Month, 
           family = "poisson",
           data = GSB_workup) 
summary(glm)

library(lme4)
#https://slcladal.github.io/mmws.html
#SHOULD MONTH BE NUMERIC, OR CHARACTER? DISCRETE OR CONTINUOUS, RANDOM OR FIXED!?!?
GSB_workup <- GSB_workup %>% 
  mutate(Month = as.numeric(Month)) 
  
#ma.lmer <- lmer(Observations ~ Month + (1|Loc), REML = T, data = GSB_workup)
#Season.lmer <- lmer(Observations ~ Season + (1|Loc), REML = T, data = GSB_workup)
#fish.lmer <- lmer(Observations ~ Season + (1|Loc) + (1|ID) + (Loc|ID), REML = T, data = GSB_workup)
fish2.lmer <- lmer(Observations ~ Season + (1|Loc)  + (1|ID), REML = T, data = GSB_workup)
#fishy.lmer <- lmer(Observations ~ Season + (1|Loc) + (1|Loc:Season) + (1|ID), REML = T, data = GSB_workup)
#card.lmer <- lmer(Observations ~ Season + (1|Cardinal)  + (1|ID), REML = T, data = GSB_workup)
#cardin.lmer <- lmer(Observations ~ Season + (1|Cardinal) + (1|Cardinal:Season) + (1|ID), REML = T, data = GSB_workup)

#summary(card.lmer)
summary(fish2.lmer)
confint(fish2.lmer, oldNames = FALSE)
#confint(card.lmer, oldNames = FALSE)
ranef(fish2.lmer)
#ranef(card.lmer)
plot(fish2.lmer)
#plot(card.lmer)
anova(fish2.lmer) #returns ANOVA table for fixed factor
ranova(fish2.lmer) #returns ANOVA-like table for random factors (but is wonky)
#anova(card.lmer)
#fish.fixed <- aov(Observations ~ Loc * ID * Month, data = GSB_workup)
#summary(fish.fixed)
#mb.lmer <- lmer(Observations ~ Month + (1 + Month | Loc), REML = T, data = GSB_workup)
#stack.lmer <- lmer(Observations ~ Month + (Month || Loc), data = GSB_workup)

#anova(fish2.lmer, card.lmer, test = "Chisq", refit = F)
#test.aov <- aov(Observations ~ Season + ID + Cardinal, data = GSB_workup)
#summary(test.aov)

#anova(fish2.lmer, card.lmer, test.aov, test = "Chisq", refit = F)
#summary
#m1.lmer <- lmer(Observations ~ (1|Loc) + Month, data = GSB_workup)

coef(fish2.lmer)
plot(summary(fish2.lmer)$residuals, ylab = "Residuals", pch = 20)
plot(fish2.lmer, Loc ~ resid(.), abline = 0 ) # generate diagnostic plots
#examine the Loc's in isolation standardized residuals 
#versus fitted values (Pinheiro and Bates 2000, 175).
plot(ma.lmer, resid(., type = "pearson") ~ fitted(.) | Loc, id = 0.05, 
     adj = -0.3, pch = 20, col = "gray40", cex = .5)

#look at dispersion; heteroscedasticity is a problem
par(mfrow = c(2, 2))           # display plots in 2 rows and 2 columns
plot(fish2.lmer, pch = 20, col = "black", lty = "dotted"); par(mfrow = c(1, 1))

#data are.... kind of normal?
qqnorm(resid(fish2.lmer))
qqline(resid(fish2.lmer))

par(mfrow = c(1,3))
qqnorm(ranef(fish2.lmer)$ID[,1], main = "Random effects of ID")
qqnorm(ranef(fish2.lmer)$Loc[,1], main = "Random effects of Loc")
qqnorm(resid(fish2.lmer), main = "Residuals")

par(mfrow = c(1,3))
qqnorm(ranef(card.lmer)$ID[,1], main = "Random effects of ID")
qqnorm(ranef(card.lmer)$Cardinal[,1], main = "Random effects of Loc")
qqnorm(resid(card.lmer), main = "Residuals")

## observed responses versus the within-group fitted values
plot(fish2.lmer, Observations ~ fitted(.), id = 0.05, adj = -0.3, 
     xlim = c(80, 220), cex = .8, pch = 20, col = "blue")

sjPlot::tab_model(fish2.lmer)
sjPlot::tab_model(card.lmer)

sjPlot::plot_model(fish2.lmer, type = "pred", terms = c("Season")) +
  # show uncentered date rather than centered date
  scale_x_discrete(name = "Season", 
                     breaks = seq(-500, 300, 100), 
                     labels = seq(1150, 1950, 100))

# Buncha garbage ----------------------------------------------------------

library(lmerTest)

anova(gsb2)


gsb_model <- aov(obs ~ Month*Loc, data = GSB_workup)
summary(gsb_model)

library(glm)
Pois_gsb <- lm(formula = obs ~ Loc*Month,
                family = "poisson", data = GSB_workup)

summary(Pois_gsb)


obs_num <- as.numeric(GSB_workup$obs)
loc_cat <- as.character(GSB_workup$Loc)
month_cat <- as.character(GSB_workup$Month)

Pois2 <- glm(obs_num ~ loc_cat + month_cat,
             family = "poisson",
             data = GSB_workup)
summary(Pois2)
plot_summs(Pois2, scale = TRUE, exp = TRUE)

poisson.model <- glm(obs ~ Loc + Month,
                     data = GSB_workup,
                     family = poisson(link = "log"))

summary(poisson.model)
library(arm)
coef1 <- coef(poisson.model)
se.coef1 <- se.coef(poisson.model)

p.model <- cbind(coef1, se.coef1, exponent = exp(coef1))
p.model
library(jtools)
library(interactions)
plot_summs(poisson.model, scale = TRUE, exp = TRUE)
cat_plot(poisson.model, pred = Loc, modx = Month)

model1 <- aov(obs ~ Loc, data = GSB_workup)
summary(model1)  
TukeyHSD(model1)

model2 <- aov(obs ~ Month, data = GSB_workup)
summary(model2)

#model2 <- aov(obs ~ Loc + Season + Year +
                Loc:Season + Loc:Year + Season:Year, data = GSB_workup)
summary(model2)

model3 <- aov(obs ~ Diurnal + Year + Month +
              Loc:Diurnal + Loc:Year + Loc:Month + 
              Diurnal:Month + Diurnal:Year + Year:Month,
              data = GSB_workup)
summary(model3)

#model4 <- aov(obs ~ Loc + Month + Month:Loc, data = GSB_workup)
summary(model4)


aov(obs~as.factor(Loc)*as.factor(Month),data=
      GSB_workup)

anova2 <- aov(obs ~ as.factor(Loc)*as.factor(Month),
              data = GSB_workup)

summary(anova2)

res <- anova2$residuals
hist(res,main="Histogram of
residuals",xlab="Residuals")
leveneTest(obs ~ as.factor(Loc)*as.factor(Month),
           data = GSB_workup)


# Unique Individuals  -----------------------------------------------------

Distinct_2019 <- Month_data %>% 
  filter(Year >2018, Year<2020) %>% 
  group_by(ID) %>% 
  distinct(ID) %>% 
  add_column(Year = 2019)

Distinct_2020 <- Month_data %>%
  filter(Year >2019) %>% 
  group_by(ID) %>% 
  distinct(ID) %>% 
  add_column(Year = 2020)

Distinct <- bind_rows(Distinct_2019, Distinct_2020)
View(Distinct)

#What is this?
Distinct_2018 <- Month_data %>% 
  filter(year <2019) %>% 
  group_by(id) %>% 
  distinct(id) %>% 
  add_column(year = 2018)
grouped_data %>% 
  filter(year == 2020) %>% 
  group_by(loc, month) %>% 
  summarise(
    mean_dect = mean(n_detect_all, na.rm = TRUE)
  ) %>% 
  filter(grepl('7|8|9|10', month)) %>%
  arrange(month) %>% 
  View()

        
