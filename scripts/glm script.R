# Script for NRSM 495 for learning how to do some R statistics and graphs.  As 
# always, let's get started by loading the relevant packages. If you haven't 
# already, you'll need to install these packages. You can do that by 
# highlighting the following and passing it to the Console:
# install.packages(c("multcomp", "tidyverse", "Rmisc", "boot", "lme4", "car", 
# "emmeans", "multcompView", "AICcmodavg", "readxl", "ggbiplot"))

library(multcomp) # for doing elegant pairwise comparisons
library(tidyverse)  # loads several useful packages, including ggplot2, 
# tidyr, and dplyr
library(Rmisc)  # for summarySE function to summarize data frame
library(boot) # for diagnostics for GLM
library(lme4)   # for linear, general, and nonlinear mixed models
library(car)    # for doing ANOVA
library(emmeans) # posthoc tests
library(multcompView) # making posthoc tests easier to view and plot
library(AICcmodavg) # for comparing models
library(PerformanceAnalytics) # for making amazing correlation plots
library(readxl)

# importing my data---- 
BDA <- read_xlsx("C:/Users/Cupcake/Documents/Stardom-data-prep/BDA sample information.xlsx")
head(BDA)
# Setting my columns to be the correct format
# Currently all characters
 # BDA <- BDA %>%
 #  mutate(BDA$site = ordered(BDA$site, levels = c("lolo", "fish", "lost")))
# got this error:
#   Error: unexpected '=' in:
# "BDA <- BDA %>%
#  mutate(BDA$site ="
# so let me try something smaller to begin with
# df <- df %>%
#   mutate(column9 = as.factor(column9))
# got this error:
# Error in mutate(., column9 = as.factor(column9)) : 
# is.data.frame(.data) || is.list(.data) || is.environment
# (.data) is not TRUE
# I don't fully understand this error message
# let me try replacing column9 with the actual column name
# df <- df %>%
#   mutate(BDA$bix = as.factor(BDA$bix))
# hmm, maybe that was a step backwards:
# Error: unexpected '=' in:
# "df <- df %>%
# mutate(BDA$bix ="
# this didn't quite work, so let's try this
BDA$site <- as.ordered(BDA$site)
BDA$stream <- as.ordered(BDA$stream)
BDA$treatment <- as.ordered(BDA$treatment)
BDA$location <- as.ordered(BDA$location)
BDA$repitition <- as.ordered(BDA$repitition)
BDA$date <- as.ordered(BDA$date)
BDA$bix <- as.numeric(BDA$bix)
BDA$t <- as.numeric(BDA$t)
BDA$a <- as.numeric(BDA$a)
# BDA$m <- as.factor(BDA$m)
BDA$c <- as.numeric(BDA$c)
BDA$fi <- as.numeric(BDA$fi)
BDA$hix <- as.numeric(BDA$hix)
head(BDA)
str(BDA)




# Let's also set a theme for our graphs----

theme_set(theme_gray() +
            theme(axis.line = element_line(linetype = "solid"), 
                  axis.ticks = element_line(colour = "black", size = 1), 
                  panel.grid.major = element_line(colour = NA), 
                  panel.grid.minor = element_line(colour = NA),
                  axis.title = element_text(size = 22),
                  axis.text = element_text(size = 20, colour = "black"),
                  plot.title = element_text(size = 16),
                  panel.background = element_rect(fill = NA),
                  plot.background = element_rect(colour = NA),
                  legend.key = element_blank(),
                  legend.text = element_text(size = 14),
                  legend.title = element_text(size = 14)))

# Now we want to test the code
# We are using stream, location, and date to see if we can
# predict my indices

bix.stream.loation.date.glm <- glm(bix ~ stream*location*date,
                         data = BDA,
                         family = gaussian(link="identity"))
par(mfrow = c(2,2))
plot(bix.stream.loation.date.glm)
# OH MY GOD THIS IS SO EXCITING

# ok, let's do all my indices by location, stream, and date ----
bix.glm <- glm(bix ~ stream*location*date,
                                   data = BDA,
                                   family = gaussian(link="identity"))
t.glm <- glm(t ~ stream*location*date,
                                   data = BDA,
                                   family = gaussian(link="identity"))
a.glm <- glm(a ~ stream*location*date,
                                   data = BDA,
                                   family = gaussian(link="identity"))
c.glm <- glm(c ~ stream*location*date,
                                   data = BDA,
                                   family = gaussian(link="identity"))
fi.glm <- glm(fi ~ stream*location*date,
                                   data = BDA,
                                   family = gaussian(link="identity"))
hix.glm <- glm(hix ~ stream*location*date,
                                   data = BDA,
                                   family = gaussian(link="identity"))
par(mfrow = c(2,2))
plot(bix.glm)
plot(t.glm)
plot(a.glm)
plot(c.glm)
plot(fi.glm)
plot(hix.glm)
summary(bix.glm)
summary(t.glm)
summary(a.glm)
summary(c.glm)
summary(fi.glm)
summary(hix.glm)
(1-bix.glm$deviance/bix.glm$null.deviance)
(1-t.glm$deviance/t.glm$null.deviance)
(1-a.glm$deviance/a.glm$null.deviance)
(1-c.glm$deviance/c.glm$null.deviance)
(1-fi.glm$deviance/fi.glm$null.deviance)
(1-hix.glm$deviance/hix.glm$null.deviance)

# maybe test how each dependent variable interacts? What % it accounts for
# bix.stream.glm <- glm(bix ~ stream,
#                                     data = BDA,
#                                     family = gaussian(link="identity"))
# bix.location.glm <- glm(bix ~ location,
#                       data = BDA,
#                       family = gaussian(link="identity"))
# bix.date.glm <- glm(bix ~ date,
#                       data = BDA,
#                       family = gaussian(link="identity"))
# t.stream.glm <- glm(t ~ stream,
#                       data = BDA,
#                       family = gaussian(link="identity"))
# t.location.glm <- glm(t ~ location,
#                     data = BDA,
#                     family = gaussian(link="identity"))
# t.date.glm <- glm(t ~ date,
#                     data = BDA,
#                     family = gaussian(link="identity"))
# a.stream.glm <- glm(a ~ stream,
#                     data = BDA,
#                     family = gaussian(link="identity"))
# a.location.glm <- glm(a ~ location,
#                     data = BDA,
#                     family = gaussian(link="identity"))
# a.date.glm <- glm(t ~ date,
#                     data = BDA,
#                     family = gaussian(link="identity"))
# c.stream.glm <- glm(c ~ stream,
#                     data = BDA,
#                     family = gaussian(link="identity"))
# c.location.glm <- glm(c ~ location,
#                     data = BDA,
#                     family = gaussian(link="identity"))
# c.date.glm <- glm(c ~ date,
#                      data = BDA,
#                      family = gaussian(link="identity"))
# fi.stream.glm <- glm(fi ~ stream,
#                     data = BDA,
#                     family = gaussian(link="identity"))
# fi.location.glm <- glm(fi ~ location,
#                      data = BDA,
#                      family = gaussian(link="identity"))
# fi.date.glm <- glm(fi ~ date,
#                      data = BDA,
#                      family = gaussian(link="identity"))
# hix.stream.glm <- glm(hix ~ stream,
#                      data = BDA,
#                      family = gaussian(link="identity"))
# hix.location.glm <- glm(hix ~ location,
#                       data = BDA,
#                       family = gaussian(link="identity"))
# hix.date.glm <- glm(hix ~ date,
#                       data = BDA,
#                       family = gaussian(link="identity"))
# plot(bix.stream.glm)
# plot(bix.location.glm)
# plot(bix.date.glm)
# plot(t.stream.glm)
# plot(t.location.glm)
# plot(t.date.glm)
# plot(a.stream.glm)
# plot(a.location.glm)
# plot(a.date.glm)
# plot(c.stream.glm)
# plot(c.location.glm)
# plot(c.date.glm)
# plot(fi.stream.glm)
# plot(fi.location.glm)
# plot(fi.date.glm)
# plot(hix.stream.glm)
# plot(hix.location.glm)
# plot(hix.date.glm)
# summary(bix.stream.glm)
# summary(bix.stream.glm)
# summary(bix.location.glm)
# summary(bix.date.glm)
# summary(t.stream.glm)
# summary(t.location.glm)
# summary(t.date.glm)
# summary(a.stream.glm)
# summary(a.location.glm)
# summary(a.date.glm)
# summary(c.stream.glm)
# summary(c.location.glm)
# summary(c.date.glm)
# summary(fi.stream.glm)
# summary(fi.location.glm)
# summary(fi.date.glm)
# summary(hix.stream.glm)
# summary(hix.location.glm)
# summary(hix.date.glm)
# (1-bix.stream.glm$deviance/bix.stream.glm$null.deviance)
# (1-bix.location.glm$deviance/bix.location.glm$null.deviance)
# (1-bix.date.glm$deviance/bix.date.glm$null.deviance)
# (1-t.stream.glm$deviance/t.stream.glm$null.deviance)
# (1-t.location.glm$deviance/t.location.glm$null.deviance)
# (1-t.date.glm$deviance/t.date.glm$null.deviance)
# (1-a.stream.glm$deviance/a.stream.glm$null.deviance)
# (1-a.location.glm$deviance/a.location.glm$null.deviance)
# (1-a.date.glm$deviance/a.date.glm$null.deviance)
# (1-c.stream.glm$deviance/c.stream.glm$null.deviance)
# (1-c.location.glm$deviance/c.location.glm$null.deviance)
# (1-c.date.glm$deviance/c.date.glm$null.deviance)
# (1-fi.stream.glm$deviance/fi.stream.glm$null.deviance)
# (1-fi.location.glm$deviance/fi.location.glm$null.deviance)
# (1-fi.date.glm$deviance/fi.date.glm$null.deviance)
# (1-hix.stream.glm$deviance/hix.stream.glm$null.deviance)
# (1-hix.location.glm$deviance/hix.location.glm$null.deviance)
# (1-hix.date.glm$deviance/hix.date.glm$null.deviance)
# Ok, ignore all that, that was just for fun

# now onto plotting data

# bix.coefficients <- bix.glm$coefficients 
# t.coefficients <- t.glm$coefficients
# a.coefficients <- a.glm$coefficients
# c.coefficients <- c.glm$coefficients
# fi.coefficients <- fi.glm$coefficients
# hix.coefficients <- hix.glm$coefficients
# ggplot(BDA) +
#   geom_point(aes(x = bix, y = stream*location*date, color = name))
#   Ha! Just kidding, I don't need that actually

# ANOVA----
Anova(bix.glm)   
Anova(t.glm)
Anova(a.glm)
Anova(c.glm)
Anova(fi.glm)
Anova(hix.glm)

# I am going to stop here because now we are getting into territory that I am
# not familiar with and the results I have so far are giving me some questions.
# See you on Friday!   
#  instead of stream, try treatments 
#  pull out howard proper
#  howard proper is pulled out, now onto treatments instead of streams

# glm with treatments, locations, and date----
bix.glm <- glm(bix ~ treatment*location*date,
               data = BDA,
               family = gaussian(link="identity"))
t.glm <- glm(t ~ treatment*location*date,
             data = BDA,
             family = gaussian(link="identity"))
a.glm <- glm(a ~ treatment*location*date,
             data = BDA,
             family = gaussian(link="identity"))
c.glm <- glm(c ~ treatment*location*date,
             data = BDA,
             family = gaussian(link="identity"))
fi.glm <- glm(fi ~ treatment*location*date,
              data = BDA,
              family = gaussian(link="identity"))
hix.glm <- glm(hix ~ treatment*location*date,
               data = BDA,
               family = gaussian(link="identity"))

par(mfrow = c(2,2))
plot(bix.glm)
plot(t.glm)
plot(a.glm)
plot(c.glm)
plot(fi.glm)
plot(hix.glm)

summary(bix.glm)
summary(t.glm)
summary(a.glm)
summary(c.glm)
summary(fi.glm)
summary(hix.glm)

(1-bix.glm$deviance/bix.glm$null.deviance)
(1-t.glm$deviance/t.glm$null.deviance)
(1-a.glm$deviance/a.glm$null.deviance)
(1-c.glm$deviance/c.glm$null.deviance)
(1-fi.glm$deviance/fi.glm$null.deviance)
(1-hix.glm$deviance/hix.glm$null.deviance)

Anova(bix.glm)   
Anova(t.glm)
Anova(a.glm)
Anova(c.glm)
Anova(fi.glm)
Anova(hix.glm)

bix.emm <- emmeans(bix.glm, ~ treatment | location | date, type = "response")
bix.emm
t.emm <- emmeans(t.glm, ~ treatment | location | date, type = "response")
t.emm
a.emm <- emmeans(a.glm, ~ treatment | location | date, type = "response")
a.emm
c.emm <- emmeans(c.glm, ~ treatment | location | date, type = "response")
c.emm
fi.emm <- emmeans(fi.glm, ~ treatment | location | date, type = "response")
fi.emm
hix.emm <- emmeans(hix.glm, ~ treatment | location | date, type = "response")
hix.emm

# by treatment----
treatmentb.cld <- cld(bix.emm, 
                      by = c("location", "date"), 
                      alpha = 0.05, 
                      Letters = letters)
treatmentb.cld$.group <- gsub(" ", "", treatmentb.cld$.group)
treatmentb.cld <- subset(treatmentb.cld)
treatmentb.cld
treatmenta.cld <- cld(a.emm, 
                      by = c("location", "date"), 
                      alpha = 0.05, 
                      Letters = letters)
treatmenta.cld$.group <- gsub(" ", "", treatmenta.cld$.group)
treatmenta.cld <- subset(treatmenta.cld)
treatmenta.cld
treatmentt.cld <- cld(t.emm, 
                      by = c("location", "date"), 
                      alpha = 0.05, 
                      Letters = letters)
treatmentt.cld$.group <- gsub(" ", "", treatmentt.cld$.group)
treatmentt.cld <- subset(treatmentt.cld)
treatmentt.cld
treatmentc.cld <- cld(c.emm, 
                      by = c("location", "date"), 
                      alpha = 0.05, 
                      Letters = letters)
treatmentc.cld$.group <- gsub(" ", "", treatmentc.cld$.group)
treatmentc.cld <- subset(treatmentc.cld)
treatmentc.cld
treatmentfi.cld <- cld(fi.emm, 
                       by = c("location", "date"), 
                       alpha = 0.05, 
                       Letters = letters)
treatmentfi.cld$.group <- gsub(" ", "", treatmentfi.cld$.group)
treatmentfi.cld <- subset(treatmentfi.cld)
treatmentfi.cld
treatmenthix.cld <- cld(hix.emm, 
                        by = c("location", "date"), 
                        alpha = 0.05, 
                        Letters = letters)
treatmenthix.cld$.group <- gsub(" ", "", treatmenthix.cld$.group)
treatmenthix.cld <- subset(treatmenthix.cld)
treatmenthix.cld

ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = treatment, y = bix)) +
  geom_text(data = treatmentb.cld, 
            aes(x = treatment, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "Treatment", y = "bix") +
  facet_grid(location~date, scales = "free_y")
ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = treatment, y = t)) +
  geom_text(data = treatmentt.cld, 
            aes(x = treatment, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "Treatment", y = "t") +
  facet_grid(location~date, scales = "free_y")
ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = treatment, y = a)) +
  geom_text(data = treatmenta.cld, 
            aes(x = treatment, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "Treatment", y = "a") +
  facet_grid(location~date, scales = "free_y")
ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = treatment, y = c)) +
  geom_text(data = treatmentc.cld, 
            aes(x = treatment, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "Treatment", y = "c") +
  facet_grid(location~date, scales = "free_y")
ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = treatment, y = fi)) +
  geom_text(data = treatmentfi.cld, 
            aes(x = treatment, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "Treatment", y = "fi") +
  facet_grid(location~date, scales = "free_y")
ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = treatment, y = hix)) +
  geom_text(data = treatmenthix.cld, 
            aes(x = treatment, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "Treatment", y = "hix") +
  facet_grid(location~date, scales = "free_y")


# by location----
locationb.cld <- cld(bix.emm, 
                      by = c("treatment", "date"), 
                      alpha = 0.05, 
                      Letters = letters)
locationb.cld$.group <- gsub(" ", "", locationb.cld$.group)
locationb.cld <- subset(locationb.cld)
locationb.cld
locationa.cld <- cld(a.emm, 
                      by = c("treatment", "date"), 
                      alpha = 0.05, 
                      Letters = letters)
locationa.cld$.group <- gsub(" ", "", locationa.cld$.group)
locationa.cld <- subset(locationa.cld)
locationa.cld
locationt.cld <- cld(t.emm, 
                      by = c("treatment", "date"), 
                      alpha = 0.05, 
                      Letters = letters)
locationt.cld$.group <- gsub(" ", "", locationt.cld$.group)
locationt.cld <- subset(locationt.cld)
locationt.cld
locationc.cld <- cld(c.emm, 
                      by = c("treatment", "date"), 
                      alpha = 0.05, 
                      Letters = letters)
locationc.cld$.group <- gsub(" ", "", locationc.cld$.group)
locationc.cld <- subset(locationc.cld)
locationc.cld
locationfi.cld <- cld(fi.emm, 
                       by = c("treatment", "date"), 
                       alpha = 0.05, 
                       Letters = letters)
locationfi.cld$.group <- gsub(" ", "", locationfi.cld$.group)
locationfi.cld <- subset(locationfi.cld)
locationfi.cld
locationhix.cld <- cld(hix.emm, 
                        by = c("treatment", "date"), 
                        alpha = 0.05, 
                        Letters = letters)
locationhix.cld$.group <- gsub(" ", "", locationhix.cld$.group)
locationhix.cld <- subset(locationhix.cld)
locationhix.cld

ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = location, y = bix)) +
  geom_text(data = locationb.cld, 
            aes(x = location, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "location", y = "bix") +
  facet_grid(treatment~date, scales = "free_y")
ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = location, y = t)) +
  geom_text(data = locationt.cld, 
            aes(x = location, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "location", y = "t") +
  facet_grid(treatment~date, scales = "free_y")
ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = location, y = a)) +
  geom_text(data = locationa.cld, 
            aes(x = location, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "location", y = "a") +
  facet_grid(treatment~date, scales = "free_y")
ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = location, y = c)) +
  geom_text(data = locationc.cld, 
            aes(x = location, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "location", y = "c") +
  facet_grid(treatment~date, scales = "free_y")
ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = location, y = fi)) +
  geom_text(data = locationfi.cld, 
            aes(x = location, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "location", y = "fi") +
  facet_grid(treatment~date, scales = "free_y")
ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = location, y = hix)) +
  geom_text(data = locationhix.cld, 
            aes(x = location, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "location", y = "hix") +
  facet_grid(treatment~date, scales = "free_y")

# by date----
dateb.cld <- cld(bix.emm, 
                     by = c("treatment", "location"), 
                     alpha = 0.05, 
                     Letters = letters)
dateb.cld$.group <- gsub(" ", "", dateb.cld$.group)
dateb.cld <- subset(dateb.cld)
dateb.cld
datea.cld <- cld(a.emm, 
                     by = c("treatment", "location"), 
                     alpha = 0.05, 
                     Letters = letters)
datea.cld$.group <- gsub(" ", "", datea.cld$.group)
datea.cld <- subset(datea.cld)
datea.cld
datet.cld <- cld(t.emm, 
                     by = c("treatment", "location"), 
                     alpha = 0.05, 
                     Letters = letters)
datet.cld$.group <- gsub(" ", "", datet.cld$.group)
datet.cld <- subset(datet.cld)
datet.cld
datec.cld <- cld(c.emm, 
                     by = c("treatment", "location"), 
                     alpha = 0.05, 
                     Letters = letters)
datec.cld$.group <- gsub(" ", "", datec.cld$.group)
datec.cld <- subset(datec.cld)
datec.cld
datefi.cld <- cld(fi.emm, 
                      by = c("treatment", "location"), 
                      alpha = 0.05, 
                      Letters = letters)
datefi.cld$.group <- gsub(" ", "", datefi.cld$.group)
datefi.cld <- subset(datefi.cld)
datefi.cld
datehix.cld <- cld(hix.emm, 
                       by = c("treatment", "location"), 
                       alpha = 0.05, 
                       Letters = letters)
datehix.cld$.group <- gsub(" ", "", datehix.cld$.group)
datehix.cld <- subset(datehix.cld)
datehix.cld

ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = date, y = bix)) +
  geom_text(data = dateb.cld, 
            aes(x = date, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "date", y = "bix") +
  facet_grid(treatment~location, scales = "free_y")
ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = date, y = t)) +
  geom_text(data = datet.cld, 
            aes(x = date, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "date", y = "t") +
  facet_grid(treatment~location, scales = "free_y")
ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = date, y = a)) +
  geom_text(data = datea.cld, 
            aes(x = date, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "date", y = "a") +
  facet_grid(treatment~location, scales = "free_y")
ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = date, y = c)) +
  geom_text(data = datec.cld, 
            aes(x = date, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "date", y = "c") +
  facet_grid(treatment~location, scales = "free_y")
ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = date, y = fi)) +
  geom_text(data = datefi.cld, 
            aes(x = date, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "date", y = "fi") +
  facet_grid(treatment~location, scales = "free_y")
ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = date, y = hix)) +
  geom_text(data = datehix.cld, 
            aes(x = date, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "date", y = "hix") +
  facet_grid(treatment~location, scales = "free_y")
