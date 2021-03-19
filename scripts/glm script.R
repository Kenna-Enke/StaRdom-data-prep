# Script for NRSM 495 for learning how to do some R statistics and graphs.  As 
# always, let's get started by loading the relevant packages. If you haven't 
# already, you'll need to install these packages. You can do that by 
# highlighting the following and passing it to the Console:
# install.packages(c("multcomp", "tidyverse", "Rmisc", "boot", "lme4", "car", 
# "emmeans", "multcompView", "AICcmodavg", "readxl", "ggbiplot"))

# setting up packages----
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
library(cowplot)

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
BDA$stream <- ordered(BDA$stream, levels = c("lf", "f", "lh", "lp", "nfk", "tp"))
BDA$treatment <- as.ordered(BDA$treatment)
BDA$location <- as.ordered(BDA$location)
BDA$repitition <- as.ordered(BDA$repitition)
BDA$trip <- as.ordered(BDA$trip)
BDA$year <- as.ordered(BDA$year)
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
# We are using stream, location, and year to see if we can
# predict my indices

bix.stream.loation.year.glm <- glm(bix ~ stream*location*year,
                         data = BDA,
                         family = gaussian(link="identity"))
par(mfrow = c(2,2))
plot(bix.stream.loation.year.glm)
# OH MY GOD THIS IS SO EXCITING

# ok, let's do all my indices by location, stream, and year ----
bix.glm <- glm(bix ~ stream*location*year,
                                   data = BDA,
                                   family = gaussian(link="identity"))
t.glm <- glm(t ~ stream*location*year,
                                   data = BDA,
                                   family = gaussian(link="identity"))
a.glm <- glm(a ~ stream*location*year,
                                   data = BDA,
                                   family = gaussian(link="identity"))
c.glm <- glm(c ~ stream*location*year,
                                   data = BDA,
                                   family = gaussian(link="identity"))
fi.glm <- glm(fi ~ stream*location*year,
                                   data = BDA,
                                   family = gaussian(link="identity"))
hix.glm <- glm(hix ~ stream*location*year,
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
# bix.year.glm <- glm(bix ~ year,
#                       data = BDA,
#                       family = gaussian(link="identity"))
# t.stream.glm <- glm(t ~ stream,
#                       data = BDA,
#                       family = gaussian(link="identity"))
# t.location.glm <- glm(t ~ location,
#                     data = BDA,
#                     family = gaussian(link="identity"))
# t.year.glm <- glm(t ~ year,
#                     data = BDA,
#                     family = gaussian(link="identity"))
# a.stream.glm <- glm(a ~ stream,
#                     data = BDA,
#                     family = gaussian(link="identity"))
# a.location.glm <- glm(a ~ location,
#                     data = BDA,
#                     family = gaussian(link="identity"))
# a.year.glm <- glm(t ~ year,
#                     data = BDA,
#                     family = gaussian(link="identity"))
# c.stream.glm <- glm(c ~ stream,
#                     data = BDA,
#                     family = gaussian(link="identity"))
# c.location.glm <- glm(c ~ location,
#                     data = BDA,
#                     family = gaussian(link="identity"))
# c.year.glm <- glm(c ~ year,
#                      data = BDA,
#                      family = gaussian(link="identity"))
# fi.stream.glm <- glm(fi ~ stream,
#                     data = BDA,
#                     family = gaussian(link="identity"))
# fi.location.glm <- glm(fi ~ location,
#                      data = BDA,
#                      family = gaussian(link="identity"))
# fi.year.glm <- glm(fi ~ year,
#                      data = BDA,
#                      family = gaussian(link="identity"))
# hix.stream.glm <- glm(hix ~ stream,
#                      data = BDA,
#                      family = gaussian(link="identity"))
# hix.location.glm <- glm(hix ~ location,
#                       data = BDA,
#                       family = gaussian(link="identity"))
# hix.year.glm <- glm(hix ~ year,
#                       data = BDA,
#                       family = gaussian(link="identity"))
# plot(bix.stream.glm)
# plot(bix.location.glm)
# plot(bix.year.glm)
# plot(t.stream.glm)
# plot(t.location.glm)
# plot(t.year.glm)
# plot(a.stream.glm)
# plot(a.location.glm)
# plot(a.year.glm)
# plot(c.stream.glm)
# plot(c.location.glm)
# plot(c.year.glm)
# plot(fi.stream.glm)
# plot(fi.location.glm)
# plot(fi.year.glm)
# plot(hix.stream.glm)
# plot(hix.location.glm)
# plot(hix.year.glm)
# summary(bix.stream.glm)
# summary(bix.stream.glm)
# summary(bix.location.glm)
# summary(bix.year.glm)
# summary(t.stream.glm)
# summary(t.location.glm)
# summary(t.year.glm)
# summary(a.stream.glm)
# summary(a.location.glm)
# summary(a.year.glm)
# summary(c.stream.glm)
# summary(c.location.glm)
# summary(c.year.glm)
# summary(fi.stream.glm)
# summary(fi.location.glm)
# summary(fi.year.glm)
# summary(hix.stream.glm)
# summary(hix.location.glm)
# summary(hix.year.glm)
# (1-bix.stream.glm$deviance/bix.stream.glm$null.deviance)
# (1-bix.location.glm$deviance/bix.location.glm$null.deviance)
# (1-bix.year.glm$deviance/bix.year.glm$null.deviance)
# (1-t.stream.glm$deviance/t.stream.glm$null.deviance)
# (1-t.location.glm$deviance/t.location.glm$null.deviance)
# (1-t.year.glm$deviance/t.year.glm$null.deviance)
# (1-a.stream.glm$deviance/a.stream.glm$null.deviance)
# (1-a.location.glm$deviance/a.location.glm$null.deviance)
# (1-a.year.glm$deviance/a.year.glm$null.deviance)
# (1-c.stream.glm$deviance/c.stream.glm$null.deviance)
# (1-c.location.glm$deviance/c.location.glm$null.deviance)
# (1-c.year.glm$deviance/c.year.glm$null.deviance)
# (1-fi.stream.glm$deviance/fi.stream.glm$null.deviance)
# (1-fi.location.glm$deviance/fi.location.glm$null.deviance)
# (1-fi.year.glm$deviance/fi.year.glm$null.deviance)
# (1-hix.stream.glm$deviance/hix.stream.glm$null.deviance)
# (1-hix.location.glm$deviance/hix.location.glm$null.deviance)
# (1-hix.year.glm$deviance/hix.year.glm$null.deviance)
# Ok, ignore all that, that was just for fun

# now onto plotting data

# bix.coefficients <- bix.glm$coefficients 
# t.coefficients <- t.glm$coefficients
# a.coefficients <- a.glm$coefficients
# c.coefficients <- c.glm$coefficients
# fi.coefficients <- fi.glm$coefficients
# hix.coefficients <- hix.glm$coefficients
# ggplot(BDA) +
#   geom_point(aes(x = bix, y = stream*location*year, color = name))
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

# glm with treatments, locations, and year----
bix.glm <- glm(bix ~ treatment*location*year,
               data = BDA,
               family = gaussian(link="identity"))
t.glm <- glm(t ~ treatment*location*year,
             data = BDA,
             family = gaussian(link="identity"))
a.glm <- glm(a ~ treatment*location*year,
             data = BDA,
             family = gaussian(link="identity"))
c.glm <- glm(c ~ treatment*location*year,
             data = BDA,
             family = gaussian(link="identity"))
fi.glm <- glm(fi ~ treatment*location*year,
              data = BDA,
              family = gaussian(link="identity"))
hix.glm <- glm(hix ~ treatment*location*year,
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

bix.emm <- emmeans(bix.glm, ~ treatment | location | year, type = "response")
bix.emm
t.emm <- emmeans(t.glm, ~ treatment | location | year, type = "response")
t.emm
a.emm <- emmeans(a.glm, ~ treatment | location | year, type = "response")
a.emm
c.emm <- emmeans(c.glm, ~ treatment | location | year, type = "response")
c.emm
fi.emm <- emmeans(fi.glm, ~ treatment | location | year, type = "response")
fi.emm
hix.emm <- emmeans(hix.glm, ~ treatment | location | year, type = "response")
hix.emm

# by treatment----
treatmentb.cld <- cld(bix.emm, 
                      by = c("location", "year"), 
                      alpha = 0.05, 
                      Letters = letters)
treatmentb.cld$.group <- gsub(" ", "", treatmentb.cld$.group)
treatmentb.cld <- subset(treatmentb.cld)
treatmentb.cld
treatmenta.cld <- cld(a.emm, 
                      by = c("location", "year"), 
                      alpha = 0.05, 
                      Letters = letters)
treatmenta.cld$.group <- gsub(" ", "", treatmenta.cld$.group)
treatmenta.cld <- subset(treatmenta.cld)
treatmenta.cld
treatmentt.cld <- cld(t.emm, 
                      by = c("location", "year"), 
                      alpha = 0.05, 
                      Letters = letters)
treatmentt.cld$.group <- gsub(" ", "", treatmentt.cld$.group)
treatmentt.cld <- subset(treatmentt.cld)
treatmentt.cld
treatmentc.cld <- cld(c.emm, 
                      by = c("location", "year"), 
                      alpha = 0.05, 
                      Letters = letters)
treatmentc.cld$.group <- gsub(" ", "", treatmentc.cld$.group)
treatmentc.cld <- subset(treatmentc.cld)
treatmentc.cld
treatmentfi.cld <- cld(fi.emm, 
                       by = c("location", "year"), 
                       alpha = 0.05, 
                       Letters = letters)
treatmentfi.cld$.group <- gsub(" ", "", treatmentfi.cld$.group)
treatmentfi.cld <- subset(treatmentfi.cld)
treatmentfi.cld
treatmenthix.cld <- cld(hix.emm, 
                        by = c("location", "year"), 
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
  facet_grid(location~year, scales = "free_y")
ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = treatment, y = t)) +
  geom_text(data = treatmentt.cld, 
            aes(x = treatment, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "Treatment", y = "t") +
  facet_grid(location~year, scales = "free_y")
ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = treatment, y = a)) +
  geom_text(data = treatmenta.cld, 
            aes(x = treatment, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "Treatment", y = "a") +
  facet_grid(location~year, scales = "free_y")
ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = treatment, y = c)) +
  geom_text(data = treatmentc.cld, 
            aes(x = treatment, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "Treatment", y = "c") +
  facet_grid(location~year, scales = "free_y")
ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = treatment, y = fi)) +
  geom_text(data = treatmentfi.cld, 
            aes(x = treatment, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "Treatment", y = "fi") +
  facet_grid(location~year, scales = "free_y")
ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = treatment, y = hix)) +
  geom_text(data = treatmenthix.cld, 
            aes(x = treatment, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "Treatment", y = "hix") +
  facet_grid(location~year, scales = "free_y")


# by location----
locationb.cld <- cld(bix.emm, 
                      by = c("treatment", "year"), 
                      alpha = 0.05, 
                      Letters = letters)
locationb.cld$.group <- gsub(" ", "", locationb.cld$.group)
locationb.cld <- subset(locationb.cld)
locationb.cld
locationa.cld <- cld(a.emm, 
                      by = c("treatment", "year"), 
                      alpha = 0.05, 
                      Letters = letters)
locationa.cld$.group <- gsub(" ", "", locationa.cld$.group)
locationa.cld <- subset(locationa.cld)
locationa.cld
locationt.cld <- cld(t.emm, 
                      by = c("treatment", "year"), 
                      alpha = 0.05, 
                      Letters = letters)
locationt.cld$.group <- gsub(" ", "", locationt.cld$.group)
locationt.cld <- subset(locationt.cld)
locationt.cld
locationc.cld <- cld(c.emm, 
                      by = c("treatment", "year"), 
                      alpha = 0.05, 
                      Letters = letters)
locationc.cld$.group <- gsub(" ", "", locationc.cld$.group)
locationc.cld <- subset(locationc.cld)
locationc.cld
locationfi.cld <- cld(fi.emm, 
                       by = c("treatment", "year"), 
                       alpha = 0.05, 
                       Letters = letters)
locationfi.cld$.group <- gsub(" ", "", locationfi.cld$.group)
locationfi.cld <- subset(locationfi.cld)
locationfi.cld
locationhix.cld <- cld(hix.emm, 
                        by = c("treatment", "year"), 
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
  facet_grid(treatment~year, scales = "free_y")
ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = location, y = t)) +
  geom_text(data = locationt.cld, 
            aes(x = location, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "location", y = "t") +
  facet_grid(treatment~year, scales = "free_y")
ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = location, y = a)) +
  geom_text(data = locationa.cld, 
            aes(x = location, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "location", y = "a") +
  facet_grid(treatment~year, scales = "free_y")
ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = location, y = c)) +
  geom_text(data = locationc.cld, 
            aes(x = location, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "location", y = "c") +
  facet_grid(treatment~year, scales = "free_y")
ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = location, y = fi)) +
  geom_text(data = locationfi.cld, 
            aes(x = location, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "location", y = "fi") +
  facet_grid(treatment~year, scales = "free_y")
ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = location, y = hix)) +
  geom_text(data = locationhix.cld, 
            aes(x = location, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "location", y = "hix") +
  facet_grid(treatment~year, scales = "free_y")

# by year----
yearb.cld <- cld(bix.emm, 
                     by = c("treatment", "location"), 
                     alpha = 0.05, 
                     Letters = letters)
yearb.cld$.group <- gsub(" ", "", yearb.cld$.group)
yearb.cld <- subset(yearb.cld)
yearb.cld
yeara.cld <- cld(a.emm, 
                     by = c("treatment", "location"), 
                     alpha = 0.05, 
                     Letters = letters)
yeara.cld$.group <- gsub(" ", "", yeara.cld$.group)
yeara.cld <- subset(yeara.cld)
yeara.cld
yeart.cld <- cld(t.emm, 
                     by = c("treatment", "location"), 
                     alpha = 0.05, 
                     Letters = letters)
yeart.cld$.group <- gsub(" ", "", yeart.cld$.group)
yeart.cld <- subset(yeart.cld)
yeart.cld
yearc.cld <- cld(c.emm, 
                     by = c("treatment", "location"), 
                     alpha = 0.05, 
                     Letters = letters)
yearc.cld$.group <- gsub(" ", "", yearc.cld$.group)
yearc.cld <- subset(yearc.cld)
yearc.cld
yearfi.cld <- cld(fi.emm, 
                      by = c("treatment", "location"), 
                      alpha = 0.05, 
                      Letters = letters)
yearfi.cld$.group <- gsub(" ", "", yearfi.cld$.group)
yearfi.cld <- subset(yearfi.cld)
yearfi.cld
yearhix.cld <- cld(hix.emm, 
                       by = c("treatment", "location"), 
                       alpha = 0.05, 
                       Letters = letters)
yearhix.cld$.group <- gsub(" ", "", yearhix.cld$.group)
yearhix.cld <- subset(yearhix.cld)
yearhix.cld

ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = year, y = bix)) +
  geom_text(data = yearb.cld, 
            aes(x = year, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "year", y = "bix") +
  facet_grid(treatment~location, scales = "free_y")
ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = year, y = t)) +
  geom_text(data = yeart.cld, 
            aes(x = year, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "year", y = "t") +
  facet_grid(treatment~location, scales = "free_y")
ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = year, y = a)) +
  geom_text(data = yeara.cld, 
            aes(x = year, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "year", y = "a") +
  facet_grid(treatment~location, scales = "free_y")
ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = year, y = c)) +
  geom_text(data = yearc.cld, 
            aes(x = year, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "year", y = "c") +
  facet_grid(treatment~location, scales = "free_y")
ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = year, y = fi)) +
  geom_text(data = yearfi.cld, 
            aes(x = year, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "year", y = "fi") +
  facet_grid(treatment~location, scales = "free_y")
ggplot() +
  geom_boxplot(data = BDA, 
               aes(x = year, y = hix)) +
  geom_text(data = yearhix.cld, 
            aes(x = year, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), size = 5) +
  labs(x = "year", y = "hix") +
  facet_grid(treatment~location, scales = "free_y")

# breaking down further ----
bix.glm1 <- glm(bix ~ treatment:location + treatment:year + location:year +
                  treatment + location + year,
               data = BDA,
               family = gaussian(link="identity"))
t.glm1 <- glm(t ~ treatment:location + treatment:year + location:year + 
                treatment + location + year,
             data = BDA,
             family = gaussian(link="identity"))
a.glm1 <- glm(a ~ treatment:location + treatment:year + location:year + 
                treatment + location + year,
             data = BDA,
             family = gaussian(link="identity"))
c.glm1 <- glm(c ~ treatment:location + treatment:year + location:year + 
                treatment + location + year,
             data = BDA,
             family = gaussian(link="identity"))
fi.glm1 <- glm(fi ~ treatment:location + treatment:year + location:year + 
                 treatment + location + year,
              data = BDA,
              family = gaussian(link="identity"))
hix.glm1 <- glm(hix ~ treatment:location + treatment:year + location:year + 
                  treatment + location + year,
               data = BDA,
               family = gaussian(link="identity"))
par(mfrow = c(2,2))
plot(bix.glm1)
plot(t.glm1)
plot(a.glm1)
plot(c.glm1)
plot(fi.glm1)
plot(hix.glm1)
Anova(bix.glm1)   
Anova(t.glm1)
Anova(a.glm1)
Anova(c.glm1)
Anova(fi.glm1)
Anova(hix.glm1)

# subset treatment by location by year with only impact----
? subset
bix.glm2 <- glm(bix ~ location:year + location + year,
                data = BDA,
                family = gaussian(link="identity"))
t.glm2 <- glm(t ~ location:year + location + year,
              data = BDA,
              family = gaussian(link="identity"))
a.glm2 <- glm(a ~ location:year + location + year,
              data = BDA,
              family = gaussian(link="identity"))
c.glm2 <- glm(c ~ location:year + location + year,
              data = BDA,
              family = gaussian(link="identity"))
fi.glm2 <- glm(fi ~ location:year + location + year,
               data = BDA,
               family = gaussian(link="identity"))
hix.glm2 <- glm(hix ~ location:year + location + year,
                data = BDA,
                family = gaussian(link="identity"))

par(mfrow = c(2,2))
plot(bix.glm2)
plot(t.glm2)
plot(a.glm2)
plot(c.glm2)
plot(fi.glm2)
plot(hix.glm2)
Anova(bix.glm2)   
Anova(t.glm2)
Anova(a.glm2)
Anova(c.glm2)
Anova(fi.glm2)
Anova(hix.glm2)

# by trip----
bix.glm3 <- glm(bix ~ location:trip + location + trip,
                data = BDA,
                family = gaussian(link="identity"))
t.glm3 <- glm(t ~ location:trip + location + trip,
              data = BDA,
              family = gaussian(link="identity"))
a.glm3 <- glm(a ~ location:trip + location + trip,
              data = BDA,
              family = gaussian(link="identity"))
c.glm3 <- glm(c ~ location:trip + location + trip,
              data = BDA,
              family = gaussian(link="identity"))
fi.glm3 <- glm(fi ~ location:trip + location + trip,
               data = BDA,
               family = gaussian(link="identity"))
hix.glm3 <- glm(hix ~ location:trip + location + trip,
                data = BDA,
                family = gaussian(link="identity"))
par(mfrow = c(2,2))
plot(bix.glm3)
plot(t.glm3)
plot(a.glm3)
plot(c.glm3)
plot(fi.glm3)
plot(hix.glm3)
Anova(bix.glm3)   
Anova(t.glm3)
Anova(a.glm3)
Anova(c.glm3)
Anova(fi.glm3)
Anova(hix.glm3)

bix.glm4 <- glm(bix ~ treatment*location*trip,
               data = BDA,
               family = gaussian(link="identity"))
t.glm4 <- glm(t ~ treatment*location*trip,
             data = BDA,
             family = gaussian(link="identity"))
a.glm4 <- glm(a ~ treatment*location*trip,
             data = BDA,
             family = gaussian(link="identity"))
c.glm4 <- glm(c ~ treatment*location*trip,
             data = BDA,
             family = gaussian(link="identity"))
fi.glm4 <- glm(fi ~ treatment*location*trip,
              data = BDA,
              family = gaussian(link="identity"))
hix.glm4 <- glm(hix ~ treatment*location*trip,
               data = BDA,
               family = gaussian(link="identity"))

par(mfrow = c(2,2))
plot(bix.glm4)
plot(t.glm4)
plot(a.glm4)
plot(c.glm4)
plot(fi.glm4)
plot(hix.glm4)


Anova(bix.glm4)   
Anova(t.glm4)
Anova(a.glm4)
Anova(c.glm4)
Anova(fi.glm4)
Anova(hix.glm4)
Anova(hix.glm4)

## anovas for specific questions----
# look at treatment by location by year and then by trip
# stream by location by time and then stream by time

# treatment by location by year
bix.glm3 <- glm(bix ~ treatment*location*year,
                data = BDA,
                family = gaussian(link="identity"))
t.glm3 <- glm(t ~ treatment*location*year,
              data = BDA,
              family = gaussian(link="identity"))
a.glm3 <- glm(a ~ treatment*location*year,
              data = BDA,
              family = gaussian(link="identity"))
c.glm3 <- glm(c ~ treatment*location*year,
              data = BDA,
              family = gaussian(link="identity"))
fi.glm3 <- glm(fi ~ treatment*location*year,
               data = BDA,
               family = gaussian(link="identity"))
hix.glm3 <- glm(hix ~ treatment*location*year,
                data = BDA,
                family = gaussian(link="identity"))

# par(mfrow = c(2,2))
# plot(bix.glm3)
# plot(t.glm3)
# plot(a.glm3)
# plot(c.glm3)
# plot(fi.glm3)
# plot(hix.glm3)


Anova(bix.glm3)   
Anova(t.glm3)
Anova(a.glm3)
Anova(c.glm3)
Anova(fi.glm3)
Anova(hix.glm3)
Anova(hix.glm3)

# treatment by location by trip
bix.glm4 <- glm(bix ~ treatment*location*trip,
                data = BDA,
                family = gaussian(link="identity"))
t.glm4 <- glm(t ~ treatment*location*trip,
              data = BDA,
              family = gaussian(link="identity"))
a.glm4 <- glm(a ~ treatment*location*trip,
              data = BDA,
              family = gaussian(link="identity"))
c.glm4 <- glm(c ~ treatment*location*trip,
              data = BDA,
              family = gaussian(link="identity"))
fi.glm4 <- glm(fi ~ treatment*location*trip,
               data = BDA,
               family = gaussian(link="identity"))
hix.glm4 <- glm(hix ~ treatment*location*trip,
                data = BDA,
                family = gaussian(link="identity"))

# par(mfrow = c(2,2))
# plot(bix.glm4)
# plot(t.glm4)
# plot(a.glm4)
# plot(c.glm4)
# plot(fi.glm4)
# plot(hix.glm4)


Anova(bix.glm4)   
Anova(t.glm4)
Anova(a.glm4)
Anova(c.glm4)
Anova(fi.glm4)
Anova(hix.glm4)
Anova(hix.glm4)

# treatment by year
bix.glm5 <- glm(bix ~ treatment*year,
                data = BDA,
                family = gaussian(link="identity"))
t.glm5 <- glm(t ~ treatment*year,
              data = BDA,
              family = gaussian(link="identity"))
a.glm5 <- glm(a ~ treatment*year,
              data = BDA,
              family = gaussian(link="identity"))
c.glm5 <- glm(c ~ treatment*year,
              data = BDA,
              family = gaussian(link="identity"))
fi.glm5 <- glm(fi ~ treatment*year,
               data = BDA,
               family = gaussian(link="identity"))
hix.glm5 <- glm(hix ~ treatment*year,
                data = BDA,
                family = gaussian(link="identity"))

# par(mfrow = c(2,2))
# plot(bix.glm5)
# plot(t.glm5)
# plot(a.glm5)
# plot(c.glm5)
# plot(fi.glm5)
# plot(hix.glm5)


Anova(bix.glm5)   
Anova(t.glm5)
Anova(a.glm5)
Anova(c.glm5)
Anova(fi.glm5)
Anova(hix.glm5)
aov(hix.glm5)

# treatment by trip
bix.glm6 <- glm(bix ~ treatment*trip,
                data = BDA,
                family = gaussian(link="identity"))
t.glm6 <- glm(t ~ treatment*trip,
              data = BDA,
              family = gaussian(link="identity"))
a.glm6 <- glm(a ~ treatment*trip,
              data = BDA,
              family = gaussian(link="identity"))
c.glm6 <- glm(c ~ treatment*trip,
              data = BDA,
              family = gaussian(link="identity"))
fi.glm6 <- glm(fi ~ treatment*trip,
               data = BDA,
               family = gaussian(link="identity"))
hix.glm6 <- glm(hix ~ treatment*trip,
                data = BDA,
                family = gaussian(link="identity"))

# par(mfrow = c(2,2))
# plot(bix.glm6)
# plot(t.glm6)
# plot(a.glm6)
# plot(c.glm6)
# plot(fi.glm6)
# plot(hix.glm6)


Anova(bix.glm6)   
Anova(t.glm6)
Anova(a.glm6)
Anova(c.glm6)
Anova(fi.glm6)
Anova(hix.glm6)
Anova(hix.glm6) 

# stream by location by year
bix.glm7 <- glm(bix ~ stream*location*year,
                data = BDA,
                family = gaussian(link="identity"))
t.glm7 <- glm(t ~ stream*location*year,
              data = BDA,
              family = gaussian(link="identity"))
a.glm7 <- glm(a ~ stream*location*year,
              data = BDA,
              family = gaussian(link="identity"))
c.glm7 <- glm(c ~ stream*location*year,
              data = BDA,
              family = gaussian(link="identity"))
fi.glm7 <- glm(fi ~ stream*location*year,
               data = BDA,
               family = gaussian(link="identity"))
hix.glm7 <- glm(hix ~ stream*location*year,
                data = BDA,
                family = gaussian(link="identity"))

# par(mfrow = c(2,2))
# plot(bix.glm7)
# plot(t.glm7)
# plot(a.glm7)
# plot(c.glm7)
# plot(fi.glm7)
# plot(hix.glm7)


Anova(bix.glm7)   
Anova(t.glm7)
Anova(a.glm7)
Anova(c.glm7)
Anova(fi.glm7)
Anova(hix.glm7)
Anova(hix.glm7) 

# stream by location by trip
bix.glm8 <- glm(bix ~ stream*location*trip,
                data = BDA,
                family = gaussian(link = "identity"))
t.glm8 <- glm(t ~ stream*location*trip,
              data = BDA,
              family = gaussian(link="identity"))
a.glm8 <- glm(a ~ stream*location*trip,
              data = BDA,
              family = gaussian(link="identity"))
c.glm8 <- glm(c ~ stream*location*trip,
              data = BDA,
              family = gaussian(link="identity"))
fi.glm8 <- glm(fi ~ stream*location*trip,
               data = BDA,
               family = gaussian(link="identity"))
hix.glm8 <- glm(hix ~ stream*location*trip,
                data = BDA,
                family = gaussian(link="identity"))

# par(mfrow = c(2,2))
# plot(bix.glm8)
# plot(t.glm8)
# plot(a.glm8)
# plot(c.glm8)
# plot(fi.glm8)
# plot(hix.glm8)


Anova(bix.glm8)   
Anova(t.glm8)
Anova(a.glm8)
Anova(c.glm8)
Anova(fi.glm8)
Anova(hix.glm8)
Anova(hix.glm8) 

#emmeans
bix.emm2 <- emmeans(bix.glm8, ~ stream | location | trip, type = "response")
bix.emm2
t.emm2 <- emmeans(t.glm8, ~ stream | location | trip, type = "response")
t.emm2
a.emm2 <- emmeans(a.glm8, ~ stream | location | trip, type = "response")
a.emm2
c.emm2 <- emmeans(c.glm8, ~ stream | location | trip, type = "response")
c.emm2
fi.emm2 <- emmeans(fi.glm8, ~ stream | location | trip, type = "response")
fi.emm2
hix.emm2 <- emmeans(hix.glm8, ~ stream | location | trip, type = "response")
hix.emm2

#cld (Not working due to nested variables)

bix.cld <- cld(bix.emm2, 
               by = c("stream", "trip"),
               alpha = 0.05,
               Letters = letters)
hix.cld <- cld(hix.emm2, 
               by = c("stream", "trip"),
               alpha = 0.05,
               Letters = letters)
t.cld <- cld(t.emm2, 
               by = c("stream", "trip"),
               alpha = 0.05,
               Letters = letters)
a.cld <- cld(a.emm2, 
               by = c("stream", "trip"),
               alpha = 0.05,
               Letters = letters)
c.cld <- cld(c.emm2, 
               by = c("stream", "trip"),
               alpha = 0.05,
               Letters = letters)
fi.cld <- cld(fi.emm2, 
               by = c("stream", "trip"),
               alpha = 0.05,
               Letters = letters)
# Reformatting bix.emm2 to be a data frame for better plotting
bix.emm2 <- as.data.frame(bix.emm2)
# levels(bix.emm2$trip) <- c(1:16)
bix.emm2$trip <- as.numeric(bix.emm2$trip)

# stream by year
 
bix.glm9 <- glm(bix ~ stream*year,
                data = BDA,
                family = gaussian(link="identity"))
t.glm9 <- glm(t ~ stream*year,
              data = BDA,
              family = gaussian(link="identity"))
a.glm9 <- glm(a ~ stream*year,
              data = BDA,
              family = gaussian(link="identity"))
c.glm9 <- glm(c ~ stream*year,
              data = BDA,
              family = gaussian(link="identity"))
fi.glm9 <- glm(fi ~ stream*year,
               data = BDA,
               family = gaussian(link="identity"))
hix.glm9 <- glm(hix ~ stream*year,
                data = BDA,
                family = gaussian(link="identity"))

# par(mfrow = c(2,2))
# plot(bix.glm9)
# plot(t.glm9)
# plot(a.glm9)
# plot(c.glm9)
# plot(fi.glm9)
# plot(hix.glm9)

Anova(bix.glm9)   
Anova(t.glm9)
Anova(a.glm9)
Anova(c.glm9)
Anova(fi.glm9)
Anova(hix.glm9)
Anova(hix.glm9) 

# stream by trip
bix.glm10 <- glm(bix ~ stream*trip,
                data = BDA,
                family = gaussian(link="identity"))
t.glm10 <- glm(t ~ stream*trip,
              data = BDA,
              family = gaussian(link="identity"))
a.glm10 <- glm(a ~ stream*trip,
              data = BDA,
              family = gaussian(link="identity"))
c.glm10 <- glm(c ~ stream*trip,
              data = BDA,
              family = gaussian(link="identity"))
fi.glm10 <- glm(fi ~ stream*trip,
               data = BDA,
               family = gaussian(link="identity"))
hix.glm10 <- glm(hix ~ stream*trip,
                data = BDA,
                family = gaussian(link="identity"))

# par(mfrow = c(2,2))
# plot(bix.glm10)
# plot(t.glm10)
# plot(a.glm10)
# plot(c.glm10)
# plot(fi.glm10)
# plot(hix.glm10)

Anova(bix.glm10)   
Anova(t.glm10)
Anova(a.glm10)
Anova(c.glm10)
Anova(fi.glm10)
Anova(hix.glm10)
Anova(hix.glm10) 

## just the anovas----
# treatment by location by year
Anova(bix.glm3)   
Anova(t.glm3)
Anova(a.glm3)
Anova(c.glm3)
Anova(fi.glm3)
Anova(hix.glm3)

# treatment by location by trip
Anova(bix.glm4)   
Anova(t.glm4)
Anova(a.glm4)
Anova(c.glm4)
Anova(fi.glm4)
Anova(hix.glm4)

# treatment by year
Anova(bix.glm5)   
Anova(t.glm5)
Anova(a.glm5)
Anova(c.glm5)
Anova(fi.glm5)
Anova(hix.glm5)

# treatment by trip
Anova(bix.glm6)   
Anova(t.glm6)
Anova(a.glm6)
Anova(c.glm6)
Anova(fi.glm6)
Anova(hix.glm6)

# stream by location by year
Anova(bix.glm7)   
Anova(t.glm7)
Anova(a.glm7)
Anova(c.glm7)
Anova(fi.glm7)
Anova(hix.glm7)



#stream by location by trip
Anova(bix.glm8)   
Anova(t.glm8)
Anova(a.glm8)
Anova(c.glm8)
Anova(fi.glm8)
Anova(hix.glm8)

# stream by year
Anova(bix.glm9)   
Anova(t.glm9)
Anova(a.glm9)
Anova(c.glm9)
Anova(fi.glm9)
Anova(hix.glm9)

# stream by trip
Anova(bix.glm10)   
Anova(t.glm10)
Anova(a.glm10)
Anova(c.glm10)
Anova(fi.glm10)
Anova(hix.glm10)