# Script for NRSM 495 for learning how to do some R statistics and graphs.  As 
# always, let's get started by loading the relevant packages. If you haven't 
# already, you'll need to install these packages. You can do that by 
# highlighting the following and passing it to the Console:
# install.packages(c("multcomp", "tidyverse", "Rmisc", "boot", "lme4", "car", "emmeans", "multcompView", "AICcmodavg", "readxl", "ggbiplot"))

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

# importing my data 
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




# Let's also set a theme for our graphs

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
# ok, let's do all my indices
bix.stream.location.date.glm <- glm(bix ~ stream*location*date,
                                   data = BDA,
                                   family = gaussian(link="identity"))
t.stream.location.date.glm <- glm(t ~ stream*location*date,
                                   data = BDA,
                                   family = gaussian(link="identity"))
a.stream.location.date.glm <- glm(a ~ stream*location*date,
                                   data = BDA,
                                   family = gaussian(link="identity"))
c.stream.location.date.glm <- glm(c ~ stream*location*date,
                                   data = BDA,
                                   family = gaussian(link="identity"))
fi.stream.location.date.glm <- glm(fi ~ stream*location*date,
                                   data = BDA,
                                   family = gaussian(link="identity"))
hix.stream.location.date.glm <- glm(hix ~ stream*location*date,
                                   data = BDA,
                                   family = gaussian(link="identity"))
par(mfrow = c(2,2))
plot(bix.stream.location.date.glm)
plot(t.stream.location.date.glm)
plot(a.stream.location.date.glm)
plot(c.stream.location.date.glm)
plot(fi.stream.location.date.glm)
plot(hix.stream.location.date.glm)
summary(bix.stream.location.date.glm)
(1-bix.stream.location.date.glm$deviance/bix.stream.location.date.glm$null.deviance)
(1-t.stream.location.date.glm$deviance/t.stream.location.date.glm$null.deviance)
(1-a.stream.location.date.glm$deviance/a.stream.location.date.glm$null.deviance)
(1-c.stream.location.date.glm$deviance/c.stream.location.date.glm$null.deviance)
(1-fi.stream.location.date.glm$deviance/fi.stream.location.date.glm$null.deviance)
(1-hix.stream.location.date.glm$deviance/hix.stream.location.date.glm$null.deviance)

