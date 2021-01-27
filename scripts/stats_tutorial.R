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
library(ggplot2)
library(tibble)

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

## Example 1, irises, regression and 1-way ANOVA
## Correlations and regressions, Iris ----- 
# Sometimes, we want to see if one continuous variable is related to another 
# continuous variable. When that's the case, we usually use a linear regression
# or some similar regression based technique. We'll start by looking at the
# built in 'iris' data set and compare the length and width of petals. To see 
# where this built in data set comes from, you can search for it. 
 
?iris
 
# Let's start by taking a peek at the dataset with three different
# functions: 'head' which gives us the first 6 lines; 'glimpse', which gives a 
# little more info (data classes, # observations, # variables); and 'str', 
# which gives us the structure including the number of levels for factor 
# variables and it also tells us what kind of data structure we have 
# (data.frame):

head(iris)
glimpse(iris)
str(iris)

# I would argue that these data are in a bit of a strange order, as the species
# is in the last column. We could change that, but it doesn't really matter at
# all.

# For a first pass at looking at the data, we can use the built in plot function
# that r has to plot everything vs. everything.
plot(iris)

# OK, that is useful. It shows that many of these are related to one another 
# (e.g., Petal Length and Width). It also shows that there may be some 
# variability explained by species. Let's do it with more pizzaz for the 
# continuous variables.

chart.Correlation(iris[, 1:4])

# That gives you a bit more data. The histograms show the distribution of the
# data, the dot plots show the data plotted vs. one another, and the numbers
# show the "correlation coefficient". If we take this number and square it, 
# it gives the R2, which we describe as the proportion of the variance described
# for the y axis variable by the x. The asterisks indicate how "significant" 
# the values are, with 0.05, 0.01, and 0.001 being signified by '*', '**', and 
# '***' respectively.
# 
# Cool, so it looks like there is a relationship between many of the different
# characteristics, with interesting grouping of some of the parameters. It 
# also looks like petal length and width are 'bimodal', meaning they have two 
# humps. 
# 
# Let's look more explicitly at the relationship between petal length and petal 
# width. To do this, we need to build a 'statistical model'. Not a big deal, 
# believe it or not. To compare two continuous variables like Petal.Length and 
# Petal.Width, we will make a general linear model (GLM). This is kind of like
# a linear regression, only it has a lot more options in terms of how you 
# structure it. For example, you specify the 'family' of the distribution of the
# data, as well as a link function. For continuous data such as these, a
# gaussian or Gamma family is likely the best. With both of those, a 'log' link 
# can be specified if the data are distributed with a long tail. None of these 
# data fit that bill, so we'll just use a gaussian with an 'identity' link. The
# 'formula' we enter is 'y ~ x', which in this case equates to 

# Petal.Length = m * Petal.Width + b

# Where m is the slope of the line associated with Petal.Width and b is the 
# intercept.

length.width.glm1 <- glm(Petal.Length ~ Petal.Width,
    data = iris,
    family = gaussian(link="identity"))

# First thing to do is look at some plots to make sure there is nothing really
# strange in the data, where really strange means strong patterns in the 
# following plots...note that you'll have to go to the console and hit enter a
# few times to go through the plots):
par(mfrow = c(2,2))

plot(length.width.glm1)

# These plots show that: 1) your residuals (scatter around best fit line) are 
# pretty balanced around 0 (that's great!); 2) your data are fairly normal, 
# though glm is less susceptible to that causing problems; 3) sqrrt of deviance
# residuals is also pretty balanced; 4) so are the residuals vs. leverage. This
# does point to a few points that might be exerting undue influence, but it is
# not too bad.

# We can see a summary of what the model gives us:

summary(length.width.glm1)

# This output gives us a lot of useful information. It reminds us of how we fit the model
# while also giving us the coefficients for the slope (m) and intercept (b) for 
# a line of best fit. We can see the "null deviance" and the "residual
# deviance" and calculate a correlation coefficient as is shown above by taking
# 1-residual deviance/null deviance straight from the model object:

(1-length.width.glm1$deviance/length.width.glm1$null.deviance)

# This gives us 0.93, suggesting that width explains 93% of the variance in
# length! If we take the square root, we get 0.96, which is the same as we 
# measured in the correlation plot. Let's plot it up, and let's color the 
# different species different colors. Let's plot it as points, but also as a 
# line of best fit pulling in the slope and intecept coefficients from the
# model:

length.width.coefficients <- length.width.glm1$coefficients 

ggplot(iris) +
  geom_point(aes(x = Petal.Width, y = Petal.Length, color = Species)) +
  geom_line(aes(x = Petal.Width, 
                y = length.width.coefficients[2]*Petal.Width + 
                  length.width.coefficients[1])) +
  labs(x = "Petal width (cm)", y = "Petal Length (cm)")

# Some clear patterns here in the data by species. Also, best fit line seems to
# fit the data. That's always reassuring.
# 
## 1-way ANOVA, Iris ----
# Let's see if the species are different statistically looking at Length. To do 
# this, we'll again make a model. The only difference is that we'll put Species 
# in, and it is a categorical variable.

species.sepal.length.glm <-
  glm(Sepal.Length ~ Species,
      data = iris,
      family = gaussian(link = identity))

# Here, rather than using the summary data to evaluate the model, what we really
# want to do is see if there is heterogeneity in length explained by the 
# parameter Species. We do this by doing an "ANalysis Of VAriance" or ANOVA.
# This only tells us if there are differences, not where those differences lie.
# If we see no differences significant at p <0.05, we are done and we suggest 
# that the species are similar.

Anova(species.sepal.length.glm)

# OK, if we look at the Pr value, it suggests that the probability that the 
# differences between species arising due to random chance is < 2.2 x 10^-16.
# That is super low! Now we need to see where those differences lie. We will
# do that in a two step process, first using emmeans to compare the mean values
# for each species, then we'll compare the pairs in using 'cld', which both
# compares them and assigns letters (cld stands for compact letter display) to
# indicate where differences lie.

species.sepal.length.emm <- emmeans(species.sepal.length.glm, ~ Species, 
                                    type="response")

species.sepal.length.cld <- cld(species.sepal.length.emm, alpha = 0.05, Letters = letters)
species.sepal.length.cld

# So it looks like all three species are significantly different from one 
# another, which matches what we see with our eyes. Cool part is we can use
# the output of cld to automagically label our data when we plot it up. Since
# the '.group' column is not lined up, we need to remove spaces. We can do that 
# with 'gsub' which will substitue a lack of space ("") for spaces (" ").

species.sepal.length.cld$.group <- 
  gsub(" ", "", species.sepal.length.cld$.group)
species.sepal.length.cld

# There is some extra text at the bottom, but we can get rid of the extra text
# by using 'subset', but not specifying any other parameters.

species.sepal.length.cld <- subset(species.sepal.length.cld)
species.sepal.length.cld

# OK, good to go. Let's plot it up.

ggplot(species.sepal.length.cld) +
  geom_point(aes(x = Species, y = emmean)) +
  geom_errorbar(aes(x = Species, ymin = asymp.LCL, ymax = asymp.UCL)) +
  geom_text(aes(x = Species, y = emmean, label = .group))


# Hahahaha, those error bars are funny. Let's fix those so they aren't so 
# gigantic, and also make the points bigger, rename the axis, and adjust the
# location of the letters using and 'vjust'. I'm assuming that these iris 
# measurments are cm, so we'll label it that.

ggplot(species.sepal.length.cld) +
  geom_point(aes(x = Species, y = emmean), size = 5) +
  geom_errorbar(aes(x = Species, ymin = asymp.LCL, ymax = asymp.UCL), 
                width = 0.1) + #LCL is lower confidence limit, UCL is upper
  labs(y = "Sepal length (cm)") +
  geom_text(data = species.sepal.length.cld, 
            aes(x = Species, y = emmean, label = .group,
                vjust = c(-1.6, -1.6, 2.2)), size = 6)

# Much less comical. Letter above virginica was hiding when I put it above (
# vjust = -1.6) , so I gave individualized  vjust for each point 
# (vjust = c(-1.6, -1.6, 2.2)).  Notice that all of the error
# bars are about the same size. This is due to the fact that the errors are the
# modeled errors. If we want to show the actual variability. We can still put
# on the letters identifying differences, and to do this, we'll need to pull
# seperate objects to each of the ggplot geoms. I'll also add both a vjust
# and an hjust to make it easier to fit the letters on there.

ggplot() +
  geom_boxplot(data = iris, aes(x = Species, y = Sepal.Length))+
  geom_text(data = species.sepal.length.cld, 
            aes(x = Species, y = emmean, label = .group,
            vjust = -3.2, hjust = 1.3), size = 6) +
  labs(y = "Sepal length (cm)")
  
  
## 2-way ANOVA, ToothGrowth ----
# Here we have a data set with tooth growth measured as length (len) in guinea 
# pigs given one of three different doses (dose) of ascorbic acid as vit C (C) 
# or as orange juice (OJ). 
# 
# Note: Where we had one categorical variable for our ANOVA (one-way ANOVA)
# above, here we have two (dose and supp). This means in our GLM we will check
# for each of the main effects (dose or supp) while ignoring the other main 
# effects, as well as their interaction, which takes into account the fact that
# dose may have a different effect for each supplement form, and supplement may 
# have a different effect for each dose. This will be clearer below. First,
# let's take a look at the data.

toothgrowth <- ToothGrowth
str(toothgrowth) 

# Dose is in there as numeric, and that is one way to analyze it, but let's 
# change it to an ordered factor by using as.ordered. 

toothgrowth$dose <- as.ordered(toothgrowth$dose)

# OK, let's make a model. Here, we'll put in dose and supp to explain len

toothgrowth.glm <- glm(len ~ dose*supp, #len ~ dose + supp + dose:supp
                      data = toothgrowth,
                      family = gaussian(link="identity"))

# Let's plot the data.

plot(toothgrowth.glm)

# Looks good. Let's run an ANOVA. Here you'll get your main effects of dose and
# supp, and your interaction (dose:supp).

Anova(toothgrowth.glm)

# Given that the interaction is significant (Pr <0.05), we need to look at each level of
# the main effects within the levels of the other, and our full model is the
# most appropriate one. Like before, we look at the emmeans, then use cld to
# compare the individual combinations of supp and dose. Only different thing
# we need to do with cld is to enter 'by =' to describe which parameter we want
# to use to group data, and which we want to compare.

toothgrowth.emm <- emmeans(toothgrowth.glm, ~ supp | dose, type = "response")
toothgrowth.emm

# Looks like there are some differences, but let's test and simplify to make
# things ready for plotting. First, we'll group by supp to compare dose

dose.cld <- cld(toothgrowth.emm, 
                by = "supp", 
                alpha = 0.05, 
                Letters = letters)

dose.cld$.group <- gsub(" ", "", dose.cld$.group)
dose.cld <- subset(dose.cld)

supp.cld <- cld(toothgrowth.emm, 
                by = "dose", 
                alpha = 0.05, 
                Letters = letters)

supp.cld$.group <- gsub(" ", "", supp.cld$.group)
supp.cld <- subset(supp.cld)

# Let's plot up the data grouped by dose and comparing the effect of supp. 

ggplot() +
  geom_boxplot(data = toothgrowth, 
             aes(x = dose, y = len, fill = supp)) +
  geom_text(data = supp.cld, 
            aes(x = dose, y = emmean, 
                group = supp, label = .group, 
                vjust = -4, hjust = -0.3),
            size = 5) +
  labs(x = "Supplement", y = "Length (microns)") 

# Uh, well, the labels aren't so helpful if they aren't above the box and 
# whiskers. Luckily we can "dodge" them, to make them line up with the right
# box. Also, those colors are less than awesome. There are a whole range of 
# color palettes to choos from. One set are available within scale_fill_brewer
# (type ?scale_fill_brewer to see a list with no pictures, or run code below):

browseURL("https://www.r-bloggers.com/how-to-expand-color-palette-with-ggplot-and-rcolorbrewer")

# Let's plot it now with dodged labels and nicer colors:

ggplot() +
  geom_boxplot(data = toothgrowth, 
               aes(x = dose, y = len, fill = supp)) +
  geom_text(data = supp.cld, 
            aes(x = dose, y = emmean, 
                group = supp, label = .group, 
                vjust = -4, hjust = -0.3), 
            position = position_dodge(0.75),
            size = 5) +
  scale_fill_brewer(palette = "Spectral") +
  labs(x = "Supplement", y = "Length (microns)") 

# Let's plot up the data grouped by dose and comparing the effect of supp

ggplot() +
  geom_boxplot(data = toothgrowth, 
               aes(x = supp, y = len, fill = dose)) +
  geom_text(data = dose.cld, 
            aes(x = supp, y = emmean, 
                group = dose, label = .group, 
                vjust = -4, hjust = -0.3), 
            position = position_dodge(0.75),
            size = 5) +
  scale_fill_brewer(palette = "Spectral") +
  labs(x = "Supplement", y = "Length (microns)") 
# We could also do this by faceting instead of separating in one graph panel
ggplot() +
  geom_boxplot(data = toothgrowth, 
               aes(x = dose, y = len)) +
  geom_text(data = dose.cld, 
            aes(x = dose, y = emmean, label = .group, 
                vjust = -4, hjust = -0.3), 
            position = position_dodge(0.75),
            size = 5) +
  scale_fill_brewer(palette = "Spectral") +
  labs(x = "Supplement", y = "Length (microns)") +
  facet_grid(~supp)

# Three-way interaction example
# First, we'll make some new data, namely a "width" variable and a temp variable.

teeth <- toothgrowth %>%
  mutate(width = len/10 + c(rnorm(5, 1, 0.2),     # VC 0.5 Low
                            rnorm(5, 3, 0.2),     # VC 0.5 High
                            rnorm(5, 6, 0.2),     # VC 1   Low
                            rnorm(5, 9, 0.2),     # VC 1   High
                            rnorm(5, 10, 0.2),     # VC 2   Low
                            rnorm(5, 12, 0.2),     # VC 2   High
                            rnorm(5, 1, 0.2),     # OJ 0.5 Low
                            rnorm(5, 2, 0.2),     # OJ 0.5 High
                            rnorm(5, 3, 0.2),     # OJ 1   Low
                            rnorm(5, 6, 0.2),     # OJ 1   High
                            rnorm(5, 9, 0.2),     # OJ 2   Low
                            rnorm(5, 11, 0.2))) %>%# OJ 2   High
  mutate(temp = c(rep("low", 5),
                  rep("high", 5),
                  rep("low", 5),
                  rep("high", 5),
                  rep("low", 5),
                  rep("high", 5))) %>%
  mutate(temp = as.factor(temp))

# Next, make a glm with dose, supp, and temp as predictors, and with width as
# the response variable:

teeth.glm <- glm(width ~ dose*supp*temp,
                       data = teeth,
                       family = gaussian(link="identity"))

# Let's plot the data.

plot(teeth.glm)

# Looks good. Let's run an ANOVA. 

Anova(teeth.glm)

# Given that the three-way interaction is significant (Pr <0.05), we need to 
# look at each level of one variable within every combination of the other two.
# Like before, we look at the emmeans, then use cld to compare the individual 
# combinations of dose, supp, and temp. Only different thing
# we need to do with cld is to enter "by = c("supp" and "dose")" to denote
# which parameters we want to use to group data.

teeth.emm <- emmeans(teeth.glm, ~ supp | dose | temp, type = "response")
teeth.emm

temp.cld <- cld(teeth.emm, 
                by = c("supp", "dose"), 
                alpha = 0.05, 
                Letters = letters)

temp.cld$.group <- gsub(" ", "", temp.cld$.group)
temp.cld <- subset(temp.cld)

ggplot() +
  geom_boxplot(data = teeth, 
               aes(x = temp, y = width)) +
  geom_text(data = temp.cld, 
            aes(x = temp, y = emmean, label = .group, 
                vjust = -1.5, hjust = -0.3), 
            position = position_dodge(0.75),
            size = 5) +
  scale_fill_brewer(palette = "Spectral") +
  labs(x = "Temperature", y = "Width (mm)") +
  facet_grid(dose~supp, scales = "free_y")
