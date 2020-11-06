# This is for manipulating our raw data into a form that we can use for staRdom 
# In this script we are:
# separating data files into two folders, absorbance and waterfall
# Absorbance goes into the absorbance file, waterfall and blank into waterfall
# Then we are renaming files to match the requirements for the staRdom package
# Finally we are reshaping our data to match the requirements from staRdom
# From here, you should be able to take the two final input folders and plug them
# directly into staRdom
# Loading relevant packages----
library(tidyr)
library(dplyr)
library(readr) 
# Separating files based on file names----
# creates lists of the blank waterfall files, sample waterfall files, and absorbance 
# files so later we can separate files into folders according to these lists
files.blank <- list.files(path = "./Project Files", 
                                    full.names = TRUE, recursive = TRUE,
                                    pattern = "Waterfall Plot Blank")
files.waterfall <- list.files(path = "./Project Files", 
                              full.names = TRUE, recursive = TRUE,
                              pattern = "Waterfall Plot Sample")

files.absorbance <- list.files(path = "./Project Files",
                               full.names = TRUE, recursive = TRUE,
                               pattern = "Abs")
# Moving to temporary folders----
# Once in temp folders we can then modify the data structure to match StaRdom
# Specifically we want the file names to be 'BDYYMMDDS##R#(Blank if blank 
# otherwise it is just BD...).txt'
# NOTE: these temporary files are NOT going to be used in staRdom 
# Here, we copy and paste the files into the temporary folders
for (f in files.waterfall) file.copy(from = f, to = "./input/waterfall_temp")
for (f in files.blank) file.copy(from = f, to ="./input/waterfall_temp")
for (f in files.absorbance) file.copy(from = f, to = "./input/absorbance_temp")
# Setting as dataframe----
files.waterfall.new <- list.files(path = "./input/waterfall_temp", 
                              full.names = TRUE, recursive = TRUE,
                              pattern = "Waterfall Plot Sample")
files.waterfall.new <- data.frame(names = files.waterfall.new)
files.blank.new <- list.files(path = "./input/waterfall_temp", 
                                    full.names = TRUE, recursive = TRUE,
                                    pattern = "Waterfall Plot Blank")
files.blank.new <- data.frame(names = files.blank.new)
files.absorbance.new <- list.files(path = "./input/absorbance_temp",
                               full.names = TRUE, recursive = TRUE,
                               pattern = "Abs")
files.absorbance.new <- data.frame(names = files.absorbance.new)
# Separating names----
# Now we are dissecting the file names apart so we can pull out the relevant 
# information and leave the rest behind.
# This will allow our files to be plugged directly in the staRdom package later
# For Blank
# Our current file names look like: "./input/waterfall_temp/BD200625S01R1 
# (01)-Waterfall Plot Blank.dat"
# We want to separate this into five columns: "directory", "sample", "temp", 
# "blank", "suffix"
# in order to get these we are separating them (sep = ) by counting out how many
# letters are in each part
files.blank.sep <- separate(files.blank.new, names, 
                            c("directory", "sample", "temp", "blank", "suffix"), 
                            sep = c(23, 36, -9, -3))
# now we need to correct the suffix: staRdom needs either .txt or .csv files so we are changing "dat" to "txt"
files.blank.sep$suffix <- "txt"
# now we are taking the parts of the name we want ("directory" + "sample" + "blank" + "suffix") and combining them under a new column "names"
files.blank.unite <- unite(files.blank.sep, names, 
                           c("directory", "sample", "blank", "suffix"), 
                           sep = "", remove = TRUE)
# now we need to take only the "names" column to use as our new file names- removing "temp"
files.blank.unite <- select(files.blank.unite, "names")
# For Waterfall
# we now want four columns as we no longer want a "blank" column, that part gets absorbed into "temp"
files.waterfall.sep <- separate(files.waterfall.new, names, 
                                c("directory", "sample", "temp", "suffix"), 
                                sep = c(23, 36, -4))
files.waterfall.sep$suffix <- ".txt"
files.waterfall.unite <- unite(files.waterfall.sep, names, 
                               c("directory", "sample", "suffix"), 
                               sep = "", remove = TRUE)
files.waterfall.unite <- select(files.waterfall.unite, "names")
# For Absorbance
# this is the same format as waterfall except it has more letters in the name than the either two, so you have to change 'sep =' to reflect that
files.absorbance.sep <- separate(files.absorbance.new, names, 
                                 c("directory", "sample", "temp", "suffix"), 
                                 sep = c(24, 37, -4))
files.absorbance.sep$suffix <- ".txt"
files.absorbance.unite <- unite(files.absorbance.sep, names, 
                                c("directory", "sample", "suffix"), 
                                sep = "", remove = TRUE)
files.absorbance.unite <- select(files.absorbance.unite, "names")
##Renaming files in temp directories-----
# We want to rename the files in their respective new 'temp' directories. This 
# means going: 
# from: files.blank.new to: files.blank.unite
# from: files.waterfall.new to: files.waterfall.unite
# from: files.absorbance.new to: files.absorbance.unite
# Use file.rename and set the names as vectors for successful renaming.
file.rename(from = as.vector(files.blank.new$names), 
            to = as.vector(files.blank.unite$names))
file.rename(from = as.vector(files.waterfall.new$names), 
            to = as.vector(files.waterfall.unite$names))
file.rename(from = as.vector(files.absorbance.new$names), 
            to = as.vector(files.absorbance.unite$names))
# Reshaping data test run----
# For absorbance data, reshape the absorbance data by importing using 
# read.delim(), eliminating headers and extraneous columns, then exporting to 
# input/absorbance. 
# Let's test with one file, then redo it to do all files.
# df <- read.delim("./input/absorbance_temp/BD200625S01R1.txt", 
#                  header = FALSE, skip = 3)
# df <- select(df, c(1, 10))
# write_delim(df, path = "./input/absorbance_new/df", delim = "\t", 
#             col_names = FALSE)
# # If using sample - blank waterfall plot, reshape the data by
# # importing, removing the second row, changing header for column 1 to 
# # "Wavelength", and exporting to input/waterfall
# 
# df2 <- read.delim("./input/waterfall_temp/BD200625S14R3.txt", header = TRUE)
# colnames(df2) <- gsub("X", "", colnames(df2))
# write_delim(df2, path = "./input/waterfall_new/df2", delim = "\t", 
#             col_names = TRUE)

# Reshaping all files----
# We can use lapply to do the reshaping and copying to the appropriate "_new" 
# folders, but not with  write_delim. For some reason, that is incompatible. No 
# problem, we can use write.table instead. Also, helps to start with a fresh 
# list of files.
# Reshaping all absorbance files:
# First, a fresh file list:
local.absorbance <- list.files(path = "./input/absorbance_temp",
                               full.names = TRUE,
                               pattern = "txt")

# Make a list of all the files in local.absorbance, where each element in the
# list consists of the data in that file pulled using the read.delim function
# and skipping the first 3 lines.
 
all.files.absorbance <- lapply(local.absorbance, read.delim, header = FALSE,
                               skip = 3)

# Next, pull out only the first and tenth columns for all of those data frames.
all.files.absorbance <- lapply(all.files.absorbance, select, c(1, 10))

# Name each dataframe by the parent file name, but first change the path
local.absorbance <- gsub("absorbance_temp", "absorbance_new", local.absorbance)

names(all.files.absorbance) <- local.absorbance

# Now, use a for loop to export each data frame in the list to a file having the
# corresponding name that we assigned the dataframes within the list.
for(i in 1:length(all.files.absorbance)) {
  write.table(all.files.absorbance[i], 
              sep = "\t",
              file = names(all.files.absorbance[i]),
              col.names = FALSE, row.names = FALSE)
}

# Let's try for waterfalls. We'll use 'grep' paired with invert to just choose
# the sample plots. Same steps as above, except we need to add column names.
# Tried to have it write the column names from the dataframes, but it added
# the file name to each one, and thus made it a mess. Instead, we overwrite them.

local.waterfall <- grep(list.files(path = "./input/waterfall_temp", 
                                   full.names = TRUE),
                        pattern="Blank", 
                        invert=TRUE, value=TRUE)

all.files.waterfall <- lapply(local.waterfall, read.delim, header = TRUE)


local.waterfall <- gsub("waterfall_temp", "waterfall_new", local.waterfall)

names(all.files.waterfall) <- local.waterfall

names(all.files.waterfall[1])

for(i in 1:length(all.files.waterfall)) {
  write.table(all.files.waterfall[i], 
              sep = "\t",
              file = names(all.files.waterfall[i]),
              col.names = c("Wavelength", seq(from = 450, to = 240, by = -2)),
              row.names = FALSE)
}

# Finally, repeat for blank files.
local.waterfall.blank <- list.files(path = "./input/waterfall_temp", 
                                   full.names = TRUE, pattern="Blank")

all.files.waterfall.blank <- lapply(local.waterfall.blank, 
                                    read.delim, header = TRUE)

# all.files.waterfall <- lapply(all.files.waterfall, function(x) {
#   colnames(x) <- gsub("X", "", colnames(x))
#   return(x)
# })

local.waterfall.blank <- gsub("waterfall_temp", "waterfall_new", 
                              local.waterfall.blank)

names(all.files.waterfall.blank) <- local.waterfall.blank


for(i in 1:length(all.files.waterfall.blank)) {
  write.table(all.files.waterfall.blank[i], 
              sep = "\t",
              file = names(all.files.waterfall.blank[i]),
              col.names = c("Wavelength", seq(from = 450, to = 240, by = -2)),
              row.names = FALSE)
}
