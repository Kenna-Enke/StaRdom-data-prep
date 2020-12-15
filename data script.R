# This is for manipulating our raw data into a form that we can use for staRdom 
# 
# In this script we are:
# separating data files into two folders, absorbance and waterfall
# Absorbance goes into the absorbance file, waterfall and blank into waterfall
# Then we are renaming files to match the requirements for the staRdom package
# Finally we are reshaping our data to match the requirements from staRdom
# From here, you should be able to take the two final input folders and plug them
# directly into staRdom
# 
# This script:
#   1) Copies EEMS and abs files from individual dirs into shared directorys
#   2) Renames the files to a format useable in staRdom
#   3) Reshapes the files and copies them to a new directory
#   
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

# We were getting errors because the absorbance wavelength (240 to 450) range was 
# smaller than the emission wavelength range (250 to ~800). While staRdom does
# allow one to select the wavelengths to be plotted and saved through "em_range"
# and "ex_range", it does peak picking and inner filter effect correction
# beforehand and thus having a lack of overlap between absorbance and the
# excitation wavelength was a problem. We should change the setup for the 
# Aqualog to avoid this problem (scan from 240 to 550 at least). 
# 
# Luckily, absorbance is pretty small beyond 450 and we should be OK if we
# put in some filler values (but need to note this in the methods). The first
# attempt at doing this was to set all absorbance values over 450 to a low value
# minimizing any inner filter correction out past this level. To do this,
# we create "filler" data, then merge it with the absorbance data using "rbind". 
# Filler data were originally 0, and this threw errors. Changed to 0.00001, which
# caused further errors. Changed to 0.001, and no problems. Errors were
# "pattern" errors.
# 
# Creating filler data for adjusting the dataframes and rbind to combine.

filler <- data.frame(V1 = seq(600, 452, -2), V10 = 0.001)
filler$V1 <- as.integer(filler$V1)
all.files.absorbance <- lapply(all.files.absorbance, function(x) rbind(filler, x))

# Now, use a for loop to export each data frame in the list to a file having the
# corresponding name that we assigned the dataframes within the list.

for(i in 1:length(all.files.absorbance)) {
    write.table(all.files.absorbance[i], 
              sep = "\t",
              file = names(all.files.absorbance[i]),
              col.names = FALSE, row.names = FALSE)
}

# For reshaping waterfalls, we will:
#   1) use "grep" paired with invert to just choose the "sample" data
#   2) Use "lapply" and "read.delim" to import the data into a list 
#   3) Use "gsub to create new filenames in a new path
#   4) Use "names" to rename the elements in the list 

local.waterfall <- grep(list.files(path = "./input/waterfall_temp", 
                                   full.names = TRUE),
                        pattern="Blank", 
                        invert=TRUE, value=TRUE)

all.files.waterfall <- lapply(local.waterfall, read.delim, header = TRUE)

local.waterfall <- gsub("waterfall_temp", "waterfall_new", local.waterfall)

names(all.files.waterfall) <- local.waterfall

# We were getting errors in the staRdom script for not having the same
# range of wavelengths for absorbance and emission. Above, we created some filler
# absorbance data to fill absorbance data out to 600 nm. We also need to trim 
# down the EEMS data. To remedy this, we can use lapply to only select the first 
# 154 rows, which covers from ~250 to <600 nm.

all.files.waterfall <- lapply(all.files.waterfall, function(x) return(x[1:154, ]))

# Write all list elements to new files using the element names as the file
# names.

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

local.waterfall.blank <- gsub("waterfall_temp", "waterfall_new", 
                              local.waterfall.blank)

names(all.files.waterfall.blank) <- local.waterfall.blank

all.files.waterfall.blank <- lapply(all.files.waterfall.blank, function(x) return(x[1:154, ]))


for(i in 1:length(all.files.waterfall.blank)) {
  write.table(all.files.waterfall.blank[i], 
              sep = "\t",
              file = names(all.files.waterfall.blank[i]),
              col.names = c("Wavelength", seq(from = 450, to = 240, by = -2)),
              row.names = FALSE)
}