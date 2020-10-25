# This is for changing our raw data into a form that we can use for staRdom 
# In here we are:
# separating the absorbance files from the waterfall and blank files and putting them in temp folders
# using the separate function to split the file names into 4-5 different columns based on what we want the end format to be 
# replacing the "dat" in each file with "txt" to get the correct file form
# using the unite function to combine the correct columns (the ones we just created using the separate function) to get the file name we want
# selecting the united column to be pulled out and used as our new file names
# pasting our files with their new file names in our endgame folders (which will then be used for staRdom)
# Loading relevant packages
library(tidyr)
library(dplyr)
library(readr)

# Separating files based on file names
# files.blank <- blank files that will then go into the waterfall_temp folder. These need to retain "Blank" to be separate from other files
files.blank <- list.files(path = "./Project Files", 
                                    full.names = TRUE, recursive = TRUE,
                                    pattern = "Waterfall Plot Blank")
# files.waterfall <- waterfall files that will then go into the waterfall_temp folder along with the blank files.
files.waterfall <- list.files(path = "./Project Files", 
                              full.names = TRUE, recursive = TRUE,
                              pattern = "Waterfall Plot Sample")
# files.absorbance <- absorbance files that will go into the absorbance_temp folder.
# these files need to remain separate from the waterfall and blank files for the structure of staRdom 
files.absorbance <- list.files(path = "./Project Files",
                               full.names = TRUE, recursive = TRUE,
                               pattern = "Abs")
# Moving separated files into new temporary folders
# Once in temp folders we can then modify the data structure to match StaRdom
# Specifically we want the file names to be BDYYMMDDS##R#(Blank if blank otherwise it is just BD...).txt
# Here, we copy and paste the files into the new folders
for (f in files.waterfall) file.copy(from = f, to = "./input/waterfall_temp")
for (f in files.blank) file.copy(from = f, to ="./input/waterfall_temp")
for (f in files.absorbance) file.copy(from = f, to = "./input/absorbance_temp")

# Now we need a list of the files in the new folder, and the list needs to be
# in a data frame in order to separate the names into columns

files.waterfall.new <- list.files(path = "./input/waterfall_temp", 
                              full.names = TRUE, recursive = TRUE,
                              pattern = "Waterfall Plot Sample")
files.waterfall.new <- data.frame(names= files.waterfall.new)

files.blank.new <- list.files(path = "./input/waterfall_temp", 
                                    full.names = TRUE, recursive = TRUE,
                                    pattern = "Waterfall Plot Blank")
files.blank.new <- data.frame(names = files.blank.new)

files.absorbance.new <- list.files(path = "./input/absorbance_temp",
                               full.names = TRUE, recursive = TRUE,
                               pattern = "Abs")
files.absorbance.new <- data.frame(names = files.absorbance.new)
# Separating out the names first 

# For Blank
# Our current file names look like: "./input/waterfall_temp/BD200625S01R1 (01)-Waterfall Plot Blank.dat
# We want to separate this into five columns: "directory", "sample", "temp", "blank", "suffix"
# directory <- "./input/waterfall_temp/"
# sample <- "BD200625S01R1"
# temp <- " (01) - Waterfall Plot " (including the spaces)
# blank <- "Blank." (inlcuding the "." as later on we want to modify suffix)
# suffix <- "dat"
# in order to get these we are separating them (sep = ) by counting out how many letters are in each part
files.blank.sep <- separate(files.blank.new, names, c("directory", "sample", "temp", "blank", "suffix"), sep = c(23, 36, -9, -3))
# now we need to correct the suffix: staRdom needs either .txt or .csv files so we are changing "dat" to "txt"
files.blank.sep$suffix <- "txt"
# now we are taking the parts of the name we want ("directory" + "sample" + "blank" + "suffix") and combining them under a new column "names"
files.blank.unite <- unite(files.blank.sep, names, c("directory", "sample", "blank", "suffix"), sep = "", remove = TRUE)
# now we need to take only the "names" column to use as our new file names- removing "temp"
files.blank.unite <- select(files.blank.unite, "names")

# For Waterfall
# we now want four columns as we no longer want a "blank" column, that part gets absorbed into "temp"
# directory <- "./input/waterfall_temp/"
# sample <- "BD200625S01R1"
# temp <- " (01) - Waterfall Plot Sample"
# suffix <- ".dat" this will just have to change to ".txt"
files.waterfall.sep <- separate(files.waterfall.new, names, c("directory", "sample", "temp", "suffix"), sep = c(23, 36, -4))
files.waterfall.sep$suffix <- ".txt"
files.waterfall.unite <- unite(files.waterfall.sep, names, c("directory", "sample", "suffix"), sep = "", remove = TRUE)
files.waterfall.unite <- select(files.waterfall.unite, "names")

# For Absorbance
# this is the same format as waterfall except it has more letters in the name than the either two, so you have to change sep = to reflect that
files.absorbance.sep <- separate(files.absorbance.new, names, c("directory", "sample", "temp", "suffix"), sep = c(24, 37, -4))
files.absorbance.sep$suffix <- ".txt"
files.absorbance.unite <- unite(files.absorbance.sep, names, c("directory", "sample", "suffix"), sep = "", remove = TRUE)
files.absorbance.unite <- select(files.absorbance.unite, "names")



## BPC: Commented out the below section. It can likely be removed soon ----
# Can I use the for function from earlier to copy files.blank.unite to the new folders? 
# no, because the files are not named that yet, which means they do not have a directoy yet
# for (f in files.blank.unite) file.copy(from = f, to = "./input/waterfall_temp")

# Now we are moving onto the renaming of files portion
# We are going to move the files from the temp folders to the new folders and in the process we are going to assign them their "files.*.unite" names
# For Blank
# List old files
# old_b_files <- list.files("./input/waterfall_temp/", pattern = "Blank" , full.names = TRUE)
# old_b_files <- list.files(files.blank.unite, pattern = ".txt", full.names = TRUE)
# old_b_files
# create pathway for new files with new naming pattern
# how do I get the new names to reflect files.blank.unite?
# new_b_files <- paste0("./input/waterfall_new/files.blank.unit",1:length(old_b_files), ".txt") does not work
# OH MY GOD, I need to set "old_b_files" to "files.blank.unit" then replace the directory with the new directory? Let's see if that works
# It does not- I need a path instead of a set of values
# new_b_files <- paste0("./input/waterfall_new/",1:length(old_b_files), ".txt")
# new_b_files
# copy old files to new pathway while renaming


##Renaming files in tmp directories-----
# We want to rename the files in their respective new "tmp" directories. This 
# means going: 
# from: files.blank.new to: files.blank.unite
# from: files.waterfall.new to: files.waterfall.unite
# from: files.absorbance.new to: files.absorbance.unite
# Use file.rename and set the names as vectors for successful renaming.

# files.blank.new <- as.list(files.blank.new)
# files.blank.unite <- as.list(files.blank.unite)

file.rename(from = as.vector(files.blank.new$names), 
            to = as.vector(files.blank.unite$names))
file.rename(from = as.vector(files.waterfall.new$names), 
            to = as.vector(files.waterfall.unite$names))
file.rename(from = as.vector(files.absorbance.new$names), 
            to = as.vector(files.absorbance.unite$names))

# For absorbance data, reshape the absorbance data by importing using 
# read.delim(), eliminating headers and extraneous columns, then exporting to 
# input/absorbance 
# Let's test with one file, then redo it to do all files.

df <- read.delim("./input/absorbance_temp/BD200625S01R1.txt", header = FALSE, skip = 3)
df <- select(df, c(1, 10))
write_delim(df, path = "./input/absorbance_new/df", delim = "\t", 
            col_names = FALSE)
# If using sample - blank waterfall plot, reshape the data by
# importing, removing the second row, changing header for column 1 to 
# "Wavelength", and exporting to input/waterfall

df2 <- read.delim("./input/waterfall_temp/BD200625S14R3.txt", header = TRUE)
colnames(df2) <- gsub("X", "", colnames(df2))
write_delim(df2, path = "./input/waterfall_new/df2", delim = "\t", 
            col_names = TRUE)

# Worked with single files. Need to figure out an efficient way of doing this
# so that it will do all files in one fell swoop. First attempt:
for (f in files.absorbance.unite) {
  dave <- read.delim(files.absorbance.unite[f, 1], 
                                          header = FALSE, skip = 3)
  dave <- select(dave, c(1, 10))
  write_delim(dave, path = files.absorbance.unite[f, 1], 
              delim = "\t", col_names = FALSE)
}

abs.reshape <- function(object) {
  dave <- read.delim(object, header = FALSE, skip = 3)
  dave <- select(object, c(1, 10))
  write_delim(dave, path = object, 
              delim = "\t", col_names = FALSE)
}
apply(files.absorbance.unite, abs.reshape(files.absorbance.unite))
# did not work, got error code "Error in file(file, "rt") : invalid 'description'
# argument"
