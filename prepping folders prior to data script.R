# What I am trying to accomplish with this is to prep my data for the data script
# I currently have my files separated by date and I take the project files for 
# each individual date and plug them into the "Project Files" folder and then I
# prep the input folder by plugging in a new batch of empty folders
# So it looks like:
#   "Files by dates/Date/files" copied to "Project files"
#   "absorbance/waterfall files" copied to "input"
# And then after I have completed the data script I delete my project files
# (because I already have the original set safe in my "files by dates")
# And cut all my input and paste it into the "input" folder under the correct
# date in the files by dates folder
# So I am looking for a way to do this cleanly on R so I don't have to do it 
# manually and potentiall fuck it up

# copying project files
BD200804 <- list.files(path = "./Files by dates/BD200804/files", 
                        full.names = TRUE, recursive = TRUE, all.files = TRUE)
BD200804
for (f in BD200804) file.copy(from = f, to = "./Project Files")

# Now we have our files for the correct date in the correct folder for 
# the data script.
# Next we need to copy over the empty folders for the input 
# I created a new folder called "blank input" which has all the blank input 
# folders within it
# input <- list.files(path = "./input")
# file.copy(from = "./blank input/waterfall_new", to = "./input")
# file.copy(from = "./blank input", to = "./input")
# I changed the name "blank input" to "empty" in case the name was messing up 
# the process. 
# list.files(path = "./empty")
# file.copy(from = "./empty", to = "./input")
# blank_input <- list.files(path = "./empty",
#                           full.names = TRUE,)
# blank_input
# for (f in blank_input) file.copy(from = f, to = "./input")
# list.files("./input")

# this isn't putting anything in the input folder, so I am trying something
# else just to see if something will happen
# BD200804_input <- list.files(path = "./Files by dates/BD200804/input")
# BD200804_input
# for (f in BD200804_input) file.copy(from = f, to = "./input")
# That is not working either, which is confusing. 
# Is it because I am trying to copy folders, not files? 

# file.copy("./empty", "./input", recursive = TRUE)
# so this works, but it gives me all the "empty" folder as well
# So instead I am going to delete the "empty" folder and let my empty folders
# float-- exactly what I had it before
# file.copy("./absorbance_new", "./input", recursive = TRUE)
# This works! So I will do that for all of them
# file.copy("./absorbance_new", "./input", recursive = TRUE)
# file.copy("./absorbance_temp", "./input", recursive = TRUE)
# file.copy("./waterfall_new", "./input", recursive = TRUE)
# file.copy("./waterfall_temp", "./input", recursive = TRUE)
# This was deemed redundant by my post script

# This is now set up for data script.
# For each new date you just need to go back to the top and change the directory
# to the correct date. 