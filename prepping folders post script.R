# This is for after the "data script", "BDAtest1script", and "messing with negative
# files script" have been run
# It will take delete the files in the "Project files" folder (remember, we 
# already have the original set untouched in the original "Files by date" folder)
# It will then take the input and copy it to the "input" subfolder under the 
# correct date in the "Files by date" folder.
# Then it will delete the contents of the "input" folder, leaving it clean for 
# the next round 
# Then it will move the outputs to the "output" subfolder under the correct date 
# in the "Files by date" folder, then delete the floating ouputs, leaving it 
# clean for the next round

# Deleting files in the "Project files" folder
# PJ <- list.files(path = "./Project Files")
# PJ
# rm(list = "PJ")
# PJ
# # This is not quite working, so maybe try "unlink"?
# unlink(PJ)
# unlink("./Project Files")
# # Still nada, it is not even showing up as anything
# # Maybe try "file.remove"?
# file.remove(path = "./Project Files")
# file.remove(list.files("./Project Files"), force = TRUE)
# file.exists("./Project Files")
# file.remove("./Project Files")
# PJ <- list.files("./Project Files", pattern = ".dat")
# PJ
# for (f in PJ) file.remove(f)
# file.exists("./Project Files/BD200804S14R3 (01) - Waterfall Plot Sample.dat")
# file.remove("./Project Files/BD200804S14R3 (01) - Waterfall Plot Sample.dat")
# # This works, but if I try to do all of "./Project Files", I get an error that
# # I don't have permission.
# list.files("./Project Files", full.names = TRUE)
file.remove(list.files("./Project Files", full.names = TRUE))
# SWEET SUCCESS!!!

# Copying input to the correct date
file.copy(from = list.files("./input/absorbance_new", full.names = TRUE), to = 
            "./Files by dates/BD200804/input/absorbance_new")
file.copy(from = list.files("./input/absorbance_temp", full.names = TRUE), to = 
            "./Files by dates/BD200804/input/absorbance_temp")
file.copy(from = list.files("./input/waterfall_new", full.names = TRUE), to = 
            "./Files by dates/BD200804/input/waterfall_new")
file.copy(from = list.files("./input/waterfall_temp", full.names = TRUE), to = 
            "./Files by dates/BD200804/input/waterfall_temp")

# Deleting the "from" input
file.remove(list.files("./input/absorbance_new", full.names = TRUE))
file.remove(list.files("./input/absorbance_temp", full.names = TRUE))
file.remove(list.files("./input/waterfall_new", full.names = TRUE))
file.remove(list.files("./input/waterfall_temp", full.names = TRUE))
# REMINDER: because I did it this way, I need to go back to my pre script and 
# make sure I don't recreate my input files each time. 

# Copying output to the correct date
# The .rmd file should be putting my outputs into the output file, but it is not
# as far as I can tell. I don't know where the problem lies yet. Ask Ben?
# Until then, I will do it the difficult way with the full names
file.copy(from = "./outputeem_data_20201214_122159.RData", to = "./Files by dates/BD200804/output")
file.copy(from = "./outputpicked_peaks_20201214_122158.xlsx", to = "./Files by dates/BD200804/output")

# Deleting the "from" output
file.remove("./outputeem_data_20201214_122159.RData")
file.remove("./outputpicked_peaks_20201214_122158.xlsx")

# Now we have a clean slate to start over with 
# REMINDER: change out the date each time or else you will only run one date 
# forever.
# Also need to change the output directory each time as it changes until we can 
# figure out a way to get the output into a folder from the start.