#PARAFAC Analysis in staRdom

#1 Introduction

#1.3 Installing Packages
library("magrittr")
library("dplyr")
library("parallel")
library("eemR")
library("ggplot2")
library("staRdom")
library("tidyr")

#1.4 Parallel Processing
cores<-detectCores(logical=FALSE)

#1.5 Overview of Analysis Steps
#import raw data files
  #preprocess the dataset
    #explore the dataset(PARAFAC)        #peak picking, absorbance
      #validate the model(PARAFAC)
        #interpret the results

#3 Import Raw Data
#Need both fluorescence and absorbance data
#for example eems- for real data, replace ALL "extdata/..."with real path
folder<-system.file("extdata/EEMs/", package="staRdom")
#for real data, replace "eem_csv" with "aqualog"
eem_list<-eem_read(folder, recursive=TRUE, import_function=eem_csv)
#this is important and will be repeated MANY times
eem_overview_plot(eem_list, spp=9, contour=TRUE)
absorbance_path=system.file("extdata/absorbance", package="staRdom")
absorbance<-absorbance_read(absorbance_path, cores=cores)
#this is only necessary if individual dilution factors, Raman areas or pathlengths are used
metatable<-system.file("extdata/metatable_dreem.csv", package="staRdom")
meta<-read.table(metatable, header=TRUE, sep=",", dec=".", row.names=1)
#to create a talbe containing meta data: example
eem_metatemplate(eem_list, absorbance) %>%
  write.csv(file="metatable.csv", row.names=FALSE)

#3.1 Check Data
problem<-eem_checkdata(eem_list, absorbance, meta, metacolumns=c("dilution"), error=FALSE)

#4 Data Preparation and Correction
#4.1 Sample Names
#to replace names
eem_list<-eem_name_replace(eem_list, c("\\(FD3\\)"),c(""))

#4.2 Absorbance Baseline Correction
#can be used for any range
absorbance<-abs_blcor(absorbance,wlrange=c(680,700))

#4.3 Spectral Correction
excorfile<-system.file("extdata/CorrectionFiles/xc06se06n.csv", package="staRdom")
Excor<-data.table::fread(excorfile)
emcorfile<-system.file("extdata/CorrectionFiles/mcorrs_4nm.csv", package="staRdom")
Emcor<-data.table::fread(emcorfile)
eem_list<-eem_range(eem_list, ex=range(Excor[,1]), em=range(Emcor[,1]))
eem_list<-eem_spectral_cor(eem_list,Excor,Emcor)

#4.4 Blank Subraction
#normalizing size of blanks
eem_list<-eem_extend2largest(eem_list, interpolation=1, extend=FALSE, cores=cores)
#subracting blanks
eem_list<-eem_remove_blank(eem_list)
eem_overview_plot(eem_list, spp=9, contour=TRUE)

#4.5 Inner-Filter Effect Correction (IFE)
eem_list<-eem_ife_correction(eem_list, absorbance, cuvl=5)
eem_overview_plot(eem_list, spp=9, contour=TRUE)

#4.6 Raman Normalisation
eem_list<-eem_raman_normalisation2(eem_list, blank="blank")
eem_overview_plot(eem_list, spp=9, contour=TRUE)

#4.7 Remove Blanks 
eem_list<-eem_extract(eem_list, c("nano", "miliq", "milliq", "mq", "blank"), ignore_case=TRUE)
absorbance<-dplyr::select(absorbance, matches("nano|miliq|milliq|mq|blank", ignore.case=TRUE))

#4.8 Remove and Interpolate Scattering
#removing
remove_scatter<-c(TRUE, TRUE, TRUE, TRUE)
remove_scatter_width<-c(15, 15, 15, 15)
eem_list<-eem_rem_scat(eem_list, remove_scatter=remove_scatter, remove_scatter_width=remove_scatter_width)
eem_overview_plot(eem_list, spp=9, contour=TRUE)
#interpolating
eem_list<-eem_interp(eem_list, cores=cores, type=1, extend=FALSE)
eem_overview_plot(eem_list, spp=9, contour=TRUE)

#4.9 Correct for Dilution
dil_data<-meta["dilution"]
eem_list<-eem_dilution(eem_list, dil_data)

#4.10 Smooth Data
#not advised for PARAFAC analysis
eem4peaks<-eem_smooth(eem_list, n=4, cores=cores)

#4.11 Overview of Samples
summary(eem_list)

#5 Peak Picking and Indices
#eem_peaks can be used to extract individual peaks
bix<-eem_biological_index(eem4peaks)
coble_peaks<-eem_coble_peaks(eem4peaks)
fi<-eem_fluorescence_index(eem4peaks)
hix<-eem_humification_index(eem4peaks, scale=TRUE)
indices_peaks<-bix %>%
  full_join(coble_peaks, by="sample") %>%
  full_join(fi, by="sample") %>%
  full_join(hix, by="sample")
indices_peaks

#6 Absorbance Indices
slope_parms<-abs_parms(absorbance, cuvl = 1, cores=cores)
slope_parms

#7 Creating a PARAFAC Model
#7.1 Loading Data
#7.1.1 Load drEEM example dataset
#can also be used to import Matlab EEM data
dreem_raw<-tempfile()
download.file("http://models.life.ku.dk/sites/default/files/drEEM_dataset.zip", dreem_raw)
dreem_data<-unz(dreem_raw, filename="Backup/PortSurveyData_corrected.mat", open="rb") %>%
  R.matlab::readMat()
unlink(dreem_raw)
eem_list<-lapply(dreem_data$filelist.eem, function(file){
  file<-dreem_data$filelist.eem[1]
  n<-which(dreem_data$filelist.eem==file)
  file<-file %>%
    gsub("^\\s+|\\s+$", "", .) %>%
    sub(pattern =  "(.*)\\..*$", replacement="\\1",.)
  eem<-list(file=paste0("drEEM/dataset/", file), sample=file, x=dreem_data$xcRu[n,,] %>% as.matrix(), ex=dreem_data$Ex %>% as.vector(), em=dreem_data$Em.in %>% as.vector(), location="drEEM/dataset/")
  class(eem)<-"eem"
  attr(eem, "is_blank_corrected")<-TRUE
  attr(eem, "is_scatter_corrected")<-FALSE
  attr(eem, "is_ife_corrected")<-TRUE
  attr(eem, "is_raman_normalized")<-TRUE
  attr(eem, "manufacturer")<-"unknown"
  eem}) %>%
  'class<-'("eemlist")
eem_names(eem_list)<-paste0("d", eem_names(eem_list))
#in sample, removing "bl" or "0A" samples
ol<-function(x){x==("bl")|x=="0A"}
extract<-dreem_data$sites %>% unlist() %>% ol %>% which() 
eem_list<-eem_list %>% eem_extract(extract)
eem_list<-eem_rem_scat(eem_list, remove_scatter =c(TRUE, TRUE, TRUE, TRUE), remove_scatter_width = c(15, 15, 18, 19), interpolation=FALSE, cores=cores)
eem_list<-eem_import_dir(dir)

#7.2 Sample Set Wavelength Ranges (in case of deviating samples)
#eem_extend2largest can be used to add NAif values present in another sample are missing
#eem_red2smallest removes wavelengths that are missing in at least one sample from the whole set

#7.3 Find and Remove Noise in EEM Data
  #eem_extract removes whole samples either by name or number
  #eem_range removes data outside the given wavelength ranges in all samples
  #eem_exclude removes data from the sample set, provided by a list
  #eem_rem_scat and eem_remove_scattering are used to set data in Raman and Aryleigh scattering of 1st and 2nd order to NA. 
    #eem_rem_scat removes several scatterings in one step
    #eem_remove_scattering removes one scattering at a time
  #eem_setNA replaces data by NA in rectangular shape and in specific samples
  #eem_matmult multiplies each EEM matrix by a certain matrix. This matrix can be used to set parts of the data to 0 or NA
  #eem_interp is used to interpolate data previously set to NA
#replace "d667sf" with sample name
eem_list %>%
  eem_extract(sample="^d667sf$", keep=TRUE)%>%
  ggeem(contour=TRUE)
eem_list<-eem_list %>% eem_range(ex=c(250, Inf), em=c(0,580))
eem_list<-eem_list %>%
  eem_setNA(sample=176, ex=345:350, interpolate= FALSE) %>%
  eem_setNA(em=560:576, ex=280:295, interpolate=FALSE)
eem_list<-eem_interp(eem_list, type=1, extend=FALSE, cores=cores)

#7.4 Explore Dataset
data(pf_models)
dim_min<-3
dim_max<-7
nstart<-25
maxit<-5000
ctol<-10^-6
pf1<-eem_parafac(eem_list, comps=seq(dim_min, dim_max), normalise= FALSE, const=c("uncons", "uncons", "uncons"), maxit=maxit, nstart=nstart, ctol=ctol, cores=cores)
pf1n<-eem_parafac(eem_list, comps=seq(dim_min, dim_max), normalise=FALSE, const=c("nonneg", "nonneg", "nonneg"), maxit=maxit, nstart=nstart, ctol=ctol, cores=cores)
pf1<-lapply(pf1, eempf_rescaleBC, newscale="Fmax")
pf1n<-lapply(pf1n, eempf_rescaleBC, newscale="Fmax")
eempf_compare(pf1, contour=TRUE)
eempf_compare(pf1n, contour=TRUE)

#7.5 Check the Correlation Between Different Components
eempf_cortable(pf1n[[4]], normalisation=FALSE)
eempf_corplot(pf1n[[4]], progress=FALSE, normalization=FALSE)
pf2<-eem_parafac(eem_list, comps=seq(dim_min, dim_max), normalise=TRUE, const=c("nonneg", "nonneg", "nonneg"), maxit=maxit, nstart=nstart, ctol=ctol, cores=cores)
pf2<-lapply(pf2, eempf_rescaleBC, newscale="Fmax")
eempf_plot_comps(pf2, contour=TRUE, type=1)

#7.6 Find and Exclude Outliers Leverage
cpl<-eempf_leverage(pf2[[4]])
eempf_leverage_plot(cpl, qlabel=0.1)
exclude<-eempf_leverage_ident(cpl, qlabel=0.1)
exclude<-list("ex"=c(), "em"=c(), "sample"= c("dsfb676psp", "dsgb447wt"))
eem_list_ex<-eem_exclude(eem_list, exclude)
pf3<-eem_parafac(eem_list_ex, comps=seq(dim_min, dim_max), normalise=TRUE, maxit=maxit, nstart=nstart, ctol=ctol, cores=cores)
pf3<-lapply(pf3, eempf_rescaleBC, newscale="Fmax")
eempf_plot_comps(pf3, contour=TRUE, type=1)
eempf_leverage_plot(eempf_leverage(pf3[[4]]),qlabel=0.1)

#7.7 Examine Residuals
eempf_residuals_plot(pf3[[4]], eem_list, residuals_only=TRUE, select=c("d0680sfK", "d1266sf", "d1268sfK", "d1543sfK", "dsfb676psp", "dsgb447wt"), spp=6, cores=cores, contour=TRUE)

#7.8 Recalculating the Modle with Increased Accuracy
ctol<-10^-8
nstart=50
maxit=10000
pf4<-eem_parafac(eem_list_ex, comps=6, normalise=TRUE, const=c("nonneg", "nonneg", "nonneg"), maxit=maxit, nstart=nstart, ctol=ctol, output="all", cores=cores)
pf4<-lapply(pf4, eempf_rescaleBC, newscale="Fmax")
eempf_convergence(pf4[[1]])
eempf_compare(pf4, contour=TRUE)
eempf_leverage_plot(eempf_leverage(pf4[[1]]))
eempf_corplot(pf4[[1]], progress=FALSE)
pf5<-eem_parafac(eem_list, comps=6, normalise=TRUE, const=c("nonneg", "nonneg", "nonneg"), maxit=maxit, nstart=100, ctol=min(ctol*100, 0.01), cores=cores)
ggeem(pf5[[1]], contour=TRUE)
nstart<-5
pf4<-eem_parafac(eem_list_ex, comps=6, normalise=TRUE, const=c("nonneg", "nonneg", "nonneg"), maxit=maxit, nstart=nstart, ctol=ctol, cores=cores, Bstart=pf5[[1]]$B, Cstart=pf5[[1]]$C)
pf4<-lapply(pf4, eempf_rescaleBC, newscale="Fmax")
ggeem(pf4[[1]], contour=TRUE)

#7.9 Plot the Resulting Components and Loadings
eempf_comp_load_plot(pf4[[1]], contour=TRUE)

#7.10 Plotting Samples and Residuals
eempf_residuals_plot(pf4[[1]], eem_list, select=eem_names(eem_list)[10:14], cores=cores, contour=TRUE)

#7.11 Split-Half Analysis
sh<-splithalf(eem_list_ex, 6, normalise=TRUE, rand=FALSE, cores=cores, nstart=nstart, maxit=maxit, ctol=ctol)
data(sh)
splithalf_plot(sh)
sh_r<-splithalf(eem_list_ex, 6, normalise=TRUE, rand=TRUE, cores=cores, nstart=nstart, maxit=maxit, ctol=ctol)
tcc_sh_table<-splithalf_tcc(sh)
tcc_sh_table

#7.12 Loadings of Outliers
pf4_wOutliers<-A_missing(eem_list, pfmodel=pf4[[1]], cores=cores)

#7.13 Further Model Validation
#7.13.1 Core Consistency
corcondia<-eempf_corcondia(pf4[[1]], eem_list_ex)

#7.13.2 EEMqual
eemqual<-eempf_eemqual(pf4[[1]], eem_list_ex, sh, cores=cores)

#7.13.3 Importance of Components
varimp<-eempf_varimp(pf4[[1]], eem_list_ex, cores=cores)

#8 Formatting a Model
#8.1 Naming Models and Components
#getting current model names
names(pf3)
#replacing model names
names(pf3)<-c("3 components", "4 components xy", "5 components no outliers", "6 components", "7 components")
names(pf3)
#get current component names
eempf_comp_names(pf4)
eempf_comp_names(pf4)<-c("A4", "B4", "C4", "D4", "E4", "F4")
eempf_comp_names(pf3)<-list(c("A1", "B1", "C1"), 
                            c("humic", "T2", "whatever", "peak"),
                            c("rose", "peter", "frank", "dwight", "susan"),
                            c("A4", "B4", "C4", "D4", "E4", "F4"),
                            c("A5", "B5", "C5", "D5", "E5", "F5", "G5"))
pf4[[1]] %>%
  ggeem(contour=TRUE)

#8.2 Sorting Components
#eempf_reorder can reorder components within model

#9.1 Comparing your data using openfluor.org
eempf_openfluor(pf4[[1]], file="my_model_openfluor.txt")

#9.3 Creating a Report on Your Analysis
eempf_report(pf4[[1]], export="parafac_report.html", eem_list=eem_list, shmodel=sh, performance=TRUE)

#9.4 Exporting the Model
#can use eempf_export to export model to a csv file

#9.5
#eem_dilcorr creates a table containing information on how to handle diluted samples
#eem_absdil takes info from table generated by eem_dilcorr and multiplies or deletes undiluted absorbance sample data
#eem_eemdil take info from the table and renames EEM samples to match undiluted absorbance samples

#10 Experimental Functions
#10.1 SSCs Between Initialisations
pf4<-eem_parafac(eem_list_ex, comps=6, normalise=TRUE, const=c ("nonneg", "nonneg", "nonneg"), maxit=maxit, nstart=nstart, ctol=ctol, output="all", cores=cores)
ssccheck<-eempf_ssccheck(pf4[[1]]$models, best=3, cores=cores)
eempf_plot_ssccheck(ssccheck)

#10.2 Recombining Components
#not advised, but eempf_excomp can extract components and A_missing and components can be used to create new models with the extracted components

