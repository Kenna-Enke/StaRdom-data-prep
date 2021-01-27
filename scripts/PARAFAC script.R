# 8.3 Finding and removing noise in data----
# An important way of finding noise in EEM data is viewing the samples. You can use the function eem_overview_plot to view all your samples in a convenient way. With the following functions, data can be removed or set to NA (NAs are theoretically no problem for the PARAFAC algorithm, but the calculation process and the results are often more convient without) in different ways, or be interpolated, depending on what you would like to do:

# eem_extract removes whole samples either by name or by number.
# 
# eem_range removes data outside the given wavelength ranges in all samples.
# 
# eem_exclude removes data from the sample set, provided by a list.
# 
# eem_rem_scat and eem_remove_scattering are used to set data in Raman and Rayleigh scattering of 1st and 2nd order to NA. While the later on removes one scattering at a time, the first one wraps it up to remove several scatterings in one step.
# 
# eem_setNA replaces data by NA in rectangular shape and in specific samples.
# 
# eem_matmult multiplies each EEM matrix by a certain matrix. This matrix can be used to set parts of the data to 0 or NA (e.g. the area where emission wavelength is shorter than excitation wavelength).
# 
# eem_interp is used to interpolate data previously set to NA.
# 
# Sample “d667sf” will be used to show this process graphically. To extract the sample the regular expression “^d667sf$” is used. ^ stands for the beginning of the string and $ for the end and ensure an exact match. If you do not use these characters, all samples containing the string in their name are extracted.
# 
# This is where we start from:
eem_list %>% 
  eem_extract(sample = "x", keep = TRUE) %>%
  ggeem(contour = TRUE)
# The noisy range below 250 nm excitation and above 580 nm emission can be removed from the samples with the following command. As mentioned above, this was already removed in the example data.
eem_list <- eem_list %>% eem_range(ex = c(250,Inf), em = c(0,580))
# Visually found irregularities in patterns are manually replaced by NA and interpolated. From the sample “d667sf” a band covering the excitation wavelengths from 245 to 350 is removed and a rectangle covering emission wavelengths 560 to 576 and excitation wavelengths 280 to 295 is removed in all samples. For demonstration reasons, the interpolation is done in an extra step but can also be included in the removing function right away (interpolate = TRUE).
eem_list <- eem_list %>%
  eem_setNA(sample = 176, ex = 345:350, interpolate = FALSE) %>%
  eem_setNA(em = 560:576, ex = 280:295, interpolate = FALSE)
eem_list <- eem_interp(eem_list, type = 1, extend = FALSE, cores = cores)

# 8.4 Exploring dataset----
# It is crucial to find an appropriate number of components in the analysis. If the number of components is too large, it may be the case that one component was split into two; if it is too low, relevant components may get lost (Murphy et al. 2013). To help you find the appropriate number, a series of PARAFAC models can be calculated and compared. In this example 5 models with 3 to 7 components are calculated.

# The alternating-least-squares algorithm used to calculate the PARAFAC model optimises the components in order to minimise the residual error. The goal is to find a global minimum of the residual function. Depending on the random start values (random initialisation), different local minima can be found. To get the global minimum, a defined number nstart of initialisations is used for separate model calculations. The model with the smallest error is then used for further analyses, assuming it is a global minimum. 25 might by a good start for nstart although for a profound analysis higher values (e.g. 50) are suggested.
# 
# Some of these random initialisations might not converge. If there are still enough converging models, the result is reliable and there is no problem. eem_parafac returns a warning in case of less than 50 % converging models. Furthermore, it is possible to calculate a specific number of converging models by setting the argument strictly converging = TRUE. When using this, due to the iterative restarting of not converging models, this takes more time and using more starts from the beginning is a faster way to get a reasonable number of converging models.
# 
# You can speed up the calculations by using multiple cores. Beware, that calculating a PARAFAC model can take some time!
#   
#   Higher maxit (maximum number of iteration steps in PARAFAC) and lower ctol (tolerance to return result of PARAFAC, should not be larger than 10⁻⁶) increase the accuracy of the model but take more computation time. For a final model, we suggest to use a tolerance of 10⁻⁸ to 10⁻¹⁰.
# 
# In this first model creation step, the constraints are tested. While pf1 is calculated without any constraints, pf1n uses the assumption that modes are non-negative only. This is a very common assumption, because fluorescence cannot be negative. Common constraints are none (“uncons”), non-negative (“nonneg”) and unimodal, non-negative (“uninon”). Besides these, other possible constraints can be seen using the command CMLS::const().
# 
# PARAFAC modes can be rescaled so that their maximum is 1 and effects are more visible in the plots. The rescaling is corrected in the A mode (sample mode). In case of uneven peak heights, eempf_rescaleBC can help to improve the visibility of your graphs. The parameter newscale specifies the root mean-squared error of each column in matrices B and C. This is compensated in the A mode (sample mode). Alternatively newscale can be set "Fmax"; each peak has a height of 1 then.
# 
# The PARAFAC models created below can be loaded from the staRdom package’s example data instead of calculating it:
data()
# minimum and maximum of numbers of components
dim_min <- 3
dim_max <- 7
nstart <- 25 # number of similar models from which best is chosen
maxit = 5000 # maximum number of iterations in PARAFAC analysis
ctol <- 10^-6 # tolerance in PARAFAC analysis

# calculating PARAFAC models, one for each number of components
pf1 <- eem_parafac(eem_list, comps = seq(dim_min,dim_max), normalise = FALSE, const = c("uncons", "uncons", "uncons"), maxit = maxit, nstart = nstart, ctol = ctol, cores = cores)

# same model but using non-negative constraints
pf1n <- eem_parafac(eem_list, comps = seq(dim_min,dim_max), normalise = FALSE, const = c("nonneg", "nonneg", "nonneg"), maxit = maxit, nstart = nstart, ctol = ctol, cores = cores)

# rescale B and C modes to a maximum fluorescence of 1 for each component
pf1 <- lapply(pf1, eempf_rescaleBC, newscale = "Fmax")
pf1n <- lapply(pf1n, eempf_rescaleBC, newscale = "Fmax")
# Use eempf_compare to plot the created models’ components. You can see the models fits and components (rows) for models with different numbers of components (columns) in 2 different views. The single plots can be created using eempf_fits and eempf_plot_comps.
eempf_compare(pf1, contour = TRUE)
eempf_compare(pf1n, contour = TRUE)

# 8.5 Check the correlation between different components----
# The PARAFAC algorithm assumes no correlation between the components. If samples are in a wide range of DOC concentrations, a correlation of the components is likely. To avoid that, the samples can be normalised. The model with 6 components (4th model in the list, therefore [[4]]) is used for now.
# check for correlation between components table
eempf_cortable(pf1n[[4]], normalisation = FALSE)
eempf_corplot(pf1n[[4]], progress = FALSE, normalisation = FALSE)
# As some of the components are highly correlated, the model is calculated again with normalised sample data. Later normalisation is reversed automatically by multiplying the A modes (samples loadings) with the normalisation factors for exports and plots. Reversing normalisation can be done manually applying norm2A. The parameter normalisation in eempf_cortable and eempf_corplot is set FALSE (default) if the model quality is to be assessed. For interpretations and patterns in the actual fluorescence in your samples normalisation has to be set TRUE to see the actual values.
pf2 <- eem_parafac(eem_list, comps = seq(dim_min,dim_max), normalise = TRUE, const = c("nonneg", "nonneg", "nonneg"), maxit = maxit, nstart = nstart, ctol = ctol, cores = cores)

# rescale B and C modes
pf2 <- lapply(pf2, eempf_rescaleBC, newscale = "Fmax")
# eempf_compare(pf2, contour = TRUE) # use this to show the same plot as above
# for now, we are happy with just the components
eempf_plot_comps(pf2, contour = TRUE, type = 1)

# 8.6 Find and exclude outliers leverage----
# The leverage is calculated by eempf_leverage and can be plotted with eempf_leverage_plot. Using eempf_leverage_ident to plot the leverage shows an interactive plot where you can click on certain values to save them in a variable. Three plots (samples, excitation, emission) show up one after the other. You can select wavelengths or samples by clicking on the certain point in the plot and click ‘Finish’ when you are done. The variable exclude in the example contains all selected values. The returned list can directly be used to exclude the outliers using eem_exclude (see above). qlabel defines the size of the upper quantile that is labelled in the plots. eempf_mleverage can be used to compare the leverages of samples in different models.
# calculate leverage
cpl <- eempf_leverage(pf2[[4]])

# plot leverage (nice plot)
eempf_leverage_plot(cpl,qlabel=0.1)
# plot leverage, not so nice plot but interactive to select what to exclude
# saved in exclude, can be used to start over again with eem_list_ex <- eem_list %>% eem_exclude(exclude) above
exclude <- eempf_leverage_ident(cpl,qlabel=0.1)
# In order to save the decisions of the graphical outlier determination not only as a variable but also in a script, we advise to generate the exclude list manually. This can help you following your way later. See example below.
# samples, excitation and emission wavelengths to exclude, makes sense after calculation of leverage
exclude <- list("ex" = c(),
                "em" = c(),
                "sample" = c("dsfb676psp","dsgb447wt")
)

# exclude outliers if neccessary. if so, restart analysis
eem_list_ex <- eem_exclude(eem_list, exclude)
# A new PARAFAC model is then generated without outliers:
pf3 <- eem_parafac(eem_list_ex, comps = seq(dim_min,dim_max), normalise = TRUE, maxit = maxit, nstart = nstart, ctol = ctol, cores = cores)
pf3 <- lapply(pf3, eempf_rescaleBC, newscale = "Fmax")
eempf_plot_comps(pf3, contour = TRUE, type = 1)
eempf_leverage_plot(eempf_leverage(pf3[[4]]),qlabel=0.1)

# 8.7 Examine residuals----
# The outliers determined above are included to show the difference in residuals. Analysing these residuals can show deficits in model creation, problems with sample handling and lab equipment or it can already be helpful in answering scientific questions.
eempf_residuals_plot(pf3[[4]], eem_list, residuals_only = TRUE, select = c("d0680sfK", "d1266sf", "d1268sfK", "d1543sfK", "dsfb676psp", "dsgb447wt"), spp = 6, cores = cores, contour = TRUE)

# 8.8 Recalculating the model with increased accuracy----
# Due to long calculation times with higher accuracy in the model calculation, the tolerance is only increased in the last step. Just the model with 6 components is recalculated. Check the ratio of converging models to be sure, the best is chosen from a reasonable number.
ctol <- 10^-8 # decrease tolerance in PARAFAC analysis
nstart = 50 # increase number of random starts
maxit = 10000 # increase number of maximum interations

pf4 <- eem_parafac(eem_list_ex, comps = 6, normalise = TRUE, const = c("nonneg", "nonneg", "nonneg"), maxit = maxit, nstart = nstart, ctol = ctol, output = "all", cores = cores)

pf4 <- lapply(pf4, eempf_rescaleBC, newscale = "Fmax")
# Check the convergence behaviour of the created models:
eempf_convergence(pf4[[1]])
# just one model, not really a need to compare
eempf_compare(pf4, contour = TRUE)
eempf_leverage_plot(eempf_leverage(pf4[[1]])) 
# [[1]] means the 4th model in the list, 6 component model in that case
eempf_corplot(pf4[[1]], progress = FALSE)
# It is possible to use the results from a previous model as starting conditions (Astart, Bstart and Cstart). Supplying start matrices is only possible if a model with the same number of components, excitation and emission wavelengths is calculated. If samples differ (e.g. outlier removal or inclusion) simply remove the Astart. In the example, we set up a very rough model using a reduced tolerance but a lot of random starts. In case of doubts, do not use start matrices!
# calculating a rough model, nstart is high (100) but ctol is 2 magnitudes larger or at least 0.01
pf5 <- eem_parafac(eem_list, comps = 6, normalise = TRUE, const = c("nonneg", "nonneg", "nonneg"), maxit = maxit, nstart = 100, ctol = min(ctol*100,0.01), cores = cores)

# plot is not shown
ggeem(pf5[[1]], contour = TRUE)

nstart <- 5
pf4 <- eem_parafac(eem_list_ex, comps = 6, normalise = TRUE, const = c("nonneg", "nonneg", "nonneg"), maxit = maxit, nstart = nstart, ctol = ctol, cores = cores, Bstart = pf5[[1]]$B, Cstart = pf5[[1]]$C)

pf4 <- lapply(pf4, eempf_rescaleBC, newscale = "Fmax")

# plot is not shown
ggeem(pf4[[1]], contour = TRUE)
# Please redo the steps dscribed in the section “Creating a PARAFAC model” until you are satisfied with the results.

# 8.9 Plot the resulting components and loadings----
# The following plot shows the shape of the determined components and the loadings in the different samples. The components’ shapes are important for an interpretation from a chemical point of view. The loadings show the fluorescence differences of different components in different samples.
eempf_comp_load_plot(pf4[[1]], contour = TRUE)
# eempf_plot_comps(pf4[1], type = 2) # this function can be used to view the B- and C-modes
# Separate plots can be generated by using ggeem for components and eempf_load_plot for the loadings.
# 
# It is possible to view the components in 3D using eempf_comps3D.

# 8.10 Plot samples and residuals----
# The plots show samples in columns and the rows show the components (6 in that case), the residuals and the whole sample.
# plot components in each sample, residual and whole sample
eempf_residuals_plot(pf4[[1]], eem_list, select = eem_names(eem_list)[10:14], cores = cores, contour = TRUE)

# 8.11 Split-half analysis----
# The split-half analysis is intended to show the stability of your model. The data is recombined in 6 different ways and results from each sub-sample should be similar (Murphy et al. 2013).
#calculate split_half analysis
sh <- splithalf(eem_list_ex, 6, normalise = TRUE, rand = FALSE, cores = cores, nstart = nstart, maxit = maxit, ctol = ctol)
# Split-half analysis takes some time, so the results are included in the package.

# data(sh)
# Plot the results from the split-half analysis. Your model is stable, if the graphs of all components look similar.
# splithalf_plot(sh)
# you can also use the already known plots from eempf_compare
# sh %>% unlist(recursive = FALSE) %>% eempf_compare()
# If some splits look similar (or see below, have a Tucker’s congruency coefficient close to 1) and others do not, this is a strong hint that the particular splits or the values for maxit and nstart might be responsible for unstable conditions. Problems can be caused by splitting the data according to natural differences (e.g. sampling date A and sampling date B) which will then lead to different PARAFAC models. Changing these manually (using the parameter splits = ...) or randomly (rand = TRUE, used in the example below) can help in that case.
# sh_r <- splithalf(eem_list_ex, 6, normalise = TRUE, rand = TRUE, cores = cores, nstart = nstart, maxit = maxit, ctol = ctol)
# Tucker’s Congruency Coefficients is a value for the similarity of the splits (and different loadings in general) and splithalf_tcc returns a table showing the values. 1 would be perfect similarity.
# tcc_sh_table <- splithalf_tcc(sh)
# tcc_sh_table
# The function eempf_ssc can be used to compare models, not only from a split-half analysis. It returns the shift- and shape-sensitive congruence (SSC, Wünsch et al. 2019). Alternatively, the TCC can be calculated using the attribute tcc = TRUE. TCCs and SSCs can be combined over excitation and emission spectra using m = TRUE to calculate the modified TCC (Parr et al. 2014) or modified SSC respectively.

# 8.12 Loadings of outliers----
# A modes (loadings) of previously excluded outliers can be calculated using A_missing. This should be used carefully, but enables you to get loadings, that can be further investigated. Please supply an eemlist containing all samples you are interested in. B- and C-modes of the original model are kept and A-modes are calculated.
pf4_wOutliers <- A_missing(eem_list, pfmodel = pf4[[1]], cores = cores)

# 8.13 Further model validation----
# As a way of model validation, the core consistency can be calculated. Please see Murphy et al. (2013) for the limited usability of core consistency for model validation from natural EEMs.

corcondia <- eempf_corcondia(pf4[[1]], eem_list_ex)

# EEMqual is a quality parameter integrating the model fit, the core consistency and the split-half analysis according to Bro and Vidal (2011). Due to the fact that the core consistency is included, the limitations are similar (see above).

eemqual <- eempf_eemqual(pf4[[1]], eem_list_ex, sh, cores = cores)

# Currently, there is not one particular way to determine the importance of each component in a model. Still, answering this question is interesting from an ecological point of view. Here, we propose one way, that is commonly used in different modelling processes. You can calculate the component importance using eempf_varimp. Starting from the model you created, each component is removed at a time and a new model is created using the exact previous components. The difference between the original R-squared and the one from the new model with one component reduced.

varimp <- eempf_varimp(pf4[[1]], eem_list_ex, cores = cores)

# 9.1 naming models and components----
# Models and components can be named. These names can be important for you to organise your models and interpretations of the components. They are also shown in the plots. Here are some examples:

# get current model names (none set so far!)
names(pf3)
## NULL
# set new model names, number of models must be equal to number of names
names(pf3) <- c("3 components", "4 components xy","5 components no outliers","6 components","7 components")
names(pf3)
## [1] "3 components"             "4 components xy"         
## [3] "5 components no outliers" "6 components"            
## [5] "7 components"
# get current component names
eempf_comp_names(pf4)
## [[1]]
## [1] "Comp.1" "Comp.2" "Comp.3" "Comp.4" "Comp.5" "Comp.6"
# set new model names, number of models must be equal to number of names
eempf_comp_names(pf4) <- c("A4","B4","C4","D4","E4","F4")

# in case of more than one model(e.g. pf3):
eempf_comp_names(pf3) <- list(c("A1","B1","C1"), # names for 1st model
                              c("humic","T2","whatever","peak"),
                              c("rose","peter","frank","dwight","susan"),
                              c("A4","B4","C4","D4","E4","F4"),
                              c("A5","B5","C5","D5","E5","F5","G5") # names for 5th model
)

pf4[[1]] %>%
  ggeem(contour = TRUE)

# 9.2 Sorting components----
# Components within a PARAFAC model can be reordered using eempf_reorder.

# 10.1 comparing your data using openfluor.org----
# You can use eempf_openfluor to export a file that can be uploaded to openfluor.org (Murphy et al. 2014). Please check the file header manually after export as some values are not set automatically. Openfluor offers ways to compare your components with those found in other publications. This is a very important step in interpreting the results and coming to ecological sound conclusions.

eempf_openfluor(pf4[[1]], file = "my_model_openfluor.txt")

# 10.2 Comparing data using SSC----
# (Wünsch et al. 2019) showed, that the TCC is sometimes not sensitive enough to separate different components. Therefore, they developed the shift- and shape sensitive congruence coefficient (SSC). It can be calculated using the function eempf_ssc.

# 10.3 Creating a report on your analysis----
# The report created by eempf_report contains important settings and results of your analysis and is exported as an html file. You can specify the information you want to include.

eempf_report(pf4[[1]], export = "parafac_report.html", eem_list = eem_list, shmodel = sh, performance = TRUE)

# 10.4 Exporting the model----
# Using eempf_export you can export your model matrices to a csv file.

# eem_metatemplate is intended as a list of samples and a template for a table containing metadata. Writing this table to a file eases the step of gathering needed values for all samples.

# 10.5 Combine results and handle dilution issues----
# eem_dilcorr creates a table containing information on how to handle diluted samples. Absorbance spectra need to be replaced by undiluted measurements preferably or multiplied by the dilution factor. Names of EEM samples can be adjusted to be similar to their undiluted absorbance sample. You can choose for each sample how you want to proceed on the command line. The table contains information about these two steps.
# 
# eem_absdil takes information from the table generated by eem_dilcorr and multiplies or deletes undiluted absorbance sample data.
# 
# eem_eemdil takes information from the table generated by eem_dilcorr and renames EEM samples to match undiluted absorbance samples.
 