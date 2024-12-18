
# This script is course materials of HMSC course of 2022, and entirely written by Ovaskainen et al. 
# The script has been downloaded from this website: https://www.helsinki.fi/en/researchgroups/statistical-ecology/software/past-hmsc-workshops
# For more information on HMSC framework, refer to the book: 
      # Ovaskainen, O. and Abrego, N. 2020. Joint Species Distribution Modelling - With Applications in R. Cambridge University Press.
# Or this publication:
      # Ovaskainen, O., Tikhonov, G., Norberg, A., Blanchet, F. G., Duan, L., Dunson, D., Roslin, T. and Abrego, N. 2017. How to make more out of community data? 
      # A conceptual framework and its implementation as models and software. Ecology Letters 20, 561-576. https://doi.org/10.1111/ele.12757.


# You can use this script to run these analyses on Complete checklist data downloaded from FinBIF (laji.fi)


##################################################################################################
# Set the base directory using your favorite method
setwd("filepath")

##################################################################################################
# INPUT AND OUTPUT OF THIS SCRIPT (BEGINNING)
##################################################################################################
#	INPUT. Unfitted models.

#	OUTPUT. Fitted models, with fitting done for multiple RUNs:
# first short MCMC chains (to provide some results fast), and then with increasingly long MCMC chains
# (until MCMC convergence or computational limit is reached): the files
# "models/models_thin_1_samples_5_chains_4.Rdata" (RUN 0),
# "models/models_thin_1_samples_250_chains_4.Rdata" (RUN 1),
# "models/models_thin_10_samples_250_chains_4.Rdata" (RUN 2), 
# "models/models_thin_100_samples_250_chains_4.Rdata" (RUN 3), and so on.
##################################################################################################
# INPUT AND OUTPUT OF THIS SCRIPT (END)
##################################################################################################


##################################################################################################
# SETTING COMMONLY ADJUSTED PARAMETERS TO NULL WHICH CORRESPONDS TO DEFAULT CHOICE (BEGINNING)
##################################################################################################
nParallel = NULL #Default: nParallel = nChains
##################################################################################################
# SETTING COMMONLY ADJUSTED PARAMETERS TO NULL WHICH CORRESPONDS TO DEFAULT CHOICE (END)
##################################################################################################

##################################################################################################
# CHANGE DEFAULT OPTIONS BY REMOVING COMMENT AND SETTING VALUE (BEGINNING)
# NOTE THAT THIS IS THE ONLY SECTION OF THE SCRIPT THAT YOU TYPICALLY NEED TO MODIFY
##################################################################################################
#nParallel = 1 #Set to 1 to disable parallel computing
##################################################################################################
# CHANGE DEFAULT OPTIONS BY REMOVING COMMENT AND SETTING VALUE (END)
# NOTE THAT THIS IS THE ONLY SECTION OF THE SCRIPT THAT YOU TYPICALLY NEED TO MODIFY
##################################################################################################

##################################################################################################
# SET DIRECTORIES (BEGINNING)
##################################################################################################
localDir = "."
modelDir = file.path(localDir, "models")
##################################################################################################
# SET DIRECTORIES (END)
##################################################################################################

library(Hmsc)

load(file=file.path(modelDir,"unfitted_models.RData"))


#model selection (create the model structure you want to compare)
#m0 = models[[1]]
#m1 = update(m0, XFormula = ~ tree + volume + decay) 
#m2 = update(m0, ranLevels=list(id = rL.id))
#models$noindex.model = m1
#models$nosite.model = m2
  
nm = length(models)
samples_list = c(250)
thin_list = c(100)
nChains = 2
if(is.null(nParallel)) nParallel = nChains
Lst = 1
while(Lst <= length(samples_list)){
  thin = thin_list[Lst]
  samples = samples_list[Lst]
  print(paste0("thin = ",as.character(thin),"; samples = ",as.character(samples)))
  filename = file.path(modelDir,paste("models_thin_", as.character(thin),
                                             "_samples_", as.character(samples),
                                             "_chains_",as.character(nChains),
                                             ".Rdata",sep = ""))
  if(file.exists(filename)){
    print("model had been fitted already")
  } else {
    print(date())
    for (mi in 1:nm) {
      print(paste0("model = ",names(models)[mi]))
      m = models[[mi]]
      m = sampleMcmc(m, samples = samples, thin=thin,
                     adaptNf=rep(ceiling(0.4*samples*thin),m$nr), 
                     transient = ceiling(0.5*samples*thin),
                     nChains = nChains,
                     nParallel = nParallel) 
      models[[mi]] = m
    }
    save(models,file=filename)
  }
  Lst = Lst + 1
}

