# TEMPLATE SCRIPT: ANALYSIS OF COMPLETE LIST DATA UWING HMSC MODELING FOR JOINT SPECIES DISTRIBUTION MODELING
# (1) READING IN AND PROCESSING DATA
# This template script reads the "Complete Lists" (Täydelliset listat) dataset downloaded from laji.fi to model species occurrences using HMSC framework (Hierarchical Modelling of Species Communities).
# This code makes the necessary modifications to the dataset so that HMSC scripts can be executed.
# For more information on HMSC framework, refer to the book: Ovaskainen, O. and Abrego, N. 2020. Joint Species Distribution Modelling - With Applications in R. Cambridge University Press.
# This script has been written by Valeria Valanne (Finnish Museum of Natural History) on the basis of code distributed as supporting information of the book above and HMSC courses of 2020 and 2022.

# In this example, the dataset used is "Bumblebees - Complete List".
# The data has been downloaded from the Finnish Biodiversity Information Facility (laji.fi) (see READ ME file in Complete-lists-LUOMUS Github page for instruction on downloading the data).
# When downloading, it is recommended to include at least the following columns:
  # Submission.identifier, Taxonomic.order, Scientific.name, Species, ETRS.TM35FIN (or WGS84), 
  # Location.accuracy..m., Time, Observer.s..Collector.s., The.stated.certainty.of.the.observation
  # Depending on the species group, you may also include columns such as Life.stage or Number 



# (1) ######################################################################################################
# DOWNLOAD LIBRARIES ---- 

packages=c("Hmsc","readxl","dplyr","stringr","data.table")
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages,library,character.only=T)



# (2) ######################################################################################################
# READ IN DATA ----

setwd("filepath") # set working directory 

# creating folders in directory, needed later
localDir = "."
dataDir = file.path(localDir, "data")
modelDir = file.path(localDir, "models")
if(!dir.exists(dataDir)) dir.create(dataDir)
if(!dir.exists(modelDir)) dir.create(modelDir)


# tsv / excel files from laji.fi
df <- read.delim("laji-data.tsv")
#df <- read.delim(file = file.path(dataDir,"laji-data.tsv"), stringsAsFactors = T) # if data in data directory





# (3) ######################################################################################################
# CHOOSE SPECIES GROUP AND CLEAN DATA ---- 

# CHOOSE SPECIES GROUP
# If you have downloaded data from multiple Complete list species groups choose one of them here
df <- df[which(df$Collection=="Bumblebees - complete lists"),] # Bumblebees for example

# DELETE NOT NEEDED INFORMATION
# You can delete observations from e.g. observations from species groups recorded to the list but that do not belong in the species group and you wish to remove
# you can use also information from column Taxonomic.order

# Bumblebee example:
df<-df %>%
  mutate(Scientific.name = ifelse(grepl("kaltaiset", Species), "Bombus", Scientific.name)) # kaltaiset = finnish for 'alike'/'similar', word used in bumblebee species aggregates. Note: unique to Bumblebee complete lists!
df <- df[which(grepl("Bombus", df$Scientific.name)),] 
 
 
# Delete inaccurate observations
unique(df$Location.accuracy..m.)
df <- df[-which(df$Location.accuracy..m.>100 ),]  # No more than 100 meters as an example. Include precise enough observations for the purpose of your study.
df <- df[which(df$The.stated.certainty.of.the.observation=="Sure" ),] # Include only 'sure' observations if you wish.


# OBSERVER - ID
# Creating unique observer ID for each observer
# This can be useful in modeling
setDT(df)[ , ObsID := .GRP, by = .(Observer.s..Collector.s.)] 
df <- select(df, -c(Observer.s..Collector.s.))  # Delete observer names from data





# (4) ######################################################################################################
# CREATING VARIABLES NEEDED FOR MODELLING ----


##################################################################################################
# 4.1)  SURVEY EFFORT
# Make column for time used for survey = survey effort. Useful in modelling.
# Time used might be useful also in selecting surveys of comparable survey effort (=time spent)

df <- as.data.frame(df)
df[c('Day', 'Time2')] <- str_split_fixed(df$Time, ' ', 2) 
df$Time2 <- str_replace(df$Time2, "\\[", "") 
df$Time2 <- str_replace(df$Time2, "]", "")
df[c('Start', 'End')] <- str_split_fixed(df$Time2, '-', 2)  
df <- select(df, -c(Time2)) 
df$Start <- ifelse(nchar(df$Start)==4, paste0("0",df$Start),df$Start) ; df$End <-  ifelse(nchar(df$End)==4, paste0("0",df$End),df$End) # Correct time format hh:mm
df$Start <- paste0(df$Day, " ", df$Start) ; df$End <- paste0(df$Day, " ", df$End) # How difftime wants to receive the info (see below)

# Creating Survey.effort: how long survey took (in minutes)
df$Survey.effort <- ""
for (i in 1:nrow(df)) {
  df[i,]$Survey.effort <- difftime(df[i,]$End, df[i,]$Start, units = "mins") # Now in minutes. Option: "mins" -> "hours" etc
}

df$Survey.effort <- as.integer(df$Survey.effort) 
df$Survey.effort_Log <- log(df$Survey.effort) # Survey effort might be more useful in Log



##################################################################################################
# 4.2) FILTERING OBSERVATIONS BASED ON SURVEY TIME 
# Filter OUT observation batches (lists) with incomplete time data and standardise among observation batches for modeling.
# Some kind of filtering is recommended: at least removing observation batches (lists) that are marked as spanning multiple days or where survey effort is too low, indicating error or incomplete list.
# However, here you can define timeframe relevant for your study. This may be advisable to standardise the quality of the final dataset.

length(unique(df$Submission.identifier)) # How many lists before filtering observations (below)

df <- df[which(df$Survey.effort>0 ),] # Good to delete those which show survey effort 0 - might have some problem. Also helps to look at distribution (below)
hist(df$Survey.effort, breaks = 100) # Distribution of time spent - can be used to determine the span of survey efforts used for later analysis


df <- df[which(df$Survey.effort>=10 & df$Survey.effort<=60 ),] # For example: only those lists which were collected within 10-60 minutes
# df <- df[which(df$Survey.effort<=200 ),] 

length(unique(df$Submission.identifier)) # Number of lists after filtering



##################################################################################################
# 4.3) TEMPORAL DIVISION INTO MONTHS/SEASONS
# In modeling it may be useful to divide observation batches temporally, for example, by years, seasons, or months.
# For instance, in the case of insects and plants, the month or phase of summer may influence which species are observed.

df$Month <- str_sub(df$Day, 6, 7) # Month
# df$Year <- str_sub(df$Day, 1, 4) # Year

# Based on the observations, the observation time can also be divided into different parts according to the date.
# For example, observations collected only during summer 2024:

# df$Day <- as.Date(df$Day, "%Y-%m-%d")
# df$Period <- ""
# df$Period <- ifelse(df$Day<"2024-06-01", "1", df$Period ) 
# df$Period <- ifelse(df$Period=="" & df$Day<"2024-06-16", "2", df$Period ) 
# df$Period <- ifelse(df$Period=="" & df$Day<"2024-07-01", "3", df$Period ) 
# df$Period <- ifelse(df$Period=="" & df$Day<"2024-07-16", "4", df$Period ) 
# df$Period <- ifelse(df$Period=="" & df$Day<"2024-08-01", "5", df$Period ) 
# df$Period <- ifelse(df$Period=="" & df$Day<"2024-08-16", "6", df$Period ) 
# df$Period <- ifelse(df$Period=="" & df$Day>"2024-08-15", "7", df$Period ) 






# (5) ######################################################################################################
# CREATE A FILE OF COORDINATE POINTS  ----
# Environmental variables are used in modeling to explain species occurrences/distribution.
# Any relevant dataset can be used. Here you can download the coordinate information of surveypoints into a csv. 
# This csv can be read into a GIS program as a point layer, and then environmental information can be extracted from chosen environmental data

# Environmental datasets can be downloaded, for example, from the spatial data download service Paituli: https://paituli.csc.fi/download.html 
# or from SYKE's spatial data download page:  https://www.syke.fi/fi-fi/avoin_tieto/paikkatietoaineistot/ladattavat_paikkatietoaineistot 
# For example: CORINE Land Cover 2018 Finland dataset, or climate variables etc.


# CREATE A CSV FILE FOR A GIS PROGRAM
# This CSV file can be exported to the GIS program of your choice to produce the environmental variable dataset.
df_coordinates <- unique(df[c("Submission.identifier","ETRS.TM35FIN.N","ETRS.TM35FIN.E")])
write.csv(df_coordinates, file = file.path(dataDir,"laji_data_coordinates.csv" ))




# (6) ######################################################################################################
# IMPORT ENVIRONMENTAL VARIABLE DATA AND COMBINE IT WITH SPECIES DATA ----
# This is not mandatory. You can also bring environmental data directly as XData.
# This step however ensures that each row of species data and environmental data (in XData) will match in the modeling phase.

# Read the environmental variable dataset (csv / xlsx)
env_variables <- read.csv("filepath/file.csv")
#env_variables <- read_excel("filepath/file.xlsx")
#env_variables <- read.delim(file = file.path(dataDir,"filepath/file.csv")) # if data in data directory


# Combine the imported dataset with the species dataset, forming df2
# Both (df and env_variables) must have a common, identically named column, such as "Submission.identifier", linking e.g. correct coordinate point
df2 <- merge(df, env_variables, by = "Submission.identifier", all.x = TRUE, all.y = FALSE)  

unique(df$Scientific.name)[1]


# (7) ######################################################################################################
# CREATE THE TABLE REQUIRED FOR HMSC MODELING ----

# Each species gets its own column based on the scientific name
new_columns <- unique(df2$Scientific.name) # Use scientific.name or other column of your choice. Bombus specific note: all Bombus not determined to species level now in same column.
for (col in new_columns) {
  df2[[col]] <- "0"  
}

# Presence absence data
for (i in 1:nrow(df2)) {
  identifier <- df2$Submission.identifier[i] 
  spp <- df2[df2$Submission.identifier == identifier, "Scientific.name"]
  
  # present = 1,  not present = 0
  for (species_name in spp) {
    set(df2, i = i, j = species_name, value =1)
  }
}

# Save first species name:
First.spp<-unique(df2$Scientific.name)[1]


# Finishing of dataframe
df2 <- select(df2, -c(Scientific.name, Species))  # Delete redundant species columns
df2 <- df2[!duplicated(df2[,c("Submission.identifier")]),] # Delete duplicates




# (8) ######################################################################################################
# SAVE DATA FRAME FOR MODELLING (optional) ----

write.csv( df2, file = file.path(dataDir,"laji_data_for_modelling.csv" ))


# (9) ######################################################################################################
# DEFINING HMSC MODEL ---- 

##################################################################################################
# 9.1) SPECIES DATA INTO Y MATRIX ----
Y <- df2[,c(which(colnames(df2)==First.spp):ncol(df2))]
Y <- as.data.frame(sapply(Y, as.numeric))
Y <- as.matrix(Y)

#  Species prevalence
# At this point, you can remove very rare species from the analysis, as modeling them reliably can be difficult for the model.
prev <- colSums(Y)  ; ncol(Y) # prevalence ; original nro of species
hist(prev) # distribution of prevalence

# Filtering based on prevalence
# sel.sp=(prev>=10) # For example: select species that have been observed at least 10 times in the dataset.
# Y <- Y[,sel.sp] ; ncol(Y)


##################################################################################################
# 9.2) ENVIRONMENTAL VARIABLES INTO XData AND DEFINING XFromula ----


XData <- df2[,c("Habitat", "Rainfall", "Temperature", "Year", "Survey.effort_Log", "ObsID", "Month" )] # EXAMPLE. Add the variables that you want to use to model species occurrence.

# Good to check that the classes of variables are correct (factor instead of caracter etc)
str(XData)

# Nro of rows should be the same
dim(Y)
dim(XData)

all(rownames(Y)==XData$X)
which(rownames(Y)!=XData$X)


# Defining XFormula of the model
# For example:
XFromula = ~ Habitat + Rainfall + Temperature + Survey.effort_Log



##################################################################################################
# 9.3) TRAIT DATA, RANDOM VARIABLES AND PHYLOGENETIC INFORMATION (optional) ----

# To the model you can also add Traits
# For example:
# alltraits = read.csv(file.path(dataDir, "traits.csv"))
# TrData = data.frame(Species = alltraits$Species, Size = alltraits$Size, Migration = alltraits$Migration)
# rownames(TrData)<-TrData$Species
# TrFormula = ~  Size + Migration # EXAMPLE

# You can also add phylogenetic information
# taxonomicTree = ape::read.tree(file = file.path(dataDir, "phylo_tree.tre")) # EXAMPLE


#You also need studyDesign (if model has random variables)
# For example:
studyDesign = data.frame(Year = XData$Year)
str(studyDesign)


# Defining random variables:
# For example, you can define the year as a random variable in the model. This variable must also be included in the studyDesign
rL.Year = HmscRandomLevel(units = levels(studyDesign$Year)) 



##################################################################################################
# 9.4) MODEL DEFINITION (Hmsc model constructor) ----
# Add all necessary components to the model. Below is a simple model to which you can add phylogeny, traits data, and random variables.
# You can also define multiple models, for example, m1 and m2:

# Basic model
m1 = Hmsc(Y=Y,
         distr="probit",                   # probit for presence-absence
         XData = XData,  XFormula=XFormula)
#   TrData = TrData, TrFormula = TrFormula,
#   phyloTree = taxonomicTree,
#   studyDesign = studyDesign,
#  ranLevels=list(Year = rL.Year)

m1
head(m1$X)


# Random model
m2 = Hmsc(Y=Y,
          distr="probit",
          XData = XData,  XFormula=XFormula,
          studyDesign = studyDesign,
          ranLevels=list(Year = rL.Year))
#   TrData = TrData, TrFormula = TrFormula,
#   phyloTree = taxonomicTree)

m2
head(m2$X)




##################################################################################################
#  9.5) MODEL SAVING ----
# Save the models so that they can be run later
models = list(m1,m2)
names(models) = c("model_basic", "model_random" ) 
save( models, file = file.path(modelDir, "unfitted_models.RData"))



##################################################################################################
# 9.6) TEST: Can the models be run without errors? ----

for(i in 1:length(models)){
  print(i)
  sampleMcmc(models[[i]],samples=2)
}


