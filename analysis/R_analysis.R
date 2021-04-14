# Title     : Rank-ordered analysis of bikeway quality survey
# Objective : Two main analysis objectives:
#             1) Estimate coefficients for the buffer width & height preference using rank-odered logit.
#                a) Sub-objective is to get mean value of max-buffer preference.
#             2) Analyze aggregate weights for Pavement Quality Index (PQI)
#                from criteria, visibility, and debris.
#             Bonus) Review open-response for trends?
# Created by: Nick Fournier
# Created on: 4/14/2021

# Packages
library(mlogit)
library(data.table)
library(ggplot2)

# A column name dictionary for easier subsetting
col_dict <- list(
    'IDS'           = c('TIMESTAMP', 'USERNAME'),
    'RANK_CRIT'     = c('RANK_CRIT_STRUCTURAL', 'RANK_CRIT_FUNCTIONAL', 'RANK_CRIT_MAINTENANCE'),
    'RANK_VIS'      = c('RANK_VIS_ELLERY', 'RANK_VIS_EMBARCADERO', 'RANK_VIS_HAMPSHIRE', 'RANK_VIS_BROADWAY', 'RANK_VIS_POTRERO', 'RANK_VIS_MERIDIAN'),
    'RANK_DEBRIS'   = c('RANK_DEBRIS_NON_PUNCT','RANK_DEBRIS_PARTICLES','RANK_DEBRIS_PRECIP', 'RANK_DEBRIS_SLIP', 'RANK_DEBRIS_PUNCT'),
    'RANK_LANE'     = c('RANK_LANE_POSTPROTECT', 'RANK_LANE_BUFFERED', 'RANK_LANE_RAISED', 'RANK_LANE_STANDARD', 'RANK_LANE_PARKINGPROTECT', 'RANK_LANE_CURBPROTECT'),
    'PREF_MEAS'     = c('DEBRIS_MEAS_NON_PUNCT', 'DEBRIS_MEAS_PUNCT', 'DEBRIS_MEAS_SLIP', 'DEBRIS_MEAS_PARTICLES', 'DEBRIS_MEAS_PRECIP'),
    'OPEN_RESPONSE' = c('OR_MEASUREMENTS', 'OR_FEATURES', 'MAX_BUFFER'),
    'DEMOGS'        = c('AGE', 'GENDER', 'CYCLIST_TYPE', 'CYCLIST_FREQ')
)

# A height/width dictionary per lane type
width_height <- list('POSTPROTECT'    = data.frame('WIDTH' = 0.5, 'HEIGHT' = 3),
                     'BUFFERED'       = data.frame('WIDTH' = 3,   'HEIGHT' = 0),
                     'RAISED'         = data.frame('WIDTH' = 1,   'HEIGHT' = 0.5),
                     'STANDARD'       = data.frame('WIDTH' = 0.5,   'HEIGHT' = 0),
                     'PARKINGPROTECT' = data.frame('WIDTH' = 12,  'HEIGHT' = 5),
                     'CURBPROTECT'    = data.frame('WIDTH' = 3,   'HEIGHT' = 0.5)
)

#### IMPORT DATA ####
bikedata <- fread('./data/cleaned_survey_data_download_latest.csv')


#### RANK-ORDERED LOGIT OF LANE-RANKING ####
# Melt to long form
bikedata_ranklane <- melt(bikedata,
                      value.name = 'ch',
                      variable.name = 'RANK_LANE',
                      measure.vars = col_dict[['RANK_LANE']],
                      id.vars = c('chid', col_dict[['DEMOGS']]))

# House cleaning of variable prefix
bikedata_ranklane$RANK_LANE <- gsub('RANK_LANE_', '', bikedata_ranklane$RANK_LANE)

# Add height and width
bikedata_ranklane <- merge(bikedata_ranklane,
                           data.table(RANK_LANE=names(width_height), rbindlist(width_height)),
                           by = 'RANK_LANE')

# Normalize data
minmax <- function(x) 1 + ( x - min(x) ) / ( max(x) - min(x) )
zscore <- function(x) (x-mean(x))/sd(x)

bikedata_ranklane[ , (c('WIDTH','HEIGHT','AGE')) := lapply(.SD, minmax), .SDcols = c('WIDTH','HEIGHT','AGE')]
# bikedata_ranklane$WIDTH <- runif(nrow(bikedata_ranklane))
# bikedata_ranklane$HEIGHT <- runif(nrow(bikedata_ranklane))
bikedata_ranklane$WIDTH <- 1
bikedata_ranklane$HEIGHT <- 1

# Order by person-choice ID for clarity and convert to data.frame for mlogit
bikedata_ranklane <- as.data.frame(bikedata_ranklane[order(chid), ])

#Formats dataframe to mlogit data type
bikedata_mlogit <- dfidx(bikedata_ranklane, choice = "ch", ranked = TRUE,
                         idx = c("chid", "RANK_LANE"), idnames = c("chid", "alt"))

# Estimate model
summary(rank_lane_res <- mlogit(ch ~ 1 + WIDTH | AGE,
               bikedata_mlogit, reflevel = "STANDARD"))





#### AGGREGATE RANKING SCORES ####




