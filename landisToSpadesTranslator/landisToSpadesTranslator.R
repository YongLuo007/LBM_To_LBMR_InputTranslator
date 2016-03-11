
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "landisToSpadesTranslator",
  description = "This is a translator using LANDIS II inputs for SpaDES inputs, currently is built for biomass succession mdoel",
  keywords = c("LANDIS II SpaDES"),
  authors = c(person(c("Yong"), "Luo", email="yluo1@lakeheadu.com", role=c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("1.1.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", # e.g., "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "landisToSpadesTranslator.Rmd"),
  reqdPkgs = list("data.table", "dplyr", "raster"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter("speciesInitialTime", "numeric", 0, NA_real_, NA_real_,
                    "This describes the simulation time at which the first plot event should occur"),
    defineParameter("speciesEcoregionInitialTime", "numeric", 0, NA_real_, NA_real_,
                    "This describes the simulation time at which the first save event should occur"),
    defineParameter("initialCommunitiesInitialTime", "numeric", 0, NA_real_, NA_real_,
                    "This describes the simulation time at which the first save event should occur"),
    defineParameter("ecoregionInitialTime", "numeric", 0, NA_real_, NA_real_,
                    "This describes the simulation time at which the first save event should occur"),
    defineParameter("minRelativeBInitialTime", "numeric", 0, NA_real_, NA_real_,
                    "This describes the simulation time at which the first save event should occur"),
    defineParameter("sufficientLightInitialTime", "numeric", 0, NA_real_, NA_real_,
                    "This describes the simulation time at which the first save event should occur")
  ),
  inputObjects = data.frame(
    objectName = c("landisInputPath"),
    objectClass = c("character"),
    sourceURL = "",
    other = NA_character_,
    stringsAsFactors = FALSE
  ),
  outputObjects = data.frame(
    objectName = NA_character_,
    objectClass = NA_character_,
    other = NA_character_,
    stringsAsFactors = FALSE
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.landisToSpadesTranslator = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)

    # do stuff for this event
    sim <- sim$landisToSpadesTranslatorInit(sim)

    # schedule future event(s)
    sim <- scheduleEvent(sim, params(sim)$landisToSpadesTranslator$speciesInitialTime,
                         "landisToSpadesTranslator", "species")
    sim <- scheduleEvent(sim, params(sim)$landisToSpadesTranslator$speciesEcoregionInitialTime,
                         "landisToSpadesTranslator", "speciesEcoregion")
    sim <- scheduleEvent(sim, params(sim)$landisToSpadesTranslator$initialCommunitiesInitialTime,
                         "landisToSpadesTranslator", "initialCommunities")
    sim <- scheduleEvent(sim, params(sim)$landisToSpadesTranslator$ecoregionInitialTime,
                         "landisToSpadesTranslator", "ecoregion")
    sim <- scheduleEvent(sim, params(sim)$landisToSpadesTranslator$minRelativeBInitialTime,
                         "landisToSpadesTranslator", "minRelativeB")
    sim <- scheduleEvent(sim, params(sim)$landisToSpadesTranslator$sufficientLightInitialTime,
                         "landisToSpadesTranslator", "sufficientLight")
  } 
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
landisToSpadesTranslatorInit <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  # load the files from a path that contains landis inputs
  maxcol <- max(count.fields(file.path(landisInputPath, mainInputFile), sep = ""))
  mainInput <- read.table(file.path(landisInputPath, mainInputFile),
                          fill = TRUE,
                          sep = "",
                          header = FALSE,
                          blank.lines.skip = TRUE,
                          col.names = c(paste("col",1:maxcol, sep = "")),
                          stringsAsFactor = FALSE)
  mainInput <- data.table(mainInput)
  mainInput <- mainInput[col1 != ">>",]
  sim$mainInput <- mainInput
  rm(maxcol)
  
  # load the species table
  maxcol <- max(count.fields(file.path(landisInputPath, speciesFile), sep = ""))
  sim$species <- read.table(file.path(landisInputPath, speciesFile),
                            fill = TRUE,
                            sep = "",
                            header = FALSE,
                            blank.lines.skip = TRUE,
                            col.names = c(paste("col",1:maxcol, sep = "")),
                            stringsAsFactor = FALSE)
  rm(maxcol)
  
  # input species ecoregion dynamics table
  maxcol <- max(count.fields(file.path(landisInputPath, speciesEcoregionFile), sep = ""))
  sim$speciesEcoregion <- read.table(file.path(landisInputPath, speciesEcoregionFile),
                                     fill = TRUE,
                                     sep = "",
                                     header = FALSE,
                                     blank.lines.skip = TRUE,
                                     col.names = c(paste("col",1:maxcol, sep = "")),
                                     stringsAsFactor = FALSE)
  rm(maxcol)
  
  # input initial communities
  maxcol <- max(count.fields(file.path(landisInputPath, initialCommunitiesFile), sep = ""))
  sim$initialCommunities <- read.table(file.path(landisInputPath, initialCommunitiesFile),
                                       fill = TRUE,
                                       sep = "",
                                       blank.lines.skip = TRUE,
                                       col.names = c("species", paste("age",1:(maxcol-1), sep = "")),
                                       stringsAsFactor = FALSE)
  
  # input initial community map
  sim$initialCommunitiesMap <- raster(file.path(landisInputPath, initialCommunitiesMapFile))
  
  # input ecoregion table
  maxcol <- max(count.fields(file.path(landisInputPath, ecoregionFile), sep = ""))
  sim$ecoregion <- read.table(file.path(landisInputPath, ecoregionFile),
                              fill = TRUE,
                              sep = "",
                              header = FALSE,
                              blank.lines.skip = TRUE,
                              col.names = c(paste("col",1:maxcol, sep = "")),
                              stringsAsFactor = FALSE)
  # load ecoregion map
  sim$ecoregionMap <- raster(file.path(landisInputPath, ecoregionMapFile))
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for save events
landisToSpadesTranslatorSpecies <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  species <- sim$species
  species <- data.table(species[,1:11])
  species <- species[col1!= "LandisData",]
  species <- species[col1!= ">>",]
  names(species) <- c("species", "longevity", "sexualmature", "shadetolerance", 
                      "firetolerance", "seeddistance_eff", "seeddistance_max", 
                      "resproutprob", "resproutage_min", "resproutage_max",
                      "postfireregen")
  species$longevity <- as.integer(species$longevity)
  species$sexualmature <- as.integer(species$sexualmature)
  species$shadetolerance <- as.integer(species$shadetolerance)
  species$firetolerance <- as.integer(species$firetolerance)
  species$seeddistance_eff <- as.integer(species$seeddistance_eff)
  species$seeddistance_max <- as.integer(species$seeddistance_max)
  species$resproutprob <- as.numeric(species$resproutprob)
  species$resproutage_min <- as.integer(species$resproutage_min)
  species$resproutage_max <- as.integer(species$resproutage_max)
  
  # obtain other species attribute from main input file
  speciesAddon <- data.frame(mainInput)
  startRow <- which(speciesAddon$col1 == "SpeciesParameters")
  speciesAddon <- speciesAddon[(startRow+1):(startRow+nrow(species)),1:6]
  names(speciesAddon) <- c("species", "leaflongevity", "wooddecayrate",
                           "mortalityshape", "growthcurve", "leafLignin")
  speciesAddon$leaflongevity <- as.numeric(speciesAddon$leaflongevity)
  speciesAddon$wooddecayrate <- as.numeric(speciesAddon$wooddecayrate)
  speciesAddon$mortalityshape <- as.numeric(speciesAddon$mortalityshape)
  speciesAddon$growthcurve <- as.numeric(speciesAddon$growthcurve)
  speciesAddon$leafLignin <- as.numeric(speciesAddon$leafLignin)
  speciesAddon <- data.table(speciesAddon, key = "species")
  species <- setkey(species, species)[speciesAddon, nomatch = 0]
  sim$species <- species
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for save events
landisToSpadesTranslatorSpeciesEcoregion <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  speciesEcoregion <- data.table(sim$speciesEcoregion)
  speciesEcoregion <- speciesEcoregion[col1 != "LandisData",]
  speciesEcoregion <- speciesEcoregion[col1 != ">>",]
  names(speciesEcoregion)[1:6] <- c("year", "ecoregion", "species",
                                    "establishprob", "maxANPP", "maxB")
  speciesEcoregion <- speciesEcoregion[,.(year, ecoregion, species,
                                          establishprob, maxANPP, maxB)]
  speciesEcoregion$year <- as.integer(speciesEcoregion$year)
  speciesEcoregion$establishprob <- as.numeric(speciesEcoregion$establishprob)
  speciesEcoregion$maxANPP <- as.integer(speciesEcoregion$maxANPP)
  speciesEcoregion$maxB <- as.integer(speciesEcoregion$maxB)
  sim$speciesEcoregion <- speciesEcoregion
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for save events
landisToSpadesTranslatorInitialCommunities <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  initialCommunities <- sim$initialCommunities
  initialCommunities <- data.table(initialCommunities)
  initialCommunities <- initialCommunities[species != "LandisData",]
  initialCommunities <- initialCommunities[species != ">>",]
  initialCommunities <- cbind(data.table(mapcode = 1:nrow(initialCommunities),
                                         description = NA),
                              initialCommunities)
  mapcodeindex <- which(initialCommunities$species == "MapCode")
  for (i in 1:(length(mapcodeindex)-1)){
    initialCommunities$mapcode[(mapcodeindex[i]+1):(mapcodeindex[i+1]-1)] <- as.numeric(initialCommunities$age1[mapcodeindex[i]])
  }
  initialCommunities <- initialCommunities[species != "MapCode",] %>%
    data.frame
  for(i in 4:ncol(initialCommunities)){
    initialCommunities[,i] <- as.integer(initialCommunities[,i])
  }
  initialCommunities <- data.table(initialCommunities)
  sim$initialCommunities <- initialCommunities
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for save events
landisToSpadesTranslatorEcoregion <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  ecoregion <- sim$ecoregion
  ecoregion <- data.table(ecoregion)
  ecoregion <- ecoregion[col1 != "LandisData",]
  ecoregion <- ecoregion[col1 != ">>",]
  names(ecoregion)[1:4] <- c("active", "mapcode", "ecoregion", "description")
  ecoregion$mapcode <- as.integer(ecoregion$mapcode)
  sim$ecoregion <- ecoregion
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

landisToSpadesTranslatorMinRelativeB <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  minRelativeB <- sim$mainInput %>%
    data.frame
  startRow <- which(minRelativeB$col1 == "MinRelativeBiomass")
  minRelativeB <- minRelativeB[(startRow+1):(startRow+6),]
  minRelativeB[1,2:ncol(minRelativeB)] <- minRelativeB[1,1:(ncol(minRelativeB)-1)]
  names(minRelativeB) <- NULL
  minRelativeB <- minRelativeB[,-1] %>%
    t(.) %>%
    gsub(pattern="%",replacement="") %>%
    data.table
  names(minRelativeB) <- c("ecoregion", "X1", "X2", "X3", "X4", "X5")
  minRelativeB <- minRelativeB %>%
    mutate_each(funs(as.numeric(as.character(.))/100), vars=-ecoregion)
  sim$minRelativeB <- minRelativeB
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

landisToSpadesTranslatorSufficientLight <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  sufficientLight <- sim$mainInput %>%
    data.frame
  startRow <- which(sufficientLight$col1 == "SufficientLight")
  sufficientLight <- sufficientLight[(startRow+1):(startRow+5), 1:7]
  for(i in 1:ncol(sufficientLight)){
    sufficientLight[,i] <- as.numeric(sufficientLight[,i])
  }
  names(sufficientLight) <- c("speciesshadetolerance",
                              "X0", "X1", "X2", "X3", "X4", "X5")
  sim$sufficientLight <- sufficientLight
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
