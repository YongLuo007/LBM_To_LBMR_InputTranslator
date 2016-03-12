
rm(list=ls())


landisInputPath <- "C:/Users/yonluo/Documents/simulations/boreal plain"
mainInputFile <- "main_input_file_BO_PLAINS.txt"
speciesFile <- "SpeciesAttributes_LANDIS_II_BO_PLAINS.txt"
speciesEcoregionFile <- "dynamic_input_BO_PLAINS_LANDIS_II_baseline.txt"
initialCommunitiesFile <- "BO_PLAINS_species.dat"
initialCommunitiesMapFile <- "initialCommunitiesMap.tif"
ecoregionFile <- "landtypes_BP.txt"
ecoregionMapFile <- "landtypes_BP.tif"




objects <- list("landisInputPath" = landisInputPath,
                "mainInputFile" = mainInputFile,
                "speciesFile" = speciesFile,
                "speciesEcoregionFile" = speciesEcoregionFile,
                "initialCommunitiesFile" = initialCommunitiesFile,
                "initialCommunitiesMapFile" = initialCommunitiesMapFile,
                "ecoregionFile" = ecoregionFile,
                "ecoregionMapFile" = ecoregionMapFile)
parameters <- list(.progress = list(),
                   .globals = list(),
                   landisToSpadesTranslator = list())
path <- list(modulePath=file.path("~/GitHub/landisToSpadesTranslator"),
             outputPath="~/output")
modules <- list("landisToSpadesTranslator")
times <- list(start = 0, end = 1)
mySim <- simInit(times = times, #params = parameters,
                 modules = modules, objects = objects, paths = path)
mySim <- spades(mySim, debug=FALSE)
