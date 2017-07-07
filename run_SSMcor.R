
library(nimble, lib.loc = '~/Documents/')
nimbleOptions(MCMCprogressBar = FALSE)
library(coda)
setwd('~/github/hybridBlockSamplers')
source('data/modelData.R')
source('AFSS_to_RW_block_sampler.R')
##
##nreps <- 1
nreps <- 10
##
compareDF <- data.frame(sampler = NA, nfa = NA, nIter = NA, runTime = NA, minEfficiency = NA)
allEffDF <-  data.frame(sampler = NA, nfa = NA, nIter = NA, runTime = NA, repNum = NA, param = NA, eff = NA)
allSamplersTable <- data.frame(sampler = 'RW_block', nfa = NA)
allSamplersTable <- rbind(allSamplersTable,
                          expand.grid(sampler = 'RW_block_goodCov', nfa = NA))
allSamplersTable <- rbind(allSamplersTable,
                          expand.grid(sampler = 'AF_slice',
                                      nfa = NA))
allSamplersTable <- rbind(allSamplersTable,
                          expand.grid(sampler = 'AFSS_to_RW_block',
                                      nfa = c(1,2,4,8)))
##
modelName <- 'SSMcor'
##
model <- get(paste0(modelName,'Model'))
code <- get(paste0(modelName, 'Code'))
params <- get(paste0(modelName,'Params'))
constants <- get(paste0(modelName, 'Consts'))
inits <- get(paste0(modelName,'Initial'))
data <- get(paste0(modelName, 'Data'))
propCov <- get(paste0(modelName, 'GoodPropCov'))
goodScale <- get(paste0(modelName, 'GoodScale'))
iterVals <- get(paste0(modelName, 'NumIters'))
##
cmodel <- compileNimble(model)
##
for(samplerRowNum in 1:dim(allSamplersTable)[1]){
    samplerRow <- allSamplersTable[samplerRowNum,]
    sampler <- as.character(samplerRow$sampler)
    print(samplerRow)
    conf <- configureMCMC(model)
    conf$removeSamplers(params)
    if(sampler == 'RW_block'){
        conf$addSampler(params, type = sampler, control = list())
    }
    else if(sampler == 'RW_block_goodCov'){
        conf$addSampler(params, type = 'RW_block', control = list(propCov = propCov, scale = goodScale))
    }
    else if(sampler == 'AF_slice'){
        conf$addSampler(params, type = 'AF_slice', control = list())
    }
    else if(sampler == 'AFSS_to_RW_block'){
        conf$addSampler(params, type = 'AFSS_to_RW_block', 
                        control = list(AF_sliceControl =  list(sliceWidths = 'oneVec',
                                           factorBurnIn = 15000, factorAdaptInterval = 1000,
                                           sliceBurnIn = 512, sliceMaxSteps = 100),
                            RWcontrol = list(propCov = diag(length(params)), scale = 1,
                                adaptInterval = 200, adaptScaleOnly = FALSE, adaptive = TRUE),
                                       numFactorAdaptations = samplerRow$nfa))
    }
    mcmc <- buildMCMC(conf)
    cmcmc <- compileNimble(mcmc)
    cmodel$setInits(inits)
    for(nIter in iterVals){
        message(paste0('using nIter = ', as.character(nIter)))
        mcmcTime <- numeric(nreps)
        mcmcEff <- numeric(nreps)
        cat('rep: ')
        for(iter in 1:nreps){
            cat(paste0(iter, ' '))
            mcmcTime[iter] <- system.time(cmcmc$run(nIter))[['elapsed']]
            ##browser()  ## ?????
            effValues <- coda::effectiveSize(coda::as.mcmc(as.matrix(cmcmc$mvSamples)))/(mcmcTime[iter]+.01)
            effNames <- names(effValues)
            effValues <- as.numeric(effValues)
            mcmcEff[iter] <- min(effValues)
            newRow <- cbind(samplerRow, data.frame(nIter = nIter, runTime = mcmcTime[iter], minEfficiency = mcmcEff[iter]))
            compareDF <- rbind(compareDF, newRow)
            newAllEffDF <- data.frame(sampler = sampler, nfa = samplerRow$nfa, nIter = nIter,
                                      runTime = mcmcTime[iter], repNum = iter, param = effNames, eff = effValues,
                                      stringsAsFactors = FALSE)
            allEffDF <- rbind(allEffDF, newAllEffDF)
        }
        cat('
')
    }
    if(all(is.na(compareDF[1,]))) compareDF <- compareDF[-1, ]
    if(all(is.na(allEffDF[1,])))  allEffDF <-  allEffDF[-1, ]
}
##
filename <- paste0('results/', modelName, 'Results.RData')
save(compareDF, allEffDF, file = filename)

