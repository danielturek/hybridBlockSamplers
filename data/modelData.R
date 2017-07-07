
require(nimble)

##setwd('~/github/hybridBlockSamplers')



###############################################
# simple pump model - example where afss not necessary?
###############################################



pumpCode <- nimbleCode({
    for (i in 1:N){
        theta[i] ~ dgamma(alpha,beta)
        lambda[i] <- theta[i]*t[i]
        x[i] ~ dpois(lambda[i])
    }
    alpha ~ dexp(1.0)
    beta ~ dgamma(0.1,1.0)
})
pumpConsts <- list(N = 10,
                   t = c(94.3, 15.7, 62.9, 126, 5.24,
                       31.4, 1.05, 1.05, 2.1, 10.5))
pumpData <- list(x = c(5, 1, 5, 14, 3, 19, 1, 1, 4, 22))

pumpInitial <- list(alpha = 0.8237675, beta = 1.2634812,  theta = rep(0.1, pumpConsts$N))

pumpParams <- c('alpha', 'beta')

pumpModel <- nimbleModel(code = pumpCode, name = 'pump', constants = pumpConsts,
                         data = pumpData, inits = pumpInitial)

pumpNumIters <- c(1000, 10000, 100000)

pumpGoodPropCov <- matrix(c(0.0843184,  0.103402,
                            0.103402,  0.302111), nrow = 2, byrow = TRUE)

pumpGoodScale <- 1.383051



##########################################################
# Nimble SSM model -- Independent
##########################################################

load('data/model_SSMindependent.RData')

SSMindCode <- code
SSMindConsts <- constants
SSMindData <- data
SSMindInitial <- inits
SSMindParams <- c('mu', 'b', 'sigOE', 'sigPN')


SSMindGoodPropCov <- matrix(c( 2.561579e-02, -0.04641807,    4.744978e-05, -2.448161e-05, 
                              -.04641807,     0.8647810698, -5.241958e-04,  2.664147e-04, 
                               4.744978e-05, -0.0005241958,  1.188512e-04, -2.171712e-05, 
                              -2.448161e-05,  0.0002664147, -2.171712e-05,  2.262059e-04), nrow=4, byrow = TRUE)

SSMindGoodScale <- 0.5879964


SSMindModel <- nimbleModel(code = SSMindCode, name = 'SSMind', constants = SSMindConsts, data = SSMindData,
                             inits = SSMindInitial)

SSMindNumIters <- c(1000, 10000, 100000)





##########################################################
# Nimble SSM model -- Correlated
##########################################################

load('data/model_SSMcorrelated.RData')

SSMcorCode <- code
SSMcorConsts <- constants
SSMcorData <- data
SSMcorInitial <- inits
SSMcorParams <- c('a', 'b', 'sigOE', 'sigPN')


SSMcorGoodPropCov <- matrix(c( 5.027787e-05, -7.672826e-04, -1.171209e-06, -1.122275e-06, 
                              -7.672826e-04,  1.554514e-02,  2.360247e-05,  2.061841e-05, 
                              -1.171209e-06,  2.360247e-05,  8.096607e-05, -1.160311e-05, 
                              -1.122275e-06,  2.061841e-05, -1.160311e-05,  2.202926e-04), nrow = 4, byrow = TRUE)


SSMcorGoodScale <- 0.3402731


SSMcorModel <- nimbleModel(code = SSMcorCode, name = 'SSMcor', constants = SSMcorConsts, data = SSMcorData,
                             inits = SSMcorInitial)


SSMcorNumIters <- c(1000, 10000, 100000)




##########################################################
# ice autoregressive model
##########################################################

load('data/model_ice.RData')

iceCode <- code
iceConsts <- constants
iceData <- data
iceInitial <- inits
iceModel <- nimbleModel(code = iceCode, name = 'ice', constants = iceConsts, data = iceData,
                             inits = iceInitial)


iceParams <- c(paste0('alpha[', 2:13, ']'), 'sigma')

load('data/propCov_ice.RData')
iceGoodPropCov <-  propCov

iceGoodScale <- 0.5182714

iceNumIters    <- c(1000, 10000, 100000)





##########################################################
# litters random effects model
##########################################################

load('data/model_litters.RData')

littersCode <- code
littersConsts <- constants
littersData <- data
littersInitial <- inits
littersModel <- nimbleModel(code = littersCode, name = 'litters', constants = littersConsts, data = littersData,
                             inits = littersInitial)


littersParams <- c('a[1]', 'b[1]', 'a[2]', 'b[2]')

load('data/propCov_litters.RData')
littersGoodPropCov <-  propCov

littersGoodScale <- 0.3793813

littersNumIters <- c(1000, 10000, 100000)




##########################################################
# mhp (large GLM)
##########################################################

load('data/model_mhp.RData')

mhpCode <- code
mhpConsts <- constants
mhpData <- data
mhpInitial <- inits
mhpModel <- nimbleModel(code = mhpCode, name = 'mhp', constants = mhpConsts, data = mhpData,
                             inits = mhpInitial)

mhpParams <- c('mu', 'be[1]', 'bp[1]', 'bp[2]', 'bp[3]', 'bep[1]', 'bep[2]', 'bep[3]', 'sds', 'sdse', 'sdsp')

load('data/propCov_mhp.RData')
mhpGoodPropCov <-  propCov

mhpGoodScale <- 0.3761706

mhpNumIters <- c(1000, 10000)











if(FALSE) {

    setwd('~/github/hybridBlockSamplers')
    require(nimble)
    nimbleOptions(buildInterfacesForCompiledNestedNimbleFunctions = TRUE)


    Rmodel <- spatialModel  ## CHANGE

    conf <- configureMCMC(Rmodel)
    conf$printMonitors()
    conf$printSamplers()
    conf$addSampler(spatialParams, 'RW_block')  ## CHANGE
    conf$printSamplers()
    Rmcmc <- buildMCMC(conf)
    
    Cmodel <- compileNimble(Rmodel)
    Cmcmc <- compileNimble(Rmcmc, project = Rmodel)

    set.seed(0)
    system.time(samples <- runMCMC(Cmcmc, 50000))

    ind <- 859                      ## CHANGE THE INDEX
    Rmcmc$samplerFunctions$contentsList[[ind]]$target
    Cmcmc$samplerFunctions$contentsList[[ind]]$propCov
    Cmcmc$samplerFunctions$contentsList[[ind]]$scale
    propCov <- Cmcmc$samplerFunctions$contentsList[[ind]]$propCov
    propCov - t(propCov)
    save(propCov, file = 'data/propCov_spatial.RData')    ## CHANGE

    nimbleOptions(buildInterfacesForCompiledNestedNimbleFunctions = FALSE)


    ##samplesPlot(samples)

}


