

##getParVirtual <- nimbleFunctionVirtual(
##    methods = list(
##        scalarGet = function(){returnType(double(0))}
##    )
##)
## 
##doPars2 <- nimbleFunction(
##    name = 'doPars2',
##    contains = getParVirtual,
##    setup = function(parName, mvSaved) {},
##    methods = list(
##        scalarGet = function(){
##            paramVal <- mvSaved[parName, 1][1]
##            returnType(double(0))
##            return(paramVal)
##        }
##    )
##)


sampler_AFSS_to_RW_block <- nimbleFunction(
    name = 'sampler_AFSS_to_RW_block',
    contains = sampler_BASE,
    setup = function(model, mvSaved, target, control) {
        AFSS_sampler <- sampler_AF_slice(model, mvSaved, target = target, control = control$AF_sliceControl)
        RW_block_sampler <- sampler_RW_block(model, mvSaved, target = target, control = control$RWcontrol)
        numFactorAdaptations <- control$numFactorAdaptations
        useRWSampler <- 0
        
        ##nESSSamps <- control$AF_sliceControl$factorAdaptInterval
        ##RWAdaptInterval <- control$RWcontrol$adaptInterval
        ##timesRan <- 0
        ##nESSReps <- 200
        ##numParamVars <- length(target)
        ##l <- ceiling(min(1000, nESSSamps/20)) #length of each block, ensures it's not too big
        ##q <- nESSSamps - l + 1 #total number of blocks available to sample from
        ##h <- ceiling(nESSSamps/l) #number of blocks to use for q function calculation
        ##doVarList <- nimbleFunctionList(getParVirtual)
        ##for(i in 1:numParamVars){
        ##    doVarList[[i]] <- doPars2(target[i], mvSaved)
        ##}
        ##storeSamples <- matrix(0, nrow = numParamVars, ncol = nESSSamps)
        ##essMeanSD   <- rep(0, numParamVars)
        ##essSD <- rep(0, numParamVars)
        ##ESS <-  rep(0, numParamVars)
        ##timesAdapted <- 0
        ##meanMatrix <-  matrix(0, nrow = numParamVars, ncol = q)
    },
    run = function() {
        ##timesRan <<- timesRan + 1
        if(useRWSampler == 1) {
            RW_block_sampler$run()
        } else {
            AFSS_sampler$run()
            if(AFSS_sampler$factorTimesAdapted >= numFactorAdaptations) {
                RW_block_sampler$propCov <<- AFSS_sampler$empirCov
                useRWSampler <<- 1
                print('transitioning to RW_block sampler')
            }
        }
        ##if(timeSwitch == 1){
        ##    if(timesRan > nAFSSIters){
        ##        RW_block_sampler$run()
        ##    }
        ##    else{
        ##        AFSS_sampler$run()
        ##        if(timesRan == nAFSSIters){
        ##            copyAdaptiveParams()
        ##        }
        ##    }
        ##}
        ##else{
        ##    if(useRWSampler == 1){
        ##        RW_block_sampler$run()
        ##    }
        ##    else{
        ##        AFSS_sampler$run()
        ##        calcESSandAdapt()
        ##    }
        ##}
    },
    methods = list(
        ##copyAdaptiveParams = function() {
        ##    RW_block_sampler$propCov <<- AFSS_sampler$empirCov
        ##},
        ##calcESSandAdapt = function() {
        ##    for(par in 1:numParamVars){
        ##        storeSamples[par, timesRan] <<- doVarList[[par]]$scalarGet()
        ##    }
        ##    if(timesRan == nESSSamps){
        ##        essCalcs <- matrix(0, nrow = numParamVars, ncol = nESSReps)
        ##        timesAdapted <<- timesAdapted + 1
        ##        for(par in 1:numParamVars){
        ##            for(r in 1:nESSReps){
        ##                for(i in 1:h){
        ##                    randNum <- rbeta(1,1,1)
        ##                    randIndex <- ceiling(randNum*q) #random starting index for blocks (post burn-in)
        ##                    for(j in 1:l){
        ##                        essCalcs[par, r] <- essCalcs[par, r] + storeSamples[par, randIndex + j - 1]
        ##                    }
        ##                }
        ##                essCalcs[par, r]  <-  essCalcs[par, r]/(h*l)
        ##            }
        ##            essMeanSD[par] <<- sd(essCalcs[par, ])
        ##            essSD[par] <<- sd(storeSamples[par,])
        ##            ESS[par] <<- (essSD[par]/essMeanSD[par])^2
        ##        }
        ##        minESS <- min(ESS)
        ##        if(timesAdapted > numESSAdaptations){
        ##            copyAdaptiveParams()
        ##            useRWSampler <<- 1
        ##        }
        ##        timesRan <<- 0
        ##    }
        ##},
        reset = function() {
            RW_block_sampler$reset()
            AFSS_sampler$reset()
            useRWSampler <<- 0
            ##timesRan <<- 0
            ##timesAdapted <<- 0
        }
    ))



