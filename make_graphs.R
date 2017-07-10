setwd('~/github/hybridBlockSamplers/results/')
require(ggplot2)
require(dplyr)
#####################################
## set model name here!!
modelName <- 'pump'
modelName <- 'SSMind'
modelName <- 'SSMcor'
modelName <- 'litters'
modelName <- 'mhp'
modelName <- 'ice'
#####################################

filename <- paste0(modelName, 'Results.RData')
load(filename)

##ls()

dim(allEffDF)
str(allEffDF)
##'data.frame':	420 obs. of  7 variables:
## $ sampler: chr  "RW_block" "RW_block" "RW_block" "RW_block" ...
## $ nfa    : num  NA NA NA NA NA NA NA NA NA NA ...
## $ nIter  : num  1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 ...
## $ runTime: num  0.025 0.025 0.024 0.024 0.024 ...
## $ repNum : int  1 1 2 2 3 3 4 4 5 5 ...
## $ param  : chr  "alpha" "beta" "alpha" "beta" ...
## $ eff    : num  1793 1185 2088 1371 2687 ...


dim(compareDF)
str(compareDF)
##'data.frame':	210 obs. of  5 variables:
## $ sampler      : chr  "RW_block" "RW_block" "RW_block" "RW_block" ...
## $ nfa          : num  NA NA NA NA NA NA NA NA NA NA ...
## $ nIter        : num  1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 ...
## $ runTime      : num  0.025 0.024 0.024 0.023 0.025 ...
## $ minEfficiency: num  1185 1371 1644 1582 1159 ...


compareDF %>%
    group_by(sampler, nfa, nIter) %>%
        summarize(mean = mean(minEfficiency), sd = sd(minEfficiency)) %>%
            mutate(id = paste0(sampler, nfa)) -> df


            
ggplot(df, aes(x=nIter, y=mean, colour=id)) +
    geom_line() +
        geom_point() + 
            geom_errorbar(aes(ymin=len-se, ymax=len+se), width=.1)




ggplot(compareDF ,
       aes(x = nIter, y = minEfficiency, color = sampler)) + geom_point() + coord_cartesian(ylim = c(0, 10000))

ggplot(compareDF %>% filter(nIter == 5000),
       aes(x = nIter, y = minEfficiency, color = sampler)) + geom_boxplot()


