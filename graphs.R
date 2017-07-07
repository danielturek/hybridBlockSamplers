


setwd('~/github/hybridBlockSamplers')
require(ggplot2)
require(dplyr)


#####################################
## set model name here!!
modelName <- 'pump'
#####################################

filename <- paste0('results/', modelName, 'Results.RData')
load(filename)


ggplot(compareDF, aes(x = nIter, y = minEfficiency, color = sampler)) + geom_point()

ggplot(compareDF %>% filter(nIter == 5000),
       aes(x = nIter, y = minEfficiency, color = sampler)) + geom_boxplot()


compareDF %>%
    filter(sampler == 'AFSS_to_RW_block') %>%
        group_by(nfa, nIter) %>%
            summarise(m = mean(minEfficiency), time = mean(runTime))

