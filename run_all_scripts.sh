
#!/bin/bash

R CMD BATCH --vanilla run_pump.R &
R CMD BATCH --vanilla run_SSMind.R &
R CMD BATCH --vanilla run_SSMcor.R &
R CMD BATCH --vanilla run_litters.R &
R CMD BATCH --vanilla run_ice.R &
R CMD BATCH --vanilla run_mhp.R &






