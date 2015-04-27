source("header.R")

set_folders("input")

load_rdata()

bison %<>% select(-Yr)
weather %<>% rename(Year = bisonyear)

set_folders("clean")

save_rdata()

# comp_survey_int
# interval from may 15(calving) to comp surveys (CC,YC,BC)
#
# YC
# Yearling cow ratio
#
# CC
# calf-cow ratio
#
# BC
# bull cow ratio
#
# herdN_int
# interval from may 15(calving) to herd survey
#
# HerdN
# Herd estimate
#
# Mort_collisions
# Mortailites--traffic collisions
#
# Mort_hunterkill
# hunter kills
