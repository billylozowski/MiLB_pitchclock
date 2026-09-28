
# Initial Visuals.R maps the pitch clock data to explore patterns and other potential research questions.
 
# ==============================================================================
# packages and data
# ==============================================================================

library(tidyverse)
library(readxl)
library(psych)

d <- read_excel("Data/CDavis_pitchclock.xlsx", sheet = "master")

# ==============================================================================
# filter data (i.e., select columns)
# ============================================================================== 

d <- d %>%
   select(1:6, 10, 12:29)

# [1] "lastname"          "firstname"       
# [3] "playerid"          "mlbid"           
# [5] "teamname"          "posit"           
# [7] "ht"                "ht (in)"         
# [9] "ht (cm)"           "ht(m)"           
# [11] "wt"               "mass(kg)"        
# [13] "bmi"              "bats"            
# [15] "throws"           "borndate"        
# [17] "place"            "status"          
# [19] "notes"            "ucl_status"      
# [21] "addDate"          "40man"           
# [23] "endDate"          "minorteam"       
# [25] "level"            "level-5=AAA_1=A-"
# [27] "Pitchclock"       "Days injury List"
# [29] "age_at_injury"  

# ==============================================================================
# clean data (i.e., re-factor columns etc.)
# ==============================================================================

d <- d %>%
   mutate(level      = factor(level, levels = c("A-","A","A+","AA","AAA")),
          borndate   = as.Date(borndate, format = "%m/%d/%Y"),
          `40man`    = factor(`40man`, levels = c("N", "Y"))) %>%
   rename(level_code = `level-5=AAA_1=A-`)

summary_d <- describeBy(d %>% select(where(is.numeric)), 
                        group = d$level,
                        mat = TRUE)

write.csv(summary_d, file = "Data/Summary Statistics by Competition Level.csv")

# ==============================================================================
# reshape the data to long format
# ==============================================================================

d_long <- d %>%
   pivot_longer(cols      = -c("lastname", "firstname", "playerid", "mlbid",
                               "teamname", "posit", "bats", "throws", "borndate",
                               "place", "status", "notes", "addDate", "Pitchclock",
                               "40man", "endDate", "minorteam", "level"),
                names_to  = "variable",
                values_to = "value")

# ==============================================================================
# simple plots
# ==============================================================================

# by competition level (violin & jitter)
ggplot(d_long, aes(level, value)) +
   geom_jitter(aes(color = level), width = 0.15, alpha = 0.7) +
   geom_violin(alpha = 0.1, trim = FALSE) +
   theme_classic() +
   facet_wrap(~variable, scales = "free_y")


# by pitch clock constraint (violin & jitter)
ggplot(d_long, aes(level, value, fill = factor(Pitchclock))) +
   geom_jitter(aes(color = factor(Pitchclock)),
               position = position_jitterdodge(jitter.width = 0.15,
                                               dodge.width = 0.8),
               alpha = 0.5) +
   geom_violin(alpha = 0.1, trim = FALSE,
               position = position_dodge(width = 0.8)) +
   theme_classic() +
   facet_wrap(~variable, scales = "free_y")

