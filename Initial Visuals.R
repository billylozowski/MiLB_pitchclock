
# Initial Visuals.R maps the pitch clock data to explore patterns and other potential research questions.
 
# ==============================================================================
# packages and data
# ==============================================================================

library(tidyverse)
library(readxl)
library(psych)
library(writexl)

d <- read_excel("Data/CDavis_pitchclock.xlsx", sheet = "master")

# ==============================================================================
# filter data (i.e., select columns)
# ============================================================================== 

d <- d %>%
   select(1:6, 10, 12:22, 24:29)

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
   # rename the level code column
   rename(level_code = `level-5=AAA_1=A-`) %>%
   # are they a switch hitter (0 = no, 1 = yes)
   mutate(switch_hitter = case_when(bats == "R" & throws == "R" ~ 0,
                                    bats == "L" & throws == "L" ~ 0,
                                    TRUE ~ 1)) %>%
   relocate(switch_hitter, .after = throws)

#geographical restructure
aaa_restructure_date <- as.Date("2021-04-06")
aa_restructure_date  <- as.Date("2021-05-04")

# did injury occur before (0) or after (1) geographical restructuring?
d <- d %>%
   mutate(post_restructure = case_when(level == "AAA" & addDate < aaa_restructure_date | level == "AA" & addDate < aa_restructure_date ~ 0,
                                        TRUE ~ 1)) %>%
   relocate(post_restructure, .after = addDate)

# summarise statistics by competition level
# =========================================
summary_d <- describeBy(d %>% select(where(is.numeric)), 
                        group = d$level,
                        mat = TRUE) %>%
   tibble::rownames_to_column("variable")

# only pitchers
# =============

d_pitchers <- d %>% filter(posit == "P" | posit == "p") %>%
   mutate(posit = "P") # ensure all values are uppercase "P"

# summarise statistics by competition level
summary_d_pitchers <- describeBy(d_pitchers %>% select(where(is.numeric)), 
                        group = d_pitchers$level,
                        mat = TRUE)%>%
   tibble::rownames_to_column("variable")

# total time on IL for each pitcher
# ---------------------------------

d_pitchers_total <- d_pitchers %>%
   select(1:4, 19, 22, 25, 26) %>%
   group_by(mlbid, playerid, lastname, firstname, post_restructure) %>%
   summarise(n = n(),
             `Days injury List` = sum(`Days injury List`, na.rm = TRUE),
             .groups = "drop")

# time on IL for each pitcher, by Pitchclock
# ------------------------------------------

d_pitchers_PC <- d_pitchers %>%
   select(1:4, 19, 22:26) %>%
   group_by(mlbid, playerid, lastname, firstname, post_restructure, Pitchclock) %>%
   summarise(n = n(),
             `Days injury List` = sum(`Days injury List`, na.rm = TRUE),
             .groups = "drop")         

# time on IL for each pitcher, by level
# -------------------------------------

d_pitchers_level <- d_pitchers %>%
   select(1:4, 19, 22:26) %>%
   group_by(mlbid, playerid, lastname, firstname, post_restructure, level, level_code) %>%
   summarise(n = n(),
             `Days injury List` = sum(`Days injury List`, na.rm = TRUE),
             .groups = "drop")     

# time on IL for each pitcher, by PC & level
# ------------------------------------------

d_pitchers_PC_level <- d_pitchers %>%
   select(1:4, 19, 22:26) %>%
   group_by(mlbid, playerid, lastname, firstname, post_restructure, level, level_code, Pitchclock) %>%
   summarise(n = n(),
             `Days injury List` = sum(`Days injury List`, na.rm = TRUE),
             .groups = "drop")  


# save filtered master and summary statistics
# ===========================================

write_xlsx(list(Master          = d,
                `Pitchers Only` = d_pitchers),
           path = "Data/Filtered Master Data.xlsx")

write_xlsx(list(Master          = summary_d,
                `Pitchers Only` = summary_d_pitchers),
           path = "Data/Summary Statistics by Competition Level.xlsx")

write_xlsx(list(`Total Time`      = d_pitchers_total,
                `By Pitch Clock`  = d_pitchers_PC,
                `By Level`        = d_pitchers_level,
                `By PC and Level` = d_pitchers_PC_level),
           path = "Data/Time on IL.xlsx")

# ==============================================================================
# reshape the data to long format
# ==============================================================================

d_long <- d_pitchers %>%
   pivot_longer(cols      = -c("lastname","firstname","playerid","mlbid",
                               "teamname","posit","bats","throws","switch_hitter",
                               "borndate","place","status","notes","addDate",
                               "Pitchclock","40man","minorteam","level", 
                               "post_restructure"),
                names_to  = "variable",
                values_to = "value")

# ==============================================================================
# simple plots
# ==============================================================================

d_long <- d_long %>% filter(variable != "level_code")

# by competition level (violin & jitter)
plot_level <- ggplot(d_long, aes(level, value)) +
   geom_jitter(aes(color = level), width = 0.15, alpha = 0.7) +
   geom_violin(alpha = 0.1, trim = FALSE) +
   theme_classic() +
   theme(legend.position = "bottom") +
   facet_wrap(~variable, scales = "free_y")

ggsave(plot_level, file = "Tables and Figures/Variables x Competition Level.png",
       height = 7, width = 12, dpi = 600)

# by pitch clock constraint (violin & jitter)
plot_PC <- ggplot(d_long, aes(level, value, fill = factor(Pitchclock))) +
   geom_jitter(aes(color = factor(Pitchclock)),
               position = position_jitterdodge(jitter.width = 0.15,
                                               dodge.width = 0.8),
               alpha = 0.5) +
   geom_violin(alpha = 0.1, trim = FALSE,
               position = position_dodge(width = 0.8)) +
   scale_fill_discrete(name    = "Pitch Clock",
                       labels  = c("No PC", "Original PC", "Modified PC")) +
   scale_color_discrete(name   = "Pitch Clock",
                        labels = c("No PC", "Original PC", "Modified PC")) +
   theme_classic() +
   theme(legend.position = "bottom") +
   facet_wrap(~variable, scales = "free_y")

ggsave(plot_PC, file = "Tables and Figures/Variables x Competition Level x Pitch Clock.png",
       height = 7, width = 12, dpi = 600)


# by pitch clock constraint (violin & jitter) - Days injury List & ucl_status only

d_VOI <- d_long %>% filter(variable == "ucl_status" | variable == "Days injury List")

plot_VOI <- ggplot(d_VOI, aes(level, value, fill = factor(Pitchclock))) +
   geom_jitter(aes(color = factor(Pitchclock)),
               position = position_jitterdodge(jitter.width = 0.15,
                                               dodge.width = 0.8),
               alpha = 0.5) +
   geom_violin(alpha = 0.1, trim = FALSE,
               position = position_dodge(width = 0.8)) +
   scale_fill_discrete(name    = "Pitch Clock",
                       labels  = c("No PC", "Original PC", "Modified PC")) +
   scale_color_discrete(name   = "Pitch Clock",
                        labels = c("No PC", "Original PC", "Modified PC")) +
   theme_classic() +
   theme(legend.position = "bottom") +
   facet_wrap(~variable, scales = "free_y")

ggsave(plot_VOI, file = "Tables and Figures/Injury Variables x Competition Level x Pitch Clock.png",
       height = 3, width = 8, dpi = 600)

# geographical restructure
# ========================

# by pitch clock constraint (violin & jitter)
plot_GEO <- ggplot(d_long, aes(level, value, fill = factor(post_restructure))) +
   geom_jitter(aes(color = factor(post_restructure)),
               position = position_jitterdodge(jitter.width = 0.15,
                                               dodge.width = 0.8),
               alpha = 0.5) +
   geom_violin(alpha = 0.1, trim = FALSE,
               position = position_dodge(width = 0.8)) +
   scale_fill_discrete(name    = "Pre- v.s Post-Restructure",
                       labels  = c("Pre", "Post")) +
   scale_color_discrete(name   = "Pre- v.s Post-Restructure",
                        labels = c("Pre", "Post")) +
   theme_classic() +
   theme(legend.position = "bottom") +
   facet_wrap(~variable, scales = "free_y")

ggsave(plot_GEO, file = "Tables and Figures/Variables x Competition Level x Geo Restructure.png",
       height = 7, width = 12, dpi = 600)


# by pitch clock constraint (violin & jitter) - Days injury List & ucl_status only

plot_VOI <- ggplot(d_VOI, aes(level, value, fill = factor(post_restructure))) +
   geom_jitter(aes(color = factor(post_restructure)),
               position = position_jitterdodge(jitter.width = 0.15,
                                               dodge.width = 0.8),
               alpha = 0.5) +
   geom_violin(alpha = 0.1, trim = FALSE,
               position = position_dodge(width = 0.8)) +
   scale_fill_discrete(name    = "Pre- v.s Post-Restructure",
                       labels  = c("Pre", "Post")) +
   scale_color_discrete(name   = "Pre- v.s Post-Restructure",
                        labels = c("Pre", "Post")) +
   theme_classic() +
   theme(legend.position = "bottom") +
   facet_wrap(~variable, scales = "free_y")

ggsave(plot_VOI, file = "Tables and Figures/Injury Variables x Competition Level x Geo Restructure.png",
       height = 3, width = 8, dpi = 600)

