
# Season Injury Stats.R compiles injury data by season relative to the number of players/pitchers
# rostered in the MLB (that season).
# 
# It then plots this data for two DVs (ucl_status and Days injury List)

# ==============================================================================
# load data
# ==============================================================================

library(readxl)
library(psych)
library(tidyverse)
library(writexl)

d <- read_excel("Data/Oringinal Data/CDavis_pitchclock.xlsx", 
                sheet = "master")

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
# define seasons from 2012-13 to 2023-24
# ==============================================================================

season_dates <- tibble(season        = c("2012","2013","2014",
                                         "2015","2016","2017",
                                         "2018","2019","2020",
                                         "2021","2022","2023",
                                         "2024"),
                       season_start  = c("2012-03-28","2013-03-31","2014-03-22",
                                         "2015-04-05","2016-04-03","2017-04-02",
                                         "2018-03-29","2019-03-20","2020-07-23",
                                         "2021-04-01","2022-04-07","2023-03-30",
                                         "2024-03-20"),
                       season_end    = c("2012-10-28","2013-10-30","2014-10-29",
                                         "2015-11-01","2016-11-02","2017-11-01",
                                         "2018-10-28","2019-10-30","2020-10-27",
                                         "2021-11-02","2022-11-05","2023-11-01",
                                         "2024-10-30")
                       ) %>%
   mutate(season_start = as.Date(season_start),
          season_end   = as.Date(season_end))

#geographical restructure
aaa_restructure_date <- as.Date("2021-04-06")
aa_restructure_date  <- as.Date("2021-05-04")

# ==============================================================================
# assign each injury date (addDate) to a season year
# ==============================================================================

d <- d %>%
   # rename the level code column
   rename(level_code = `level-5=AAA_1=A-`) %>%
   # are they a switch hitter (0 = no, 1 = yes)
   mutate(switch_hitter = case_when(bats == "R" & throws == "R" ~ 0,
                                    bats == "L" & throws == "L" ~ 0,
                                    TRUE ~ 1)) %>%
   relocate(switch_hitter, .after = throws) %>%
   mutate(injury_year = format(addDate, "%Y"),
          switch_hitter = as.factor(switch_hitter),
          borndate    = as.Date(borndate, format = "%m/%d/%Y"),
          ucl_status  = as.factor(ucl_status),
          `40man`     = as.integer(factor(`40man`, levels = c("N", "Y"))),
          level       = factor(level, levels = c("A-","A","A+","AA","AAA")),
          Pitchclock  = as.factor(Pitchclock)) %>%
   left_join(season_dates, 
             by = join_by(injury_year == season)) %>%
   mutate(in_season              = as.integer(addDate >= season_start & addDate <= season_end)) %>%
   relocate(injury_year,  .after = addDate) %>%
   relocate(season_start, .after = injury_year) %>%
   relocate(season_end,   .after = season_start) %>%
   relocate(in_season,    .after = season_end) %>%
   mutate(post_restructure       = case_when(level == "AAA" & addDate < aaa_restructure_date | 
                                             level == "AA" & addDate < aa_restructure_date ~ 0,
                                             TRUE ~ 1)) %>%
   mutate(in_season        = as.factor(in_season),
          post_restructure = as.factor(post_restructure)) %>%
   relocate(post_restructure, .after = in_season)

# ==============================================================================
# filter to leave only pitchers
# ==============================================================================

d_pitchers <- d %>%
   filter(posit == "P" | posit == "p") %>%
   mutate(posit = "P")

# number of injuries outside of season window
# -------------------------------------------

injury_summary <- d_pitchers %>%
   summarise(`in season (n)`     = sum(in_season == 1),
             `not in season (n)` = sum(in_season == 0),
             `% in season`       = round(`in season (n)`/sum(`in season (n)`,`not in season (n)`) *100, 1))

injury_summary_season <- d_pitchers %>%
   group_by(injury_year) %>%
   summarise(`in season (n)`     = sum(in_season == 1),
             `not in season (n)` = sum(in_season == 0),
             `% in season`       = round(`in season (n)`/sum(`in season (n)`,`not in season (n)`) *100, 1))

write_xlsx(list(total        = injury_summary,
                total_season = injury_summary_season),
           path = "Data/Injury Count & Distribution (pitchers only).xlsx")

# ==============================================================================
# summary statistics
# ==============================================================================

# summarise statistics by competition level
summary_d <- describeBy(d %>% select(where(is.numeric)), 
                        group = d$level,
                        mat = TRUE) %>%
   tibble::rownames_to_column("variable")

# pitchers only
summary_d_pitchers <- describeBy(d_pitchers %>% select(where(is.numeric)), 
                                 group = d_pitchers$level,
                                 mat = TRUE)%>%
   tibble::rownames_to_column("variable")

# total time on IL for each pitcher
# ---------------------------------

d_pitchers_total <- d_pitchers %>%
   select(1:4, 22:23, 26, 29, 30) %>%
   group_by(mlbid, playerid, lastname, firstname, in_season, post_restructure) %>%
   summarise(n = n(),
             `Days injury List` = sum(`Days injury List`, na.rm = TRUE),
             .groups = "drop")

# time on IL for each pitcher, by Pitchclock
# ------------------------------------------

d_pitchers_PC <- d_pitchers %>%
   select(1:4, 22:23, 26:30) %>%
   group_by(mlbid, playerid, lastname, firstname, in_season, post_restructure, Pitchclock) %>%
   summarise(n = n(),
             `Days injury List` = sum(`Days injury List`, na.rm = TRUE),
             .groups = "drop")         

# time on IL for each pitcher, by level
# -------------------------------------

d_pitchers_level <- d_pitchers %>%
   select(1:4, 22:23, 26:30) %>%
   group_by(mlbid, playerid, lastname, firstname, in_season, post_restructure, level, level_code) %>%
   summarise(n = n(),
             `Days injury List` = sum(`Days injury List`, na.rm = TRUE),
             .groups = "drop")     

# time on IL for each pitcher, by PC & level
# ------------------------------------------

d_pitchers_PC_level <- d_pitchers %>%
   select(1:4, 22:23, 26:30) %>%
   group_by(mlbid, playerid, lastname, firstname, in_season, post_restructure, level, level_code, Pitchclock) %>%
   summarise(n = n(),
             `Days injury List` = sum(`Days injury List`, na.rm = TRUE),
             .groups = "drop")

# save filtered master and summary statistics
# -------------------------------------------

write_xlsx(list(Master            = d,
                `Pitchers Only`   = d_pitchers),
           path = "Data/Filtered Master Data (with season).xlsx")

write_xlsx(list(Master            = summary_d,
                `Pitchers Only`   = summary_d_pitchers),
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
   mutate(ucl_status      = as.numeric(as.character(ucl_status))) %>%
   pivot_longer(cols      = c(ucl_status, `Days injury List`),
                names_to  = "variable",
                values_to = "value")

# ==============================================================================
# plot the data
# ==============================================================================

# Days injury List x level x Pitchclock 
# -------------------------------------
d_DIL_PC <- d_long %>% 
   filter(variable == "Days injury List") %>%
   group_by(level, Pitchclock) %>%
   summarise(mean = mean(value, na.rm = TRUE),
             sd   = sd(value, na.rm = TRUE),
             n    = sum(!is.na(value)),
             .groups = "drop") %>%
   complete(level, Pitchclock,
            fill = list(n = 0))

# plot
DIL_1 <- ggplot(d_DIL_PC, aes(x = level, y = mean, colour = Pitchclock)) +
   geom_errorbar(aes(ymin = mean - sd,
                     ymax = mean + sd),
                 position = position_dodge(width = 0.7),
                 width = 0.15,
                 na.rm = TRUE) +
   geom_point(position = position_dodge(width = 0.7),
              size = 3,
              na.rm = TRUE) +
   geom_text(aes(label = ifelse(round(mean, 0) == 0, "", round(mean, 0))),
             position = position_dodge(width = 0.7),
             hjust = -0.5,
             vjust = 0.5,
             na.rm = TRUE,
             show.legend = FALSE) +
   scale_color_discrete(name   = "",
                        labels = c("No PC", "Original PC", "Modified PC"),
                        drop = FALSE) +
   scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
   theme_classic() +
   labs(title = "Avg. Days on Injury List by Pitchclock",
        x = NULL, y = NULL) +
   theme(legend.position = "bottom",
         plot.title = element_text(hjust = 0.5))

ggsave(DIL_1, file = "Tables and Figures/Avg. Days on Injury List by Pitchclock.png",
       height = 5, width = 7, dpi = 600)

# Days injury List x level x geographical restructure
# ---------------------------------------------------
d_DIL_geog <- d_long %>% 
   filter(variable == "Days injury List") %>%
   group_by(level, post_restructure) %>%
   summarise(mean = mean(value, na.rm = TRUE),
             sd   = sd(value, na.rm = TRUE),
             .groups = "drop") %>%
   complete(level, post_restructure)

# plot
DIL_2 <- ggplot(d_DIL_geog, aes(x = level, y = mean, colour = post_restructure)) +
   geom_errorbar(aes(ymin = mean - sd,
                     ymax = mean + sd),
                 position = position_dodge(width = 0.7),
                 width = 0.15,
                 na.rm = TRUE) +
   geom_point(position = position_dodge(width = 0.7),
              size = 3,
              na.rm = TRUE) +
   geom_text(aes(label = ifelse(round(mean, 0) == 0, "", round(mean, 0))),
             position = position_dodge(width = 0.7),
             hjust = -0.5,
             vjust = 0.5,
             na.rm = TRUE,
             show.legend = FALSE) +
   scale_color_discrete(name   = "",
                        labels = c("Pre", "Post"),
                        drop = FALSE) +
   scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
   theme_classic() +
   labs(title = "Avg. Days on Injury List \n Pre-/Post-Geographical Restructure",
        x = NULL, y = NULL) +
   theme(legend.position = "bottom",
         plot.title = element_text(hjust = 0.5))

ggsave(DIL_2, file = "Tables and Figures/Avg. Days on Injury List by Geographical Restructure.png",
       height = 5, width = 7, dpi = 600)

# UCL status x level
# ------------------
d_UCL_PC <- d_long %>% 
   filter(variable == "ucl_status") %>%
   group_by(level, Pitchclock) %>%
   summarise(count = sum(value, na.rm = TRUE),
             .groups = "drop") %>%
   complete(level, Pitchclock) %>%
   mutate(missing = is.na(count),
          count = replace_na(count, 0))

# plot
UCL_1 <- ggplot(d_UCL_PC, aes(x = level, y = count, fill = Pitchclock)) +
   geom_col(width = 0.7,
            position = position_dodge(width = 0.8)) +
   geom_text(aes(label = ifelse(missing, "", count)),
             position = position_dodge(width = 0.8),
             vjust = -0.5) +
   scale_fill_discrete(name    = "",
                       labels  = c("No PC", "Original PC", "Modified PC"),
                       drop = FALSE) +
   scale_color_discrete(name   = "",
                        labels = c("No PC", "Original PC", "Modified PC")) +
   scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
   theme_classic() +
   labs(title = "UCL Injury Count by Pitchclock",
        x = NULL, y = NULL) +
   theme(legend.position = "bottom",
         plot.title = element_text(hjust = 0.5))

ggsave(UCL_1, file = "Tables and Figures/UCL Injury Count by Pitchclock.png",
       height = 5, width = 7, dpi = 600)

# UCL status x geographical restructure
# -------------------------------------
d_UCL_geog <- d_long %>% 
   filter(variable == "ucl_status") %>%
   group_by(level, post_restructure) %>%
   summarise(count = sum(value, na.rm = TRUE),
             .groups = "drop") %>%
   complete(level, post_restructure) %>%
   mutate(missing = is.na(count),
          count = replace_na(count, 0))

# plot
UCL_2 <- ggplot(d_UCL_geog, aes(x = level, y = count, fill = post_restructure)) +
   geom_col(width = 0.5,
            position = position_dodge(width = 0.6)) +
   geom_text(aes(label = ifelse(missing, "", count)),
             position = position_dodge(width = 0.6),
             vjust = -0.5) +
   scale_fill_discrete(name    = "",
                       labels  = c("Pre", "Post")) +
   scale_color_discrete(name   = "",
                        labels = c("Pre", "Post")) +
   scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
   theme_classic() +
   labs(title = "UCL Injury Count \n Pre-/Post-Geographical Restructure",
        x = NULL, y = NULL) +
   theme(legend.position = "bottom",
         plot.title = element_text(hjust = 0.5))

ggsave(UCL_2, file = "Tables and Figures/UCL Injury Count by Geographical Restructure.png",
       height = 5, width = 7, dpi = 600)

# ==============================================================================

# # by competition level (violin & jitter)
# plot_level <- ggplot(d_long, aes(level, value)) +
#    geom_jitter(aes(color = level), width = 0.15, height = 0.1, alpha = 0.5) +
#    geom_violin(alpha = 0.1, trim = FALSE) +
#    theme_classic() +
#    theme(legend.position = "bottom") +
#    facet_wrap(~variable, scales = "free_y")
# 
# ggsave(plot_level, file = "Tables and Figures/DVs x Competition Level.png",
#        height = 3, width = 8, dpi = 600)
# 
# # by pitch clock constraint (violin & jitter)
# plot_PC <- ggplot(d_long, aes(level, value, fill = factor(Pitchclock))) +
#    geom_jitter(aes(color = factor(Pitchclock)),
#                position = position_jitterdodge(jitter.width = 0.15,
#                                                dodge.width = 0.8),
#                alpha = 0.5) +
#    geom_violin(alpha = 0.1, trim = FALSE,
#                position = position_dodge(width = 0.8)) +
#    scale_fill_discrete(name    = "Pitch Clock",
#                        labels  = c("No PC", "Original PC", "Modified PC")) +
#    scale_color_discrete(name   = "Pitch Clock",
#                         labels = c("No PC", "Original PC", "Modified PC")) +
#    theme_classic() +
#    theme(legend.position = "bottom") +
#    facet_wrap(~variable, scales = "free_y")
# 
# ggsave(plot_PC, file = "Tables and Figures/DVs x Competition Level x Pitch Clock.png",
#        height = 3, width = 8, dpi = 600)
# 
# # geographical restructure
# # ========================
# 
# # by pitch clock constraint (violin & jitter)
# plot_GEO <- ggplot(d_long, aes(level, value, fill = factor(post_restructure))) +
#    geom_jitter(aes(color = factor(post_restructure)),
#                position = position_jitterdodge(jitter.width = 0.15,
#                                                dodge.width = 0.8),
#                alpha = 0.5) +
#    geom_violin(alpha = 0.1, trim = FALSE,
#                position = position_dodge(width = 0.8)) +
#    scale_fill_discrete(name    = "Pre- v.s Post-Restructure",
#                        labels  = c("Pre", "Post")) +
#    scale_color_discrete(name   = "Pre- v.s Post-Restructure",
#                         labels = c("Pre", "Post")) +
#    theme_classic() +
#    theme(legend.position = "bottom") +
#    facet_wrap(~variable, scales = "free_y")
# 
# ggsave(plot_GEO, file = "Tables and Figures/DVs x Competition Level x Geo Restructure.png",
#        height = 3, width = 8, dpi = 600)