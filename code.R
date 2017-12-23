#------------------------------------------------------------------------------#
#                                 Libraries                                    #
#------------------------------------------------------------------------------#

# Automatically checks and installs missing packages
packages <- c("tidyverse", "tigris", "geofacet")
install.packages(packages)
library(tidyverse)
library(tigris)
library(geofacet)

#------------------------------------------------------------------------------#
#                            Plotting Functions                                #
#------------------------------------------------------------------------------#

#' Individial state plots of high school graduate rate
#' @param df DataFrame
state_plots <- function(df){
  plot <- list()
  states <- unique(df$state)

  for(i in 1:length(states)) {
    plot[[i]] <- df %>%
      filter(state == states[i]) %>%
      ggplot(aes(x = YEAR, y = edu, color = RACE)) +
      geom_point(aes(size = pop)) +
      geom_line() +
      theme_bw() +
      scale_x_continuous() +
      scale_y_continuous(breaks = seq(0, 1, by = .1)) +
      ylab("High School Graduation Rate") +
      xlab("Year") +
      labs(title = paste0(states[i], ": High School Graduation Rate"),
           subtitle = paste0("Age: 20 -- 30"),
           caption = paste0("Data from IPUMS"),
           color = "Race")
  }
  
  return(plot)
}

#' First differences plot
#' @param df DataFrame
diff_state_plots <- function(df){
  plot <- list()
  states <- unique(df$state)
  
  for(i in 1:length(states)) {
    plot[[i]] <- df %>%
      filter(state == states[i]) %>%
      ggplot(aes(x = YEAR, y = first_diff, color = RACE)) +
      geom_point(aes(size = pop)) +
      geom_line() +
      theme_bw() +
      scale_x_continuous(breaks = seq(1970, 2010, 10)) +
      scale_y_continuous(breaks = seq(-1, 1, by = .1)) +
      ylab("High School Graduation Rate") +
      xlab("Year") +
      labs(title = paste0(states[i],
                          ": First difference of High School Graduation Rate"),
           subtitle = paste0("Age: 20 -- 30"),
           caption = paste0("Data from IPUMS"),
           color = "Race")
  }
  
  return(plot)
}
  
#' National plot of high school graduation rate
#' @param df DataFrame
national_plot <- function(df){
  plot <- df %>%
    ggplot(aes(x = YEAR, y = edu, color = RACE)) +
    geom_point(aes(size = pop)) +
    geom_line() +
    facet_geo(~ state) +
    theme_bw() +
    scale_x_continuous() +
    scale_y_continuous(breaks = seq(0, 1, by = .1)) +
    ylab("High School Graduation Rate") +
    xlab("Year") +
    labs(title = paste0("National High School Graduation Rate"),
         subtitle = paste0("Age: 20 -- 30"),
         caption = paste0("Data from IPUMS"),
         color = "Race") 
  
  return(plot)
}

#' First Differences national plot
#' @param df DataFrame
diff_national_plot <- function(df){
  plot <- df %>%
    ggplot(aes(x = YEAR, y = first_diff, color = RACE)) +
    geom_point(aes(size = pop)) +
    geom_line() +
    facet_geo(~ state) +
    theme_bw() +
    scale_x_continuous() +
    scale_y_continuous(breaks = seq(-1, 1, by = .2)) +
    ylab("First Difference High School Graduation Rate") +
    xlab("Year") +
    labs(title = 
           paste0("First Difference National High School Graduation Rate"),
         subtitle = paste0("Age: 20 -- 30"),
         caption = paste0("Data from IPUMS"),
         color = "Race") 
  
  return(plot)
}


#' Saves plots in specified folders
#' @param dta DataFrame
#' @param state result of state_plots
#' @param diff_state result of diff_state_plots
#' @param national result of national_plot
#' @param diff_national result of diff_national_plot
#' @param repo Address of repo
save_plots <- function(dta, state, diff_state, national, diff_national, repo){
  
  dir.create(paste0(repo, "/state_plots"))
  dir.create(paste0(repo, "/diff_state_plots"))
  dir.create(paste0(repo, "/national_plot"))
  dir.create(paste0(repo, "/diff_national_plot"))
  
  state_names <- unique(dta$state)
  for(i in 1:length(state_names)){
    ggsave(plot = state[[i]],
           filename = paste0(state_names[i], ".png"),
           path = paste0(repo, "/state_plots/"),
           width = 11, height = 11, units = 'in')
    
    ggsave(plot = diff_state[[i]],
           filename = paste0("diff_", state_names[i], ".png"),
           path = paste0(repo, "/diff_state_plots/"),
           width = 11, height = 11, units = 'in')
  }
  
  ggsave(plot = national,
         filename = "national_plot.png",
         path = paste0(repo, "/national_plot/"),
         width = 22, height = 11, units = 'in')
  ggsave(plot = diff_national,
         filename = "diff_national_plot.png",
         path = paste0(repo, "/diff_national_plot/"),
         width = 22, height = 11, units = 'in')
}

#------------------------------------------------------------------------------#
#                                    Code                                      #
#------------------------------------------------------------------------------#

# Data import from IPUMS
repo <- getwd()
raw_data <- read_csv(paste0(repo, "/highschool_grad_rate.csv"))

# Data Cleaning
# I use tigris to get corresponding FIPSCODES-to-state matching
codes <- tigris::fips_codes %>%
  distinct(state, state, state_code, state_name) %>%
  mutate(state_code = as.integer(state_code))

# Dplyr allows to group by state, race, and year to make calculating the grad
# rate straightforward
dta <- raw_data %>%
  full_join(codes, by=c("STATEFIP" = "state_code")) %>%
  mutate(RACE = case_when(RACE == 1 ~ "White",
                          RACE == 2 ~ "AfAm"),
         index = 1:nrow(.)) %>%
  group_by(state, RACE, YEAR) %>%
  count(EDUC) %>%
  mutate(ratio = n/sum(n) * ifelse(EDUC >= 6, 1, 0)) %>%
  summarise(edu = sum(ratio), pop = sum(n)) %>%
  na.omit() %>%
  mutate(first_diff = edu - lag(edu))

state <- state_plots(dta)
diff_state <- diff_state_plots(dta)
national <- national_plot(dta)
diff_national <- diff_national_plot(dta)
save_plots(dta, state, diff_state,
           national, diff_national, repo)
