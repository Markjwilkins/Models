##following: http://www.statsandsnakeoil.com/2019/01/01/predicting-the-premier-league-with-dixon-coles/


library(regista)
library(footballdatr)
library(gt)


##import league one 2020 results
matches<-footballdatr::fetch_data("england", division = 2, season = 2020) %>%
  factor_teams(c("home", "away"))

##set teams as factors
teams <- factor(levels(matches$home), levels = levels(matches$home))

##find unplayed matches
unplayed_games <-
  crossing(home = teams,
           away = teams) %>%
  filter(home != away) %>%
  anti_join(matches, by = c("home", "away"))

##fit dixon-coles to matches played
model <- dixoncoles(hgoal, agoal, home, away, data = matches)

##extract attack/defense team estimates
team_parameters <-
  tidy.dixoncoles(model) %>%
  filter(parameter %in% c("off", "def")) %>%
  mutate(value = exp(value)) %>%
  spread(parameter, value)

##plot team estimates
team_parameters %>%
  ggplot(aes(x = def, y = off, label = team)) +
  geom_point(alpha = 0.5, size = 4) +
  ggrepel::geom_text_repel(size = 4) +
  geom_hline(yintercept = mean(team_parameters$off), colour = "red", linetype = "dotted", alpha = 0.7)+
  geom_vline(xintercept = mean(team_parameters$def), colour = "red", linetype = "dotted", alpha = 0.7)+
  labs(title = "League One 2020/21 // Team Strength Estimate",
       subtitle = "04/02/2021",
       y = "Attacking Strength",
       x = "Defensive Strength",
       caption = "@AnalyticsOxford")+
  theme_minimal() +
    theme(plot.title = element_text(size = 20, face = "bold"),
          axis.title = element_text(size = 12, face = "bold"),
          axis.text = element_text(size = 10),
          plot.caption = element_text(size = 12, face = "bold"))


##match prob for remaining fixtures
match_probabilities <-
  regista::augment.dixoncoles(model, unplayed_games, type.predict = "outcomes") %>%
  unnest() %>%
  spread(outcome, prob) %>% 
  mutate(match = paste(home, away))

##weekend home teams
home<-c("Gillingham", "Milton Keynes Dons", "Doncaster", "Portsmouth", 
        "Accrington", "Wigan", "Swindon", "Ipswich", "Rochdale",
        "Burton", "Peterboro", "Fleetwood Town")

##weekend away teams
away<-c("Lincoln", "Sunderland", "Oxford", "Plymouth", "Northampton",
        "AFC Wimbledon", "Shrewsbury", "Blackpool", "Charlton",
        "Hull", "Crewe", "Bristol Rvs")

##combine weekend matches
weekend_matches<-bind_cols(home, away)
colnames(weekend_matches)<-c("home", "away")

##pull matches
weekend_matches<-weekend_matches %>% 
  mutate(match = paste(home, away)) %>% 
  pull(match)

##filter
weekend<-match_probabilities %>% 
  filter(match%in%weekend_matches) %>% 
  select(-match) %>% 
  select(home, away, home_win, draw, away_win) %>% 
  mutate(across(where(is.numeric), scales::percent, na.rm = TRUE))

##weekend matches table plot
weekend %>% 
  gt() %>% 
  cols_label(home = md("**Home Team**"),
             away = md("**Away Team**"),
             home_win = md("**Home Win %**"),
             draw = md("**Draw %**"),
             away_win = md("**Away Win %**")) %>% 
tab_header(title = md("**Match Probabilities**"),
           subtitle = "League One - Weekend 5/6 February 2021") %>% 
  tab_source_note(
    source_note = "@AnalyticsOxford"
  ) %>% 
cols_align(align = "center")

##select individual match
ox_don<-match_probabilities %>% 
  filter(home=="Doncaster",
         away=="Oxford")

data_1<-data.frame(outcome = c("Doncaster", "Draw", "Oxford"),
                 prob = c(ox_don$home_win, ox_don$draw, ox_don$away_win)) %>% 
  mutate(prob = round(prob, digits = 3),
         prob_pc = prob*100)

##plot match prob
ggplot(data = data_1, aes(x=reorder(outcome, prob_pc), y = prob_pc))+
  geom_col(fill = "midnightblue") +
  coord_flip() +
  geom_text(aes(label = prob_pc), size = 5, colour = "white", hjust = 1.5, fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  labs(title = paste(ox_don$home[1], "vs", ox_don$away[1]),
       subtitle = "Match Probability",
       x = "",
       y = "Probability",
       caption = "@AnalyticsOxford")+
  theme_minimal()+
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        plot.caption = element_text(size = 12, face = "bold"))

##forecast scores on unplayed matches
unplayed_scorelines <-
  regista::augment.dixoncoles(model, unplayed_games, type.predict = "scorelines") %>%
  unnest()

##filter unplayed match
ox_don_1<-unplayed_scorelines %>% 
  filter(home=="Oxford")

##plot score matrix for remaining home games
ox_don_1 %>% 
  filter(hgoal<=6,
         agoal<=6) %>% 
  ggplot(aes(hgoal, agoal))+
  geom_tile(aes(fill = prob)) +
  scale_fill_gradient(low = "white", high = "midnightblue")+
  geom_text(aes(label = round(prob, digits = 2)), size = 4)+
  labs(title = "Estimated Goals - Oxford at Home",
       subtitle = "2020/21",
       y = "Away Goals",
       x = "Home Goals")+
  theme_minimal()+
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        plot.caption = element_text(size = 12, face = "bold"),
        legend.position = "none",
        strip.text = element_text(size = 12, face = "bold"))+
  facet_wrap(~away)

##scores of completed matches
played_scorelines <-
  matches %>%
  select(home, away, hgoal, agoal) %>%
  mutate(prob = 1.0)

##predicted scoreline for unplayed matches
scorelines <- bind_rows(
  played_scorelines,
  unplayed_scorelines
)

##sim season function
simulate_season <- function(scoreline_probabilities) {
  scoreline_probabilities %>%
    nest(hgoal, agoal, prob, .key = "scorelines") %>%
    mutate(sampled = map(scorelines, ~ sample_n(., 1, weight = prob))) %>%
    select(-scorelines) %>%
    unnest(cols = c(sampled))
}

##select simulations
n_sims<-1000

##simulate scorelines 
simulated_tables <-
  rerun(n_sims, simulate_season(scorelines))

##select unplayed match - count result output
unplayed<-simulated_tables %>% 
  bind_rows() %>% 
  filter(home=="Doncaster",
         away=="Oxford") %>% 
  mutate(score = paste(hgoal,agoal, sep = "-")) %>% 
  group_by(score) %>% 
  count()

##plot result output
unplayed %>% 
  ggplot(aes(x = reorder(score, n), y = n))+
  geom_col(fill = "midnightblue") +
  coord_flip() +
  geom_text(aes(label = n), hjust = 1.5, colour = "grey97")+
  labs(x = "Scoreline - Home/Away",
       y = "Count",
       title = "Oxford vs Doncaster - 1000 simulations",
       subtitle = "League One - 2020/21",
       caption = "@AnalyticsOxford") +
  theme_minimal()+
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        plot.caption = element_text(size = 12, face = "bold"),
        legend.position = "none",
        strip.text = element_text(size = 12, face = "bold"))

##calculate league table
calculate_table <- function(games) {
  games_augmented <-
    games %>%
    mutate(
      hpoints = case_when(
        hgoal > agoal  ~ 3,
        hgoal == agoal ~ 1,
        agoal > hgoal  ~ 0
      ),
      apoints = case_when(
        hgoal > agoal  ~ 0,
        hgoal == agoal ~ 1,
        agoal > hgoal  ~ 3
      )
    )
  
  games_home <-
    games_augmented %>%
    select(
      team   = home,
      gf     = hgoal,
      ga     = agoal,
      points = hpoints
    )
  
  games_away <-
    games_augmented %>%
    select(
      team   = away,
      gf     = agoal,
      ga     = hgoal,
      points = apoints
    )
  
  bind_rows(games_home, games_away) %>%
    group_by(team) %>%
    summarise(w  = sum(gf > ga),
              d  = sum(gf == ga),
              l  = sum(gf < ga),
              gf = sum(gf),
              ga = sum(ga),
              gd = gf - ga,
              points = sum(points)) %>%
    arrange(desc(points), desc(gd), desc(gf)) %>%
    mutate(position = row_number())
}

calculate_table(matches)

##simulate table
simulated_tables <-
  rerun(n_sims, simulate_season(scorelines)) %>%
  map(calculate_table) %>%
  bind_rows(.id = "simulation_id")

##number of teams in the league
n_teams <- length(teams)

##plot simulated league positions
simulated_tables %>%
  count(team, position) %>%
  ggplot(aes(x = position, y = n / n_sims)) +
  geom_col(fill = "midnightblue") +
  facet_wrap(~reorder(team, position)) +
  scale_x_continuous(breaks = seq(0,24, by = 2)) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 1)) +
  geom_vline(xintercept = 2.5, linetype = "dotted", colour = "red", alpha = 0.7)+
  geom_vline(xintercept = 6.5, linetype = "dotted", colour = "red", alpha = 0.7)+
  geom_vline(xintercept = 20.5, linetype = "dotted", colour = "red", alpha = 0.7)+
  annotate("text", x=1.1, y=0.99, label = "Auto", size = 3)+
  annotate("text", x=4.5, y=0.99, label = "Play Off", size = 3)+
  annotate("text", x=22.7, y=0.99, label = "Relegation", size = 3)+
  labs(title = "Position probabilities",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(title       = element_text(size = 30),
        strip.text  = element_text(size = 20),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10))



