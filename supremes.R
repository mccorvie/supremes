library(tidyverse)
library( lubridate)

# roe v wade decided Jan 22, 1973

presidents <- tribble(
  ~name, ~party, ~start, ~end,  ~nominations, ~confirmed,
  #  "Nixon",  "R", "19690120",   "19740809",  6,4, 
  "Nixon post-Roe",  "R", "19730122",   "19740809",  0,0, 
  "Ford",            "R", "19740809",   "19770120",  1,1, # actual ford start date
  "Carter",          "D", "19770120",   "19810120",  0,0,
  "Reagan",          "R", "19810120",   "19890120",  5,4,
  "Bush1",           "R", "19890120",   "19930120",  2,2,
  "Clinton",         "D", "19930120",   "20010120",  2,2,
  "Bush2",           "R", "20010120",   "20090120",  4,2,
  "Obama",           "D", "20090120",   "20170120",  3,2,
  "Trump",           "R", "20170120",   "20210120",  3,3,
  "Biden",           "D", "20210120",   NA,          1,1
)%>%
  mutate(
    start = ymd( start),
    end = coalesce( ymd( end ), today()),
    term = as.numeric(difftime( end,start, units="days"))
  )

# what is the average number of confirmations per day?
conf_lambda <- sum(presidents$confirmed)/sum(presidents$term)
conf_lambda

party_summary <- presidents %>%
  group_by( party ) %>%
  summarize( term = sum( term ), confirmed= sum( confirmed ))

D_term <- party_summary %>% filter( party=="D") %>%pull( term)
R_term <- party_summary %>% filter( party=="R") %>%pull( term)

D_confirmed <- party_summary %>% filter( party=="D") %>%pull( confirmed)
R_confirmed <- party_summary %>% filter( party=="R") %>%pull( confirmed)

actual_skew <- R_confirmed/(R_confirmed+D_confirmed)
term_skew <- R_term / (R_term+D_term)

actual_skew
term_skew


probability_table <- expand_grid( D_confirmed = 0:25, R_confirmed = 0:25 ) %>%
  filter( D_confirmed >0 | R_confirmed>0) %>%
  mutate( 
    confirmed = D_confirmed + R_confirmed,
    R_probability = dpois( R_confirmed, conf_lambda * R_term ),
    D_probability = dpois( D_confirmed, conf_lambda * D_term ),
    probability = R_probability * D_probability,
    confirmation_skew = R_confirmed/(R_confirmed+D_confirmed)
  ) 

prob_more_skewed <- probability_table %>%
  filter( confirmation_skew >= actual_skew ) %>%
  summarize( probability = sum( probability )) %>%
  pull( probability)

prob_more_skewed

skew_distribution <- probability_table %>%
  arrange( confirmation_skew ) %>%
  mutate( cum_prob = cumsum( probability )) %>%
  group_by( confirmation_skew) %>%
  summarize( cum_prob = max( cum_prob )) %>%
  ungroup



mean <- pull( moments, m1)
sd   <- sqrt( pull( moments, m2)- mean*mean   )
moments <- probability_table %>%
  summarize( 
    m1 = sum( probability * confirmation_skew ), 
    m2 = sum( probability * confirmation_skew * confirmation_skew  )
  )

median <- skew_distribution %>% 
  filter( cum_prob <= 0.5) %>%
  summarize( median = max( confirmation_skew)) %>%
  pull( median)

median
sd
mean

xx <- seq( 0,1,0.01)
nn <- tibble(  confirmation_skew = xx, cum_prob = pnorm( xx, mean, sd))

prob_more_skewed
1-pnorm( actual_skew, mean, sd )

ggplot( skew_distribution, aes(x=confirmation_skew, y=1-cum_prob)) + 
  #geom_hline( yintercept = actual_skew, lty="dotted") +
  geom_vline( xintercept = actual_skew, lty="dashed") +
  geom_line( col="purple") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  ggtitle( "Republican Supreme Court Confirmation Skew" ) +
  xlab( "Skew (Rep. % of Confirmations)") + 
  ylab( "Probability of Greater Skew") + 
#  geom_line( data=nn, color = "gold3" )+
  theme_minimal()

  
ggplot( filter( probability_table, D_confirmed<=20, R_confirmed<=20),aes( x=D_confirmed, y=R_confirmed)) +
  geom_tile(aes(fill = probability)) +
  scale_fill_distiller(direction=1) + 
  geom_abline(slope = 1, intercept=0, lty="dashed")+
  geom_point( x=D_confirmed, y=R_confirmed)+
  geom_text( x=D_confirmed, y=R_confirmed+1.5, label="Actual")+
  xlab( "Dem. confirmations") +
  ylab( "Rep. confirmations" ) +
  ggtitle( "Number of Supreme Court Confirmations", subtitle= "Probability Density ") +
  theme_minimal()
  



