ess <- foreign::read.dta("data/ess.dta", convert.factors=TRUE)
library(tidyverse)
options(warn = -1))

# Exercise 1: Let's look at voting#
# Let's ask a simple question: do people vote?

ess %>% filter(!is.na(vote)) %>% count(vote) %>%
  mutate("%" = round(n/sum(n) * 100, digits=1)) %>%
  knitr::kable("pandoc", caption = "Electoral Turnout in European Democracies",
  col.names = c('voted', 'N', '%'), align="lccc")

# How does this vary by country?#

ess %>% group_by(country) %>% filter(!is.na(vote)) %>%
  count(vote) %>% mutate("%" = round(n/sum(n) * 100, digits=1)) %>%
  knitr::kable("pandoc", caption = "Electoral Turnout in European Democracies",
  col.names = c('Country', 'Voted', 'N', '%'), align="cccc")

# We can present this graphically to make it clearer#
ess %>% group_by(country) %>% filter(!is.na(vote)) %>%
  count(vote) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!vote=="did not vote") %>%
  ggplot(aes(x=reorder(country, -prop), y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 1: Turnout by Country", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

# What's changed over time? 

ess %>% group_by(year, country) %>% filter(!is.na(vote)) %>%
  count(vote) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!vote=="did not vote") %>%
  ggplot(aes(year, prop))+
  geom_line()+
  facet_wrap(~country, nrow = 3)+
  labs(x="", y="", title="Figure 2: Turnout by Country Over Time", caption="ESS 2002-2018")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  scale_x_continuous(breaks = c(2002, 2006, 2010, 2014, 2018))


# Exercise 2: Let's test our theories#
# Q1: Do Socio-Economic Resources Matter#
# What about Education?#

ess %>% group_by(educat) %>% filter(!is.na(vote), !is.na(educat)) %>%
  count(vote) %>% mutate("%" =n/sum(n)*100) %>%
  knitr::kable("pandoc", caption = "Education and Electoral Turnout",
  col.names = c('Education', 'Voted', 'N', '%'), align="ccccc", digits=1)

# We can graph the relationship

ess %>% group_by(educat) %>% filter(!is.na(vote), !is.na(educat)) %>%
  count(vote) %>% mutate(prop=prop.table(n*100)) %>% mutate("%" =prop*100) %>%
  ggplot(aes(x=educat, y=prop, fill=vote))+
  labs(x="", y="", title="Figure 2: Turnout by Education", caption="ESS 2002-2018")+
  scale_fill_manual(values=c("#FF0000", "#0000FF"))+
  geom_bar(stat="identity", position="dodge")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom")


# Turnout by Education and Country #

ess %>% group_by(educat, country) %>% filter(!is.na(vote), !is.na(educat)) %>%
  count(vote) %>% mutate(prop=prop.table(n*100)) %>% mutate("%" =prop*100) %>%
  ggplot(aes(x=educat, y=prop, fill = vote))+
  labs(x="", y="", title="Figure 3: Turnout by Education and Country", caption="Source: ESS 2002-2018")+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values=c("#FF0000", "#0000FF"))+
  facet_wrap(~country, nrow=3)+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom")

# Q2: Does Rationality Influence Turnout?

ess %>% group_by(satecon) %>% filter(!is.na(vote), !is.na(satecon)) %>%
  count(vote) %>% mutate(prop= n /sum(n)*100) %>%
  knitr::kable("pandoc", caption = "Perceptions of the Economy and Electoral Turnout",
  col.names=c('Economic Perceptions', 'Voted', 'N', '%'), align="ccccc", digits=1)

# We can graph this relationship

ess %>% group_by(satecon) %>% filter(!is.na(vote), !is.na(satecon)) %>%
  count(vote) %>% mutate(prop=prop.table(n*100)) %>% mutate("%" =prop*100) %>%
  ggplot(aes(x=satecon, y=prop, fill = vote))+
  labs(x="", y="", title="Figure 4: Turnout by Perceptions of the Economy", caption="Source: ESS 2002-2018")+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values=c("#FF0000", "#0000FF"))+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom")+
  scale_x_discrete(labels=c("low" = "low", "medium" = "medium", "high" = "high"))


# Does it differ between the countries?

ess %>% group_by(satecon, country) %>% filter(!is.na(vote), !is.na(satecon)) %>%
  count(vote) %>% mutate(prop=prop.table(n*100)) %>% mutate("%" =prop*100) %>%
  ggplot(aes(x=satecon, y=prop, fill = vote))+
  labs(x="", y="", title="Figure 5: Turnout by Economic Satisfaction and Country", caption="ESS 2016")+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values=c("#FF0000", "#0000FF"))+
  facet_wrap(~country, nrow=3)+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom")+
  scale_x_discrete(labels=c("low" = "low", "medium" = "medium", "high" = "high"))


#Q3 Does culture Matter?

ess %>% group_by(ptrust) %>% filter(!is.na(vote), !is.na(ptrust)) %>%
  count(vote) %>% mutate(prop= n / sum(n)*100) %>%
  knitr::kable("pandoc", caption = "Trust in Politics and Electoral Turnout",
  col.names=c('Trust', 'Voted', 'N', '%'), align="ccccc", digits=1)

ess %>% group_by(ptrust) %>% filter(!is.na(vote), !is.na(ptrust)) %>%
  count(vote) %>% mutate(prop=prop.table(n*100)) %>%
  ggplot(aes(x=ptrust, y=prop, fill = vote))+
  labs(x="", y="", title="Figure 6: Turnout by Trust in Politics", caption="ESS 2002-2018")+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values=c("#FF0000", "#0000FF"))+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom")+
  scale_x_discrete(labels=c("low" = "low", "medium" = "medium", "high" = "high"))

ess %>% group_by(ptrust, country) %>% filter(!is.na(vote), !is.na(ptrust)) %>%
  count(vote) %>% mutate(prop=prop.table(n*100)) %>%
  ggplot(aes(x=ptrust, y=prop, fill = vote))+
  labs(x="", y="", title="Figure 7: Turnout by Trust in Politics", caption="ESS 2002-2018")+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values=c("#FF0000", "#0000FF"))+
  facet_wrap(~country, nrow=3)+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom")+
  scale_x_discrete(labels=c("low" = "low", "medium" = "medium", "high" = "high"))

# Bonus Question: Let's ask if young people vote. 
# We can do this by comparing the percentage of young people found in the sample with the percentage found amongst the voters?

a <- ess %>% 
filter(!is.na(age)) %>% 
mutate(generation = if_else(age >= 18 & age <= 25, "18-25", "26+")) %>% 
count(generation) %>% 
mutate(percent = round(n /sum(n)*100, digits = 1)) 

b <- ess %>% 
filter(!is.na(age)) %>% 
mutate(generation = if_else(age >= 18 & age <= 25, "18-25", "26+")) %>% 
filter(vote == "voted") %>% 
group_by(generation) %>% 
count(vote) %>% 
ungroup() %>% 
mutate(percent = round(n/sum(n)*100, digits = 1))

left_join(a, b, by = "generation") %>% 
select(-c(n.x, n.y, vote)) %>% 
knitr::kable("pandoc", 
             caption = "Percentage of Age within Sample and Percentage of Age Cohort amongst Voters",
             col.names = c('Generation', 
                           'Percentage Within Sample', 
                           'Percentage Within Voters'), 
             align="ccc") %>% 
print()

# Three questions to conclude with:

      #So, thinking over the theories we've look at: which ones work?

      #Do all work equally well in all countries?

      #Does it matter that young people seem less likely to vote?
