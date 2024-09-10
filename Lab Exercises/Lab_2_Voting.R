library(tidyverse)
library(kableExtra)
options(warn = -1)

# Exercise 1: Let's look at voting#
# Let's ask a simple question: do people vote?

ess %>% filter(!is.na(vote)) %>% count(vote) %>%
  mutate("%" = round(n/sum(n) * 100, digits=1)) %>%
  kbl(caption = "Electoral Turnout in European Democracies",
  col.names = c('voted', 'N', '%'), align="lccc") %>%
  kable_classic_2(full_width=F, position= "left")%>%
  footnote(general = "Source: ESS 2002-2018")

# How does this vary by country?#

ess %>% group_by(country) %>% filter(!is.na(vote)) %>%
  count(vote) %>% mutate("%" = round(n/sum(n) * 100, digits=1)) %>%
  kbl(caption = "Electoral Turnout in European Democracies",
  col.names = c('Country', 'Voted', 'N', '%'), align="cccc")%>% 
  kable_classic_2(full_width=F, position= "left") %>% 
  footnote("European Social Survey, 2002-2018")

# We can present this graphically to make it clearer#
ess %>% group_by(country) %>% filter(!is.na(vote)) %>%
  count(vote) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!vote=="did not vote") %>%
  ggplot(aes(x=reorder(country, -prop), y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 1: Turnout by Country", caption="Source: ESS 2002-2018")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()
 

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
  kbl(caption = "Education and Electoral Turnout",
  col.names = c('Education', 'Voted', 'N', '%'), align="ccccc", digits=1)%>% 
  kable_classic_2(full_width=F, position= "left") %>% 
  footnote("European Social Survey, 2002-2018")

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
  kbl(caption = "Perceptions of the Economy and Electoral Turnout",
  col.names=c('Economic Perceptions', 'Voted', 'N', '%'), align="ccccc", digits=1)%>% 
  kable_classic_2(full_width=F, position= "left") %>% 
  footnote("European Social Survey, 2002-2018")

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
  labs(x="", y="", title="Figure 5: Turnout by Economic Satisfaction and Country", caption="ESS 2002-2018")+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values=c("#FF0000", "#0000FF"))+
  facet_wrap(~country, nrow=3)+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom")+
  scale_x_discrete(labels=c("low" = "low", "medium" = "medium", "high" = "high"))


#Q3 Does history matter? Does the legacy of post-communism influence participation?

ess %>% 
  filter(!is.na(vote)) %>% 
  mutate(post_communist = if_else(country == "Poland", "post-communist", "not post-communist")) %>% 
  group_by(post_communist) %>% 
  count(vote) %>% 
  mutate(percent = round(n /sum(n)*100, digits = 1)) %>% filter(vote == "voted")%>%
  kbl(caption = "The Legacy of Communism and Electoral Turnout",
      col.names=c('Country', 'Voted', 'N', '%'), align="ccccc", digits=1)%>% 
  kable_classic_2(full_width=F, position= "left") %>% 
  footnote("European Social Survey, 2002-2018")

# Q4 Representative vs. Direct Democracy

ess %>% 
  filter(!is.na(vote)) %>% 
  mutate(direct_democracy = if_else(country == "Switzerland", "Direct Democracy", "Representative Democracy")) %>% 
  group_by(direct_democracy) %>% 
  count(vote) %>% 
  mutate(percent = round(n /sum(n)*100, digits = 1)) %>% filter(vote == "voted")%>%
  kbl(caption = "Representative vs. Direct Democracy and Electoral Turnout",
      col.names=c('Type of Democracy', 'Voted', 'N', '%'), align="ccccc", digits=1)%>% 
  kable_classic_2(full_width=F, position= "left") %>% 
  footnote("European Social Survey, 2002-2018")

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
kbl(caption = "Percentage of Age within Sample and Percentage of Age Cohort amongst Voters",
             col.names = c('Generation', 
                           'Percentage Within Sample', 
                           'Percentage Within Voters'), 
             align="ccc") %>% 
  kable_classic_2(full_width=F, position= "left") %>% 
  footnote("European Social Survey, 2002-2018")

rm(a,b)

# Three questions to conclude with:

      #So, thinking over the theories we've look at: which ones work?

      #Do all work equally well in all countries?

      #Does it matter that young people seem less likely to vote?
