ess <- foreign::read.dta("data/ess.dta", convert.factors=TRUE)
library(tidyverse)
options(warn = -1)

# Exercise 1: Let's look at voting#
#Let's ask a simple question: do people vote?

ess %>% filter(!is.na(vote)) %>% count(vote) %>%
  mutate("%" = round(n/sum(n) * 100, digits=1)) %>%
  knitr::kable("pandoc", caption = "Electoral Turnout in European Democracies",
  col.names = c('voted', 'N', '%'), align="lccc")

#How does this vary by country?#

ess %>% group_by(country) %>% filter(!is.na(vote)) %>%
  count(vote) %>% mutate("%" = round(n/sum(n) * 100, digits=1)) %>%
  knitr::kable("pandoc", caption = "Electoral Turnout in European Democracies",
  col.names = c('Country', 'Voted', 'N', '%'), align="cccc")

#We can present this graphically to make it clearer#
ess %>% group_by(country) %>% filter(!is.na(vote)) %>%
  count(vote) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!vote=="did not vote") %>%
  ggplot(aes(x=reorder(country, -prop), y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 1: Turnout by Country", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

#Exercise 2: Let's test our theories#
#Q1: Do Socio-Economic Resources Matter#
#What about Education?#

ess %>% group_by(educat) %>% filter(!is.na(vote), !is.na(educat)) %>%
  count(vote) %>% mutate("%" =n/sum(n)*100) %>%
  knitr::kable("pandoc", caption = "Education and Electoral Turnout",
  col.names = c('Education', 'Voted', 'N', '%'), align="ccccc", digits=1)

#We can graph the relationship

ess %>% group_by(educat) %>% filter(!is.na(vote), !is.na(educat)) %>%
  count(vote) %>% mutate(prop=prop.table(n*100)) %>% mutate("%" =prop*100) %>%
  ggplot(aes(x=educat, y=prop, fill=vote))+
  labs(x="", y="", title="Figure 2: Turnout by Education", caption="ESS 2016")+
  scale_fill_manual(values=c("#FF0000", "#0000FF"))+
  geom_bar(stat="identity", position="dodge")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom")


#Turnout by Education and Country#

ess %>% group_by(educat, country) %>% filter(!is.na(vote), !is.na(educat)) %>%
  count(vote) %>% mutate(prop=prop.table(n*100)) %>% mutate("%" =prop*100) %>%
  ggplot(aes(x=educat, y=prop, fill=vote))+
  labs(x="", y="", title="Figure 3: Turnout by Education and Country", caption="Source: ESS 2002-2018")+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(values=c("#FF0000", "#0000FF"))+
  facet_wrap(~cntry, nrow=3)+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom")

#Q2: Does Rationality Influence Turnout?

ess %>% group_by(econsat) %>% filter(!is.na(vote), !is.na(econsat)) %>%
  count(vote1) %>% mutate(prop= n /sum(n)*100) %>%
  knitr::kable("pandoc", caption = "Perceptions of the Economy and Electoral Turnout",
  col.names=c('Economic Perceptions', 'Voted', 'N', '%'), align="ccccc", digits=1)

#We can graph this relationship

ess %>% group_by(satecon) %>% filter(!is.na(vote), !is.na(satecon)) %>%
  count(vote) %>% mutate(prop=prop.table(n*100)) %>% mutate("%" =prop*100) %>%
  ggplot(aes(x=satecon, y=prop, fill=vote))+
  labs(x="", y="", title="Figure 4: Turnout by Perceptions of the Economy", caption="ESS 2016")+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(values=c("#FF0000", "#0000FF"))+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom")+
  scale_x_discrete(labels=c("low" = "low", "medium" = "medium", "high" = "high"))


#Does it differ between the countries?

ess %>% group_by(satecon, country) %>% filter(!is.na(vote), !is.na(satecon)) %>%
  count(vote) %>% mutate(prop=prop.table(n*100)) %>% mutate("%" =prop*100) %>%
  ggplot(aes(x=satecon, y=prop, fill=vote))+
  labs(x="", y="", title="Figure 5: Turnout by Economic Satisfaction and Country", caption="ESS 2016")+
  geom_bar(stat="identity", position="dodge")+
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
  ggplot(aes(x=ptrust, y=prop, fill=vote))+
  labs(x="", y="", title="Figure 6: Turnout by Trust in Politics", caption="ESS 2016")+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(values=c("#FF0000", "#0000FF"))+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom")+
  scale_x_discrete(labels=c("low" = "low", "medium" = "medium", "high" = "high"))

ess %>% group_by(ptrust, country) %>% filter(!is.na(vote), !is.na(ptrust)) %>%
  count(vote) %>% mutate(prop=prop.table(n*100)) %>%
  ggplot(aes(x=ptrust, y=prop, fill=vote))+
  labs(x="", y="", title="Figure 7: Turnout by Trust in Politics", caption="ESS 2016")+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(values=c("#FF0000", "#0000FF"))+
  facet_wrap(~country, nrow=3)+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom")+
  scale_x_discrete(labels=c("low" = "low", "medium" = "medium", "high" = "high"))

# Two questions to conclude with:

      #So, thinking over the theories we've look at: which ones work?

      #Do all work equally well in all countries?
