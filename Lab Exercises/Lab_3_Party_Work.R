# In the lecture, we found a particularly challenging picture of political parties. We found:

                  # (1) weakening attachments

                  # (2) falling (and concentrating) membership; and

                  # (3) Negative public images.

# In this lab, we're going to look at who works for political parties,
# examining their socio-economic background to see if there is inequality.

library(tidyverse)
library(kableExtra)
options(warn = -1)

#Let's begin by looking at the general levels of working for parties:

ess %>% 
filter(!is.na(party)) %>% 
count(party) %>%
  mutate("%" = round(n/sum(n) * 100, digits=1)) %>%
  kbl(caption = "Work for Party in European Democracies",
  col.names = c('Worked', 'N', '%'), align="lccc")%>%
  kable_classic_2(full_width=F, position= "left")%>%
  footnote(general = "Source: ESS 2002-2018")

#How does this vary by country?#

ess %>% 
group_by(country) %>% 
filter(!is.na(party)) %>%
  count(party) %>% 
mutate("%" = round(n/sum(n) * 100, digits=1)) %>%
  kbl(caption = "Comparing Work for Party in European Democracies",
  col.names = c('Country', 'Worked', 'N', '%'), align="cccc")%>%
  kable_classic_2(full_width=F, position= "left")%>%
  footnote(general = "Source: ESS 2002-2018")

# Let's graph this to make the findings stand out a bit better

ess %>% 
group_by(country) %>% 
filter(!is.na(party)) %>%
  count(party) %>% 
mutate(prop=prop.table(n*100)) %>%
  filter(!party=="not worked") %>%
  ggplot(aes(x=reorder(country, -prop), y=prop)) +
  geom_col()+
  labs(x="", y="", title="Figure 1: Worked for Party by Country", caption="ESS 2002-2018")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()
 

# How does the age of the population differ from that of workers of political parties?

a <- ess %>% 
group_by(country) %>% 
summarise(mean= mean(age, na.rm=T))

b <- ess %>%
group_by(country, party) %>% 
summarise(avg_age_pp= mean(age, na.rm=T)) %>% filter(!is.na(party), !party=="not worked")

left_join(a, b, by = "country") %>% 
select(-party) %>%
     kbl(caption = "Average Population and Workers of Parties in European Democracies",
     col.names = c('Country', 
                   'Average Age Population', 
                   'Average Age Worker Political Party'), 
         digits =1, 
         align="cccc") %>%
     kable_classic_2(full_width=F, position= "left")%>%
     footnote(general = "Source: ESS 2002-2018")

# Is there gender balance amongst workers of political parties compared with the population?#

c <- ess %>% 
group_by(country) %>%
  filter(!is.na(gender))%>%
  count(gender) %>%
  mutate(perc_pop=n/sum(n)*100)

d <- ess %>% 
filter(party=="worked") %>% 
       group_by(country, gender) %>%
       count(party) %>% 
       group_by(country) %>% 
        mutate(perc_wk_pp=n/sum(n)*100) %>%
       select(-party)

left_join(c, d, by=c("country", "gender")) %>% select(-n.x, -n.y) %>%
  kbl(caption = "Gender Balance in Population and Workers of Parties in European Democracies",
               col.names = c('Country', 'Gender', 'Population', 'Workers of Political Party'),
               digits =1, align="cccc") %>%
  kable_classic_2(full_width=F, position= "left")%>%
  footnote(general = "Source: ESS 2002-2018")

#Let's take a look at Education#


ess %>% 
group_by(educat) %>% 
filter(!is.na(party), !is.na(educat)) %>%
       count(party) %>% 
mutate(prop=prop.table(n*100)) %>%
       filter(!party=="not worked") %>%
       ggplot(aes(x=educat, y=prop)) +
       geom_col()+
       labs(x="", 
            y="", 
            title="Figure 2: Worked for Party by Education", 
            caption="ESS 2002-2018")+
       scale_y_continuous(labels=scales::percent)+
       theme_bw()
       
ess %>% 
group_by(educat, country) %>% 
filter(!is.na(party), !is.na(educat)) %>%
  count(party) %>% 
mutate(prop=prop.table(n*100)) %>%
  filter(!party=="not worked") %>%
  ggplot(aes(x=educat, y=prop)) +
  geom_col()+
  facet_wrap(~country, nrow = 3)+
  labs(x="",
       y="", 
       title="Figure 3: Worked for Party by Education and Country", 
       caption="ESS 2002-2018")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  scale_x_discrete(labels=c("Basic" = "Basic", "HS" = "HS", "University degree" = "Uni", "postgrad" = "pg"))


#What about the income profile of those who work for parties?

ess %>% 
group_by(quin) %>% 
filter(!is.na(party), !is.na(quin)) %>%
  count(party) %>% 
mutate(prop=prop.table(n*100)) %>%
  filter(!party=="not worked") %>%
  ggplot(aes(x=quin, y=prop)) +
  geom_col()+
  labs(x="", 
       y="", 
       title="Figure 2: Worked for Party by Income Quintile", 
       caption="ESS 2002-2018")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()

ess %>% 
group_by(quin, country) %>% 
filter(!is.na(party), !is.na(quin)) %>%
  count(party) %>% 
mutate(prop=prop.table(n*100)) %>%
  filter(!party=="not worked") %>%
  ggplot(aes(x=quin, y=prop)) +
  geom_col()+
  facet_wrap(~country, nrow = 3)+
  labs(x="", 
       y="", 
       title="Figure 2: Worked for Party by Income Quintile", 
       caption="ESS 2002-2018")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()


rm(a,b,c,d)


# What can we conclude about people who work for parties? Are they a model of equality and diversity?
