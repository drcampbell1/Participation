# In the lecture, we found a particularly challenging picture of political parties. We found:

                  # (1) weakening attachments

                  # (2) falling (and concentrating) membership; and

                  # (3) Negative public images.

# In this lab, we're going to look at who works for political parties,
# examining their socio-economic background to see if there is inequality.

ess <- foreign::read.dta("data/ess.dta", convert.factors=TRUE)
library(tidyverse)
options(warn = -1)

#Let's begin by looking at the general levels of working for parties:

ess %>% filter(!is.na(party)) %>% count(party) %>%
  mutate("%" = round(n/sum(n) * 100, digits=1)) %>%
  knitr::kable("pandoc", caption = "Work for Party in European Democracies",
  col.names = c('Worked', 'N', '%'), align="lccc")

#How does this vary by country?#

ess %>% group_by(country) %>% filter(!is.na(party)) %>%
  count(party) %>% mutate("%" = round(n/sum(n) * 100, digits=1)) %>%
  knitr::kable("pandoc", caption = "Comparing Work for Party in European Democracies",
  col.names = c('Country', 'Worked', 'N', '%'), align="cccc")

# Let's graph this to make the findings stand out a bit better

ess %>% group_by(country) %>% filter(!is.na(party)) %>%
  count(party) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!party=="have not worked for pp") %>%
  ggplot(aes(x=reorder(country, -prop), y=prop)) +
  geom_col()+
  labs(x="", y="", title="Figure 1: Worked for Party by Country", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

# How does the age of the population differ from that of workers of political parties?

a <- ess %>% group_by(country) %>% summarise(mean= mean(age, na.rm=T))
b <- ess %>% group_by(country, party) %>% summarise(avg_age_pp= mean(age, na.rm=T)) %>% filter(!is.na(party), !party=="have not worked for pp")
left_join(a, b, by = "country") %>% select(-party) %>%
     knitr::kable("pandoc", caption = "Average Population and Workers of Parties in European Democracies",
     col.names = c('Country', 'Average Age Population', 'Average Age Worker Political Party'), digits =1, align="cccc") %>% print()

# Is there gender balance amongst workers of political parties compared with the population?#

c <- ess %>% group_by(country) %>%
  count(gender) %>%
  mutate(perc_pop=n/sum(n)*100)
d <- ess %>% filter(party=="have worked for pp") %>% group_by(country, gender) %>%
       count(party) %>% group_by(country) %>% mutate(perc_wk_pp=n/sum(n)*100) %>%
       select(-party)
left_join(c, d, by=c("country", "gender")) %>% select(-n.x, -n.y) %>%
  knitr::kable("pandoc", caption = "Gender Balance in Population and Workers of Parties in European Democracies",
               col.names = c('Country', 'Gender', 'Population', 'Workers of Political Party'),
               digits =1, align="cccc") %>% print()

#Let's take a look at Education#


ess %>% group_by(educat) %>% filter(!is.na(party), !is.na(educat)) %>%
       count(wparty) %>% mutate(prop=prop.table(n*100)) %>%
       filter(!party=="have not worked for pp") %>%
       ggplot(aes(x=educat, y=prop)) +
       geom_col()+
       labs(x="", y="", title="Figure 2: Worked for Party by Education", caption="ESS 2016")+
       scale_y_continuous(labels=scales::percent)+
       theme_bw()+
       guides(fill=FALSE)

ess %>% group_by(educat, country) %>% filter(!is.na(party), !is.na(educat)) %>%
  count(party) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!party=="have not worked for pp") %>%
  ggplot(aes(x=educat, y=prop)) +
  geom_col()+
  facet_grid(~country)+
  labs(x="", y="", title="Figure 3: Worked for Party by Education and Country", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)+
  scale_x_discrete(labels=c("Basic" = "HS", "University degree" = "Uni", "postgrad" = "pg"))


#Economically Optimistic Party Workers?

ess %>% group_by(econsat) %>% filter(!is.na(party), !is.na(econsat)) %>%
  count(party) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!party=="have not worked for pp") %>%
  ggplot(aes(x=econsat, y=prop)) +
  geom_col()+
  labs(x="", y="", title="Figure 4: Worked for Party by Economic Optimism", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

ess %>% group_by(econsat, country) %>% filter(!is.na(party), !is.na(econsat)) %>%
  count(party) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!party=="have not worked for pp") %>%
  ggplot(aes(x=econsat, y=prop)) +
  geom_col()+
  facet_grid(~country)+
  labs(x="", y="", title="Figure 5: Worked for Party by Economic Optimism and Country", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)+
  scale_x_discrete(labels=c("dissatisfied" = "dis", "neither dissatisfied nor satisfied" = "neither", "satisfied" = "sat"))

rm(a,b,c,d)


# What can we conclude about people who work for parties? Are they a model of equality and diversity?
