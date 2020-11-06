# Tweets function ---------------------------------------------------------
library(tidyverse)

# Topics GGPLOT------------------------------------------------------------------

# data_with_topic <- read_csv(("https://raw.githubusercontent.com/rladies/IWD/master/websites_repos_topics_tweets/data_with_topic.csv")) %>%
#   filter(City != 'Bari') #Bari has a cheers for xmas and new year


data_with_topic <- read_csv("data_with_topic.csv") %>%
     filter(City != 'Bari') #Bari has a cheers for xmas and new year

ggplot_data <- data_with_topic %>%
  filter(topic == "ggplot") %>%
  filter(!is.na(City)) %>%
  filter(!str_detect(html_url, ".Rmd")) %>% ## Istambul had 2 rows with same content, 1 .R and one .Rmd
  select(City, html_url, topic) %>%
  mutate(language = case_when(
    City %in% c("Montevideo", "Cordoba", "Barcelona", "Buenosaires", "Madrid", "Mendoza", "Santarosa") ~ "Spanish",
    City %in% c("Belohorizonte") ~ "Portuguese" ,
    City %in% c("Bari") ~ "Italian" ,
    TRUE ~ "English"
  ))


# Topics Shiny ------------------------------------------------------------

shiny_data <- data_with_topic %>%
  filter(topic == "shiny") %>%
  filter(!is.na(City)) %>%
  filter(!str_detect(html_url, "_images.zip")) %>% ## Istambul had 2 rows with same content
  select(City, html_url, topic)%>%
  mutate(language = case_when(
    City %in% c("Montevideo", "Cordoba", "Barcelona", "Buenosaires", "Madrid", "Mendoza", "Santarosa") ~ "Spanish",
    City %in% c("Belohorizonte") ~ "Portuguese" ,
    City %in% c("Bari") ~ "Italian" ,
    TRUE ~ "English"
  ))

# Several Topics ------------------------------------------------------------


several_topics_data <- data_with_topic %>%
  filter(topic %in% c("rmarkdown", "blogdown","tidyverse", "dplyr", "git","Web scrapping","spatial","Modelling","Machine learning","Package","Data Vizualization")) %>%
  filter(!is.na(City)) %>%
  select(City, html_url, topic)%>%
  #Add a rmarkdown workshop from R-Ladies Santa Rosa
  add_row(      
    City = "Santarosa",
    html_url = "https://github.com/rladies/meetup-presentations_santarosa/tree/master/TallerRMarkdown",
    topic = "rmarkdown") %>%
  add_row(      
    City = "Santarosa",
    html_url = "https://github.com/rladies/meetup-presentations_santarosa/tree/master/RdesdeCero",
    topic = "Introduction") %>%
  #Add a data.table workshop from R-Ladies Madrid
  add_row(      
    City = "Madrid",
    html_url = "https://github.com/rladies/meetup-presentations_madrid/tree/master/madrid_2019_02_07_datatableIntro",
    topic = "datatable") %>%
  add_row(      
    City = "Madrid",
    html_url = "https://github.com/rladies/meetup-presentations_madrid/tree/master/madrid_20190415_global-graph-celebration-day",
    topic = "graph") %>%
  add_row(      
    City = "Madrid",
    html_url = "https://github.com/rladies/meetup-presentations_madrid/tree/master/madrid_20170225_r-finance",
    topic = "rfinance") %>%
  # Add RYouWithMe
  add_row(      
    City = "Sydney",
    html_url = "https://rladiessydney.org/courses/ryouwithme/",
    topic = "Introduction") %>%
  mutate(language = case_when(
    City %in% c("Montevideo", "Cordoba", "Barcelona", "Buenosaires", "Madrid", "Mendoza", "Santarosa") ~ "Spanish",
    City %in% c("Belohorizonte") ~ "Portuguese" ,
    City %in% c("Bari") ~ "Italian" ,
    TRUE ~ "English"
  ))





# Unifico los archivos

topics_data <- union(several_topics_data, ggplot_data)
topics_data <- union(topics_data, shiny_data)

readr::write_csv(topics_data, path = here::here("rladieslesson.csv"))



topics_data %>% 
  group_by(topic) %>% 
  mutate(val = n()) %>%
  select(topic, val) %>%
  unique() %>%
  ungroup() %>%
  mutate(name = fct_reorder(topic, val)) %>%
  ggplot(aes(x=name, y=val)) +
  geom_bar(stat="identity", fill="#88398a", alpha=.6, width=.4) +
  coord_flip() +
  ylab("Lessons numbers") +
  xlab("") +
  geom_text(
    aes(label = val, y = val + 0.55),
    position = position_dodge(0.9),
    vjust = 0
  )+
  ggtitle("R-Ladies Materials by Topics")
    
