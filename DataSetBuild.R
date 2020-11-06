


websites <- read_csv("https://raw.githubusercontent.com/rladies/starter-kit/master/Current-Chapters.csv") %>%
  filter(!is.na(Website)) %>%
  select(City, Country, Website, Email, Meetup)

repos <- read_csv("https://raw.githubusercontent.com/rladies/starter-kit/master/Current-Chapters.csv") %>%
  filter(!is.na(GitHub)) %>%
  select(City, Country, GitHub, Email, Meetup) 

readr::write_csv(websites,
                 path = here::here("websites.csv"))

readr::write_csv(repos,
                 path = here::here("repos.csv"))
