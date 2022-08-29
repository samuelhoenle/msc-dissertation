library(magrittr)
library(tidyverse)
library(sf)

prepare_data <- function(survey_export_path, tracks_export_path, clean = T) {
  survey_data <- read_csv(survey_export_path)
  
  survey_data %<>%
    rename(gender = `Was ist dein Geschlecht?`) %>%
    mutate(gender = factor(gender,
                           levels = c('männlich', 'weiblich', 'anderes'),
                           labels = c('male', 'female', 'other'),
                           ordered = F))
  survey_data %<>%
    rename(yob = `In welchem Jahr wurdest du geboren?`) %>% 
    mutate(yob = as.integer(yob)) %>% 
    mutate(age = 2022 - yob)
  survey_data %<>%
    rename(plz = `Was ist deine Postleitzahl?`) %>%
    mutate(plz = as.integer(plz)) %>% 
    mutate(plz = plyr::round_any(plz, 10, f = floor))
  survey_data %<>%
    rename(education = `Was ist dein höchster Schulabschluss?`) %>% 
    mutate(education = factor(education,
                              levels = c('(noch) kein Abschluss', 'Volks-/Hauptschule ohne Lehre', 'Volks-/Hauptschule mit Lehre/Fachschule', 'Matura', 'Universität/(Fach)Hochschule'),
                              labels = c('none', 'primary', 'primary+vocational', 'secondary', 'tertiary'),
                              ordered = T))
  survey_data %<>%
    rename(marstat = `Bist du verheiratet?`) %>% 
    mutate(marstat = factor(marstat,
                            levels = c("Nicht verheiratet", "Verheiratet/Eingetragene Partnerschaft", "Getrennt", "Geschieden", "Verwitwet"),
                            labels = c('not married', 'married', 'separated', 'divorced', 'widowed'),
                            ordered = F))
  survey_data %<>%
    rename(hhtype = `Welche Beschreibung trifft am besten auf deine derzeitige Wohnsituation zu? Ich wohne…`) %>%
    mutate(hhtype = factor(hhtype,
                           levels = c("anders", "mit Partner*in ohne Kinder oder mit erwachsenen Kindern", "alleine", "mit Partner*in und Kindern", "alleine mit Kindern", "mit Kindern und anderen Personen (alleine oder mit Partner*in)"),
                           labels = c('other', 'couple without children aged < 25', 'One-person household', 'couple with children aged < 25', 'single parent with children aged < 25', 'couple/single parent with children < 25 and other persons')))
  survey_data %<>%
    rename(nkids = `Wie viele Kleinkinder (0-4 Jahre) leben in deinem Haushalt?`) %>%
    mutate(nkids = as.integer(nkids))
  survey_data %<>%
    rename(nyouth = `Wie viele Kinder und junge Erwachsene (5-24 Jahre) leben in deinem Haushalt?`) %>% 
    mutate(nyouth = as.integer(nyouth))
  survey_data %<>%
    rename(nadults = `Wie viele Erwachsene (25-64 Jahre) leben in deinem Haushalt?`) %>% 
    mutate(nadults = as.integer(nadults))
  survey_data %<>%
    rename(nelderly = `Wie viele ältere Erwachsene (65+ Jahre) leben in deinem Haushalt?`) %>%
    mutate(nelderly = as.integer(nelderly))
  survey_data$nkids[survey_data$nkids + survey_data$nyouth + survey_data$nadults + survey_data$nelderly == 0] <- NA # 15
  survey_data$nyouth[survey_data$nyouth + survey_data$nadults + survey_data$nelderly == 0] <- NA # 17
  survey_data$nadults[survey_data$nadults + survey_data$nelderly == 0 & is.na(survey_data$nyouth)] <- NA
  survey_data$nelderly[survey_data$nelderly == 0 & is.na(survey_data$nyouth) & is.na(survey_data$nelderly)] <- NA
  survey_data %<>%
    rename(nwork = `Wie viele Personen zwischen 16-64 Jahre in deinem Haushalt arbeiten?`) %>% 
    mutate(nwork = as.integer(nwork))
  survey_data %<>%
    rename(worksituation = `Wie würdest du deine derzeitige Arbeitssituation beschreiben?`) %>% 
    mutate(worksituation = factor(worksituation,
                                  levels = c("Angestellt – Teilzeit", "Angestellt – Vollzeit", "Selbstständig", "in Ausbildung", "Arbeitslos", "ausschließlich im Haushalt tätig", "anders", "im Ruhestand", "im Militär-/Zivildienst", "in Karenz", "dauerhaft arbeitsunfähig"),
                                  labels = c('part-time', 'full-time', 'self-employed', 'in education', 'unemployed', 'domestic tasks', 'other', 'retired', 'military/civil service', 'on leave',  'permanently disabled')))
  survey_data %<>%
    rename(persinc = `Wie hoch ist dein persönliches Monats-Nettoeinkommen?`) %>% 
    mutate(persinc = factor(persinc,
                            levels = c('≤ € 250', '€ 251 - € 1000', '€ 1001 - € 1900', '€ 1901 - € 2700', '€ 2701 - € 3700', '> € 3700'),
                            ordered = T))
  survey_data %<>%
    rename(hhinc = `Wie hoch ist das Monats-Nettoeinkommen deines gesamten Haushalts?`) %>% 
    mutate(hhinc = factor(hhinc,
                          levels = c('≤ € 1300', '€ 1301 - € 2000', '€ 2001 - € 3300', '€ 3301 - € 4900', '€ 4901 - € 6800', '> € 6800'),
                          ordered = T))
  survey_data %<>%
    mutate(homeoffice = str_detect(`Stehen dir diese Dinge an deinem Arbeitsplatz zur Verfügung?`, 'Möglichkeit zum Home-Office')) %>% 
    mutate(flextime = str_detect(`Stehen dir diese Dinge an deinem Arbeitsplatz zur Verfügung?`, 'flexible Arbeitszeiten')) %>%
    dplyr::select(-`Stehen dir diese Dinge an deinem Arbeitsplatz zur Verfügung?`)
  survey_data$homeoffice[is.na(survey_data$homeoffice)] <- F
  survey_data$flextime[is.na(survey_data$flextime)] <- F
  survey_data %<>%
    rename(citizenship = `Welche Staatsbürgerschaft hast du?`) %>% 
    mutate(citizenship = factor(citizenship, ordered = F))
  survey_data %<>% mutate(citizenship_eu = citizenship %in% c('Österreich', 'Deutschland', 'Frankreich', 'Irland', 'Luxemburg', 'Italien', 'Spanien', 'Polen', 'Rumänien', 'Ungarn', 'Schweden', 'Finnland', 'Estland', 'Lettland', 'Litauen', 'Tschechien', 'Slowakei', 'Slowenien', 'Griechenland', 'Zypern', 'Portugal', 'Belgien', 'Niederlande', 'Dänemark mit Färöäer (FO) und Grönland (GL)', 'Malta', 'Bulgarien', 'Kroatien'))
  survey_data %<>%
    rename(cob = `Wo wurdest du geboren?`) %>% 
    mutate(cob = factor(citizenship, ordered = F))
  survey_data %<>%
    rename(cobp1 = `Wo wurde dein Elternteil 1 geboren?`) %>% 
    mutate(cobp1 = factor(citizenship, ordered = F))
  survey_data %<>%
    rename(cobp2 = `Wo wurde dein Elternteil 2 geboren?`) %>% 
    mutate(cobp2 = factor(citizenship, ordered = F))
  # list of western/white countries to determine whether respondent might be subject to racism/xenophobia
  wlist <- c('Österreich', 'Deutschland', 'Frankreich', 'Irland', 'Luxemburg', 'Italien', 'Spanien', 'Schweden', 'Finnland', 'Portugal', 'Belgien', 'Niederlande', 'Dänemark mit Färöäer (FO) und Grönland (GL)', 'Malta', 'Schweiz', 'Vereinigtes Königreich', 'Vereinigte Staaten von Amerika', 'Australien', 'Neuseeland', 'Kanada', 'Norwegen', 'Island', 'Andorra', 'Liechtenstein', 'San Marino', 'Monaco')
  survey_data %<>% mutate(nwparent = !((cobp1 %in% wlist) & (cobp2 %in% wlist)))
  survey_data %<>%
    mutate(avb_pt = str_detect(`Welche Verkehrsmittel stehen dir üblicherweise zur Verfügung?`, 'Öffentliche Verkehrsmittel')) %>% 
    mutate(avb_bike = str_detect(`Welche Verkehrsmittel stehen dir üblicherweise zur Verfügung?`, '(Fahrrad \\{and\\}|Fahrrad$)')) %>% 
    mutate(avb_ebike = str_detect(`Welche Verkehrsmittel stehen dir üblicherweise zur Verfügung?`, 'E-Bike')) %>% 
    mutate(avb_bikesharing = str_detect(`Welche Verkehrsmittel stehen dir üblicherweise zur Verfügung?`, 'Fahrrad-Sharing-Dienst')) %>% 
    mutate(avb_car = str_detect(`Welche Verkehrsmittel stehen dir üblicherweise zur Verfügung?`, 'Auto')) %>% 
    mutate(avb_carsharing = str_detect(`Welche Verkehrsmittel stehen dir üblicherweise zur Verfügung?`, 'Car-Sharing-Dienst')) %>% 
    mutate(avb_motorbike = str_detect(`Welche Verkehrsmittel stehen dir üblicherweise zur Verfügung?`, '(Motorrad \\{and\\}|Motorrad$)')) %>% 
    mutate(avb_motorbikesharing = str_detect(`Welche Verkehrsmittel stehen dir üblicherweise zur Verfügung?`, 'Motorrad-Sharing-Dienst')) %>% 
    mutate(avb_escooter = str_detect(`Welche Verkehrsmittel stehen dir üblicherweise zur Verfügung?`, '(E-Scooter \\{and\\}|E-Scooter$)')) %>% 
    mutate(avb_escootersharing = str_detect(`Welche Verkehrsmittel stehen dir üblicherweise zur Verfügung?`, 'E-Scooter-Sharing-Dienst')) %>% 
    dplyr::select(-`Welche Verkehrsmittel stehen dir üblicherweise zur Verfügung?`)
  survey_data %<>%
    rename(hhbikes = `Wie viele funktionierende Fahrräder gibt es in deinem Haushalt?`) %>% 
    mutate(hhbikes = as.integer(hhbikes))
  survey_data$hhbikes[survey_data$hhbikes > 20] <- NA # 1
  survey_data %<>%
    mutate(ict_bike = str_detect(`Für welche Verkehrsmittel benutzt du dein Smartphone?`, 'Fahrrad')) %>% 
    mutate(ict_pt = str_detect(`Für welche Verkehrsmittel benutzt du dein Smartphone?`, 'Öffentlicher Verkehr')) %>% 
    mutate(ict_walk = str_detect(`Für welche Verkehrsmittel benutzt du dein Smartphone?`, 'zu Fuß')) %>% 
    mutate(ict_car = str_detect(`Für welche Verkehrsmittel benutzt du dein Smartphone?`, 'Auto')) %>% 
    mutate(ict_other = str_detect(`Für welche Verkehrsmittel benutzt du dein Smartphone?`, 'andere')) %>%
    dplyr::select(-`Für welche Verkehrsmittel benutzt du dein Smartphone?`)
  
  tracks <- st_read(tracks_path)
  
  tracks <- merge(tracks, survey_data, by.x = 'token', by.y = 'bc_token')
  
  if (clean) {
    over_20_bikes <- length(unique(tracks$token))
    tracks <- tracks %>% filter(hhbikes < 21)
    over_20_bikes <- over_20_bikes - length(unique(tracks$token))
    removals = list('over_20_bikes' = over_20_bikes)
  } else {
    removals = list()
  }
  
  responses_with_tracks <- survey_data %>% filter(bc_token %in% unique(tracks$token))
  
  return(list('survey' = responses_with_tracks, 'survey_complete' = survey_data, 'tracks' = tracks, 'removals' = removals))
}
