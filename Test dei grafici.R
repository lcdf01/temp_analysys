#------pacchetti------------------------------------------------------------------

install.packages('gganimate')
install.packages("plotly")
install.packages("ggmap")
install.packages("viridis")

# Carica i pacchetti
library(ggplot2)      #grafici
library(lubridate)    #date
library(gganimate)    #animazioni
library(tidyr)        #pulizia dati
library(dplyr)        #manipolazione dati
library(readr)        #per leggere i csv
library(plotly)       #grafici interattivi
library(ggmap)        #mappe
library(viridis)      #colori
library(RColorBrewer) #colori caldi/freddi

#------introduzione dataset----------------------------------------------------------
temp <- read_csv("city_temperature.csv")
co2 <-  read_csv("co2_emissions_kt_by_country.csv")

# ci sono alcune righe con l'anno sbagliato "200" o "201", modifichiamolo

temp$Year <- as.numeric(as.character(temp$Year)) # Converti la colonna "Year" in un tipo di dato numerico per poterlo modificare

temp_senza_2020 <- filter(temp, Year < 2020) # elimina i dati del 2020 perchè parziali e non corretti

# Modifica gli anni errati utilizzando mutate() e case_when()
temp_senza_2020 <- temp_senza_2020 %>%
  mutate(
    Year = case_when(
      Year == 200 ~ 2000,
      Year == 201 ~ 2001,
      TRUE ~ Year  # Mantieni gli altri anni non modificati
    )
  ) %>%
  mutate(AvgTemperature = replace(AvgTemperature, AvgTemperature == -99, NA))


#la temperatura è in fahreneit, bisogna metterla in gradi celsius
temp_senza_2020$AvgTemperatureCelsius <- (temp_senza_2020$AvgTemperature - 32) / 1.8
#ora possiamo lavorarci sopra


#------come è cambiata nel corso degli anni la temperatura media annuale in ogni continente-----------------------------
Continente_media_temp_ann <- temp_senza_2020 %>%
  group_by(Region, Year) %>% #serve per raggruppare i dati
  summarize(MediaTemperaturaAnnuale = mean(AvgTemperatureCelsius, na.rm = TRUE)) #calcola la media mensile, na.rm = TRUE è per eliminare i valori null dal calcolo

# Crea il grafico a linee diviso per continente
Continente_grafico_media_temp_ann <- ggplot(Continente_media_temp_ann, aes(x = Year, y = MediaTemperaturaAnnuale, color = Region)) +
  geom_line() +
  geom_point() +
  labs(x = "Anno", y = "Temperatura media annuale (°C)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"), axis.title = element_text(size = 14), axis.text = element_text(size = 12))

# Converti il grafico in un oggetto plotly
Continente_grafico_media_temp_ann <- ggplotly(Continente_grafico_media_temp_ann)

# Visualizza il grafico
Continente_grafico_media_temp_ann

Continente_grafico_media_temp_ann <- ggplot(Continente_media_temp_ann, aes(x = Year, y = MediaTemperaturaAnnuale, color = Region)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, size = 1, color = "blue") +  # Aggiunta della linea di trend e regressione
  labs(x = "Anno", y = "Temperatura media annuale (°C)") +
  theme_minimal() +
  facet_wrap(~Region) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12))

# Visualizza il grafico
Continente_grafico_media_temp_ann

#------mappa/grafico solo stati uniti--------------------------------------------------------------------------------------------
USA_temp_media_2019 <- temp_senza_2020 %>%
  filter(Region == "North America") %>%
  filter(Year == 2019) %>%
  group_by(State) %>% #raggruppa i dati in base allo stato (Alabama, California, ...)
  summarize(MediaTemperaturaMensile = mean(AvgTemperatureCelsius, na.rm = TRUE)) %>%
  na.omit() #omette tutte le righe che hanno NA in qualsiasi colonna

USA_map <- map_data("state")                    #Usa la funzione `map_data()` per ottenere i dati geografici
USA_map <- rename(USA_map, 'State'='region')    #rinomina la colonna region per effettuare il join su State
USA_map$State <- str_to_title(USA_map$State)    #bisogna mettere la maiuscola come prima lettera se no i due dataset non coincidono

#join di mappa + dati temperatura
USA_Temp_Map_2019 <- inner_join(USA_map, USA_temp_media_2019, relationship = "many-to-many")

USA_Interactive_map <- ggplot(data = USA_Temp_Map_2019) + 
  geom_polygon(aes(x = long, y = lat, fill = MediaTemperaturaMensile, group = group), #group serve a separare i poligoni mediante la colonna group (cioè i vari stati)
               color = "white") +   #disegna i confini degli stati
  coord_fixed(1.3) + #mantiene le proporzioni corrette
  scale_fill_gradientn(colors = brewer.pal(9, "YlOrRd"), na.value = "grey") + #usa un gradiente di colori preimpostato
  labs(fill = "Temperatura Media (°C)") +
  theme_void() +
  geom_text(data = aggregate(cbind(long, lat, MediaTemperaturaMensile) ~ State, USA_Temp_Map_2019, FUN = mean),
            #Nuovo dataset, l'aggregazione di 3 colonne e della 4 che farà da group by, il cbind crea un nuovo dataframe di cui verrà calcolata
            #la media di ognuna delle colonne raggruppate. Serve per calcolare le coordinate il centro degli stati per posizionarne il nome
            aes(x = long, y = lat, label = State, text = paste("Temperatura Media:", round(MediaTemperaturaMensile, 2), "°C")),
            #mostra il nome dello stato, la seconda parte serve per il Plotly e mostrare la Tmedia quando si va sopra il nome dello stato
            size = 3, color = "black", fontface = "bold", alpha = 0.8, nudge_y = -0.5) +
  theme(legend.position = "bottom", legend.title = element_blank(), legend.direction = "horizontal") + #legenda orizzontale
  guides(fill = guide_colorbar(barwidth = 15, barheight = 1, title.position = "top", title.hjust = 0.5)) #decide le estetiche della legenda

USA_Interactive_map <- ggplotly(USA_Interactive_map)

USA_Interactive_map

#------boxplot di come varia la temperatura a Roma e Milano col passare del tempo---------------------------------------

Italia_media_temp <- temp_senza_2020 %>%
  filter(Country == "Italy") %>%
  group_by(City, Year, Month) %>%
  summarize(MediaTemperaturaMensile = mean(AvgTemperatureCelsius, na.rm = TRUE))

# Crea il grafico con boxplot delle città Roma e Milano e ne confronta i dati, inoltre cambia i numeri dei mesi con le loro abbreviazioni
Italia_Interactive_media_temp <- ggplot(data = Italia_media_temp, aes(x = factor(Month, labels = month.abb), y = MediaTemperaturaMensile, fill = City)) +
  geom_boxplot(position = "dodge") +
  labs(x = "Mese", y = "Temperatura Media (°C)", fill = "Città") +
  scale_fill_viridis_d() +
  scale_x_discrete(labels = month.abb) +
  theme(legend.position = "bottom", legend.title = element_blank(), legend.direction = "horizontal") #legenda orizzontale

# Rendi il grafico interattivo con plotly
Italia_Interactive_media_temp <- ggplotly(Italia_Interactive_media_temp)

Italia_Interactive_media_temp

#------grafico di quanto è variata la temperatura media nel mondo negli anni con trend line---------------------------------------

Continente_var_media_temp_ann <- temp_senza_2020 %>%
  group_by(Region, Year) %>% #serve per raggruppare i dati
  summarize(MediaTemperaturaAnnuale = mean(AvgTemperatureCelsius, na.rm = TRUE)) %>% #calcola la media mensile, na.rm = TRUE è per eliminare i valori null dal calcolo
  mutate(Var_temp_media = round(MediaTemperaturaAnnuale - lag(MediaTemperaturaAnnuale), 3)) %>% #lag serve per prendere la temperatura media dell'anno precedente
  na.omit() #omette tutte le righe che hanno NA in qualsiasi colonna

Continente_var_interactive_temp <- ggplot(data = Continente_var_media_temp_ann, aes(x = Year, y = Var_temp_media, color = Region)) +
  geom_line(size = 1) +
  geom_point(size = 2, shape = 19) +
  
  geom_line(data = Continente_var_media_temp_ann %>% group_by(Region) %>% mutate(max_Var_temp = max(Var_temp_media)),
            aes(x = Year, y = max_Var_temp), size = 1, alpha = 0.9, color = "black") +
  geom_line(data = Continente_var_media_temp_ann %>% group_by(Region) %>% mutate(min_Var_temp = min(Var_temp_media)),
            aes(x = Year, y = min_Var_temp), size = 1, alpha = 0.9, color = "black") +
  
  geom_text(data = Continente_var_media_temp_ann %>% group_by(Region) %>% filter(Var_temp_media == max(Var_temp_media)),
            aes(x = Year, y = Var_temp_media, label = paste(Var_temp_media, "°C")), size = 2.5, color = "black", hjust = 0, vjust = -0.7) +
  geom_text(data = Continente_var_media_temp_ann %>% group_by(Region) %>% filter(Var_temp_media == min(Var_temp_media)),
            aes(x = Year, y = Var_temp_media, label = paste(Var_temp_media, "°C")), size = 2.5, color = "black", hjust = 0, vjust = +1) +
  
  labs(x = "Anno", y = "Variazione di T media annuale (°C)") +
  scale_color_viridis_d() +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12)) +
  facet_wrap(~Region)

Continente_var_interactive_temp

#Continente_var_interactive_temp <- ggplotly(Continente_var_interactive_temp)

#Continente_var_interactive_temp


#------animazione di come cambia la T in Europa col passare degli anni---------------------------------------
Europe_media_annual_temp <- temp_senza_2020 %>%
  filter(Region == "Europe") %>%
  group_by(Country, Year) %>%                                                        #raggruppa i dati in base al paese
  summarize(MediaTemperaturaAnnuale = mean(AvgTemperatureCelsius, na.rm = TRUE)) %>%
  na.omit() %>%                                                                      #omette tutte le righe che hanno NA in qualsiasi colonna
  mutate(Country = ifelse(Country == "United Kingdom", "UK", Country)) %>%           #trasforma i paesi che hanno nomi non congruenti
  mutate(Country = ifelse(Country == "The Netherlands", "Netherlands", Country)) %>%
  mutate(Country = ifelse(Country == "Macedonia", "North Macedonia", Country)) %>%
  mutate(Country = ifelse(Country == "Yugoslavia", "Serbia", Country))
  
Europe_countries <- c(                                        #scriviamo il nome dei paesi da visualizzare
  "Albania", "Austria", "Belarus", "Belgium", "Bulgaria", "Croatia", "Czech Republic", "Denmark", "Finland", "France", "Germany", 
  "Hungary", "Iceland", "Ireland", "Italy", "Latvia", "North Macedonia", "Norway", "Poland", "Portugal", "Romania", "Slovakia", "Spain",
  "Sweden", "Switzerland", "Netherlands", "Ukraine", "UK", "Serbia"
)

Europe_countries <- map_data("world", region = Europe_countries) %>%  #Usa la funzione `map_data()` per ottenere i dati geografici
  select( -order, -subregion)                                         #elimina colonne superflue
Europe_countries <- rename(Europe_countries, 'Country'='region')      #rinomina la colonna region per effettuare il join su Country

Europe_countries_names <- Europe_countries %>%                        #calcola le coordinate per posizionare i nomi e le temp degli stati
  group_by(Country) %>%                                               #al centro
  summarise(long = mean(long), lat = mean(lat))

#join per mettere i valori di temperatura anche come testo
Europe_countries_names <- inner_join(Europe_countries_names, Europe_media_annual_temp, relationship = "many-to-many")

#join per associare le coordinate ai paesi alle loro temperature
Europe_interactive_map <- inner_join(Europe_countries, Europe_media_annual_temp, relationship = "many-to-many")

# Crea la mappa dell'europa con le temperature medie
Europe_map_media_annual_temp <- ggplot(data = Europe_interactive_map) +
  geom_polygon(aes(x = long, y = lat, fill = MediaTemperaturaAnnuale, group = group), color = "black", size = 0.05) +
  geom_text(data = Europe_countries_names, aes(x = long, y = lat, label = paste(Country, "\n", round(MediaTemperaturaAnnuale, 3), "°C")), 
            color = "black", size = 3, fontface = "bold") +
  scale_fill_gradientn(colors = brewer.pal(9, "YlOrRd"), na.value = "grey") +
  coord_fixed(1.3) +
  theme_void() +
  labs(x = NULL, y = NULL) +
  transition_states(Year, transition_length = 2, state_length = 1) +  # Mostra ogni stato (anno) per 2 secondi
  labs(fill = "Temperatura Media (°C)", title = "Temperatura Media Europa negli anni {closest_state}") +
  ease_aes('linear')

# Riproduci l'animazione
animation <- animate(Europe_map_media_annual_temp, nframes = 100, fps = 10, width = 1920, height = 1080)

# Salva l'animazione come file mp4
anim_save("Europe_temperature_animation.mp4", animation)
#------correlazione con le emissioni di CO2---------------------------------------
world_countries <- map_data("world") %>%
  group_by(region) %>%
  rename('Country'='region')

World_CO2_Media_emi <- co2 %>%
  select(-country_code) %>%
  group_by(country_name, year) %>%                                 #raggruppa i dati in base al paese e all'anno
  filter(year == 2019) %>%
  na.omit() %>%                                                       #omette tutte le righe che hanno NA in qualsiasi colonna
  rename('Country'='country_name') %>%
  rename('CO2_val'='value') %>%
  mutate(Country = ifelse(Country == "Venezuela, RB", "Venezuela", Country)) %>%
  mutate(Country = ifelse(Country == "Yemen, Rep.", "Yemen", Country)) %>%
  mutate(Country = ifelse(Country == "St. Vincent and the Grenadines", "Saint Vincent", Country)) %>%
  mutate(Country = ifelse(Country == "United States", "USA", Country)) %>%
  mutate(Country = ifelse(Country == "Turkiye", "Turkey", Country)) %>%
  mutate(Country = ifelse(Country == "Trinidad and Tobago", "Tobago", Country)) %>%
  mutate(Country = ifelse(Country == "Syrian Arab Republic", "Syria", Country)) %>%
  mutate(Country = ifelse(Country == "Slovak Republic", "Slovakia", Country)) %>%
  mutate(Country = ifelse(Country == "Sub-Saharan Africa", "Western Sahara", Country)) %>%
  mutate(Country = ifelse(Country == "Russian Federation", "Russia", Country)) %>%
  mutate(Country = ifelse(Country == "Korea, Rep.", "North Korea", Country)) %>%
  mutate(Country = ifelse(Country == "St. Lucia", "Saint Lucia", Country)) %>%
  mutate(Country = ifelse(Country == "St. Kitts and Nevis", "Saint Kitts", Country)) %>%
  mutate(Country = ifelse(Country == "Kyrgyz Republic", "Kyrgyzstan", Country)) %>%
  mutate(Country = ifelse(Country == "Iran, Islamic Rep.", "Iran", Country)) %>%
  mutate(Country = ifelse(Country == "Gambia, The", "Gambia", Country)) %>%
  mutate(Country = ifelse(Country == "United Kingdom", "UK", Country)) %>%
  mutate(Country = ifelse(Country == "Micronesia, Fed. Sts.", "Micronesia", Country)) %>%
  mutate(Country = ifelse(Country == "Egypt, Arab Rep.", "Egypt", Country)) %>%
  mutate(Country = ifelse(Country == "Czechia", "Czech Republic", Country)) %>%
  mutate(Country = ifelse(Country == "Cabo Verde", "Cape Verde", Country)) %>%
  mutate(Country = ifelse(Country == "Congo, Rep.", "Republic of Congo", Country)) %>%
  mutate(Country = ifelse(Country == "Congo, Dem. Rep.", "Democratic Republic of the Congo", Country)) %>%
  mutate(Country = ifelse(Country == "Cote d'Ivoire", "Ivory Coast", Country)) %>%
  mutate(Country = ifelse(Country == "Bahamas, The", "Bahamas", Country)) %>%
  mutate(Country = ifelse(Country == "Antigua and Barbuda", "Barbuda", Country)) %>%
  mutate(Country = ifelse(Country == "Samoa", "American Samoa", Country))

world_countries <- map_data("world") %>%                            #Usa la funzione `map_data()` per ottenere i dati geografici
  select( -order, -subregion)                                       #elimina colonne superflue
world_countries <- rename(world_countries, 'Country'='region')      #rinomina la colonna region per effettuare il join su Country

#join per associare le coordinate ai paesi alle loro emissioni
world_interactive_map <- inner_join(world_countries, World_CO2_Media_emi, relationship = "many-to-many")

# Crea la mappa del mondo con le emissioni medie
World_map_media_annual_temp <- ggplot(data = world_interactive_map) +
  geom_polygon(aes(x = long, y = lat, fill = CO2_val, group = group), color = "black", size = 0.05) +
  geom_polygon(data = world_interactive_map %>% filter(Country == "USA" | Country == "China" | Country == "India" | Country == "Russia"), 
               aes(x = long, y = lat, fill = CO2_val, group = group), color = "red", size = 0.3) +
  scale_fill_gradientn(colors = brewer.pal(9, "YlOrRd"), na.value = "grey") +
  coord_fixed(1.3) +
  labs(fill = "Emissioni di CO2 (kiloton kt)", title = "Emissioni di CO2 nel mondo nel 2019") +
  theme_void() +
  labs(x = NULL, y = NULL)

World_map_media_annual_temp

#------3.1) China---------------------------------------
China_CO2_Media_emi <- co2 %>%
  select(-country_code) %>%
  group_by(country_name, year) %>%                                 #raggruppa i dati in base al paese e all'anno
  filter(country_name == "China") %>%
  filter(year >= 1995) %>%
  na.omit() %>%                                                       #omette tutte le righe che hanno NA in qualsiasi colonna
  rename('Country'='country_name') %>%
  rename('CO2_val'='value') %>%
  rename('Year'='year')

#join per associare le coordinate ai paesi alle loro emissioni
China_CO2_Map <- inner_join(world_countries, China_CO2_Media_emi, relationship = "many-to-many")

# Crea la mappa della cina con le emissioni medie
China_CO2_Interactive_Map <- ggplot(data = China_CO2_Map) +
  geom_polygon(aes(x = long, y = lat, fill = CO2_val, group = group), color = "black", size = 0.05) +
  geom_text(aes(x = mean(long), y = mean(lat), label = round(CO2_val, digits = 2)), color = "black", size = 18) +
  scale_fill_gradientn(colors = brewer.pal(9, "YlOrRd"), na.value = "grey") +
  coord_fixed(1.3) +
  theme_void() + 
  theme(legend.position = "none") + # Rimuovi la legenda
  transition_states(Year, transition_length = 2, state_length = 1) +  # Mostra ogni stato (anno) per 2 secondi
  labs(fill = "Emissioni di CO2 (kiloton kt)", title = "Emissioni medie CO2 in Cina negli anni {closest_state}", 
       x = NULL, y = NULL) +
  ease_aes('linear')

# Estrai il massimo valore delle emissioni di CO2 per la Cina per usarlo come massimo della scala
massimo_CO2_Cina <- max(China_CO2_Map$CO2_val, na.rm = TRUE)

# Riproduci l'animazione
animation <- animate(China_CO2_Interactive_Map, nframes = 100, fps = 10, width = 960, height = 1080)

# Salva l'animazione come file mp4
anim_save("anim_China_CO2_animation.mp4", animation)

#------3.2) Stati Uniti---------------------------------------
USA_CO2_Media_emi <- co2 %>%
  select(-country_code) %>%
  group_by(country_name, year) %>%                                 #raggruppa i dati in base al paese e all'anno
  filter(country_name == "United States") %>%
  filter(year >= 1995) %>%
  na.omit() %>%                                                    #omette tutte le righe che hanno NA in qualsiasi colonna
  rename('Country'='country_name') %>%
  rename('CO2_val'='value') %>%
  mutate(Country = ifelse(Country == "United States", "USA", Country)) %>%
  rename('Year'='year')

#join per associare le coordinate ai paesi alle loro temperature
USA_CO2_Map <- inner_join(world_countries, USA_CO2_Media_emi, relationship = "many-to-many")

# Crea la mappa degli USA con le emissioni medie
USA_CO2_Interactive_Map <- ggplot(data = USA_CO2_Map) +
  geom_polygon(aes(x = long, y = lat, fill = CO2_val, group = group), color = "black", size = 0.05) +
  geom_text(aes(x = mean(long), y = mean(lat), label = round(CO2_val, digits = 2)), color = "black", size = 10) +
  scale_fill_gradientn(limits = c(0, massimo_CO2_Cina), colors = brewer.pal(9, "YlOrRd"), na.value = "grey") +
  coord_fixed(1.3) +
  theme_void() +
  theme(legend.position = "none") + # Rimuovi la legenda
  transition_states(Year, transition_length = 2, state_length = 1) +  # Mostra ogni stato (anno) per 2 secondi
    labs(fill = "Emissioni di CO2 (kiloton kt)", title = "Emissioni medie CO2 in USA negli anni {closest_state}", 
       x = NULL, y = NULL) +
  ease_aes('linear')

# Riproduci l'animazione
animation <- animate(USA_CO2_Interactive_Map, nframes = 100, fps = 10, width = 960, height = 1080)

# Salva l'animazione come file mp4
anim_save("anim_USA_CO2_animation.mp4", animation)
#------3.3) Russia---------------------------------------
Russia_CO2_Media_emi <- co2 %>%
  select(-country_code) %>%
  group_by(country_name, year) %>%                                 #raggruppa i dati in base al paese e all'anno
  filter(country_name == "Russian Federation") %>%
  filter(year >= 1995) %>%
  na.omit() %>%                                                       #omette tutte le righe che hanno NA in qualsiasi colonna
  rename('Country'='country_name') %>%
  rename('CO2_val'='value') %>%
  mutate(Country = ifelse(Country == "Russian Federation", "Russia", Country)) %>%
  rename('Year'='year')

#join per associare le coordinate ai paesi alle loro emissioni
Russia_CO2_Map <- inner_join(world_countries, Russia_CO2_Media_emi, relationship = "many-to-many")

# Crea la mappa della russia con le emissioni medie
Russia_CO2_Interactive_Map <- ggplot(data = Russia_CO2_Map) +
  geom_polygon(aes(x = long, y = lat, fill = CO2_val, group = group), color = "black", size = 0.05) +
  geom_text(aes(x = mean(long), y = mean(lat), label = round(CO2_val, digits = 2)), color = "black", size = 10) +
  scale_fill_gradientn(limits = c(0, massimo_CO2_Cina), colors = brewer.pal(9, "YlOrRd"), na.value = "grey") +
  coord_fixed(1.3) +
  theme_void() +
  theme(legend.position = "none") + # Rimuovi la legenda
  transition_states(Year, transition_length = 2, state_length = 1) +  # Mostra ogni stato (anno) per 2 secondi
  labs(fill = "Emissioni di CO2 (kiloton kt)", title = "Emissioni medie CO2 in Russia negli anni {closest_state}", 
       x = NULL, y = NULL) +
  ease_aes('linear')

# Riproduci l'animazione
animation <- animate(Russia_CO2_Interactive_Map, nframes = 100, fps = 10, width = 960, height = 1080)

# Salva l'animazione come file mp4
anim_save("anim_Russia_CO2_animation.mp4", animation)
#------3.4) India---------------------------------------
India_CO2_Media_emi <- co2 %>%
  select(-country_code) %>%
  group_by(country_name, year) %>%                                 #raggruppa i dati in base al paese e all'anno
  filter(country_name == "India") %>%
  filter(year >= 1995) %>%
  na.omit() %>%                                                       #omette tutte le righe che hanno NA in qualsiasi colonna
  rename('Country'='country_name') %>%
  rename('CO2_val'='value') %>%
  rename('Year'='year')

#join per associare le coordinate ai paesi alle loro temperature
India_CO2_Map <- inner_join(world_countries, India_CO2_Media_emi, relationship = "many-to-many")

# Crea la mappa dell'India con le emissioni medie
India_CO2_Interactive_Map <- ggplot(data = India_CO2_Map) +
  geom_polygon(aes(x = long, y = lat, fill = CO2_val, group = group), color = "black", size = 0.05) +
  geom_text(aes(x = mean(long), y = mean(lat), label = round(CO2_val, digits = 2)), color = "black", size = 10) +
  scale_fill_gradientn(limits = c(0, massimo_CO2_Cina), colors = brewer.pal(9, "YlOrRd"), na.value = "grey") +
  coord_fixed(1.3) +
  theme_void() +
  theme(legend.position = "none") + # Rimuovi la legenda
  transition_states(Year, transition_length = 2, state_length = 1) +  # Mostra ogni stato (anno) per 2 secondi
  labs(fill = "Emissioni di CO2 (kiloton kt)", title = "Emissioni medie CO2 in India negli anni {closest_state}", 
       x = NULL, y = NULL) +
  ease_aes('linear')

# Riproduci l'animazione
animation <- animate(India_CO2_Interactive_Map, nframes = 100, fps = 10, width = 960, height = 1080)

# Salva l'animazione come file mp4
anim_save("anim_India_CO2_animation.mp4", animation)

#------4.1) confronto con temperatura China---------------------------------------
China_media_annual_temp <- temp_senza_2020 %>%
  filter(Country == "China") %>%
  group_by(Country, Year) %>%                                                        #raggruppa i dati in base al paese
  summarize(MediaTemperaturaAnnuale = mean(AvgTemperatureCelsius, na.rm = TRUE)) %>%
  na.omit()

#join per associare le coordinate ai paesi alle loro temperature
China_Temp_map <- inner_join(world_countries, China_media_annual_temp, relationship = "many-to-many")

# Crea la mappa della Cina con le temperature medie
China_Temp_Interactive_Map <- ggplot(data = China_Temp_map) +
  geom_polygon(aes(x = long, y = lat, fill = MediaTemperaturaAnnuale, group = group), color = "black", size = 0.05) +
  geom_text(aes(x = mean(long), y = mean(lat), label = round(MediaTemperaturaAnnuale, digits = 2)), color = "black", size = 16) +
  scale_fill_gradientn(
    colors = brewer.pal(9, "YlOrRd"),
    na.value = "grey",
    limits = c(10, 27),  # Imposta i limiti inferiore e superiore
    breaks = seq(10, 27, by = 2)  # Imposta le interruzioni delle scale
  ) +
  coord_fixed(1.3) +
  theme_void() +
  theme(legend.position = "none") + # Rimuovi la legenda
  transition_states(Year, transition_length = 2, state_length = 1) +  # Mostra ogni stato (anno) per 2 secondi
  labs(fill = "Temperatura Media (°C)", title = "Temperatura Media Cina negli anni {closest_state}", 
       x = NULL, y = NULL) +
  ease_aes('linear')

# Riproduci l'animazione
animation <- animate(China_Temp_Interactive_Map, nframes = 100, fps = 10, width = 960, height = 1080)

# Salva l'animazione come file mp4
anim_save("anim_China_temperature_animation.mp4", animation)
#------4.2) confronto con temperatura Russia---------------------------------------
Russia_media_annual_temp <- temp_senza_2020 %>%
  filter(Country == "Russia") %>%
  group_by(Country, Year) %>%                                                        #raggruppa i dati in base al paese
  summarize(MediaTemperaturaAnnuale = mean(AvgTemperatureCelsius, na.rm = TRUE)) %>%
  na.omit()

#join per associare le coordinate ai paesi alle loro temperature
Russia_Temp_map <- inner_join(world_countries, Russia_media_annual_temp, relationship = "many-to-many")

# Crea la mappa della Russia con le temperature medie
Russia_Temp_Interactive_Map <- ggplot(data = Russia_Temp_map) +
  geom_polygon(aes(x = long, y = lat, fill = MediaTemperaturaAnnuale, group = group), color = "black", size = 0.05) +
  geom_text(aes(x = mean(long), y = mean(lat), label = round(MediaTemperaturaAnnuale, digits = 2)), color = "black", size = 16) +
  scale_fill_gradientn(
    colors = brewer.pal(9, "YlOrRd"),
    na.value = "grey",
    limits = c(10, 27),  # Imposta i limiti inferiore e superiore
    breaks = seq(10, 27, by = 2)  # Imposta le interruzioni delle scale
  ) +
  coord_fixed(1.3) +
  theme_void() +
  theme(legend.position = "none") + # Rimuovi la legenda
  transition_states(Year, transition_length = 2, state_length = 1) +  # Mostra ogni stato (anno) per 2 secondi
  labs(fill = "Temperatura Media (°C)", title = "Temperatura Media Russia negli anni {closest_state}", 
       x = NULL, y = NULL) +
  ease_aes('linear')

# Riproduci l'animazione
animation <- animate(Russia_Temp_Interactive_Map, nframes = 100, fps = 10, width = 960, height = 1080)

# Salva l'animazione come file mp4
anim_save("anim_Russia_temperature_animation.mp4", animation)
#------4.3) confronto con temperatura USA---------------------------------------
USA_media_annual_temp <- temp_senza_2020 %>%
  filter(Country == "US") %>%
  group_by(Country, Year) %>%                                                        #raggruppa i dati in base al paese
  summarize(MediaTemperaturaAnnuale = mean(AvgTemperatureCelsius, na.rm = TRUE)) %>%
  na.omit() %>%
  mutate(Country = ifelse(Country == "US", "USA", Country))

#join per associare le coordinate ai paesi alle loro temperature
USA_Temp_map <- inner_join(world_countries, USA_media_annual_temp, relationship = "many-to-many")

# Crea la mappa degli USA con le temperature medie
USA_Temp_Interactive_Map <- ggplot(data = USA_Temp_map) +
  geom_polygon(aes(x = long, y = lat, fill = MediaTemperaturaAnnuale, group = group), color = "black", size = 0.05) +
  geom_text(aes(x = mean(long), y = mean(lat), label = round(MediaTemperaturaAnnuale, digits = 2)), color = "black", size = 16) +
  scale_fill_gradientn(
    colors = brewer.pal(9, "YlOrRd"),
    na.value = "grey",
    limits = c(10, 27),  # Imposta i limiti inferiore e superiore
    breaks = seq(10, 27, by = 2)  # Imposta le interruzioni delle scale
  ) +
  coord_fixed(1.3) +
  theme_void() +
  theme(legend.position = "none") + # Rimuovi la legenda
  transition_states(Year, transition_length = 2, state_length = 1) +  # Mostra ogni stato (anno) per 2 secondi
  labs(fill = "Temperatura Media (°C)", title = "Temperatura Media USA negli anni {closest_state}", 
       x = NULL, y = NULL) +
  ease_aes('linear')

# Riproduci l'animazione
animation <- animate(USA_Temp_Interactive_Map, nframes = 100, fps = 10, width = 960, height = 1080)

# Salva l'animazione come file mp4
anim_save("anim_USA_temperature_animation.mp4", animation)
#------4.4) confronto con temperatura India---------------------------------------
India_media_annual_temp <- temp_senza_2020 %>%
  filter(Country == "India") %>%
  group_by(Country, Year) %>%                                                        #raggruppa i dati in base al paese
  summarize(MediaTemperaturaAnnuale = mean(AvgTemperatureCelsius, na.rm = TRUE)) %>%
  na.omit()

#join per associare le coordinate ai paesi alle loro temperature
India_Temp_map <- inner_join(world_countries, India_media_annual_temp, relationship = "many-to-many")

# Crea la mappa dell'India con le temperature medie
India_Temp_Interactive_Map <- ggplot(data = India_Temp_map) +
  geom_polygon(aes(x = long, y = lat, fill = MediaTemperaturaAnnuale, group = group), color = "black", size = 0.05) +
  geom_text(aes(x = mean(long), y = mean(lat), label = round(MediaTemperaturaAnnuale, digits = 2)), color = "black", size = 16) +
  scale_fill_gradientn(
    colors = brewer.pal(9, "YlOrRd"),
    na.value = "grey",
    limits = c(10, 29),  # Imposta i limiti inferiore e superiore
    breaks = seq(10, 28, by = 2)  # Imposta le interruzioni delle scale
  ) +
  coord_fixed(1.3) +
  theme_void() +
  theme(legend.position = "none") + # Rimuovi la legenda
  transition_states(Year, transition_length = 2, state_length = 1) +  # Mostra ogni stato (anno) per 2 secondi
  labs(fill = "Temperatura Media (°C)", title = "Temperatura Media India negli anni {closest_state}", 
       x = NULL, y = NULL) +
  ease_aes('linear')

# Riproduci l'animazione
animation <- animate(India_Temp_Interactive_Map, nframes = 100, fps = 10, width = 960, height = 1080)

# Salva l'animazione come file mp4
anim_save("anim_India_temperature_animation.mp4", animation)