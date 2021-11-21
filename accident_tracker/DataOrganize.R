DataOrganize <- function(id) {
  id <- na.omit(id)
  id <- mutate(id,Accident_Time = time)
  id <- id %>% separate(time, c("Year", "Month","Date","Actualtime"))
  id$Actualtime<- as.numeric(as.character(id$Actualtime))
  id <- mutate(id, Weather = wthr.cond)
  id <- id %>% mutate(Weather = replace(Weather, Weather %in% c("Fair","Cloudy",  "Mostly Cloudy",  "Partly Cloudy", "N/A Precipitation", "Overcast", "Clear", "Scattered Clouds"), "Fair / Cloudy"))
  id <- id %>% mutate(Weather = replace(Weather, Weather %in% c("Light Rain","Rain / Windy","Heavy Rain", "Heavy T-Storm", "Light Drizzle","T-Storm", "Light Rain / Windy","Drizzle","Thunder in the Vicinity", "Rain", "Light Rain Shower","Heavy Rain / Windy", "Light Freezing Drizzle", "Light Freezing Rain","Freezing Rain", "Showers in the Vicinity","T-Storm / Windy","Heavy Drizzle", "Light Rain with Thunder","Light Drizzle / Windy","Thunder","Small Hail", "Heavy T-Storm / Windy", "Ice Pellets", "Light Ice Pellets","Heavy Thunderstorms and Rain","Light Thunderstorms and Rain","Thunderstorms and Rain"), "Rain / Thunder"))
  id <- id %>% mutate(Weather = replace(Weather, Weather %in% c("Light Snow","Heavy Snow","Wintry Mix", "Light Snow / Windy","Snow / Windy","Sleet","Light Snow and Sleet","Snow and Sleet","Heavy Snow / Windy","Blowing Snow / Windy","Blowing Snow","Wintry Mix / Windy","Light Snow with Thunder","Snow and Sleet / Windy","Heavy Snow with Thunder"), "Snow"))
  id <- id %>% mutate(Weather = replace(Weather, Weather %in% c("Haze", "Mist","Shallow Fog","Patches of Fog","Fog / Windy", "Haze / Windy","Drizzle and Fog","Partial Fog", "Smoke","Smoke / Windy","Light Freezing Fog"), "Fog"))
  id <- id %>% mutate(Weather = replace(Weather, Weather %in% c("Fair / Wind","Cloudy / Windy","Partly Cloudy / Windy","Mostly Cloudy / Windy","Blowing Dust / Windy","Blowing Dust","Sand / Dust Whirlwinds","Thunder / Windy","Squalls / Windy","Thunder and Hail / Windy","Tornado", "Fair / Windy"), "Windy"))
  
}
