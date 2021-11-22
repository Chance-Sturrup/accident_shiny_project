DataOrganize <- function(id) {
  id <- na.omit(id)
  id <- mutate(id, Weather = wthr.cond)
  id <- id %>% mutate(Weather = replace(Weather, Weather %in% c("Fair","Cloudy",  "Mostly Cloudy",  "Partly Cloudy", "N/A Precipitation", "Overcast", "Clear", "Scattered Clouds"), "Fair / Cloudy"))
  id <- id %>% mutate(Weather = replace(Weather, Weather %in% c("Light Rain","Rain / Windy","Heavy Rain",  "Light Drizzle", "Light Rain / Windy","Drizzle", "Rain", "Light Rain Shower","Heavy Rain / Windy",  "Showers in the Vicinity","Heavy Drizzle", "Light Drizzle / Windy"), "Rain"))
  id <- id %>% mutate(Weather = replace(Weather, Weather %in% c("Light Snow","Heavy Snow","Wintry Mix", "Light Snow / Windy","Snow / Windy","Light Snow and Sleet","Snow and Sleet","Heavy Snow / Windy","Blowing Snow / Windy","Blowing Snow","Wintry Mix / Windy","Light Snow with Thunder","Snow and Sleet / Windy","Heavy Snow with Thunder"), "Snow"))
  id <- id %>% mutate(Weather = replace(Weather, Weather %in% c("Haze", "Mist","Shallow Fog","Patches of Fog","Fog / Windy", "Haze / Windy","Drizzle and Fog","Partial Fog", "Smoke","Smoke / Windy","Light Freezing Fog"), "Fog"))
  id <- id %>% mutate(Weather = replace(Weather, Weather %in% c("Fair / Wind","Cloudy / Windy","Partly Cloudy / Windy","Mostly Cloudy / Windy","Blowing Dust / Windy","Blowing Dust","Sand / Dust Whirlwinds","Thunder / Windy","Squalls / Windy","Thunder and Hail / Windy","Tornado", "Fair / Windy"), "Windy"))
  id <- id %>% mutate(Weather = replace(Weather, Weather %in% c("Heavy T-Storm","T-Storm","Thunder in the Vicinity","T-Storm / Windy","Thunder","Light Rain with Thunder","Heavy T-Storm / Windy","Heavy Thunderstorms and Rain","Light Thunderstorms and Rain","Thunderstorms and Rain"), "Thunder Storm"))
  id <- id %>% mutate(Weather = replace(Weather, Weather %in% c("Light Freezing Drizzle", "Light Freezing Rain","Freezing Rain","Small Hail","Ice Pellets","Light Ice Pellets","Sleet"), "Ice / Freezing Rain"))
  
}
