#install.packages("data.table")
library(data.table)

# Read in raw data set
storm.data.raw <- read.csv("stormData.csv.bz2")

# Subset required variables from raw data
storm.data.subset <- data.frame(Storm = storm.data.raw$EVTYPE, 
                                Fatalities = storm.data.raw$FATALITIES, 
                                Injuries = storm.data.raw$INJURIES, 
                                Property.Damage.Number = storm.data.raw$PROPDMG,
                                Property.Damage.Magnitude = storm.data.raw$PROPDMGEXP, 
                                Crop.Damage.Number = storm.data.raw$CROPDMG,
                                Crop.Damage.Magnitude = storm.data.raw$CROPDMGEXP)

# Convert character values to numeric value
storm.data.subset$Property.Damage.Magnitude <- gsub("K", "1000", storm.data.subset$Property.Damage.Magnitude)
storm.data.subset$Property.Damage.Magnitude <- gsub("k", "1000", storm.data.subset$Property.Damage.Magnitude)
storm.data.subset$Property.Damage.Magnitude <- gsub("M", "1000000", storm.data.subset$Property.Damage.Magnitude)
storm.data.subset$Property.Damage.Magnitude <- gsub("m", "1000000", storm.data.subset$Property.Damage.Magnitude)
storm.data.subset$Property.Damage.Magnitude <- gsub("B", "1000000000", storm.data.subset$Property.Damage.Magnitude)
storm.data.subset$Property.Damage.Magnitude <- gsub("b", "1000000000", storm.data.subset$Property.Damage.Magnitude)
storm.data.subset$Crop.Damage.Magnitude <- gsub("K", "1000", storm.data.subset$Crop.Damage.Magnitude)
storm.data.subset$Crop.Damage.Magnitude <- gsub("k", "1000", storm.data.subset$Crop.Damage.Magnitude)
storm.data.subset$Crop.Damage.Magnitude <- gsub("M", "1000000", storm.data.subset$Crop.Damage.Magnitude)
storm.data.subset$Crop.Damage.Magnitude <- gsub("m", "1000000", storm.data.subset$Crop.Damage.Magnitude)
storm.data.subset$Crop.Damage.Magnitude <- gsub("B", "1000000000", storm.data.subset$Crop.Damage.Magnitude)
storm.data.subset$Crop.Damage.Magnitude <- gsub("b", "1000000000", storm.data.subset$Crop.Damage.Magnitude)

# Calculate Property Damages
storm.data.subset["Property.Damage"] <- as.numeric(storm.data.subset$Property.Damage.Magnitude) * storm.data.subset$Property.Damage.Number

# Calculate Crop Damages
storm.data.subset["Crop.Damage"] <- as.numeric(storm.data.subset$Crop.Damage.Magnitude) * storm.data.subset$Crop.Damage.Number

# Remove NAs from Damages
storm.data.subset$Property.Damage[is.na(storm.data.subset$Property.Damage)] <- 0
storm.data.subset$Crop.Damage[is.na(storm.data.subset$Crop.Damage)] <- 0

# Consolidate variables in subset
storm.data.subset <- data.frame(Storm = storm.data.subset$Storm, 
                                Fatalities = storm.data.subset$Fatalities, 
                                Injuries = storm.data.subset$Injuries, 
                                Property.Damage = storm.data.subset$Property.Damage, 
                                Crop.Damage = storm.data.subset$Crop.Damage)

# Convert storm.data.working from a data frame to a table
storm.data.final <- data.table(storm.data.subset[1:5])

# Sum all other columns, group by Storm variable
storm.data.final <- storm.data.final[, lapply(.SD, sum), by = Storm]

# Turn the Storm variable into a character vector
storm.data.final$Storm <- as.character(storm.data.final$Storm)

# Manually rename the most redundant variables in the set via Grepl
storm.data.final$Storm[with(storm.data.final,  grepl("summary|record|monthly", Storm, ignore.case = TRUE))] <- "EXCLUDE"
storm.data.final$Storm[with(storm.data.final,  grepl("TSTM", Storm, ignore.case = TRUE) & grepl("NON", Storm, ignore.case = TRUE))] <- "Wind"
storm.data.final$Storm[with(storm.data.final,  grepl("TSTM|thunderstorm|THUDERSTORM|THUNDEERSTORM|THUDERSTORM|THUNDERSTROM|THUNDERESTORM|THUNDERTORM|THUNDERTSORM|THUNDESTORM|THUNERSTORM|TUNDERSTORM", Storm, ignore.case = TRUE) & grepl("WND|WIND|W INDS|WINS", Storm, ignore.case = TRUE))] <- "Thunderstorm Wind"
storm.data.final$Storm[with(storm.data.final,  grepl("TSTM|thunderstorm|THUDERSTORM|THUNDEERSTORM|THUDERSTORM|THUNDERSTROM|THUNDERESTORM|THUNDERTORM|THUNDERTSORM|THUNDESTORM|THUNERSTORM|TUNDERSTORM", Storm, ignore.case = TRUE) & !grepl("wind", Storm, ignore.case = TRUE))] <- "Thunderstorms"
storm.data.final$Storm[with(storm.data.final,  grepl("strong", Storm, ignore.case = TRUE) & grepl("wind", Storm, ignore.case = TRUE))] <- "Strong Winds"
storm.data.final$Storm[with(storm.data.final,  grepl("snow", Storm, ignore.case = TRUE) & grepl("squall", Storm, ignore.case = TRUE))] <- "Snow Squalls"
storm.data.final$Storm[with(storm.data.final,  grepl("ice", Storm, ignore.case = TRUE) & grepl("storm", Storm, ignore.case = TRUE))] <- "Ice Storm"
storm.data.final$Storm[with(storm.data.final,  grepl("hurricane", Storm, ignore.case = TRUE))] <- "Hurricanes"
storm.data.final$Storm[with(storm.data.final,  grepl("devel|devil", Storm, ignore.case = TRUE))] <- "Dust Devils"
storm.data.final$Storm[with(storm.data.final,  grepl("dust", Storm, ignore.case = TRUE) & grepl("storm", Storm, ignore.case = TRUE))] <- "Dust Storms" 
storm.data.final$Storm[with(storm.data.final,  grepl("high", Storm, ignore.case = TRUE) & grepl("wind", Storm, ignore.case = TRUE))] <- "High Winds"
storm.data.final$Storm[with(storm.data.final,  grepl("flash", Storm, ignore.case = TRUE))] <- "Flash Flood"
storm.data.final$Storm[with(storm.data.final,  grepl("flood", Storm, ignore.case = TRUE) & grepl("coastal|minor|urban|river|highway|erosion|cstl|beach", Storm, ignore.case = TRUE))] <- "Other Flood"
storm.data.final$Storm[with(storm.data.final,  grepl("blizzard", Storm, ignore.case = TRUE))] <- "Blizzard"
storm.data.final$Storm[with(storm.data.final,  grepl("snow", Storm, ignore.case = TRUE))] <- "Snow-Related"
storm.data.final$Storm[with(storm.data.final,  grepl("microburst|micoburst|MIRCOBURST", Storm, ignore.case = TRUE))] <- "Microbursts"
storm.data.final$Storm[with(storm.data.final,  grepl("tornado|TORNDAO", Storm, ignore.case = TRUE))] <- "Tornadoes"
storm.data.final$Storm[with(storm.data.final,  grepl("cold|chill|frost|freeze|HYPOTHERMIA", Storm, ignore.case = TRUE))] <- "Cold/Frost Conditions"
storm.data.final$Storm[with(storm.data.final,  grepl("spout", Storm, ignore.case = TRUE) & grepl("water|WAYTER", Storm, ignore.case = TRUE))] <- "Waterspouts"
storm.data.final$Storm[with(storm.data.final,  grepl("fire", Storm, ignore.case = TRUE))] <- "Fire Conditions (Wild, Forest, Brush)"
storm.data.final$Storm[with(storm.data.final,  grepl("dry", Storm, ignore.case = TRUE))] <- "Dryness"
storm.data.final$Storm[with(storm.data.final,  grepl("freez|sleet", Storm, ignore.case = TRUE) & grepl("rain", Storm, ignore.case = TRUE))] <- "Freezing Rain"
storm.data.final$Storm[with(storm.data.final,  grepl("cloud|funnel", Storm, ignore.case = TRUE))] <- "Funnel/Wall Clouds"
storm.data.final$Storm[with(storm.data.final,  grepl("hail", Storm, ignore.case = TRUE))] <- "Hail"
storm.data.final$Storm[with(storm.data.final,  grepl("avalance|avalanche", Storm, ignore.case = TRUE))] <- "Avalanche"
storm.data.final$Storm[with(storm.data.final,  grepl("erosion|erosin", Storm, ignore.case = TRUE))] <- "Erosion"
storm.data.final$Storm[with(storm.data.final,  grepl("ice|icy", Storm, ignore.case = TRUE) & !grepl("storm", Storm, ignore.case = TRUE))] <- "Ice-Related"
storm.data.final$Storm[with(storm.data.final,  grepl("drought", Storm, ignore.case = TRUE))] <- "Drought"
storm.data.final$Storm[with(storm.data.final,  grepl("flood|urban|fld", Storm, ignore.case = TRUE) & !grepl("flash", Storm, ignore.case = TRUE))] <- "Flooding"
storm.data.final$Storm[with(storm.data.final,  grepl("WND|WIND|W INDS|WINS", Storm, ignore.case = TRUE) & !grepl("Thunderstorm", Storm, ignore.case = TRUE))] <- "Winds"
storm.data.final$Storm[with(storm.data.final,  grepl("winter|wintry", Storm, ignore.case = TRUE))] <- "Wintry Weather/Mix"
storm.data.final$Storm[with(storm.data.final,  grepl("tropical", Storm, ignore.case = TRUE) & grepl("storm", Storm, ignore.case = TRUE))] <- "Tropical Storms"
storm.data.final$Storm[with(storm.data.final,  grepl("rip", Storm, ignore.case = TRUE))] <- "Rip Currents"
storm.data.final$Storm[with(storm.data.final,  grepl("mud", Storm, ignore.case = TRUE) & grepl("slide", Storm, ignore.case = TRUE))] <- "Mudslides" 
storm.data.final$Storm[with(storm.data.final,  grepl("lightning|LIGNTNING|LIGHTING", Storm, ignore.case = TRUE))] <- "Lightning"
storm.data.final$Storm[with(storm.data.final,  grepl("landslide", Storm, ignore.case = TRUE))] <- "Landslides"
storm.data.final$Storm[with(storm.data.final,  grepl("rain|shower|precip", Storm, ignore.case = TRUE) & !grepl("freezing", Storm, ignore.case = TRUE))] <- "Precipitation-related"
storm.data.final$Storm[with(storm.data.final,  grepl("heat", Storm, ignore.case = TRUE))] <- "Heat-related"
storm.data.final$Storm[with(storm.data.final,  grepl("surge", Storm, ignore.case = TRUE) & grepl("storm", Storm, ignore.case = TRUE))] <- "Storm Surge"
storm.data.final$Storm[with(storm.data.final,  grepl("vol", Storm, ignore.case = TRUE) & grepl("ash", Storm, ignore.case = TRUE))] <- "Volcanic Ash"

storm.data.final <- storm.data.final[, lapply(.SD, sum), by = Storm]

# Write final data set to csv
write.csv(storm.data.final, file = "stormDataFinal.csv")
