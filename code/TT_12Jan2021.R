library(tidytuesdayR)
library(tidyverse)

## setting up directories
Dir <- here::here()
dataDir <- "data"
plotDir <- "plots"
# if there are no such directories create them
if(!dir.exists(file.path(Dir, dataDir))){dir.create(file.path(Dir,dataDir))}
if(!dir.exists(file.path(Dir, plotDir))){dir.create(file.path(Dir, plotDir))}

## Loading data
tt_data <- tt_load("2021-01-12")
artists <- tt_data[["artists"]]
artwork <- tt_data[["artwork"]]

## Saving the data for later
write.csv(artists, file.path(Dir,dataDir,"tate_artists.csv"), row.names = FALSE)
write.csv(artwork, file.path(Dir,dataDir,"tate_artwork.csv"), row.names = FALSE)

## Merging data sets
artwork <- artwork %>% rename(artworkId = id)
data <- merge(artists, artwork, by.x="id", by.y = "artistId")
data$gender.fctr <- factor(data$gender, levels = c("Male", "Female"))

## Plots
# Gender balance in acquired work - omitting where gender information is missing (e.g. when artist is "British School")
artwork_gender <- data %>% group_by(acquisitionYear) %>% count(gender.fctr) %>% filter(!is.na(gender.fctr), acquisitionYear >=1900) %>% 
            ggplot() + geom_bar(aes(y = n, x = acquisitionYear, fill = gender.fctr), stat="identity", position = "stack", width = 0.9) + 
            coord_cartesian(ylim=c(0,3000)) + labs(y = "Number of artworks acquired", x = "Year", caption = "Data: Tate Britain | Visualisation: @MaudGrol") + 
            ggtitle("Gender representation in artwork acquired by the Tate (1900-2013)") + scale_x_continuous(limits = c(1900,2013), expand = c(0.005, 0.005)) + 
            scale_y_continuous(expand = c(0, 0)) + scale_fill_viridis_d(name = "Artists' gender", labels = c("male", "female")) + 
            theme_classic(base_size=12) + theme(axis.text=element_text(size=12, colour = "black"), plot.title = element_text(hjust = 0.5))

artwork_gender
#ggsave(file.path(Dir, plotDir, "acquired_work_gender.png"), dpi = 300)
            
# Gender balance in artists whose work is acquired
acq_artists_gender <- data %>% distinct_at(., vars(acquisitionYear,name), .keep_all = TRUE) %>% group_by(acquisitionYear) %>%
            count(gender.fctr) %>% filter(!is.na(gender.fctr), acquisitionYear >=1900) %>%
            ggplot() + geom_bar(aes(y = n, x = acquisitionYear, fill = gender.fctr), stat="identity", position = "stack", width = 0.9) + 
            labs(y = "Number of artists who had artwork acquired", x = "Year", caption = "Data: Tate Britain | Visualisation: @MaudGrol") + ggtitle("Gender disparity in representation of artists (1900-2013)") + 
            scale_x_continuous(limits = c(1900,2013), expand = c(0.005, 0.005)) + scale_y_continuous(expand = c(0, 0)) + 
            scale_fill_viridis_d(name = "Artists' gender", labels = c("male", "female")) + theme_classic(base_size=12) + 
            theme(axis.text=element_text(size=12, colour = "black"), plot.title = element_text(hjust = 0.5))
            
acq_artists_gender            
#ggsave(file.path(Dir, plotDir, "acquired_artists_gender.png"), dpi = 300)

# Gender balance in acquired work - also plotting artworks for which gender of the artist(s) is unknown
incl_na <- data %>% group_by(acquisitionYear) %>% count(gender.fctr) %>% filter(acquisitionYear >=1900) %>% 
            ggplot() + geom_bar(aes(y = n, x = acquisitionYear, fill = gender.fctr), stat="identity", position = "stack", width = 0.9) + 
            coord_cartesian(ylim=c(0,4000)) + labs(y = "Number of artworks acquired", x = "Year", caption = "Data: Tate Britain | Visualisation: @MaudGrol") + 
            ggtitle("Gender representation in artwork acquired by the Tate (1900-2013)") + scale_x_continuous(limits = c(1900,2013), expand = c(0.005, 0.005)) + 
            scale_y_continuous(expand = c(0, 0)) + scale_fill_viridis_d(name = "Artists' gender", na.value = "grey50", labels = c("male", "female", "unknown")) + 
            theme_classic(base_size=12) + theme(axis.text=element_text(size=12, colour = "black"), plot.title = element_text(hjust = 0.5))

incl_na
#ggsave(file.path(Dir, plotDir, "plot_incl_na.png"), dpi = 300)

