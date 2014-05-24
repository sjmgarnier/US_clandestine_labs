require("XML")
require("tm")
require("data.table")
require("dplyr")
require("stringr")
require("maps")
require("ggplot2")
if (!require("graphZoo")) {
  require("devtools")
  install_github("morpionZ/graphZoo")
}
library(extrafont)
loadfonts()


#+ load.data, echo=FALSE
if (file.exists("data/clan-lab.csv")) {
  lab.data <- fread("data/clan-lab.csv")
}


#+ download.raw.data, echo=FALSE
if (!file.exists("data/clan-lab.csv")) {
  dir.create("data/clan-lab", showWarnings = FALSE)
  parsed.html <- htmlParse("http://www.justice.gov/dea/clan-lab/clan-lab.shtml")
  files <- xpathSApply(parsed.html, "//a/@href[contains(.,'clan-lab') and contains(.,'.pdf')]")
  lapply(files, FUN = function(file) {
    url <- paste0("http://www.justice.gov/dea/", file)
    dest <- paste0("data/", file)
    if (file.exists(dest)) {
      return(0)
    } else {
      download.file(url, dest)
    }
  })
}


#+ parse.raw.data, echo=FALSE
if (!file.exists("data/clan-lab.csv")) {
  pdfReader <- readPDF(control = list(text = "-layout"))
  lab.data <- data.table()
  
  for (i in 1:length(files)) {
    raw <- pdfReader(elem = list(uri = paste0("data/", files[i])), language = "en", id = "id1")
    
    if (any(grepl("No reported seizures", raw))) {} else {
      procd <- as.data.table(raw) %>%
        mutate(STATE = toupper(word(raw[1], start = -1, end = -1))) %>%
        filter(raw != "", 
               !grepl("COUNTY\\s{2,}CITY", raw), 
               !grepl("\f", raw),
               !grepl("National Clandestine", raw), 
               !grepl("[0-9] of [0-9]", raw),
               str_count(raw, "\\s{2,}") == 3) %>%
        mutate(COUNTY = word(raw, start = 1, end = 1, sep = "\\s{2,}"),
               CITY = word(raw, start = 2, end = 2, sep = "\\s{2,}"),
               ADDRESS = word(raw, start = 3, end = 3, sep = "\\s{2,}"),
               DATE = word(raw, start = 4, end = 4, sep = "\\s{2,}")) %>%
        mutate(DATE = as.Date(DATE, "%m/%d/%Y")) %>%
        select(-raw)
      
      lab.data <- rbind(lab.data, procd)
    }
  }
}


#+ geocode.data, echo=FALSE
if (!file.exists("data/clan-lab.csv")) {
  geocodeMapQuest <- function(key, street, city, county, state) {
    require("data.table")
    
    url <- paste0("http://www.mapquestapi.com/geocoding/v1/address?&key=", 
                  key, "&street=", street, "&city=", city, "&state=", state,
                  "&outFormat=csv&maxResults=1&thumbMaps=false")
    url <- gsub(" ","+", url)
    
    entry <- fread(url)
    
    data.frame(LAT = as.numeric(entry$Lat),
               LON = as.numeric(entry$Lng))
  } 
  
  key <- readline(prompt = "Provide MapQuest API key to continue: ")
  
  lab.data <- lab.data %>%
    group_by(STATE, COUNTY, CITY, ADDRESS, DATE) %>%
    do(geocodeMapQuest(key, ADDRESS, CITY, COUNTY, STATE)) %>%
    ungroup()
  
  write.csv(lab.data, "data/clan-lab.csv")
}


#+ plot.map, echo=TRUE
us.state.map <- map_data('state')
xlim <- range(us.state.map$long)
xlim <- xlim + diff(xlim) * c(-.05, .05)
ylim <- range(us.state.map$lat)
ylim <- ylim + diff(xlim) * c(-.05, .05)
subtitle <- paste0("Drug Enforcement Administration (", nrow(lab.data), " locations between ", 
                   year(min(lab.data$DATE, na.rm = TRUE)), " and ", 
                   year(max(lab.data$DATE, na.rm = TRUE)), ")")

g <- ggplot() +
  geom_polygon(data = us.state.map, 
               aes(x = long, y = lat, group = group),
               color = "white", 
               fill = "slategray4") +
  geom_point(data = lab.data,
             aes(x = LON, y = LAT),
             color = "firebrick4",
             alpha = .5) +
  coord_fixed(xlim = xlim, ylim = ylim) +
  theme_graphzoo(base_size = 28, family = "Ume P Gothic") +
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(1, 0, 0, -1), "lines")) +
  ggtitle(bquote(atop("Approximate locations of suspected clandestine laboratories", 
                          atop(italic(.(subtitle)), ""))))

g <- addBanner(g, font.size = 5.83,
               l.txt = "GRAPHZOO.TUMBLR.COM", r.txt = "SOURCE: DRUG ENFORCEMENT ADMINISTRATION")

png("clan-lab.png", width = 1200, height = 700, bg = "#F0F0F0")
g
dev.off()




