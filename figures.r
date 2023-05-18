## Heatmap of restaurants with B/C food safety ratings
## Author: Jaymie Park

pacman::p_load(data.table, tidyverse, ggplot2, leaflet, sf, ggpubr, ggthemes, ggrepel, rio, gt, webshot2)


## Load prepped data - GPS coordinates
df <- import("data/prepped_list.rds")

## Subset out those that didn't match - most likely restaurant turnovers
## N=114 ## Check this out....
df <- df[!is.na(id)]

## Set lat-long
df$lat <- df$coordinates.latitude %>% as.numeric
df$long <- df$coordinates.longitude %>% as.numeric

## Keep ratings from 2017-2018
df <- df[activity_date >= "2017-01-01"]

## Remove duplicate facility ids - keep most recent
df[, n_id := .N, by=facility_id]
df <- df %>% arrange(facility_id, desc(activity_date))
df[, i_id := seq_len(.N), by=facility_id]
df <- df[i_id==1]

## Split restaurant size and risk
df[, size := tstrsplit(pe_description, "\\SEATS ")[1]]
df[, size := gsub("RESTAURANT |\\(|\\)", "", size) %>% trimws]
df[, size := factor(size, c("0-30", "31-60",  "61-150", "151 +"), c("0-30", "31-60",  "61-150", "151+"), ordered=T)]
df[, risk := tstrsplit(pe_description, "\\SEATS ")[2]]


## Load shapefile
shp <- read_sf("data/LA_Times_Neighborhood_Boundaries/LA_Times_Neighborhood_Boundaries.shp")


## Convert to SF
df_sf <- df %>%
  st_as_sf(
    coords = c("long", "lat"),
    crs = 4326,      
    stringsAsFactors = FALSE,
    remove = TRUE
  )

## Find point in polygon
mf <- st_join(df_sf, shp, join = st_within) %>% as.data.table
## Count
mf[, n := .N, by=.(name)]
sf <- mf[, .(n, name)] %>% unique %>%
  filter(!is.na(name)) %>% arrange(-n)

## Barplot of counts by neighborhood
sf %>%
  ggplot(aes(y=fct_reorder(name, n), x=n, label=n, fill=n)) +
  geom_text(hjust=-0.5) +
  geom_bar(stat="identity") +
  scale_fill_continuous(low="thistle2", high="darkred", na.value="white") +
  theme_pubr() +
  guides(fill="none") + 
  labs(title="Number of restaurants with 'B' or 'C' safety grade rating by LA neighborhood (2017-18)", 
       subtitle="Total of 309 restaurants with 'B' or 'C' rating",
       caption="Source: LA City Restaurant and Market Health Inspections (data.lacity.org)", 
       x="Count", y="") +
  theme(plot.title = element_text(face="bold"))

ggsave("figures/barplot.png", w=12, h=10)

## Table of restaurant risk by neighborhood
t <- mf[, .(n=.N), .(name, risk)] %>% dcast(name ~ risk, value.var="n")
t <- t %>% setcolorder(c("name", "HIGH RISK", "MODERATE RISK", "LOW RISK"))
t <- t[!is.na(name)]
t[is.na(`HIGH RISK`), `HIGH RISK` := 0]
t[is.na(`MODERATE RISK`), `MODERATE RISK` := 0]
t[is.na(`LOW RISK`), `LOW RISK` := 0]

t %>% 
  gt() %>%
  cols_label(name="Neighborhood") %>%
  tab_header(title="Number of restaurants with B, C ratings by neighborhood, risk (2017-2018)") %>%
  tab_footnote(footnote="Source: LA City Restaurant and Market Health Inspections (data.lacity.org)") %>%
  gtsave("figures/table_riskneighborhood.png", expand = 10)

## Table of restaurant size, risk and rating
t <- df[, .(n=.N), .(risk, size, grade)]
t <- t[, .(n, risk, size, grade)] %>% dcast(risk + size ~ grade, value.var="n")
t <- t %>% arrange(risk, size)
t[is.na(C), C := 0]

t %>% 
  gt() %>%
  cols_label(risk="Risk grouping", size="Number of seats", B="Number with 'B' score", C="Number with 'C' score") %>%
  tab_header(title="Number of restaurants with B, C safety ratings by restaurant risk, size in LA (2017-2018)") %>%
  tab_footnote(footnote="Source: LA City Restaurant and Market Health Inspections (data.lacity.org)") %>%
  gtsave("figures/table_risksize.png", expand = 10)

## Heatmap
map_sf <- sf %>%
  merge(shp, ., by="name", all.x=T) %>%
  arrange(desc(n))

## Select top few to label
centroids <- st_centroid(map_sf[1:7,]) 
centroids$lat <- st_coordinates(centroids)[, 2]
centroids$long <- st_coordinates(centroids)[, 1]

p2 <- ggplot(map_sf, aes(fill=n)) +
  geom_sf() +
  geom_sf(data=centroids, size=1) + 
  geom_label_repel(data=centroids, aes(x=long, y=lat, label=name), fontface="bold", 
                   nudge_x = c(-0.02, 0.05, 0, -0.1, 0.15, -0.02, -0.02), 
                   nudge_y = c(0.08, -0.05, 0.05, -0.02, 0, -0.05, 0.05), fill="white")+  
  theme_map() +
  scale_fill_continuous(low="thistle2", high="darkred", na.value="white") +
  guides(fill=guide_colorbar(barwidth=10, title.theme = element_text(size=10))) + 
  labs(title="Heatmap of restaurants with 'B' or 'C' safety grade rating by LA neighborhood (2017-18)", 
       caption="Source: LA City Restaurant and Market Health Inspections (data.lacity.org)",
       fill="Count") + 
  theme(legend.position="bottom", legend.justification = c(1), plot.title = element_text(face="bold")) 

ggsave("figures/heatmap.png", h=8, w=8)


