################################################
# Cities by rates of single-moms and families, 
# and percent change from 2012-2015 Analysis
################################################

rm(list=ls())

# Set Directory
setwd("~/Google Drive/PRTeam Analysis/R Scripts/")
source("./Connecting to Redfin db in R.R") #Redfin only: data connections
setwd("~/Google Drive/PRTeam Analysis/Misc/2016-09 Misc/")
if(!file.exists("./single_mothers")){dir.create("./single_mothers")}
setwd("./single_mothers/")

# Load Packages
packagesRequired <- c('data.table',
                      'stringr',
                      'readr',
                      'dplyr',
                      'tidyr',
                      'tigris', 
                      'acs',  
                      'maptools',
                      'leaflet',
                      'rgdal',
                      'plotly')

packagesMissing <- packagesRequired[!(packagesRequired %in% installed.packages())]
for (package in packagesMissing) {
    install.packages(package)
}
for (package in packagesRequired){
    eval(parse(text=paste0('library(', package, ')')))
}

# create a geographic set to grab tabular data (acs)
geo<-geo.make(state="*", place="*")

# Use acs.lookup to find table.number
# acs.lookup(2014, table.name = "Family Type by Presence and Age of Related Children", case.sensitive = F, span = 1)
# B11004
# acs.lookup(2014, table.name = "total population", case.sensitive = F)
# B01003

family_types_12 <-acs.fetch(endyear = 2012, span = 1, geography = geo,
                table.number = "B11004", col.names = "pretty")

total_pop_12 <-acs.fetch(endyear = 2012, span = 1, geography = geo,
                      table.number = "B01003", col.names = "pretty")

family_types_15 <-acs.fetch(endyear = 2015, span = 1, geography = geo,
                         table.number = "B11004", col.names = "pretty")

total_pop_15 <-acs.fetch(endyear = 2015, span = 1, geography = geo,
                         table.number = "B01003", col.names = "pretty")

# See link below for creating the GEOID
# https://www.census.gov/geo/reference/geoidentifiers.html

# Just grab relevant columns of total number of 
# single moms, dads, and married couples with children under 18
columns<-c(3,10,16)

family_types_12_df <- data.frame(paste0(str_pad(family_types_12@geography$state, 2, "left", pad="0"), 
                                     str_pad(family_types_12@geography$place, 5, "left", pad="0")), 
                                 family_types_12@geography$NAME,
                              family_types_12@estimate[,columns], 
                              stringsAsFactors = FALSE)

total_pop_12_df <- data.frame(paste0(str_pad(total_pop_12@geography$state, 2, "left", pad="0"), 
                                  str_pad(total_pop_12@geography$place, 5, "left", pad="0")), 
                              total_pop_12@estimate, 
                        stringsAsFactors = FALSE)

family_types_15_df <- data.frame(paste0(str_pad(family_types_15@geography$state, 2, "left", pad="0"), 
                                     str_pad(family_types_15@geography$place, 5, "left", pad="0")), 
                                 family_types_15@geography$NAME,
                              family_types_15@estimate[,columns], 
                              stringsAsFactors = FALSE)

total_pop_15_df <- data.frame(paste0(str_pad(total_pop_15@geography$state, 2, "left", pad="0"), 
                                  str_pad(total_pop_15@geography$place, 5, "left", pad="0")),
                              total_pop_15@estimate, 
                              stringsAsFactors = FALSE)

# rename columns
names(family_types_12_df) <- c("GEOID", "city", "num_of_married_couples_with_children", "num_of_single_dads_with_children", "num_of_single_moms_with_children")

names(total_pop_12_df) <-c("GEOID", "total_population")

names(family_types_15_df) <- c("GEOID", "city", "num_of_married_couples_with_children", "num_of_single_dads_with_children", "num_of_single_moms_with_children")

names(total_pop_15_df) <-c("GEOID", "total_population")

# Join all data
family_types_12_df <- merge(family_types_12_df, total_pop_12_df, by="GEOID")

family_types_15_df<- merge(family_types_15_df, total_pop_15_df, by="GEOID")

family_types_all <- merge(family_types_12_df, family_types_15_df, by=c("GEOID", "city"), suffixes = c("_12","_15"))

family_types_all$city <-str_replace_all(family_types_all$city, " city,", ",")

# Calculate all variables of interest
family_types_all_changes <-family_types_all %>%
    mutate(parents_12 = num_of_single_moms_with_children_12 + 
               num_of_single_dads_with_children_12 +
               num_of_married_couples_with_children_12,
           parents_15 = num_of_single_moms_with_children_15 + 
               num_of_single_dads_with_children_15 + 
               num_of_married_couples_with_children_15,
           percent_parents_12 = parents_12/total_population_12,
           percent_parents_15 = parents_15/total_population_15,
           percent_single_mom_12 = num_of_single_moms_with_children_12/total_population_12,
           percent_single_mom_15 = num_of_single_moms_with_children_15/total_population_15,
           percentage_change_single_mom = percent_single_mom_15/percent_single_mom_12 - 1,
           percentage_change_num_single_moms = num_of_single_moms_with_children_15/num_of_single_moms_with_children_12 - 1,
           percentage_change_parents = percent_parents_15/percent_parents_12 - 1,
           percentage_change_num_parents = parents_15/parents_12-1,
           gross_population_change = total_population_15 - total_population_12,
           percentage_change_population = total_population_15/total_population_12 - 1) %>%
    arrange(desc(percentage_change_single_mom))

# Calculate overall values across all cities with data
summary_natl <-summarise(family_types_all_changes, 
          natl_population_cities = sum(total_population_15, na.rm = T),
          natl_share_single_moms = sum(num_of_single_moms_with_children_15, na.rm = T)/
              sum(total_population_15, na.rm = T),
          percentage_change_single_mom = as.numeric(natl_share_single_moms)/
              as.numeric((sum(num_of_single_moms_with_children_12, na.rm = T))/
                  as.numeric(sum(total_population_12, na.rm = T))) - 1,
          natl_share_parents = sum(parents_15, na.rm = T)/
              sum(total_population_15, na.rm = T),
          percentage_change_parents = as.numeric(natl_share_parents)/
              as.numeric((sum(parents_12, na.rm = T))/
                  as.numeric(sum(total_population_12, na.rm = T))) - 1
          )

# Save output
write_csv(family_types_all_changes, "family_types_all_changes.csv")

# Filter for cities with more than 200k people that had an increase
# in the share of single-mom households from 2012 to 2015
final_output <- family_types_all_changes %>%
    filter(percentage_change_single_mom > 0,
    total_population_15 >= 200000)

# Filter for cities with more than 200k people that had an increase
# in the share of family households from 2012 to 2015
final_output_families <- family_types_all_changes %>%
    filter(percentage_change_parents > 0,
           total_population_15 >= 200000)

########################################
# Redfin only data
########################################

# Grab median sale price in July 2016 for each city from data center
query <-"
select median_sale_price, city || ', ' || state as place, table_id
from rf_temp.agg_region_datacenter_calc
where period_begin = '2016-07-01'
and region_type = 'place'
and property_type = 'All Residential'
"

# From Redshift
median_sale_prices <- dbGetQuery(rscon, statement = query)

# Grab lat/longs for d3 visual
place_lat_longs <-"
select place_id,
display_name,
st_y(st_centroid(redfin_polygon_data)) as lat, 
st_x(st_centroid(redfin_polygon_data)) as lng
from places
where place_type_id in (1, 2)
"

# From CQ-5
place_lat_longs_data <- dbGetQuery(con, statement = place_lat_longs)

final_output <- merge(final_output, median_sale_prices, by.x ="city", by.y="place")

# Output for d3 of single-mom household growth cities
final_output_csv <- merge(final_output, place_lat_longs_data, by.x ="table_id", by.y="place_id")

final_output_csv <- final_output_csv %>%
    arrange(desc(percentage_change_single_mom)) %>%
    dplyr::select(city, percentage_change_single_mom, median_sale_price, lat, lng)

# Save csv to directory
write_csv(final_output_csv, "cities_with_pct_increase_in_single_moms.csv")

# Output for d3 of family household growth cities
final_output_families <- merge(final_output_families, median_sale_prices, by.x ="city", by.y="place")

final_output_families_csv <- merge(final_output_families, place_lat_longs_data, by.x ="table_id", by.y="place_id")

final_output_families_csv <- final_output_families_csv %>%
    arrange(desc(percentage_change_parents)) %>%
    dplyr::select(city, percentage_change_parents, median_sale_price, lat, lng)

# Save csv to directory
write_csv(final_output_families_csv, "cities_with_pct_increase_in_families.csv")

# Data used for creating HTML tables of top 10
tbl <- final_output %>%
    arrange(desc(percentage_change_single_mom)) %>%
    mutate(ranking = rank(desc(percentage_change_single_mom))) %>%
    dplyr::select(ranking, city, table_id, percentage_change_single_mom, median_sale_price, percent_single_mom_15) %>%
    mutate(percentage_change_single_mom = round(percentage_change_single_mom*100,1),
           percent_single_mom_15 = round(percent_single_mom_15*100,1))


setDT(tbl)

# Create HTML code
html <- NULL
html <- "
<h3>Cities with the Largest Increase of Single-Mom Households</h3>
<table class='ScienceTable' style = 'font-size: 14px;'>
<thead>
<tr style='height: 14px !important;'>
<th style='text-align: left' class='c0 '>Rank</th>
<th style='text-align: left' class='c1 '>City</th>
<th style='text-align: left' class='c2 '>Share of Single-Mom Households</th>
<th style='text-align: left' class='c3 '>Share of Single-Mom Households<br/>Percent Change (2012-15)</th>
<th style='text-align: left' class='c4 '>Median Sale Price<br/>July 2016</th>
</tr>
</thead>
<tbody>"

# loop through top 10 writing HTML code
for (i in 1:10) {
    html <- paste0(html, "
                   <tr class=''>
                   <td style='text-align: left' class='c0 '>", tbl[i, 1, with = FALSE], "</td>
                   <td style='text-align: left' class='c1 '><a href=https://www.redfin.com/city/", tbl[i, 3, with = FALSE], "/?utm_source=blog&utm_medium=post&utm_content=real_estate&utm_campaign=1002170'>", tbl[i, 2, with = FALSE], "</a></td>
                    <td style='text-align: left' class='c2 '>", paste0(tbl[i, 6, with = FALSE], "%"), "</td>                   
                    <td style='text-align: left' class='c3 '>", paste0(tbl[i, 4, with = FALSE], "%"), "</td>
                   <td style='text-align: left' class='c4 '>",
                        if(!is.na(tbl[i, 5, with = FALSE])){
                            paste0("$", prettyNum(tbl[i, 5, with = FALSE], big.mark = ",", scientific = FALSE))
                        }else{
                            tbl[i, 5, with = FALSE]
                        }, "
                   </td></tr>"
                   )
}

# Add national numbers 
# National median sale price from Redfin Data Center
html <- paste0(html, "
                <tr style='border-top:1px solid black' class=''>
               <td style='text-align: left' class='c0 '></td>
               <td style='text-align: left' class='c1 '><a href=https://www.redfin.com/?utm_source=blog&utm_medium=post&utm_content=real_estate&utm_campaign=1002170'><b>National</b></a></td>
            <td style='text-align: left' class='c2 '>", round(100*summary_natl$natl_share_single_moms[1],1),"%" ,"</td>
            <td style='text-align: left;color:red;' class='c3 '>", round(100*summary_natl$percentage_change_single_mom[1],1),"%" ,"</td>
               <td style='text-align: left' class='c4 '>$274,000</td>
               </tr>"
)

html <- paste0(html, "</tbody></table>")

html <- gsub("\\n", "", html)

# Save final HTML output
write.table(html, "cities_with_increase_in_single_moms.html", col.names = FALSE, row.names = FALSE, quote = FALSE)

##################################################
# Repeat for share of families

# Data used for creating HTML tables of top 10
tbl <- final_output_families %>%
    arrange(desc(percentage_change_parents)) %>%
    mutate(ranking = rank(desc(percentage_change_parents))) %>%
    dplyr::select(ranking, city, table_id, percentage_change_parents, median_sale_price, percent_parents_15) %>%
    mutate(percentage_change_parents = round(percentage_change_parents*100,1),
           percent_parents_15 = round(percent_parents_15*100,1))


setDT(tbl)

# Create HTML code
html <- NULL
html <- "
<h3>Cities with the Largest Increase in Share of Family Households</h3>
<table class='ScienceTable' style = 'font-size: 14px;'>
<thead>
<tr style='height: 14px !important;'>
<th style='text-align: left' class='c0 '>Rank</th>
<th style='text-align: left' class='c1 '>City</th>
<th style='text-align: left' class='c2 '>Share of Family Households</th>
<th style='text-align: left' class='c3 '>Share of Family Households<br/>Percent Change (2012-15)</th>
<th style='text-align: left' class='c4 '>Median Sale Price<br/>July 2016</th>
</tr>
</thead>
<tbody>"

# loop through top 10 writing HTML code
for (i in 1:10) {
    html <- paste0(html, "
                   <tr class=''>
                   <td style='text-align: left' class='c0 '>", tbl[i, 1, with = FALSE], "</td>
                   <td style='text-align: left' class='c1 '><a href=https://www.redfin.com/city/", tbl[i, 3, with = FALSE], "/?utm_source=blog&utm_medium=post&utm_content=real_estate&utm_campaign=1002170'>", tbl[i, 2, with = FALSE], "</a></td>
                   <td style='text-align: left' class='c2 '>", paste0(tbl[i, 6, with = FALSE], "%"), "</td>                   
                   <td style='text-align: left' class='c3 '>", paste0(tbl[i, 4, with = FALSE], "%"), "</td>
                   <td style='text-align: left' class='c4 '>",
                   if(!is.na(tbl[i, 5, with = FALSE])){
                       paste0("$", prettyNum(tbl[i, 5, with = FALSE], big.mark = ",", scientific = FALSE))
                   }else{
                       tbl[i, 5, with = FALSE]
                   }, "
                   </td></tr>"
    )
    }

# Add national numbers
# National median sale price from Redfin Data Center
html <- paste0(html, "
               <tr style='border-top:1px solid black' class=''>
               <td style='text-align: left' class='c0 '></td>
               <td style='text-align: left' class='c1 '><a href=https://www.redfin.com/?utm_source=blog&utm_medium=post&utm_content=real_estate&utm_campaign=1002170'><b>National</b></a></td>
               <td style='text-align: left' class='c2 '>", round(100*summary_natl$natl_share_parents[1],1),"%" ,"</td>
               <td style='text-align: left;color:red;' class='c3 '>", round(100*summary_natl$percentage_change_parents[1],1),"%" ,"</td>
               <td style='text-align: left' class='c4 '>$274,000
               </td></tr>"
)

html <- paste0(html, "</tbody></table>")

html <- gsub("\\n", "", html)

# Save final HTML output
write.table(html, "cities_with_increase_in_families.html", col.names = FALSE, row.names = FALSE, quote = FALSE)


