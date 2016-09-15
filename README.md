# Single mom and families growth cities
For this analysis Redfin pulled the one-year American Community survey for 2015 and 2012 of "Family Type by Presence of Related Children". We then pulled the total population for each city and each year as well.

After calculating the number of single dads, moms, and married-couples with children under 18 for each city, we divided that by the population of the city to get the share of single mom households and families overall. Next we computed the percent change for each of these measures. 

From this data we were able to measure the overall–that is of all 566 cities–share of single-mom households, parents, as well as the percent change from 2012 to 2015.

Then we filtered for cities with a population of more than 200,000 people, and ranked them according to their respective percent increase in single-mom households and family households.

Finally, we pulled in median sale price data from Redfin for each city, as well as nationwide from the [Redfin Data Center.](https://www.redfin.com/blog/data-center)

The maps were made in  d3.js and the HTML tables were written from the R script. All data and code is available for reference in this repository.
