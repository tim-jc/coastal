# coastal workflow

1 - open coastal_vis_functions.R, add new ride to activities tibble in the correct order

2 - open strava_scraper.R, pull down new ride data and work out start and end times to crop the ride correctly. Add these times to the activities tibble in the functions script.

3 - use the cropping times to update the SQLite DB

4 - open reverse_geocoder.R and code any locations with missing data

5 - run coastal_visualisation.R to produce visualisations

5 - export png and convert to CMYK colourspace following these instructions - https://www.istudiopublisher.com/help-pages/pgs/cmyk-colors.html

6 - export as PDF using A2 custom size with 4mm unprintable space on each edge

7 - run index.Rmd to update flexdashboard
