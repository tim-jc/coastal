# coastal workflow

Code is split by responsibility under `R/`. Use `source("R/load.R")` for project entry points.

1 - open data/coastal_activities.R, add new ride to the activities tibble in the correct order

2 - open ride_explorer.R, inspect the silver activity stream and work out start and end times to crop the ride correctly. Add these times to data/coastal_activities.R.

3 - run scripts/check_inputs.R to validate metadata and confirm the silver activities / activity_streams tables contain the expected rows

4 - open reverse_geocoder.R and code any locations with missing data

5 - run coastal_visualisation.R to produce visualisations

6 - export png and convert to CMYK colourspace following these instructions - https://www.istudiopublisher.com/help-pages/pgs/cmyk-colors.html

7 - export as PDF using A2 custom size with 4mm unprintable space on each edge

8 - run index.Rmd to update flexdashboard


# Note on Git authentication

If the access token expires you can refresh from the links in GitHub's notification email.

To reauthenticate, git push from the local repo using Terminal. This will prompt for a username and password. Enter the new authentication token in the password box
