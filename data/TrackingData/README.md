You will have to copy over the processed tracking data files from the transmitted data files folder that lives in the 'GitHub' folder (the main analysis directory) on your machine.

First, either make or make sure you have a dedicated folder for these files in the 'data' subfolder of your project.

Then, copy and paste the following chunk of code in the *console* to copy the files over to your project:

``` r
dir('/Users/salmaabdel-raheem/Documents/GitHub/PhD/RoD/Transmitted/',
    pattern = 'argos_aniMotum_crw.csv',
    recursive = TRUE,
    full.names = TRUE) %>%
  file.copy("data/TrackingData/", overwrite = TRUE)
```
