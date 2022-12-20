# sbus-drivers

Code for extracting and analyzing drivers data for various butterfly species based on their range maps, survey locations, or broader survey coverage. 

Currently `3_scripts/funs.R` contains the working functions, and `drivers-examples.R` walks through example useage of those functions.

Note down the road I may fold this repository into the repository for the population analysis (https://github.com/cbedwards/status-of-butterflies-analysis), but for now it seemed cleaner to keep the two projects separate.

## Using this repository

The layers files are large tifs that are not github-friendly, and the raw data (used for identifying survey locations) are not to be made publicly available (see our MOUs). 

Layers should be added to `1_raw_data` with some specific formatting (See "NOTE.txt" there). Once we finalize the files, I'll put together a shareable dropbox folder with the exact files and file structure.

The cleaned data file "cleaned-data-aggregated.csv" should be added to 2_data_wrangling/cleaned-data/. That file is available in the sbus shared dropbox folder.

## Todo

- It appears that the layers I have are incorrect (each year's file contains the same information, possibly?). We're tracking that down.
- Once I have access to higher temporal resolution files (ie monthly or daily), I'll update `get_drivers()` to tackle these options
- Once we have species range maps as shape files, we can write a function to pull them up by code
- Otherwise, we're ready to extract the drivers for a given species, and can write additional functions/scripts to analyze how drivers change through time.
