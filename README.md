# Yatawara-Exposure-Inequity-Gradient

This reproducible workflow conducts the statistical analyses as seen in our paper (https://doi.org/10.1016/j.aeaoa.2026.100492) through a series of scripts. Everything you need is contained within this repository. To setup, set RStudio's working directory to the folder containing the repository files and ensure the working directory includes:

1. Population_StatesCounty_2000-2023.csv
2. MHIDataUSCounties.csv
3. Scripts 01-07
4. RStudio Vers. 2026.01.1 Build 403
5. RStudio Packages as demanded by each script (simple install command for each)

Here are the steps:
1. Run Script 01 to collect the necessary EPA data from their data repository.
2. Run Script 02 to create a "master spreadsheet" that just neatly groups up the MHI, Populations, States, and Counties into a centralized CSV.
3. Run Script 05, which runs Scripts 03, 04, and 06 for all U.S. states.
4. (optional) if you want to rerun/debug a specific state, Scripts 03 and 04 have a "STATE" variable you can modify near the top that will target a desired state. Just make sure to run 03 before 04 and 06.
5. Done! You should now have figures and CSVs for each US state. Debugging info should also be provided in the root directory as well as the under each state's directory.

Debugging CSVs will contain information such as missing state county data, year gaps in historical data, unmatched counties, and more.
