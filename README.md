# Interactive Holiday Travel Map for Verywell Health

This repository runs two YAML files in the .github/workflows directory: `cdcScrape.yml` and `runChartUpdate.yml`.

The former YAML file runs on a cron schedule on the 33rd minute of every hour that will download the JSON of the latest COVID-19 [case](https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=US_MAP_DATA) and [vaccination](https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_data) rate data for US states from the [CDC Covid Tracker](https://covid.cdc.gov/covid-data-tracker/#datatracker-home) and pretty-prints them to files called `cdcCovidCases.json` and `cdcVaccines.json`, respectively. It will then commit only new versions of these JSON files to the repository.

The latter YAML file is triggered by a push to repository and will run the R script `covidVaxTravelMap.R` which takes the `cdcCovidCases.json` and `cdcVaccines.json` files with the latest data and uses it to update the Datawrapper maps that are present on Verywell health's [COVID-19 travel guidance page](https://www.verywellhealth.com/covid-cases-travel-interactive-map-5208431).

The R script also outputs a CSV file `cdcFull.csv` that is sent to the Datawrapper charts via the [Datawrapper API](https://developer.datawrapper.de/) and also commited to the repository.

State emergency declaration, mask and vaccine mandate information found in the tooltip of the maps by hovering over a state is sourced from the [Kaiser Family Foundation](https://www.kff.org/) page on [State COVID-19 Data and Policy Actions](https://www.kff.org/coronavirus-covid-19/issue-brief/state-covid-19-data-and-policy-actions/) via their [GitHub repository](https://github.com/KFFData/COVID-19-Data). 