# Maps
Maps of stuff

1) BivariateAlcDrug.R generates a bivariate map of alcohol-specific and drug misuse deaths for all 152 Local Authorities/Counties & Unitary Authorities in England.

BivariateAlcDrugsGB.R does the same, but including Scottish Local Authorities as well.

![Bivariate alcohol and drug death map](https://github.com/VictimOfMaths/Maps/blob/master/BivariateAlcDrugsGB.png)

2) HLEbyLA.R (using {rgdal} and geom_map()) and HLEbyLA v2.R (using {sf} and geom_sf()) generate a map of healthy life expectancy by Local Authority and sex across England

![HLE map](https://github.com/VictimOfMaths/Maps/blob/master/HLEbyLA.png)

3) ConstHexTern.R generates a hex map of English electoral constituencies, coloured by vote share between the three main parties at the 2019 General Election, using the wonderful {tricolore} package, with a hex map from {parlitools}.

![Tricolore Hex Map](https://github.com/VictimOfMaths/Maps/blob/master/ConstHexTern2019.png)
