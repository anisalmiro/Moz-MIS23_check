# Mozambique MIS 2023

This repo contains files to support the 2023 MIS in Mozambique. It is accessible only by the IT team supporting this activity, including Anisio, Muthambe, Eleanore, Hannah, and Matt.

**Household Listing**

Team in the field are tasked with visiting each household in the enumeration areas and reporting whether the household is present, the name of the head of household, and the members of the household. These data are recorded in Kobo collect and uploaded to the INS server.

The HH listing script pulls the data from the server, identifies eligible households (present, within the enumeration area) and randomly selected 25 households to be visited for the MIS questionnaire. The script produces a map and Excel file with the selected households and a GPX file to be uploaded in Osmand. These are sent back to the field team to guide them to the selected households.

**Main questionnaire**

The main questionnaire produces six datasets (repeats within the ODK form). These are:

-   Main household questionnaire

-   Household member repeat (age, travel)

-   Fever repeat

-   Net repeat

-   Women's questionnaire

-   Women's births repeat (with child fever)

-   Women's birth check youngest repeat (not used for analysis)

-   Biomarker children data
