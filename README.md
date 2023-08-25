MSDS498 Capstone  
8/26/2023

# Forecasting the PGA Tour

This project is an end-to-end solution for forecasting future PGA tour seasons. For more info on the methodology, see the docs folder.

App: https://pgaapp-jb4yato7fq-ue.a.run.app/

## Data Processing

Scripts are stored in ./elt/src directory. We utilize containerized microservices to ingest data from the following sources:

- datagolf.com
- espn.com
- pgatour.com

We utilize Data Golf's id system for players and events. Since we also scrape data from the web, the directory contains notebooks to map this data to the common id system. This process must be done manually.

## Modeling

All modeling scripts are contained in the ./modeling directory. Model objects are stored in ./modeling/models.

We build a number of models in order to project skill and tour outcomes. Scripts can be cateogrized as model training or projection.

### Training Scripts

`adjusted_strokes_gained.R`

- models: adjusted strokes gained (total) and adjusted strokes gained (components)

`latent_skill.R`

-  models: in-sample latent skill, skill aging curve, future latent skill

`participation_earnings.R`

- build models for: PGA promotion probability, PGA attrition probability, earnings
- outputs: automatic major qualifiers, historical expected/actual earnings

### Projection Scripts

`project_skill.R`

- Projects latent skill and estimates uncertainty for each of the next 5 seasons

`project_majors.R`

- Simulates major tournament outcomes for each of the next 3 seasons

`project_earnings.R`

- predicts season-level earnings for each of the next 5 seasons.

## Application

The prototype application is located in the ./app directory. It is written using Rshiny and surfaces historical data along with model outputs. The application is deployed via a docker container hosted on GCP.

## Other

Documentation can be found in the ./docs directory.

Each directory contains /dev folders that contain miscellaneous scripts used during development of the project. These can be ignored.
