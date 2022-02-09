#!/bin/bash

echo "Constructing SRP database: "
Rscript -e "source('02_srp.R');"

echo "Constructing SON database: "
Rscript -e "source('02_son.R');"

echo "Constructing PET database: "
Rscript -e "source('04_pet.R');"

echo "Constructing final combined database: "
Rscript -e "source('08_combine_dbs.R');"
