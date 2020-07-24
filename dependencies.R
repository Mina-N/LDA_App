#Install dependencies necessary for app

#Found at: 
#https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load("shiny")

## Data Frame Packages
pacman::p_load("dplyr","stringr","readr","readxl")

## Data Visualization Packages
pacman::p_load("ggplot2","Rtsne","LDAvis")

## Text Mining Pacakges
# Note: data.table has non-zero exit status when installing from source
#       recommend selecting "no" for "Do you want to install from sources...?"
pacman::p_load("data.table","Matrix","text2vec","tm",
               "SnowballC","rARPACK","ggupset")

## packages required for server.R
pacman::p_load("servr")