## Script for wrangling Excel file for specs by client, HBOA ##


install.packages("writexl")
install.packages("reshape2")
library(reshape2)
library(tidyverse)
library(tidyr)
library(writexl)
library(readxl)

## Read In Data ##
Absentee_Owner_Data <- read_excel("C:/Users/jgles/Desktop/HBOA Wrangling/Absentee Owner Data.xlsx")

## Upon import, the df was a type of 'table df'.  Changing that here ##
Absentee_Owner_Data <- as.data.frame(Absentee_Owner_Data)

##Renaming Columns ##
Absentee_Owner_Data <-Absentee_Owner_Data %>%
  rename(
    Address = MAddress,
    City = MCity,
    State = MState,
    Zip = MZip5,
    Zip4 = MZip4,
    County = MCounty
  )

## Renaming dataframe to work with it##
df1 <- Absentee_Owner_Data

## Dropping Unnecessary columns ##

df1 <- select(df1, -c(PropertyID, Currentavmvalue, YearBuilt, TAddress, TCity, TCounty, TState, TZip5, Tzip4, owneroccupied))

## Split the Name1 Column into 2 Columns.##

df1 <- extract(df1, Name, c("FirstName", "LastName"), "([^ ]+) (.*)")
df1 <- extract(df1, Name2, c("FirstName2", "LastName2"), "([^ ]+) (.*)")

##Delete MI From LastName##
df1$LastName <- sub('.', '', df1$LastName)
df1$LastName2 <- sub('.', '', df1$LastName2)

## Clean Up Spaces before last name ##
df1$LastName <- gsub(" ", "", df1$LastName, fixed = TRUE)
df1$LastName2 <- gsub(" ", "", df1$LastName2, fixed = TRUE)

## Detecting whether an address has an apt or unit number, then adding it to a column per specs##
df1$Unit <- str_extract(df1$Address, "Apt\\s\\d+.|Ste\\s\\d+.|Unit\\s\\d+.|#\\s\\d+.")
df1$Address <- sub('Apt\\s\\d+.|Ste\\s\\d+.|Unit\\s\\d+.|#\\s\\d+.', '', df1$Address)

## Moving Unit Column to Proper Place ##
df1 <- df1 %>%
  relocate(Unit, .before = City)

#### Title Case on all Names  ###

df1$FirstName <- str_to_title(df1$FirstName)
df1$FirstName2 <- str_to_title(df1$FirstName2)
df1$LastName <- str_to_title(df1$LastName)
df1$LastName2 <- str_to_title(df1$LastName2)


## Export to Excel ##
write_xlsx(df1, "HBOAaudienceEDITED.xlsx")
