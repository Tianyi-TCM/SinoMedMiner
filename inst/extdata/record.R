library(devtools)


document()
install()

load_all()
check()
.libPaths()
library()
library()
?calcPhi
usethis::use_data_raw()


use_data(herbs_attr, overwrite = TRUE)

data(herbs_attr)
data(herbs_function)


load("data/herbs_attr.RData")
load("data/herbs_function.RData")
library(openxlsx)
herb_dirty <- read.xlsx("data_dirty.xlsx")


herb_test <- normalize_herb_names(herb_dirty)


herb_property <- read.xlsx("inst/extdata/Herb_Property_Database.xlsx")
herb_efficacy <- read.xlsx("inst/extdata/Herb_Efficacy_Classification.xlsx")
herb_name <- read.xlsx("inst/extdata/Herb_Standard_Names.xlsx", sheet = 2)
disease_name <- read.xlsx("inst/extdata/disease_name.xlsx")
syndrome_name <- read.xlsx("inst/extdata/syndrome_name.xlsx")
use_data(herb_property,herb_efficacy, herb_name,syndrome_name,internal = TRUE, overwrite = TRUE)
?use_data
herb


