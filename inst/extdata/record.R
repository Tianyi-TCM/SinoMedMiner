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
?left_join

use_data(herbs_attr, overwrite = TRUE)

data(herbs_attr)
data(herbs_function)
?herb_to_add

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



standard_all <- c(herb_property$names, herb_efficacy$names, herb_name$names,  disease_name$disease_name   , syndrome_name$syndrome_name)

standard_all <- unique(standard_all)



use_data(herb_property,
         herb_efficacy,
         herb_name,
         disease_name,
         syndrome_name,
         standard_all,
         internal = TRUE, overwrite = TRUE)
?use_data
herb

use_github_action()

usethis::use_readme_rmd()

