# install.packages("palmerpenguins")

library("palmerpenguins")
library("pastecs")

# Get descriptive statistics
stat.desc(penguins)

analyseNlsResiduals(penguins, body_mass_g)
