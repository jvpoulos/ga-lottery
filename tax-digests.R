# Import tax digest data from Google Sheet download
oh.taxes <- read.table(paste0(data.directory,"tax-digests.csv"),header=TRUE,sep=",")

# Keep complete obs within study period
oh.taxes <- subset(oh.taxes, !is.na(oh.taxes$year) & oh.taxes$year<=1847)

# Missing dollars to zero
oh.taxes$dollars[is.na(oh.taxes$dollars)] <- 0

# Combine person tax
oh.taxes$ptax <- oh.taxes$dollars + oh.taxes$cents/100 + oh.taxes$mills/1000

# Sum acres of land
oh.taxes$acres <- oh.taxes$land1 + oh.taxes$land2 + oh.taxes$land3

# Split data into pretreatment (1805 or earlier) and post-treatment
oh.taxes.pre <- oh.taxes[oh.taxes$year <= 1805,] 
oh.taxes.post <- oh.taxes[oh.taxes$year > 1805,]

# Keep only latest tax year for pretreatment
oh.taxes.pre <- aggregate(year ~ row.no + slaves + acres + ptax, data = oh.taxes.pre, max) # pick latest tax year

# Keep only earliest tax year for posttreatment
oh.taxes.post <- aggregate(year ~ row.no + slaves + acres + ptax, data = oh.taxes.post, max) # pick latest tax year 

# Keep only posttreatment obs that have a pretreatment ob
oh.taxes.post <- oh.taxes.post[oh.taxes.post$row.no %in% oh.taxes.pre$row.no,]

# Rename columns
colnames(oh.taxes.pre) <- c("row.no","slaves.pre","acres.pre","ptax.pre","year")
colnames(oh.taxes.post) <- c("row.no","slaves.post","acres.post","ptax.post","year")

# Exclude obs with min. person tax and get rid of year column
oh.taxes.pre <- subset(oh.taxes.pre, ptax.pre>0.3125, select=c("row.no","slaves.pre","acres.pre","ptax.pre"))
oh.taxes.post <- subset(oh.taxes.post, ptax.post>0.3125, select= c("row.no","slaves.post","acres.post","ptax.post"))

# Remove duplicate observations
oh.taxes.pre <- subset(oh.taxes.pre, !duplicated(oh.taxes.pre$row.no)) # remove duplicates
oh.taxes.post <- subset(oh.taxes.post, !duplicated(oh.taxes.post$row.no)) # remove duplicates

# Clean up 
rm(oh.taxes)