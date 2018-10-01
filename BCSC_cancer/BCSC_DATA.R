options(java.parameters = "-Xmx8000m")

bc <- read.xlsx("dataset_limit.xlsx",1)

omitdata <- na.omit(bc)

table(is.na(omitdata))

write.xlsx(omitdata,file="dataset_omit.xlsx")
