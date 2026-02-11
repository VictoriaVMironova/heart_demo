# Prepare heart attack data for Shiny app
heart <- read.delim("data/heartatk4R.txt")

str(heart)

# Format column data types
heart$AGE <- as.integer(heart$AGE)
heart$LOS <- as.integer(heart$LOS)
heart$CHARGES <- as.numeric(heart$CHARGES)

heart$SEX <- factor(heart$SEX, levels = c("F", "M"), labels = c("Female", "Male"))
heart$DIED <- factor(heart$DIED, levels = c(0, 1), labels = c("Survived", "Died"))
heart$DRG <- factor(heart$DRG,
                    levels = c(121, 122, 123),
                    labels = c("Complications", "No Complications", "Died")
)
heart$DIAGNOSIS <- factor(heart$DIAGNOSIS)
colnames(heart)[colnames(heart) == "Patient"] <- "ID"

str(heart)
summary(heart)
saveRDS(heart, "data/heart.rds")

