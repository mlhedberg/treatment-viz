#Data Prep for D3 Visualization

orders <- read.csv("/Users/melissahedberg/flatiron-tech-test/flatiron_qs_orders_admins_july_16_Orders.csv")
admins <- read.csv("/Users/melissahedberg/flatiron-tech-test/flatiron_qs_orders_admins_july_16_Admins.csv")
demographics <- read.csv("/Users/melissahedberg/flatiron-tech-test/flatiron_qs_orders_admins_july_16_Demographics.csv")
patients <- read.csv("/Users/melissahedberg/flatiron-tech-test/flatiron_qs_orders_admins_july_16_Patients.csv")
practices <- read.csv("/Users/melissahedberg/flatiron-tech-test/flatiron_qs_orders_admins_july_16_Practices.csv")

#Clean date column of admins
x <- grep("^[0-9]{4}", admins$administered_date)
y <- grep("[0-9]{4}$", admins$administered_date)
A1 <- admins[x,]
A2 <- admins[y,]
A1$administered_date <- as.POSIXct(as.Date(A1$administered_date, "%Y-%m-%d"), tz = "EST")
A2$administered_date <- as.POSIXct(as.Date(A2$administered_date, "%d-%b-%Y"), tz = "EST")
admins <- rbind(A1,A2)

#Remove future admin dates
today <- as.POSIXct(as.Date("12-Sep-2016", "%d-%b-%Y"))
admins <- admins[admins$administered_date <= today, ]

#Change date format in patients
patients$diagnosis_date <- as.POSIXct(as.Date(patients$diagnosis_date, "%d-%b-%Y"), tz = "EST")
patients$advanced_diagnosis_date <- as.POSIXct(as.Date(patients$advanced_diagnosis_date, "%d-%b-%Y"), tz = "EST")

#Merge admins and practices datasets
admins_patient = merge(admins, patients, by = c("patient_id", "external_patient_id"))
treatments = merge(admins_patient, practices, by.x = c("external_practice_id", "internal_practice_id"), by.y = c("practice_id", "external_practice_id"))

#Convert dates to epoch and remove columns not needed
treatments$administered_date <- as.numeric(treatments$administered_date)

treatments_trimmed <- treatments[,c("external_practice_id", "patient_id", "administered_date", "drug_name", "practice_type")]
length = nrow(treatments_trimmed) - 1
treatments_trimmed$node <- 0:length
treatments_trimmed$x <- rep(1, times = nrow(treatments_trimmed))
treatments_trimmed$y <- treatments_trimmed$administered_date
unique(treatments_trimmed$drug_name)
# axitinib bevacizumab erlotinib hcl nivolumab


summary(treatments_trimmed$administered_date)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#1.114e+09 1.371e+09 1.399e+09 1.393e+09 1.425e+09 1.448e+09 
treatments_trimmed <- treatments_trimmed[treatments_trimmed$administered_date > 1.2E9,]
summary(treatments_trimmed$administered_date)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#1.248e+09 1.372e+09 1.399e+09 1.395e+09 1.425e+09 1.448e+09 
#bevacizumab = 1.248e+09 = node 302    #2004
#erlotinib hcl = 1.372e+09 = node 303  #2004
#axitinib = 1.399e+09 = node 304   #2012
#nivolumab = 1.425e+09 = node 305  #2014
#community = 1.372e+09 = node 306
#academic = 1.425e+09 = node 307

#Create json file of all the nodes
nodes <- treatments_trimmed[, c("x", "y", "patient_id")]
names(nodes) <- c("x", "y", "node_names")
axis2_3 <- data.frame(x=c(2, 2, 2, 2, 3, 3), 
                      y=c(1.248e+09, 1.372e+09, 1.399e+09, 1.425e+09, 1.372e+09, 1.425e+09),
                      node_names = c("bevacizumab", "erlotinib hcl", "axitinib", "nivolumab", "community", "academic"))
nodes <- rbind(nodes, axis2_3)

#Save nodes to a JSON file
require(jsonlite)
nodesJSON <- toJSON(nodes, pretty = TRUE)
write(nodesJSON, file="nodes.JSON")

#Create a JSON file for the links
library(plyr)
treatments_trimmed$target_drug <- mapvalues(treatments_trimmed$drug_name, 
          from = c("bevacizumab", "erlotinib hcl", "axitinib", "nivolumab"), 
          to = c(302, 303, 304, 305))

patients_to_drug <- treatments_trimmed[, c("node", "target_drug", "patient_id", "drug_name")]
names(patients_to_drug) <- c("node", "target_drug", "source_names", "target_names")

treatments_trimmed$target_practice <- mapvalues(treatments_trimmed$practice_type, 
                                            from = c("community", "academic"), 
                                            to = c(306, 307))

drugs_to_practice <- treatments_trimmed[, c("target_drug", "target_practice", "drug_name", "practice_type")]
names(drugs_to_practice) <- c("target_drug", "target_practice", "source_names", "target_names")

practice_to_patients <- treatments_trimmed[, c("target_practice", "node", "practice_type", "patient_id")]
names(practice_to_patients) <- c("target_practice", "node", "source_names", "target_names")

x <- rep("nodes[", times = 302)
y <- rep("]", times = 302)
patients_to_drug$source <- paste(x, patients_to_drug$node, y, sep = "")
patients_to_drug$target <- paste(x, patients_to_drug$target_drug, y, sep = "")
drugs_to_practice$source <- paste(x, drugs_to_practice$target_drug, y, sep = "")
drugs_to_practice$target <- paste(x, drugs_to_practice$target_practice, y, sep = "")
practice_to_patients$source <- paste(x, practice_to_patients$target_practice, y, sep = "")
practice_to_patients$target <- paste(x, practice_to_patients$node, y, sep = "")
links = rbind(patients_to_drug[, c("source", "target", "source_names", "target_names")], 
              drugs_to_practice[, c("source", "target", "source_names", "target_names")], 
              practice_to_patients[, c("source", "target", "source_names", "target_names")])

linksJSON <- toJSON(links, pretty = TRUE)
write(linksJSON, file="links.JSON")



#group with shapes: http://bl.ocks.org/eyaler/10586116

#IF OTHER DATASETS ARE NEEDED

#Clean date column of orders
orders$order_date <- as.Date(orders$order_date, "%d-%b-%y")

# Convert diagnosis_date and advanced_diagnosis_date variables to the Date class.
grep("^[0-9]{4}", patients$diagnosis_date)
grep("^[0-9]{4}", patients$advanced_diagnosis_date)
patients$diagnosis_date <- as.Date(patients$diagnosis_date, "%d-%b-%Y")
patients$advanced_diagnosis_date <- as.Date(patients$advanced_diagnosis_date, "%d-%b-%Y")

#Demographics remove rows where gender, age unknown
demographics <- demographics[demographics$gender != "unknown" & demographics$gender != "" & demographics$age != "NA" & demographics$race != "",]
demographics[demographics$age > 100, "age"] <- 68

