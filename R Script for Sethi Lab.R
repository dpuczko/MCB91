#Installing required packages
install.packages("ggplot2")
library(ggplot2)
install.packages("ggrepel")
library(ggrepel)

#Load in data frame
expression_data <- read.csv("C:\\Users\\Daniel\\Desktop\\internal-21q3_v7-ccle-expression-full.csv", header = TRUE) 

#Recoding the first column to be the DepMapID
rownames(expression_data) <- expression_data[,1]  
expression_data[,1] <- NULL
rownames(expression_data)[1]

#Checking the column names (since they differ from those in the Excel sheet)
colnames(expression_data)[1] 

#Checking the row names to make sure that they were successfully recoded
rownames(expression_data)[1]

#Checking to see whether the proper data from the Excel sheet is copied
expression_data[c("ACH-000113", "ACH-000001"), c("EGFR..ENSG00000146648.", "TP53..ENSG00000141510.", "ERBB2..ENSG00000141736."), drop = FALSE]

#Expression data extraction
expression_data[c("ACH-000003", "ACH-000007", "ACH-000009", "ACH-000089", "ACH-000202", "ACH-000236", "ACH-000249", "ACH-000252", "ACH-000253",
                  "ACH-000286", "ACH-000296", "ACH-000342", "ACH-000350", "ACH-000360", "ACH-000381", "ACH-000400", "ACH-000403", "ACH-000412",
                  "ACH-000421", "ACH-000467", "ACH-000470", "ACH-000489", "ACH-000491", "ACH-000501", "ACH-000532", "ACH-000552", "ACH-000565",
                  "ACH-000651", "ACH-000680", "ACH-000683", "ACH-000708", "ACH-000722", "ACH-000798", "ACH-000820", "ACH-000842", "ACH-000895",
                  "ACH-000926", "ACH-000935", "ACH-000943", "ACH-000950", "ACH-000955", "ACH-000957", "ACH-000958", "ACH-000959", "ACH-000963",
                  "ACH-000967", "ACH-000969", "ACH-000970", "ACH-000971", "ACH-000982", "ACH-000985", "ACH-000986", "ACH-000989", "ACH-000991",
                  "ACH-000997", "ACH-000998", "ACH-000999", "ACH-001061", "ACH-001345", "ACH-001399", "ACH-001454", "ACH-001456", "ACH-001458",
                  "ACH-001459", "ACH-001460", "ACH-001461", "ACH-001786", "ACH-002023", "ACH-002024", "ACH-002025", "ACH-002659"), 
                c("TP53..ENSG00000141510.", "ERBB2..ENSG00000141736.", "EGFR..ENSG00000146648.", "ERBB3..ENSG00000065361.", "ERBB4..ENSG00000178568.",
                  "APC..ENSG00000134982.", "KRAS..ENSG00000133703.", "EGF..ENSG00000138798.", "TNF..ENSG00000232810.", "NRG1..ENSG00000157168.", "NRG2..ENSG00000158458."),
                drop = FALSE]

#Load in data frame
copy_number <- read.csv("C:\\Users\\Daniel\\Desktop\\CCLE_gene_cn.csv", header = TRUE)

#Recoding the first column to be the DepMap ID
rownames(copy_number) <- copy_number[,1]
copy_number[,1] <- NULL
rownames(copy_number)[1]

#Checking the column names (since they differ from those in the Excel sheet)
colnames(copy_number)[1]

#Checking to see whether the proper data from the Excel sheet is copied
copy_number[c("ACH-000003"), c("TP53..7157.", "ERBB2..2064.", "EGFR..1956.", "ERBB3..2065.", "ERBB4..2066.", "APC..324.", "KRAS..3845.", "EGF..1950.", "TNF..7124.",
                               "NRG1..3084.", "NRG2..9542."), drop = FALSE]

#Copy number data extraction
copy_number[c("ACH-000003", "ACH-000007", "ACH-000009", "ACH-000089", "ACH-000202", "ACH-000236", "ACH-000249", "ACH-000252", "ACH-000253", "ACH-000286", "ACH-000296",
              "ACH-000342", "ACH-000350", "ACH-000360", "ACH-000381", "ACH-000400", "ACH-000403", "ACH-000412", "ACH-000421", "ACH-000467", "ACH-000470", "ACH-000489",
              "ACH-000491", "ACH-000501", "ACH-000532", "ACH-000552", "ACH-000565", "ACH-000651", "ACH-000680", "ACH-000683", "ACH-000708", "ACH-000722", "ACH-000798",
              "ACH-000820", "ACH-000842", "ACH-000895", "ACH-000926", "ACH-000935", "ACH-000943", "ACH-000950", "ACH-000955", "ACH-000957", "ACH-000958", "ACH-000959",
              "ACH-000963", "ACH-000967", "ACH-000969", "ACH-000970", "ACH-000971", "ACH-000982", "ACH-000985", "ACH-000986", "ACH-000989", "ACH-000991", "ACH-000997",
              "ACH-000998", "ACH-000999", "ACH-001039", "ACH-001061", "ACH-001081", "ACH-001091", "ACH-001199", "ACH-001345", "ACH-001399", "ACH-001454", "ACH-001456",
              "ACH-001458", "ACH-001459", "ACH-001460", "ACH-001461", "ACH-001786", "ACH-002023", "ACH-002024", "ACH-002025", "ACH-002233", "ACH-002287", "ACH-002345"),
            c("TP53..7157.", "ERBB2..2064.", "EGFR..1956.", "ERBB3..2065.", "ERBB4..2066.", "APC..324.", "KRAS..3845.", "EGF..1950.", "TNF..7124.", "NRG1..3084.",
              "NRG2..9542."), drop = FALSE]

#Load in data frame
mutations <- read.csv("C:\\Users\\Daniel\\Desktop\\internal-21q3_v12-ccle-mutations.csv", header = TRUE)

#Mutation information extraction
mutations_subset_TP53 <- subset(mutations, Hugo_Symbol == "TP53", select = c(DepMap_ID, Variant_Classification, Protein_Change), drop = FALSE)
mutations_subset_ERBB2 <- subset(mutations, Hugo_Symbol == "ERBB2", select = c(DepMap_ID, Variant_Classification, Protein_Change), drop = FALSE)
mutations_subset_EGFR <- subset(mutations, Hugo_Symbol == "EGFR", select = c(DepMap_ID, Variant_Classification, Protein_Change), drop = FALSE)
mutations_subset_ERBB3 <- subset(mutations, Hugo_Symbol == "ERBB3", select = c(DepMap_ID, Variant_Classification, Protein_Change), drop = FALSE)
mutations_subset_ERBB4 <- subset(mutations, Hugo_Symbol == "ERBB4", select = c(DepMap_ID, Variant_Classification, Protein_Change), drop = FALSE)
mutations_subset_APC <- subset(mutations, Hugo_Symbol == "APC", select = c(DepMap_ID, Variant_Classification, Protein_Change), drop = FALSE)
mutations_subset_KRAS <- subset(mutations, Hugo_Symbol == "KRAS", select = c(DepMap_ID, Variant_Classification, Protein_Change), drop = FALSE)
mutations_subset_EGF <- subset(mutations, Hugo_Symbol == "EGF", select = c(DepMap_ID, Variant_Classification, Protein_Change), drop = FALSE)
mutations_subset_TNF <- subset(mutations, Hugo_Symbol == "TNF", select = c(DepMap_ID, Variant_Classification, Protein_Change), drop = FALSE)
mutations_subset_NRG1 <- subset(mutations, Hugo_Symbol == "NRG1", select = c(DepMap_ID, Variant_Classification, Protein_Change), drop = FALSE)
mutations_subset_NRG2 <- subset(mutations, Hugo_Symbol == "NRG2", select = c(DepMap_ID, Variant_Classification, Protein_Change), drop = FALSE)

#Making expression_data a new variable that only contains the cell lines of interest
new_expression <- expression_data[c("ACH-000003", "ACH-000007", "ACH-000009", "ACH-000089", "ACH-000202", "ACH-000236", "ACH-000249", "ACH-000252", "ACH-000253",
                                    "ACH-000286", "ACH-000296", "ACH-000342", "ACH-000350", "ACH-000360", "ACH-000381", "ACH-000400", "ACH-000403", "ACH-000412",
                                    "ACH-000421", "ACH-000467", "ACH-000470", "ACH-000489", "ACH-000491", "ACH-000501", "ACH-000532", "ACH-000552", "ACH-000565",
                                    "ACH-000651", "ACH-000680", "ACH-000683", "ACH-000708", "ACH-000722", "ACH-000798", "ACH-000820", "ACH-000842", "ACH-000895",
                                    "ACH-000926", "ACH-000935", "ACH-000943", "ACH-000950", "ACH-000955", "ACH-000957", "ACH-000958", "ACH-000959", "ACH-000963",
                                    "ACH-000967", "ACH-000969", "ACH-000970", "ACH-000971", "ACH-000982", "ACH-000985", "ACH-000986", "ACH-000989", "ACH-000991",
                                    "ACH-000997", "ACH-000998", "ACH-000999", "ACH-001061", "ACH-001345", "ACH-001399", "ACH-001454", "ACH-001456", "ACH-001458",
                                    "ACH-001459", "ACH-001460", "ACH-001461", "ACH-001786", "ACH-002023", "ACH-002024", "ACH-002025", "ACH-002659"), 
                                  c("TP53..ENSG00000141510.", "ERBB2..ENSG00000141736.", "EGFR..ENSG00000146648.", "ERBB3..ENSG00000065361.", "ERBB4..ENSG00000178568.",
                                    "APC..ENSG00000134982.", "KRAS..ENSG00000133703.", "EGF..ENSG00000138798.", "TNF..ENSG00000232810.", "NRG1..ENSG00000157168.", "NRG2..ENSG00000158458.")]

#Plotting TP53 expression against ERBB2 expression and creating a linear regression model
plot(x = new_expression$ERBB2..ENSG00000141736., y = new_expression$TP53..ENSG00000141510., xlab = "ERBB2 RNA Expression", ylab = "TP53 RNA Expression", pch = 20)
abline(lm(new_expression$TP53..ENSG00000141510. ~ new_expression$ERBB2..ENSG00000141736.), col = "red")
TP53_r <- lm(new_expression$TP53..ENSG00000141510. ~ new_expression$ERBB2..ENSG00000141736.)
summary(TP53_r)
cor(new_expression$ERBB2..ENSG00000141736., new_expression$TP53..ENSG00000141510.)
cor.test(new_expression$ERBB2..ENSG00000141736., new_expression$TP53..ENSG00000141510.)

#Plotting EGFR expression against ERBB2 expression and creating a linear regression model
plot(x = new_expression$ERBB2..ENSG00000141736., y = new_expression$EGFR..ENSG00000146648., xlab = "ERBB2 RNA Expression", ylab = "EGFR RNA Expression", pch = 20)
abline(lm(new_expression$EGFR ~ new_expression$ERBB2..ENSG00000141736.), col = "red")
EGFR_r <- lm(new_expression$EGFR..ENSG00000146648. ~ new_expression$ERBB2..ENSG00000141736.)
summary(EGFR_r)
cor(new_expression$ERBB2..ENSG00000141736., new_expression$EGFR..ENSG00000146648.)
cor.test(new_expression$ERBB2..ENSG00000141736., new_expression$EGFR..ENSG00000146648.)

#Plotting ERBB3 expression against ERBB2 expression and creating a linear regression model
plot(x = new_expression$ERBB2..ENSG00000141736., y = new_expression$ERBB3..ENSG00000065361., xlab = "ERBB2 RNA Expression", ylab = "ERBB3 RNA Expression", pch = 20)
abline(lm(new_expression$ERBB3..ENSG00000065361. ~ new_expression$ERBB2..ENSG00000141736.), col = "red")
ERBB3_r <- lm(new_expression$ERBB3..ENSG00000065361. ~ new_expression$ERBB2..ENSG00000141736.)
summary(ERBB3_r)
cor(new_expression$ERBB2..ENSG00000141736., new_expression$ERBB3..ENSG00000065361.)
cor.test(new_expression$ERBB2..ENSG00000141736., new_expression$ERBB3..ENSG00000065361.)

#Plotting ERBB4 expression against ERBB2 expression and creating a linear regression model
plot(x = new_expression$ERBB2..ENSG00000141736., y = new_expression$ERBB4..ENSG00000178568., xlab = "ERBB2 RNA Expression", ylab = "ERBB4 RNA Expression", pch = 20)
abline(lm(new_expression$ERBB4..ENSG00000178568. ~ new_expression$ERBB2..ENSG00000141736.), col = "red")
ERBB4_r <- lm(new_expression$ERBB4..ENSG00000178568. ~ new_expression$ERBB2..ENSG00000141736.)
summary(ERBB4_r)
cor(new_expression$ERBB2..ENSG00000141736., new_expression$ERBB4..ENSG00000178568.)
cor.test(new_expression$ERBB2..ENSG00000141736., new_expression$ERBB4..ENSG00000178568.)

#Plotting APC expression against ERBB2 expression and creating a linear regression model
plot(x = new_expression$ERBB2..ENSG00000141736., y = new_expression$APC..ENSG00000134982., xlab = "ERBB2 RNA Expression", ylab = "APC RNA Expression", pch = 20)
abline(lm(new_expression$APC..ENSG00000134982. ~ new_expression$ERBB2..ENSG00000141736.), col = "red")
APC_r <- lm(new_expression$APC..ENSG00000134982. ~ new_expression$ERBB2..ENSG00000141736.)
summary(APC_r)
cor(new_expression$ERBB2..ENSG00000141736., new_expression$APC..ENSG00000134982.)
cor.test(new_expression$ERBB2..ENSG00000141736., new_expression$APC..ENSG00000134982.)

#Plotting KRAS expression against ERBB2 expression and creating a linear regression model
plot(x = new_expression$ERBB2..ENSG00000141736., y = new_expression$KRAS..ENSG00000133703., xlab = "ERBB2 RNA Expression", ylab = "KRAS RNA Expression", pch = 20)
abline(lm(new_expression$KRAS..ENSG00000133703. ~ new_expression$ERBB2..ENSG00000141736.), col = "red")
KRAS_r <- lm(new_expression$KRAS..ENSG00000133703. ~ new_expression$ERBB2..ENSG00000141736.)
summary(KRAS_r)
cor(new_expression$ERBB2..ENSG00000141736., new_expression$KRAS..ENSG00000133703.)
cor.test(new_expression$ERBB2..ENSG00000141736., new_expression$KRAS..ENSG00000133703.)

#Plotting EGF expression against ERBB2 expression and creating a linear regression model
plot(x = new_expression$ERBB2..ENSG00000141736., y = new_expression$EGF..ENSG00000138798., xlab = "ERBB2 RNA Expression", ylab = "EGF RNA Expression", pch = 20)
abline(lm(new_expression$EGF..ENSG00000138798. ~ new_expression$ERBB2..ENSG00000141736.), col = "red")
EGF_r <- lm(new_expression$EGF..ENSG00000138798. ~ new_expression$ERBB2..ENSG00000141736.)
summary(EGF_r)
cor(new_expression$ERBB2..ENSG00000141736., new_expression$EGF..ENSG00000138798.)
cor.test(new_expression$ERBB2..ENSG00000141736., new_expression$EGF..ENSG00000138798.)

#Plotting TNF expression against ERBB2 expression and creating a linear regression model
plot(x = new_expression$ERBB2..ENSG00000141736., y = new_expression$TNF..ENSG00000232810., xlab = "ERBB2 RNA Expression", ylab = "TNF RNA Expression", pch = 20)
abline(lm(new_expression$TNF..ENSG00000232810. ~ new_expression$ERBB2..ENSG00000141736.), col = "red")
TNF_r <- lm(new_expression$TNF..ENSG00000232810. ~ new_expression$ERBB2..ENSG00000141736.)
summary(TNF_r)
cor(new_expression$ERBB2..ENSG00000141736., new_expression$TNF..ENSG00000232810.)
cor.test(new_expression$ERBB2..ENSG00000141736., new_expression$TNF..ENSG00000232810.)

#Plotting NRG1 expression against ERBB2 expression and creating a linear regression model
plot(x = new_expression$ERBB2..ENSG00000141736., y = new_expression$NRG1..ENSG00000157168., xlab = "ERBB2 RNA Expression", ylab = "NRG1 RNA Expression", pch = 20)
abline(lm(new_expression$NRG1..ENSG00000157168. ~ new_expression$ERBB2..ENSG00000141736.), col = "red")
NRG1_r <- lm(new_expression$NRG1..ENSG00000157168. ~ new_expression$ERBB2..ENSG00000141736.)
summary(NRG1_r)
cor(new_expression$ERBB2..ENSG00000141736., new_expression$NRG1..ENSG00000157168.)
cor.test(new_expression$ERBB2..ENSG00000141736., new_expression$NRG1..ENSG00000157168.)

#Plotting NRG2 expression against ERBB2 expression and creating a linear regression model
plot(x = new_expression$ERBB2..ENSG00000141736., y = new_expression$NRG2..ENSG00000158458., xlab = "ERBB2 RNA Expression", ylab = "NRG2 RNA Expression", pch = 20)
abline(lm(new_expression$NRG2..ENSG00000158458. ~ new_expression$ERBB2..ENSG00000141736.), col = "red")
NRG2_r <- lm(new_expression$NRG2..ENSG00000158458. ~ new_expression$ERBB2..ENSG00000141736.)
summary(NRG2_r)
cor(new_expression$ERBB2..ENSG00000141736., new_expression$NRG2..ENSG00000158458.)
cor.test(new_expression$ERBB2..ENSG00000141736., new_expression$NRG2..ENSG00000158458.)

#Creating a data frame for the first and fourth quartile for ERBB2 expression
fourth_q <- new_expression[c("ACH-000350", "ACH-002023", "ACH-000967", "ACH-000998", "ACH-000253", "ACH-001454", "ACH-000982", "ACH-000360", "ACH-000959", "ACH-000400",
                            "ACH-000991", "ACH-000895", "ACH-000955", "ACH-000957", "ACH-000342", "ACH-000798", "ACH-000007", "ACH-000403"), c("TP53..ENSG00000141510.",
                            "ERBB2..ENSG00000141736.", "EGFR..ENSG00000146648.", "ERBB3..ENSG00000065361.", "ERBB4..ENSG00000178568.", "APC..ENSG00000134982.",
                            "KRAS..ENSG00000133703.", "EGF..ENSG00000138798.", "TNF..ENSG00000232810.", "NRG1..ENSG00000157168.", "NRG2..ENSG00000158458.")]

first_q <- new_expression[c("ACH-001456", "ACH-001460", "ACH-000501", "ACH-000971", "ACH-000969", "ACH-001786", "ACH-000470", "ACH-002025", "ACH-001461", "ACH-000202",
                             "ACH-000943", "ACH-001458", "ACH-002659", "ACH-000651", "ACH-000842", "ACH-001459", "ACH-000491", "ACH-002024"), c("TP53..ENSG00000141510.",
                             "ERBB2..ENSG00000141736.", "EGFR..ENSG00000146648.", "ERBB3..ENSG00000065361.", "ERBB4..ENSG00000178568.", "APC..ENSG00000134982.",
                             "KRAS..ENSG00000133703.", "EGF..ENSG00000138798.", "TNF..ENSG00000232810.", "NRG1..ENSG00000157168.", "NRG2..ENSG00000158458.")]

#Plotting TP53 expression against ERBB2 expression for the first and fourth quartile and creating a linear regression model for both
plot(x = fourth_q$ERBB2..ENSG00000141736., y = fourth_q$TP53..ENSG00000141510., xlab = "ERBB2 Expression", ylab = "TP53 Expression", pch = 20, col = "red", xlim = c(0,10), ylim = c(0,10))
points(x = first_q$ERBB2..ENSG00000141736., y = first_q$TP53..ENSG00000141510., pch = 20, col = "blue")
abline(lm(fourth_q$TP53..ENSG00000141510. ~ fourth_q$ERBB2..ENSG00000141736.), col = "red")
abline(lm(first_q$TP53..ENSG00000141510. ~ first_q$ERBB2..ENSG00000141736.), col = "blue")
legend(x = 0, y = 10.2, c("Fourth Quartile", "First Quartile"), col = c("red", "blue"), pch = c(20, 20), cex = 0.8)
cor.test(fourth_q$ERBB2..ENSG00000141736., fourth_q$TP53..ENSG00000141510.)
cor.test(first_q$ERBB2..ENSG00000141736., first_q$TP53..ENSG00000141510.)

#Plotting EGFR expression against ERBB2 expression for the first and fourth quartile and creating a linear regression model for both
plot(x = fourth_q$ERBB2..ENSG00000141736., y = fourth_q$EGFR..ENSG00000146648., xlab = "ERBB2 RNA Expression", ylab = "EGFR RNA Expression", pch = 20, col = "red", xlim = c(0,10), ylim = c(0,10))
points(x = first_q$ERBB2..ENSG00000141736., y = first_q$EGFR..ENSG00000146648., pch = 20, col = "blue")
abline(lm(fourth_q$EGFR..ENSG00000146648. ~ fourth_q$ERBB2..ENSG00000141736.), col = "red")
abline(lm(first_q$EGFR..ENSG00000146648. ~ first_q$ERBB2..ENSG00000141736.), col = "blue")
legend(x = 0, y = 10, c("Fourth Quartile", "First Quartile"), col = c("red", "blue"), pch = c(20, 20), cex = 0.8)
cor.test(fourth_q$ERBB2..ENSG00000141736., fourth_q$EGFR..ENSG00000146648.)
cor.test(first_q$ERBB2..ENSG00000141736., first_q$EGFR..ENSG00000146648.)

#Plotting ERBB3 expression against ERBB2 expression for the first and fourth quartile and creating a linear regression model for both
plot(x = fourth_q$ERBB2..ENSG00000141736., y = fourth_q$ERBB3..ENSG00000065361., xlab = "ERBB2 RNA Expression", ylab = "ERBB3 RNA Expression", pch = 20, col = "red", xlim = c(0,10), ylim = c(0,10))
points(x = first_q$ERBB2..ENSG00000141736., y = first_q$ERBB3..ENSG00000065361., pch = 20, col = "blue")
abline(lm(fourth_q$ERBB3..ENSG00000065361. ~ fourth_q$ERBB2..ENSG00000141736.), col = "red")
abline(lm(first_q$ERBB3..ENSG00000065361. ~ first_q$ERBB2..ENSG00000141736.), col = "blue")
legend(x = 0, y = 10.2, c("Fourth Quartile", "First Quartile"), col = c("red", "blue"), pch = c(20, 20), cex = 0.8)
cor.test(fourth_q$ERBB2..ENSG00000141736., fourth_q$ERBB3..ENSG00000065361.)
cor.test(first_q$ERBB2..ENSG00000141736., first_q$ERBB3..ENSG00000065361.)

#Plotting ERBB4 expression against ERBB2 expression for the first and fourth quartile and creating a linear regression model for both
plot(x = fourth_q$ERBB2..ENSG00000141736., y = fourth_q$ERBB4..ENSG00000178568., xlab = "ERBB2 RNA Expression", ylab = "ERBB4 RNA Expression", pch = 20, col = "red", xlim = c(0,10), ylim = c(0,10))
points(x = first_q$ERBB2..ENSG00000141736., y = first_q$ERBB4..ENSG00000178568., pch = 20, col = "blue")
abline(lm(fourth_q$ERBB4..ENSG00000178568. ~ fourth_q$ERBB2..ENSG00000141736.), col = "red")
abline(lm(first_q$ERBB4..ENSG00000178568. ~ first_q$ERBB2..ENSG00000141736.), col = "blue")
legend(x = 0, y = 10, c("Fourth Quartile", "First Quartile"), col = c("red", "blue"), pch = c(20, 20), cex = 0.8)
cor.test(fourth_q$ERBB2..ENSG00000141736., fourth_q$ERBB4..ENSG00000178568.)
cor.test(first_q$ERBB2..ENSG00000141736., first_q$ERBB4..ENSG00000178568.)

#Plotting APC expression against ERBB2 expression for the first and fourth quartile and creating a linear regression model for both
plot(x = fourth_q$ERBB2..ENSG00000141736., y = fourth_q$APC..ENSG00000134982., xlab = "ERBB2 RNA Expression", ylab = "APC RNA Expression", pch = 20, col = "red", xlim = c(0,10), ylim = c(0,10))
points(x = first_q$ERBB2..ENSG00000141736., y = first_q$APC..ENSG00000134982., pch = 20, col = "blue")
abline(lm(fourth_q$APC..ENSG00000134982. ~ fourth_q$ERBB2..ENSG00000141736.), col = "red")
abline(lm(first_q$APC..ENSG00000134982. ~ first_q$ERBB2..ENSG00000141736.), col = "blue")
legend(x = 0, y = 10, c("Fourth Quartile", "First Quartile"), col = c("red", "blue"), pch = c(20, 20), cex = 0.8)
cor.test(fourth_q$ERBB2..ENSG00000141736., fourth_q$APC..ENSG00000134982.)
cor.test(first_q$ERBB2..ENSG00000141736., first_q$APC..ENSG00000134982.)

#Plotting KRAS expression against ERBB2 expression for the first and fourth quartile and creating a linear regression model for both
plot(x = fourth_q$ERBB2..ENSG00000141736., y = fourth_q$KRAS..ENSG00000133703., xlab = "ERBB2 RNA Expression", ylab = "KRAS RNA Expression", pch = 20, col = "red", xlim = c(0,10), ylim = c(0,10))
points(x = first_q$ERBB2..ENSG00000141736., y = first_q$KRAS..ENSG00000133703., pch = 20, col = "blue")
abline(lm(fourth_q$KRAS..ENSG00000133703. ~ fourth_q$ERBB2..ENSG00000141736.), col = "red")
abline(lm(first_q$KRAS..ENSG00000133703. ~ first_q$ERBB2..ENSG00000141736.), col = "blue")
legend(x = 0, y = 10, c("Fourth Quartile", "First Quartile"), col = c("red", "blue"), pch = c(20, 20), cex = 0.8)
cor.test(fourth_q$ERBB2..ENSG00000141736., fourth_q$KRAS..ENSG00000133703.)
cor.test(first_q$ERBB2..ENSG00000141736., first_q$KRAS..ENSG00000133703.)

#Plotting EGF expression against ERBB2 expression for the first and fourth quartile and creating a linear regression model for both
plot(x = fourth_q$ERBB2..ENSG00000141736., y = fourth_q$EGF..ENSG00000138798., xlab = "ERBB2 RNA Expression", ylab = "EGF RNA Expression", pch = 20, col = "red", xlim = c(0,10), ylim = c(0,10))
points(x = first_q$ERBB2..ENSG00000141736., y = first_q$EGF..ENSG00000138798., pch = 20, col = "blue")
abline(lm(fourth_q$EGF..ENSG00000138798. ~ fourth_q$ERBB2..ENSG00000141736.), col = "red")
abline(lm(first_q$EGF..ENSG00000138798. ~ first_q$ERBB2..ENSG00000141736.), col = "blue")
legend(x = 0, y = 10, c("Fourth Quartile", "First Quartile"), col = c("red", "blue"), pch = c(20, 20), cex = 0.8)
cor.test(fourth_q$ERBB2..ENSG00000141736., fourth_q$EGF..ENSG00000138798.)
cor.test(first_q$ERBB2..ENSG00000141736., first_q$EGF..ENSG00000138798.)

#Plotting TNF expression against ERBB2 expression for the first and fourth quartile and creating a linear regression model for both
plot(x = fourth_q$ERBB2..ENSG00000141736., y = fourth_q$TNF..ENSG00000232810., xlab = "ERBB2 RNA Expression", ylab = "TNF RNA Expression", pch = 20, col = "red", xlim = c(0,10), ylim = c(0,10))
points(x = first_q$ERBB2..ENSG00000141736., y = first_q$TNF..ENSG00000232810., pch = 20, col = "blue")
abline(lm(fourth_q$TNF..ENSG00000232810. ~ fourth_q$ERBB2..ENSG00000141736.), col = "red")
abline(lm(first_q$TNF..ENSG00000232810. ~ first_q$ERBB2..ENSG00000141736.), col = "blue")
legend(x = 0, y = 10, c("Fourth Quartile", "First Quartile"), col = c("red", "blue"), pch = c(20, 20), cex = 0.8)
cor.test(fourth_q$ERBB2..ENSG00000141736., fourth_q$TNF..ENSG00000232810.)
cor.test(first_q$ERBB2..ENSG00000141736., first_q$TNF..ENSG00000232810.)

#Plotting NRG1 expression against ERBB2 expression for the first and fourth quartile and creating a linear regression model for both
plot(x = fourth_q$ERBB2..ENSG00000141736., y = fourth_q$NRG1..ENSG00000157168., xlab = "ERBB2 RNA Expression", ylab = "NRG1 RNA Expression", pch = 20, col = "red", xlim = c(0,10), ylim = c(0,10))
points(x = first_q$ERBB2..ENSG00000141736., y = first_q$NRG1..ENSG00000157168., pch = 20, col = "blue")
abline(lm(fourth_q$NRG1..ENSG00000157168. ~ fourth_q$ERBB2..ENSG00000141736.), col = "red")
abline(lm(first_q$NRG1..ENSG00000157168. ~ first_q$ERBB2..ENSG00000141736.), col = "blue")
legend(x = 0, y = 10, c("Fourth Quartile", "First Quartile"), col = c("red", "blue"), pch = c(20, 20), cex = 0.8)
cor.test(fourth_q$ERBB2..ENSG00000141736., fourth_q$NRG1..ENSG00000157168.)
cor.test(first_q$ERBB2..ENSG00000141736., first_q$NRG1..ENSG00000157168.)

#Plotting NRG2 expression against ERBB2 expression for the first and fourth quartile and creating a linear regression model for both
plot(x = fourth_q$ERBB2..ENSG00000141736., y = fourth_q$NRG2..ENSG00000158458., xlab = "ERBB2 RNA Expression", ylab = "NRG2 RNA Expression", pch = 20, col = "red", xlim = c(0,10), ylim = c(0,10))
points(x = first_q$ERBB2..ENSG00000141736., y = first_q$NRG2..ENSG00000158458., pch = 20, col = "blue")
abline(lm(fourth_q$NRG2..ENSG00000158458. ~ fourth_q$ERBB2..ENSG00000141736.), col = "red")
abline(lm(first_q$NRG2..ENSG00000158458. ~ first_q$ERBB2..ENSG00000141736.), col = "blue")
legend(x = 0, y = 10, c("Fourth Quartile", "First Quartile"), col = c("red", "blue"), pch = c(20, 20), cex = 0.8)
cor.test(fourth_q$ERBB2..ENSG00000141736., fourth_q$NRG2..ENSG00000158458.)
cor.test(first_q$ERBB2..ENSG00000141736., first_q$NRG2..ENSG00000158458.)

#Loading in gene effects data frame
gene_effects <- read.csv("C:\\Users\\Daniel\\Desktop\\CRISPR_gene_effect.csv", header = TRUE)

#Recoding the first column to be the DepMapID
rownames(gene_effects) <- gene_effects[,1]  
gene_effects[,1] <- NULL
rownames(gene_effects)[1]

#Loading in gene info data frame
gene_info <- read.csv("C:\\Users\\Daniel\\Desktop\\internal-21q3_v15-sample-info.csv", header = TRUE)

#Recoding the first column to be the DepMapID
rownames(gene_info) <- gene_info[,1]
gene_info[,1] <- NULL
rownames(gene_info)[1]

#Creating a data frame for the cell lines with ERBB2 Chronos dependency scores and ERBB2 expression
ERBB2_chronos <- gene_effects[c("ACH-000007", "ACH-000009", "ACH-000202", "ACH-000249", "ACH-000252", "ACH-000253", "ACH-000286", "ACH-000296", "ACH-000350", "ACH-000381", 
               "ACH-000403", "ACH-000421", "ACH-000467", "ACH-000470", "ACH-000491", "ACH-000501", "ACH-000532", "ACH-000552", "ACH-000565", "ACH-000651", 
               "ACH-000680", "ACH-000683", "ACH-000722", "ACH-000820", "ACH-000926", "ACH-000935", "ACH-000943", "ACH-000950", "ACH-000957", "ACH-000958", 
               "ACH-000959", "ACH-000963", "ACH-000969", "ACH-000970", "ACH-000971", "ACH-000985", "ACH-000986", "ACH-000991", "ACH-000997", "ACH-001061", 
               "ACH-001345", "ACH-001399", "ACH-001454", "ACH-001458", "ACH-001459", "ACH-001460", "ACH-001461", "ACH-001786", "ACH-002023", "ACH-002024", 
               "ACH-002025"), c("ERBB2..2064."), drop = FALSE]
ERBB2_expression <- new_expression[c("ACH-000007", "ACH-000009", "ACH-000202", "ACH-000249", "ACH-000252", "ACH-000253", "ACH-000286", "ACH-000296", "ACH-000350", "ACH-000381", 
                                     "ACH-000403", "ACH-000421", "ACH-000467", "ACH-000470", "ACH-000491", "ACH-000501", "ACH-000532", "ACH-000552", "ACH-000565", "ACH-000651", 
                                     "ACH-000680", "ACH-000683", "ACH-000722", "ACH-000820", "ACH-000926", "ACH-000935", "ACH-000943", "ACH-000950", "ACH-000957", "ACH-000958", 
                                     "ACH-000959", "ACH-000963", "ACH-000969", "ACH-000970", "ACH-000971", "ACH-000985", "ACH-000986", "ACH-000991", "ACH-000997", "ACH-001061", 
                                     "ACH-001345", "ACH-001399", "ACH-001454", "ACH-001458", "ACH-001459", "ACH-001460", "ACH-001461", "ACH-001786", "ACH-002023", "ACH-002024", 
                                     "ACH-002025"), c("ERBB2..ENSG00000141736."), drop = FALSE]
cell_line_gene_info <- gene_info[c("ACH-000007", "ACH-000009", "ACH-000202", "ACH-000249", "ACH-000252", "ACH-000253", "ACH-000286", "ACH-000296", "ACH-000350", "ACH-000381", 
                                   "ACH-000403", "ACH-000421", "ACH-000467", "ACH-000470", "ACH-000491", "ACH-000501", "ACH-000532", "ACH-000552", "ACH-000565", "ACH-000651", 
                                   "ACH-000680", "ACH-000683", "ACH-000722", "ACH-000820", "ACH-000926", "ACH-000935", "ACH-000943", "ACH-000950", "ACH-000957", "ACH-000958", 
                                   "ACH-000959", "ACH-000963", "ACH-000969", "ACH-000970", "ACH-000971", "ACH-000985", "ACH-000986", "ACH-000991", "ACH-000997", "ACH-001061", 
                                   "ACH-001345", "ACH-001399", "ACH-001454", "ACH-001458", "ACH-001459", "ACH-001460", "ACH-001461", "ACH-001786", "ACH-002023", "ACH-002024", 
                                   "ACH-002025"), c("stripped_cell_line_name"), drop = FALSE]
ERBB2_chronos$Expression <- ERBB2_expression$ERBB2..ENSG00000141736.
ERBB2_chronos$Name <- cell_line_gene_info$stripped_cell_line_name

#Plotting ERBB2 dependency against ERBB2 expression
ggplot(ERBB2_chronos, aes(ERBB2..2064., Expression)) + geom_point(aes(color = Expression)) + scale_color_gradient(low = "green", high = "red") + theme(panel.background = element_blank(), panel.grid.major = element_line(color = "gray88"), panel.grid.minor = element_line(color = "gray88")) + geom_hline(yintercept = 0, color = "black", size = 0.5) + geom_vline(xintercept = 0, color = "black", size = 0.5) + labs(x = "ERBB2 Gene Effect (CRISPR Chronos)", y = "ERBB2 RNA Expression") + scale_y_continuous(limits = c(0,9), breaks = seq(0, 9, by = 1))

#Plotting ERBB2 dependency against ERBB2 expression for the first and fourth quartile cell lines
chronos_14 <- ERBB2_chronos[c("ACH-000350", "ACH-002023", "ACH-000253", "ACH-001454", "ACH-000959", "ACH-000991", "ACH-000957", "ACH-000007", "ACH-000403", 
                              "ACH-001460", "ACH-000501", "ACH-000971", "ACH-000969", "ACH-001786", "ACH-000470", "ACH-002025", "ACH-001461", "ACH-000202", 
                              "ACH-000943", "ACH-001458", "ACH-000651", "ACH-001459", "ACH-000491", "ACH-002024"), c("ERBB2..2064.", "Expression", "Name"), 
                            drop = FALSE]
ggplot(chronos_14, aes(ERBB2..2064., Expression)) + geom_point(aes(color = Expression), size = 2.5) + scale_color_gradient(low = "green", high = "red") + geom_text_repel(label = chronos_14$Name, size = 3.0) + theme(panel.background = element_blank(), panel.grid.major = element_line(color = "gray88"), panel.grid.minor = element_line(color = "gray88")) + geom_hline(yintercept = 0, color = "black", size = 0.5) + geom_vline(xintercept = 0, color = "black", size = 0.5) + labs(x = "ERBB2 Gene Effect (CRISPR Chronos)", y = "ERBB2 RNA Expression") + scale_y_continuous(limits = c(0,9), breaks = seq(0, 9, by = 1))

#Adding copy number data to the expression data of the first and fourth quartile cell lines
fourth_q_cn <- copy_number[c("ACH-000350", "ACH-002023", "ACH-000967", "ACH-000998", "ACH-000253", "ACH-001454", "ACH-000982", "ACH-000360", "ACH-000959", "ACH-000400",
                             "ACH-000991", "ACH-000895", "ACH-000955", "ACH-000957", "ACH-000342", "ACH-000798", "ACH-000007", "ACH-000403"), c("ERBB2..2064."), drop = FALSE]
fourth_q$CN <- fourth_q_cn$ERBB2..2064.
first_q_cn <- copy_number[c("ACH-001456", "ACH-001460", "ACH-000501", "ACH-000971", "ACH-000969", "ACH-001786", "ACH-000470", "ACH-002025", "ACH-001461", "ACH-000202",
                            "ACH-000943", "ACH-001458", "ACH-002659", "ACH-000651", "ACH-000842", "ACH-001459", "ACH-000491", "ACH-002024"), c("ERBB2..2064."), drop = FALSE]
first_q$CN <- first_q_cn$ERBB2..2064.
new_first_q <- na.omit(first_q)

#Graphing ERBB2 expression and ERBB2 copy number for the first and fourth quartile and creating a linear regression model for both
plot(x = fourth_q$ERBB2..ENSG00000141736., y = fourth_q$CN, xlab = "ERBB2 RNA Expression", ylab = "ERBB2 Copy Number", pch = 20, col = "red", xlim = c(0, 10), ylim = c(0, 2.5))
points(x = new_first_q$ERBB2..ENSG00000141736., y = new_first_q$CN, pch = 20, col = "blue")
abline(lm(fourth_q$CN ~ fourth_q$ERBB2..ENSG00000141736.), col = "red")
abline(lm(new_first_q$CN ~ new_first_q$ERBB2..ENSG00000141736.), col = "blue")
legend(x = 0, y = 2.5, c("Fourth Quartile", "First Quartile"), col = c("red", "blue"), pch = c(20, 20), cex = 0.8)
cor.test(fourth_q$ERBB2..ENSG00000141736., fourth_q$CN)
cor.test(new_first_q$ERBB2..ENSG00000141736., new_first_q$CN)

#Loading in protein expression data frame
protein <- read.csv("C:\\Users\\Daniel\\Desktop\\total-proteome-_v1-protein-quant-current-normalized.csv")

#Recoding the first column to be the protein name
rownames(protein) <- protein[,3]
protein[,1] <- NULL
rownames(protein)[1]

#Checking to see whether the correct data is extracted from the data frame
protein[c("ERBB2_HUMAN Receptor tyrosine-protein kinase erbB-2"), c("LS513_LARGE_INTESTINE_TenPx09"), drop = FALSE]

#Protein data extraction
protein[c("ERBB2_HUMAN Receptor tyrosine-protein kinase erbB-2", "EGFR_HUMAN Epidermal growth factor receptor", "ERBB3_HUMAN Receptor tyrosine-protein kinase erbB-3",
          "ERBB4_HUMAN Receptor tyrosine-protein kinase erbB-4", "APC_HUMAN Adenomatous polyposis coli protein", "NRG1_HUMAN Isoform 2 of Pro-neuregulin-1, membrane-bound isoform"),
        c("LS513_LARGE_INTESTINE_TenPx09", "COLO320_LARGE_INTESTINE_TenPx14", "SW1417_LARGE_INTESTINE_TenPx15", "COLO678_LARGE_INTESTINE_TenPx13", "SKCO1_LARGE_INTESTINE_TenPx26",
          "NCIH747_LARGE_INTESTINE_TenPx16", "SW837_LARGE_INTESTINE_TenPx35", "HCC56_LARGE_INTESTINE_TenPx07", "NCIH716_LARGE_INTESTINE_TenPx36", "SNU61_LARGE_INTESTINE_TenPx12",
          "HT29_LARGE_INTESTINE_TenPx04", "SW620_LARGE_INTESTINE_TenPx03", "SW948_LARGE_INTESTINE_TenPx11", "SW948_LARGE_INTESTINE_TenPx20", "SNUC1_LARGE_INTESTINE_TenPx19",
          "SW403_LARGE_INTESTINE_TenPx07", "SW480_LARGE_INTESTINE_TenPx10", "CL34_LARGE_INTESTINE_TenPx38", "HT55_LARGE_INTESTINE_TenPx06", "MDST8_LARGE_INTESTINE_TenPx03",
          "RKO_LARGE_INTESTINE_TenPx04", "LS180_LARGE_INTESTINE_TenPx27", "SW48_LARGE_INTESTINE_TenPx07", "CCK81_LARGE_INTESTINE_TenPx39", "SNUC2A_LARGE_INTESTINE_TenPx17",
          "SNUC5_LARGE_INTESTINE_TenPx40", "HCT116_LARGE_INTESTINE_TenPx04", "LS411N_LARGE_INTESTINE_TenPx09", "HT115_LARGE_INTESTINE_TenPx04", "HCT15_LARGE_INTESTINE_TenPx18",
          "HCT15_LARGE_INTESTINE_TenPx30", "COLO205_LARGE_INTESTINE_TenPx20")]

#Making variables to contain the copy number data for the highest and lowest ERBB2 expressing cell lines
copy_number_fourth <- copy_number[c("ACH-000350", "ACH-002023", "ACH-000967", "ACH-000998", "ACH-000253", "ACH-001454",
                                    "ACH-000982", "ACH-000360", "ACH-000959", "ACH-000400", "ACH-000991", "ACH-000895",
                                    "ACH-000955", "ACH-000957", "ACH-000342", "ACH-000798", "ACH-000007", "ACH-000403"),
                                  c("TP53..7157.", "ERBB2..2064.", "EGFR..1956.", "ERBB3..2065.", "ERBB4..2066.", "APC..324.",
                                    "KRAS..3845.", "EGF..1950.", "TNF..7124.", "NRG1..3084.", "NRG2..9542."), drop = FALSE]
copy_number_first <- copy_number[c("ACH-001456", "ACH-001460", "ACH-000501", "ACH-000971", "ACH-000969", "ACH-001786",
                                   "ACH-000470", "ACH-002025", "ACH-001461", "ACH-000202", "ACH-000943", "ACH-001458",
                                   "ACH-000651", "ACH-000842", "ACH-001459", "ACH-000491", "ACH-002024"), c("TP53..7157.",
                                 "ERBB2..2064.", "EGFR..1956.", "ERBB3..2065.", "ERBB4..2066.", "APC..324.", "KRAS..3845.",
                                 "EGF..1950.", "TNF..7124.", "NRG1..3084.", "NRG2..9542."), drop = FALSE]

#Graphing TP53 copy number against ERBB2 copy number for the first and fourth quartiles and making a linear regression model
plot(x = copy_number_fourth$ERBB2..2064., y = copy_number_fourth$TP53..7157., xlab = "ERBB2 Copy Number", ylab = "TP53 Copy Number", pch = 20, col = "red", xlim = c(0, 2.5), ylim = c(0, 1.5))
points(x = copy_number_first$ERBB2..2064., y = copy_number_first$TP53..7157., pch = 20, col = "blue")
abline(lm(copy_number_fourth$TP53..7157. ~ copy_number_fourth$ERBB2..2064.), col = "red")
abline(lm(copy_number_first$TP53..7157. ~ copy_number_first$ERBB2..2064.), col = "blue")
legend(x = 0, y = 1.5, c("Fourth Quartile", "First Quartile"), col = c("red", "blue"), pch = c(20, 20), cex = 0.8)
cor.test(copy_number_fourth$ERBB2..2064., copy_number_fourth$TP53..7157.)
cor.test(copy_number_first$ERBB2..2064., copy_number_first$TP53..7157.)

#Graphing EGFR copy number against ERBB2 copy number for the first and fourth quartiles and making a linear regression model
plot(x = copy_number_fourth$ERBB2..2064., y = copy_number_fourth$EGFR..1956., xlab = "ERBB2 Copy Number", ylab = "EGFR Copy Number", pch = 20, col = "red", xlim = c(0, 2.5), ylim = c(0, 2))
points(x = copy_number_first$ERBB2..2064., y = copy_number_first$EGFR..1956., pch = 20, col = "blue")
abline(lm(copy_number_fourth$EGFR..1956. ~ copy_number_fourth$ERBB2..2064.), col = "red")
abline(lm(copy_number_first$EGFR..1956. ~ copy_number_first$ERBB2..2064.), col = "blue")
legend(x = 0, y = 0.5, c("Fourth Quartile", "First Quartile"), col = c("red", "blue"), pch = c(20, 20), cex = 0.8)
cor.test(copy_number_fourth$ERBB2..2064., copy_number_fourth$EGFR..1956.)
cor.test(copy_number_first$ERBB2..2064., copy_number_first$EGFR..1956.)

#Graphing ERBB3 copy number against ERBB2 copy number for the first and fourth quartiles and making a linear regression model
plot(x = copy_number_fourth$ERBB2..2064., y = copy_number_fourth$ERBB3..2065., xlab = "ERBB2 Copy Number", ylab = "ERBB3 Copy Number", pch = 20, col = "red", xlim = c(0, 2.5), ylim = c(0, 1.3))
points(x = copy_number_first$ERBB2..2064., y = copy_number_first$ERBB3..2065., pch = 20, col = "blue")
abline(lm(copy_number_fourth$ERBB3..2065. ~ copy_number_fourth$ERBB2..2064.), col = "red")
abline(lm(copy_number_first$ERBB3..2065. ~ copy_number_first$ERBB2..2064.), col = "blue")
legend(x = 0, y = 0.4, c("Fourth Quartile", "First Quartile"), col = c("red", "blue"), pch = c(20, 20), cex = 0.8)
cor.test(copy_number_fourth$ERBB2..2064., copy_number_fourth$ERBB3..2065.)
cor.test(copy_number_first$ERBB2..2064., copy_number_first$ERBB3..2065.)

#Graphing ERBB4 copy number against ERBB2 copy number for the first and fourth quartiles and making a linear regression model
plot(x = copy_number_fourth$ERBB2..2064., y = copy_number_fourth$ERBB4..2066., xlab = "ERBB2 Copy Number", ylab = "ERBB4 Copy Number", pch = 20, col = "red", xlim = c(0, 2.5), ylim = c(0, 1.3))
points(x = copy_number_first$ERBB2..2064., y = copy_number_first$ERBB4..2066., pch = 20, col = "blue")
abline(lm(copy_number_fourth$ERBB4..2066. ~ copy_number_fourth$ERBB2..2064.), col = "red")
abline(lm(copy_number_first$ERBB4..2066. ~ copy_number_first$ERBB2..2064.), col = "blue")
legend(x = 0, y = 0.4, c("Fourth Quartile", "First Quartile"), col = c("red", "blue"), pch = c(20, 20), cex = 0.8)
cor.test(copy_number_fourth$ERBB2..2064., copy_number_fourth$ERBB4..2066.)
cor.test(copy_number_first$ERBB2..2064., copy_number_first$ERBB4..2066.)

#Graphing APC copy number against ERBB2 copy number for the first and fourth quartiles and making a linear regression model
plot(x = copy_number_fourth$ERBB2..2064., y = copy_number_fourth$APC..324., xlab = "ERBB2 Copy Number", ylab = "APC Copy Number", pch = 20, col = "red", xlim = c(0, 2.5), ylim = c(0, 1.3))
points(x = copy_number_first$ERBB2..2064., y = copy_number_first$APC..324., pch = 20, col = "blue")
abline(lm(copy_number_fourth$APC..324. ~ copy_number_fourth$ERBB2..2064.), col = "red")
abline(lm(copy_number_first$APC..324. ~ copy_number_first$ERBB2..2064.), col = "blue")
legend(x = 0, y = 0.4, c("Fourth Quartile", "First Quartile"), col = c("red", "blue"), pch = c(20, 20), cex = 0.8)
cor.test(copy_number_fourth$ERBB2..2064., copy_number_fourth$APC..324.)
cor.test(copy_number_first$ERBB2..2064., copy_number_first$APC..324.)

#Graphing KRAS copy number against ERBB2 copy number for the first and fourth quartiles and making a linear regression model
plot(x = copy_number_fourth$ERBB2..2064., y = copy_number_fourth$KRAS..3845., xlab = "ERBB2 Copy Number", ylab = "KRAS Copy Number", pch = 20, col = "red", xlim = c(0, 2.5), ylim = c(0, 2.1))
points(x = copy_number_first$ERBB2..2064., y = copy_number_first$KRAS..3845., pch = 20, col = "blue")
abline(lm(copy_number_fourth$KRAS..3845. ~ copy_number_fourth$ERBB2..2064.), col = "red")
abline(lm(copy_number_first$KRAS..3845. ~ copy_number_first$ERBB2..2064.), col = "blue")
legend(x = 0, y = 0.5, c("Fourth Quartile", "First Quartile"), col = c("red", "blue"), pch = c(20, 20), cex = 0.8)
cor.test(copy_number_fourth$ERBB2..2064., copy_number_fourth$KRAS..3845.)
cor.test(copy_number_first$ERBB2..2064., copy_number_first$KRAS..3845.)

#Graphing EGF copy number against ERBB2 copy number for the first and fourth quartiles and making a linear regression model
plot(x = copy_number_fourth$ERBB2..2064., y = copy_number_fourth$EGF..1950., xlab = "ERBB2 Copy Number", ylab = "EGF Copy Number", pch = 20, col = "red", xlim = c(0, 2.5), ylim = c(0, 1.4))
points(x = copy_number_first$ERBB2..2064., y = copy_number_first$EGF..1950., pch = 20, col = "blue")
abline(lm(copy_number_fourth$EGF..1950. ~ copy_number_fourth$ERBB2..2064.), col = "red")
abline(lm(copy_number_first$EGF..1950. ~ copy_number_first$ERBB2..2064.), col = "blue")
legend(x = 0, y = 0.4, c("Fourth Quartile", "First Quartile"), col = c("red", "blue"), pch = c(20, 20), cex = 0.8)
cor.test(copy_number_fourth$ERBB2..2064., copy_number_fourth$EGF..1950.)
cor.test(copy_number_first$ERBB2..2064., copy_number_first$EGF..1950.)

#Graphing TNF copy number against ERBB2 copy number for the first and fourth quartiles and making a linear regression model
plot(x = copy_number_fourth$ERBB2..2064., y = copy_number_fourth$TNF..7124., xlab = "ERBB2 Copy Number", ylab = "TNF Copy Number", pch = 20, col = "red", xlim = c(0, 2.5), ylim = c(0, 1.3))
points(x = copy_number_first$ERBB2..2064., y = copy_number_first$TNF..7124., pch = 20, col = "blue")
abline(lm(copy_number_fourth$TNF..7124. ~ copy_number_fourth$ERBB2..2064.), col = "red")
abline(lm(copy_number_first$TNF..7124. ~ copy_number_first$ERBB2..2064.), col = "blue")
legend(x = 0, y = 0.4, c("Fourth Quartile", "First Quartile"), col = c("red", "blue"), pch = c(20, 20), cex = 0.8)
cor.test(copy_number_fourth$ERBB2..2064., copy_number_fourth$TNF..7124.)
cor.test(copy_number_first$ERBB2..2064., copy_number_first$TNF..7124.)

#Graphing NRG1 copy number against ERBB2 copy number for the first and fourth quartiles and making a linear regression model
plot(x = copy_number_fourth$ERBB2..2064., y = copy_number_fourth$NRG1..3084., xlab = "ERBB2 Copy Number", ylab = "NRG1 Copy Number", pch = 20, col = "red", xlim = c(0, 2.5), ylim = c(0, 1.25))
points(x = copy_number_first$ERBB2..2064., y = copy_number_first$NRG1..3084., pch = 20, col = "blue")
abline(lm(copy_number_fourth$NRG1..3084. ~ copy_number_fourth$ERBB2..2064.), col = "red")
abline(lm(copy_number_first$NRG1..3084. ~ copy_number_first$ERBB2..2064.), col = "blue")
legend(x = 0, y = 0.4, c("Fourth Quartile", "First Quartile"), col = c("red", "blue"), pch = c(20, 20), cex = 0.8)
cor.test(copy_number_fourth$ERBB2..2064., copy_number_fourth$NRG1..3084.)
cor.test(copy_number_first$ERBB2..2064., copy_number_first$NRG1..3084.)

#Graphing NRG2 copy number against ERBB2 copy number for the first and fourth quartiles and making a linear regression model
plot(x = copy_number_fourth$ERBB2..2064., y = copy_number_fourth$NRG2..9542., xlab = "ERBB2 Copy Number", ylab = "NRG2 Copy Number", pch = 20, col = "red", xlim = c(0, 2.5), ylim = c(0, 1.4))
points(x = copy_number_first$ERBB2..2064., y = copy_number_first$NRG2..9542., col = "blue", pch = 20)
abline(lm(copy_number_fourth$NRG2..9542. ~ copy_number_fourth$ERBB2..2064.), col = "red")
abline(lm(copy_number_first$NRG2..9542. ~ copy_number_first$ERBB2..2064.), col = "blue")
legend(x = 0, y = 0.4, c("Fourth Quartile", "First Quartile"), col = c("red", "blue"), pch = c(20, 20), cex = 0.8)
cor.test(copy_number_fourth$ERBB2..2064., copy_number_fourth$NRG2..9542.)
cor.test(copy_number_first$ERBB2..2064., copy_number_first$NRG2..9542.)

#Creating new variables for the protein expression data of the first and fourth quartiles
protein_fourth <- protein[c("ERBB2_HUMAN Receptor tyrosine-protein kinase erbB-2", "EGFR_HUMAN Epidermal growth factor receptor",
                            "ERBB3_HUMAN Receptor tyrosine-protein kinase erbB-3", "ERBB4_HUMAN Receptor tyrosine-protein kinase erbB-4",
                            "APC_HUMAN Adenomatous polyposis coli protein", "NRG1_HUMAN Isoform 2 of Pro-neuregulin-1, membrane-bound isoform"),
                          c("COLO678_LARGE_INTESTINE_TenPx13", "SNUC2A_LARGE_INTESTINE_TenPx17", "SKCO1_LARGE_INTESTINE_TenPx26",
                            "CL34_LARGE_INTESTINE_TenPx38", "LS180_LARGE_INTESTINE_TenPx27", "LS513_LARGE_INTESTINE_TenPx09", "NCIH747_LARGE_INTESTINE_TenPx16"),
                          drop = FALSE]
protein_first <- protein[c("ERBB2_HUMAN Receptor tyrosine-protein kinase erbB-2", "EGFR_HUMAN Epidermal growth factor receptor",
                           "ERBB3_HUMAN Receptor tyrosine-protein kinase erbB-3", "ERBB4_HUMAN Receptor tyrosine-protein kinase erbB-4",
                           "APC_HUMAN Adenomatous polyposis coli protein", "NRG1_HUMAN Isoform 2 of Pro-neuregulin-1, membrane-bound isoform"),
                         c("HCT116_LARGE_INTESTINE_TenPx04", "COLO320_LARGE_INTESTINE_TenPx14", "RKO_LARGE_INTESTINE_TenPx04",
                           "SW620_LARGE_INTESTINE_TenPx03", "SW480_LARGE_INTESTINE_TenPx10", "NCIH716_LARGE_INTESTINE_TenPx36"),
                         drop = FALSE]
protein_fourth_t <- t(protein_fourth)
protein_first_t <- t(protein_first)

#Renaming the columns for the protein expression data
colnames(protein_fourth_t)[1] <- "ERBB2"
colnames(protein_fourth_t)[2] <- "EGFR"
colnames(protein_fourth_t)[3] <- "ERBB3"
colnames(protein_fourth_t)[4] <- "ERBB4"
colnames(protein_fourth_t)[5] <- "APC"
colnames(protein_fourth_t)[6] <- "NRG1"
colnames(protein_fourth_t)
colnames(protein_first_t)[1] <- "ERBB2"
colnames(protein_first_t)[2] <- "EGFR"
colnames(protein_first_t)[3] <- "ERBB3"
colnames(protein_first_t)[4] <- "ERBB4"
colnames(protein_first_t)[5] <- "APC"
colnames(protein_first_t)[6] <- "NRG1"
colnames(protein_first_t)

#Converting the first and fourth quartile protein expression matrix into a dataframe
protein_fourth_matrix <- as.data.frame(protein_fourth_t)
protein_first_matrix <- as.data.frame(protein_first_t)

#Graphing EGFR protein expression against ERBB2 protein expression for the first and fourth quartiles and making a linear regression model
plot(x = protein_fourth_matrix$ERBB2, y = protein_fourth_matrix$EGFR, xlab = "ERBB2 Protein Expression", ylab = "EGFR Protein Expression", pch = 20, col = "red", xlim = c(-1, 3.5), ylim = c(-3, 1))
points(x = protein_first_matrix$ERBB2, y = protein_first_matrix$EGFR, pch = 20, col = "blue")
abline(lm(protein_fourth_matrix$EGFR ~ protein_fourth_matrix$ERBB2), col = "red")
abline(lm(protein_first_matrix$EGFR ~ protein_first_matrix$ERBB2), col = "blue")
legend(x = 2.5, y = -2, c("Fourth Quartile", "First Quartile"), col = c("red", "blue"), pch = c(20, 20), cex = 0.8)
cor.test(protein_fourth_matrix$ERBB2, protein_fourth_matrix$EGFR)
cor.test(protein_first_matrix$ERBB2, protein_first_matrix$EGFR)

#Graphing ERBB3 protein expression against ERBB2 protein expression for the first and fourth quartiles and making a linear regression model
plot(x = protein_fourth_matrix$ERBB2, y = protein_fourth_matrix$ERBB3, xlab = "ERBB2 Protein Expression", ylab = "ERBB3 Protein Expression", pch = 20, col = "red", xlim = c(-1, 3.5), ylim = c(-1.5, 2.5))
points(x = protein_first_matrix$ERBB2, y = protein_first_matrix$ERBB3, pch = 20, col = "blue")
abline(lm(protein_fourth_matrix$ERBB3 ~ protein_fourth_matrix$ERBB2), col = "red")
abline(lm(protein_first_matrix$ERBB3 ~ protein_first_matrix$ERBB2), col = "blue")
legend(x = 2.5, y = -0.5, c("Fourth Quartile", "First Quartile"), col = c("red", "blue"), pch = c(20, 20), cex = 0.8)
cor.test(protein_fourth_matrix$ERBB2, protein_fourth_matrix$ERBB3)
cor.test(protein_first_matrix$ERBB2, protein_first_matrix$ERBB3)

#Graphing ERBB4 protein expression against ERBB2 protein expression for the first and fourth quartiles and making a linear regression model
plot(x = protein_fourth_matrix$ERBB2, y = protein_fourth_matrix$ERBB4, xlab = "ERBB2 Protein Expression", ylab = "ERBB4 Protein Expression", pch = 20, col = "red", xlim = c(-1, 3.5), ylim = c(-2.5, 1.5))
points(x = protein_first_matrix$ERBB2, y = protein_first_matrix$ERBB4, pch = 20, col = "blue")
abline(lm(protein_fourth_matrix$ERBB4 ~ protein_fourth_matrix$ERBB2), col = "red")
abline(lm(protein_first_matrix$ERBB4 ~ protein_first_matrix$ERBB2), col = "blue")
legend(x = 2, y = -1.5, c("Fourth Quartile", "First Quartile"), col = c("red", "blue"), pch = c(20, 20), cex = 0.8)
cor.test(protein_fourth_matrix$ERBB2, protein_fourth_matrix$ERBB4)
cor.test(protein_first_matrix$ERBB2, protein_first_matrix$ERBB4)

#Graphing APC protein expression against ERBB2 protein expression for the first and fourth quartiles and making a linear regression model
plot(x = protein_fourth_matrix$ERBB2, y = protein_fourth_matrix$APC, xlab = "ERBB2 Protein Expression", ylab = "APC Protein Expression", pch = 20, col = "red", xlim = c(-1, 3.5), ylim = c(-2.6, 1.6))
points(x = protein_first_matrix$ERBB2, y = protein_first_matrix$APC, pch = 20, col = "blue")
abline(lm(protein_fourth_matrix$APC ~ protein_fourth_matrix$ERBB2), col = "red")
abline(lm(protein_first_matrix$APC ~ protein_first_matrix$ERBB2), col = "blue")
legend(x = 2.5, y = -1.5, c("Fourth Quartile", "First Quartile"), col = c("red", "blue"), pch = c(20, 20), cex = 0.8)
cor.test(protein_fourth_matrix$ERBB2, protein_fourth_matrix$APC)
cor.test(protein_first_matrix$ERBB2, protein_first_matrix$APC)

#Graphing NRG1 protein expression against ERBB2 protein expression for the first and fourth quartiles and making a linear regression model
plot(x = protein_fourth_matrix$ERBB2, y = protein_fourth_matrix$NRG1, xlab = "ERBB2 Protein Expression", ylab = "NRG1 Protein Expression", pch = 20, col = "red", xlim = c(-1, 3.5), ylim = c(-2.5, 0.6))
points(x = protein_first_matrix$ERBB2, y = protein_first_matrix$NRG1, pch = 20, col = "blue")
abline(lm(protein_fourth_matrix$NRG1 ~ protein_fourth_matrix$ERBB2), col = "red")
abline(lm(protein_first_matrix$NRG1 ~ protein_first_matrix$ERBB2), col = "blue")
legend(x = 2.5, y = 0, c("Fourth Quartile", "First Quartile"), col = c("red", "blue"), pch = c(20, 20), cex = 0.8)
cor.test(protein_fourth_matrix$ERBB2, protein_fourth_matrix$NRG1)

#Loading in the RNAi data frame
RNAi <- read.csv("C:\\Users\\Daniel\\Desktop\\RNAi_(Achilles+DRIVE+Marcotte,_DEMETER2).csv")

#Recoding the row names to be the DepMapID
rownames(RNAi) <- RNAi[,1]  
RNAi[,1] <- NULL
rownames(RNAi)[1]

#Extracting the RNAi data for the first and fourth quartiles
RNAi_14 <- RNAi[c("ACH-000350", "ACH-000967", "ACH-000998", "ACH-000253", "ACH-000982",
                  "ACH-000360", "ACH-000959", "ACH-000400", "ACH-000991", "ACH-000895", 
                  "ACH-000955", "ACH-000957", "ACH-000007", "ACH-000403", "ACH-000971", 
                  "ACH-000969", "ACH-000470", "ACH-000202", "ACH-000943", "ACH-000651", 
                  "ACH-000842", "ACH-000491"), c("ERBB2"), drop = FALSE]
RNAi_14$Expression <- expression_data[c("ACH-000350", "ACH-000967", "ACH-000998", "ACH-000253", "ACH-000982",
                                         "ACH-000360", "ACH-000959", "ACH-000400", "ACH-000991", "ACH-000895", 
                                         "ACH-000955", "ACH-000957", "ACH-000007", "ACH-000403", "ACH-000971", 
                                         "ACH-000969", "ACH-000470", "ACH-000202", "ACH-000943", "ACH-000651", 
                                         "ACH-000842", "ACH-000491"), c("ERBB2..ENSG00000141736.")]
RNAi_14$Name <- gene_info[c("ACH-000350", "ACH-000967", "ACH-000998", "ACH-000253", "ACH-000982",
                            "ACH-000360", "ACH-000959", "ACH-000400", "ACH-000991", "ACH-000895", 
                            "ACH-000955", "ACH-000957", "ACH-000007", "ACH-000403", "ACH-000971", 
                            "ACH-000969", "ACH-000470", "ACH-000202", "ACH-000943", "ACH-000651", 
                            "ACH-000842", "ACH-000491"), c("stripped_cell_line_name")]

#Plotting RNAi scores against ERBB2 expression for the first and fourth quartile cell lines
ggplot(RNAi_14, aes(ERBB2, Expression)) + geom_point(aes(color = Expression), size = 2.5) + scale_color_gradient(low = "green", high = "red") + geom_text_repel(label = RNAi_14$Name, size = 3.0) + theme(panel.background = element_blank(), panel.grid.major = element_line(color = "gray88"), panel.grid.minor = element_line(color = "gray88")) + geom_hline(yintercept = 0, color = "black", size = 0.5) + geom_vline(xintercept = 0, color = "black", size = 0.5) + labs(x = "ERBB2 RNAi", y = "ERBB2 RNA Expression") + scale_y_continuous(limits = c(0,9), breaks = seq(0, 9, by = 1))

#Graphing NRG1 dependency against NRG1 RNA expression 
NRG1_RNA <- expression_data[c("ACH-000003", "ACH-000007", "ACH-000009", "ACH-000089", "ACH-000202", "ACH-000236", "ACH-000249", "ACH-000252", "ACH-000253",
                             "ACH-000286", "ACH-000296", "ACH-000342", "ACH-000350", "ACH-000360", "ACH-000381", "ACH-000400", "ACH-000403", "ACH-000412",
                             "ACH-000421", "ACH-000467", "ACH-000470", "ACH-000489", "ACH-000491", "ACH-000501", "ACH-000532", "ACH-000552", "ACH-000565",
                             "ACH-000651", "ACH-000680", "ACH-000683", "ACH-000708", "ACH-000722", "ACH-000798", "ACH-000820", "ACH-000842", "ACH-000895",
                             "ACH-000926", "ACH-000935", "ACH-000943", "ACH-000950", "ACH-000955", "ACH-000957", "ACH-000958", "ACH-000959", "ACH-000963",
                             "ACH-000967", "ACH-000969", "ACH-000970", "ACH-000971", "ACH-000982", "ACH-000985", "ACH-000986", "ACH-000989", "ACH-000991",
                             "ACH-000997", "ACH-000998", "ACH-000999", "ACH-001061", "ACH-001345", "ACH-001399", "ACH-001454", "ACH-001456", "ACH-001458",
                             "ACH-001459", "ACH-001460", "ACH-001461", "ACH-001786", "ACH-002023", "ACH-002024", "ACH-002025", "ACH-002659"),
                           c("NRG1..ENSG00000157168.", "ERBB2..ENSG00000141736."), drop = FALSE]









