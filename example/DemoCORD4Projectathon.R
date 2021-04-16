#install.packages('DSI')
#install.packages('DSOpal')
#install.packages('DSLite')

#install.packages('dsBaseClient', repos='http://cran.obiba.org', type='source')


library(DSI)
library(DSOpal)
library(dsBaseClient)

#########################################################################
###                         Connections                               ###
#########################################################################

builder <- DSI::newDSLoginBuilder()
builder$append(server = "study1",  url = "http://192.168.56.100:8080/",
               user = "administrator", password = "datashield_test&",
               table = "CORD4.BlockA", driver = "OpalDriver")
builder$append(server = "study2", url = "http://192.168.56.101:8080/",
               user = "administrator", password = "datashield_test&",
               table = "CORD4.BlockA", driver = "OpalDriver")

logindataA <- logindataB <-logindataC <- builder$build()
logindataB$table <- "CORD4.BlockB"
logindataC$table <- "CORD4.BlockC"


#########################################################################
###                              Block A                              ###
#########################################################################
connA <- DSI::datashield.login(logins = logindataA, assign = TRUE, symbol = "DFA")

ds.dim("DFA", datasources = connA)
ds.colnames("DFA", datasources = connA)


ds.cut("DFA$Alter", newobj.name = "AlterA_1",
                   breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90), ngroups = FALSE, 
                  labels = c("Bis10","Bis20", "Bis30", "Bis40", "Bis50", "Bis60", "Bis70", "Bis80", "Bis90","BisMax"),
                   datasources=connA)
res <- ds.table("AlterA_1", datasources = connA)

ds.cut("DFA$Alter", newobj.name = "AlterA_2",
       breaks = c(10, 20, 30, 40, 50, 60, 70, 80), ngroups = FALSE, 
       labels = c("Bis10","Bis20", "Bis30", "Bis40", "Bis50", "Bis60", "Bis70", "Bis80","BisMax"),
       datasources=connA)
res <- ds.table("AlterA_2", datasources = connA)
res$output.list

ds.asFactor("DFA$Sex","A_Sex",datasources = connA)
res <- ds.table("AlterA_2","A_Sex", datasources = connA)
res$output.list

res_female_all <- data.frame(Alter = rownames(res$output.list$TABLES.COMBINED_all.sources_counts),
                             Geschlecht = "female",
                             Anzahl = res$output.list$TABLES.COMBINED_all.sources_counts[,1])
res_male_all <- data.frame(Alter = rownames(res$output.list$TABLES.COMBINED_all.sources_counts),
                             Geschlecht = "male",
                             Anzahl = res$output.list$TABLES.COMBINED_all.sources_counts[,2]*(-1))
res_all <- rbind(res_female_all,res_male_all)

res_female_1 <- data.frame(Alter = rownames(res$output.list$TABLE_STUDY.1_counts),
                             Geschlecht = "female",
                             Anzahl = res$output.list$TABLE_STUDY.1_counts[,1])
res_male_1 <- data.frame(Alter = rownames(res$output.list$TABLE_STUDY.1_counts),
                           Geschlecht = "male",
                           Anzahl = res$output.list$TABLE_STUDY.1_counts[,2]*(-1))
res_1 <- rbind(res_female_1,res_male_1)


#Alterspyramid konzipieren
ggplot(res_all,aes(x=Anzahl,y=Alter,fill=Geschlecht)) + geom_bar(stat="identity")

ggplot(res_1,aes(x=Anzahl,y=Alter,fill=Geschlecht))+ geom_bar(stat="identity")


# Zusatz
ds.histogram("DFA$Alter",datasources = connA)
ds.histogram("DFA$Alter",type = "combine",datasources = connA)

res <- ds.table("AlterA_2","A_Sex", 
                report.chisq.tests = TRUE, 
                datasources = connA)
res$chisq.tests

#########################################################################
###                              Block B                              ###
#########################################################################
connB <- DSI::datashield.login(logins = logindataB, assign = TRUE, symbol = "DFB")

ds.dim("DFB", datasources = connB)
ds.colnames("DFB", datasources = connB)

res <- ds.table("DFB$code_f","DFB$start_month", datasources = connB)

res <- ds.table("DFB$code_grouped_f","DFB$start_month", datasources = connB)
res$output.list

res_all <- data.frame(Monat = as.numeric(colnames(res$output.list$TABLES.COMBINED_all.sources_counts)[1:12]),
                             E70 = res$output.list$TABLES.COMBINED_all.sources_counts[1,1:12],
                            E84 = res$output.list$TABLES.COMBINED_all.sources_counts[2,1:12])

res_1 <- data.frame(Monat = as.numeric(colnames(res$output.list$TABLES.COMBINED_all.sources_counts)[1:12]),
                      E70 = res$output.list$TABLE_STUDY.1_counts[1,1:12],
                      E84 = res$output.list$TABLE_STUDY.1_counts[2,1:12])


ggplot(res_all, aes(Monat,group=1))+geom_line(aes(y=E70,color='E70',group=1))+geom_line(aes(y=E84,color='E84',group=1))+labs(title = 'Timeseries of Diagnoses',x='Month',y='Count',color ='Condition')

ggplot(res_1, aes(Monat,group=1))+geom_line(aes(y=E70,color='E70',group=1))+geom_line(aes(y=E84,color='E84',group=1))+labs(title = 'Timeseries of Diagnoses',x='Month',y='Count',color ='Condition')


#########################################################################
###                              Block C                              ###
#########################################################################

connC <- DSI::datashield.login(logins = logindataC, assign = TRUE, symbol = "DFC")

ds.dim("DFC", datasources = connC)
ds.colnames("DFC", datasources = connC)

ds.cut("DFC$AngabeAlter", newobj.name = "AlterC_1",
       breaks = c(10, 20, 30, 40, 50, 60, 70, 80), ngroups = FALSE, 
       labels = c("Bis10","Bis20", "Bis30", "Bis40", "Bis50", "Bis60", "Bis70", "Bis80", "BisMax"),
       datasources=connC)
res <- ds.table("AlterC_1", datasources = connC)

res <- ds.table("AlterC_1","DFC$outcome", datasources = connC)

ds.cut("DFC$AngabeAlter", newobj.name = "AlterC_2",
       breaks = c(10, 20, 30), ngroups = FALSE, 
       labels = c("Bis10","Bis20", "Bis30","BisBisMax"),
       datasources=connC)

res <- ds.table("AlterC_2","DFC$outcome", datasources = connC)
res$output.list

ds.cut("DFC$AngabeAlter", newobj.name = "AlterC_3",
       breaks = 4, ngroups = TRUE, dig.lab = 3,
       labels = c("1","2","3","4"),
       datasources=connC)
res <- ds.table("AlterC_3", datasources = connC)
res$output.list

ds.cut("DFC$AngabeAlter", newobj.name = "AlterC_4",
       breaks = 2, ngroups = TRUE, dig.lab = 3,
       labels = c("1","2"),
       datasources=connC)
res <- ds.table("AlterC_4","DFC$outcome", datasources = connC)
res$output.list

#Zusatz
ds.glm(formula = "outcome~AngabeAlter",
       data = "DFC",
       family = "binomial",
       datasources = connC)

ds.asFactor("DFC$AngabeGeschlecht","C_Sex",datasources = connC)
ds.glm(formula = "DFC$outcome~C_Sex",
       family = "binomial",
       datasources = connC)


       
       
       
       