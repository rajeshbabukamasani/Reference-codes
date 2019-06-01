library(RGtk2)
library(rattle)
library(rpart)
library(rpart.plot)
library(RColorBrewer)

HRtree <- rpart(Attrition~
              DepartmentHumanResources+DepartmentResearchDevelopment+
                GenderMale+
              Age+
              Marital_StatusMarried+Marital_StatusSingle+
              # EducationBachelor+EducationBelowCollege+EducationCollege+EducationDoctor+
              Education_FieldHumanResources+Education_FieldLifeSciences+Education_FieldMarketing+Education_FieldMedical+Education_FieldTechnicalDegree+
              Business_TravelNonTravel+Business_TravelTravel_Frequently+
              Distance_From_Home+
              Job_InvolvementHigh+Job_InvolvementLow+Job_InvolvementMedium+
              # Job_Level1+Job_Level2+Job_Level3+Job_Level4+
              Job_SatisfactionHigh+Job_SatisfactionLow+Job_SatisfactionMedium+
              # Hourly_Rate+
              # Daily_Rate+
              # Monthly_Rate+
              # Monthly_Income+
              # Salary_Hike+
              # Stock_Option_Level0+Stock_Option_Level1+Stock_Option_Level2+
              Over_Time+
              No_of_Companies_Worked+
              # Total_Working_Years+
              # Years_At_Company+
              # Years_In_Current_Role+
              Years_Since_Last_Promotion+
              # Years_With_Curr_Manager+
              Environment_SatisfactionHigh+Environment_SatisfactionLow+Environment_SatisfactionMedium+
              Training_Times_Last_Year+
              Work_Life_BalanceBad+Work_Life_BalanceBest+Work_Life_BalanceBetter+
              # Performance_Rating+
              Relationship_SatisfactionHigh+Relationship_SatisfactionLow+Relationship_SatisfactionMedium+
              Job_Rolev1manfdirec_HealthcRepre+Job_Rolev1Labtech_HR+Job_Rolev1Manager+Job_Rolev1ResearchDirector+Job_Rolev1ResearchScientist+Job_Rolev1SalesExecutive
            ,data=train,method="class")

HRtree.ptree <- prune(HRtree, cp = opt)

plot(HRtree.ptree)
text(HRtree.ptree, pretty=0)

summary(HRtree)
HRtree$cptable
HRtree$splits

opt <- HRtree$cptable[which.min(HRtree$cptable[,"xerror"]),"CP"]

fancyRpartPlot(HRtree.ptree)

