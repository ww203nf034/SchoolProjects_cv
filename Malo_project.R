# import packages
packages<-c('rjson','jsonlite','dplyr','data.table',
            'purrr'  ,'ggplot2','tidyverse' ,'arules',
            'stringr','sqldf','FactoMineR','arulesViz',
            'readxl', 'reshape', 'lattice', 'GGally',
            'grid', 'gridExtra', 'graphics', 'GGally')
package.check <- lapply(                                      
  packages,
  FUN = function(x){
    if(!require(x, character.only = TRUE)){
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE, quietly = T)
    }
  }
)

# TASK1 : identify invalid questions whose correct answers rate is greater then 80 % or less then 5 % 
# the process is repeated so here I only take the "correct answers rate > 80% for example"

# STEP 1 : write a function to count the percentage
f_80_percentage<-function(x){
  question_1<-data%>%
    filter(layout_name==x)%>%
    select(question_id, satisfied, n_section_title)%>%
    group_by(question_id, satisfied,n_section_title)%>%
    summarise(numbers=n())
  question_2<- data%>%
    filter(layout_name==x)%>%
    select(question_id)%>%
    group_by(question_id)%>%
    summarise(total=n())
  merge1<-merge(question_1, question_2, by='question_id')%>%
    mutate(rate=round(numbers/total*100,2))%>%
    filter(satisfied==TRUE&rate>80)%>%
    select(question_id, n_section_title)}


# STEP 2 : apply the functions to every questionnaire
# create a vector for each type of questionnaires
months<-c("8_15_days", "1_month", "2_months", "3_months", "4_months", "5_months", "6_months", 
          "7_months", "8_months", "9_months", "11_months", "12_months", "16_months", "18_months",
          "21_months", "24_months", "30_months","36_months")

# apply the function to each questionnaires
eighty<-lapply(months, f_80_percentage)

# create the first table to the merge / output table to union the reset of the questionnaires
merge<-as.data.table(eighty[1])
output<-as.data.table(eighty[1])

# transfer all the list into data table and union them 
for(i in 1:length(eighty)){
  merge_2<-as.data.table(eighty[i])            
  if(i<1){                                       
    output<-union(merge, merge_2)        
  }else{
    output<-union(output, merge_2)}                   
  i<-i+1                             
}  

# STEP 3 : count the proportion of invalid questions in total questions 
# count how many unique question
question_id_count<-print(nrow(as.data.frame(table(data$question_id))))  # nrow : count the numbers of row 

# count how many questions have more than 80 % of satisfied answer 
question_80_count<-print(nrow(as.data.frame(table(question_80$question_id))))

# count the percentage
percentage_question_80<-print(question_80_count/question_id_count)

# STEP 4 : data visualization 
# count how many unique questions by month
f_q<-function(x){
  data%>%
    select(layout_name, question_id)%>%
    filter(layout_name==x&question_id>x)%>%        
    distinct(layout_name, question_id)}
# apply the function to each questionnaires by month
questionnaire<-lapply(months, f_q) 

# create the first table to merge table to union the rest of the questionnaires
merge_viz<-as.data.table(questionnaire[1])

# create the first table to output table to union the rest of the questionnaires
output_viz<-as.data.table(questionnaire[1])

# transfer all the list into data table and union them 
for(i in 1:length(questionnaire)){
  merge_2<-as.data.table(questionnaire[i])         
  if(i<1){                                        
    output<-union(merge_viz, merge_2)        
  }else{
    output<-union(output_viz, merge_2)}                   
  i<-i+1                       
}  

# recall the original information from original dataset to pull out the unique question_id and layout_name
# 80 % 
data_80_percentage<-filter(data, question_id %in% question_80$question_id)%>%   
  distinct(layout_name, question_id)

# 5% (question_5 is a data table which contains the questions whose correct answers rate is smaller than 5%)
data_5_percentage<-filter(data, question_id %in% question_5$question_id)%>%
  distinct(layout_name, question_id)

# combine the results of 80 % and 5%
data_85<-rbind(data_80_percentage, data_5_percentage)  

# total number of invalid question per layout 
data_85_total<-data_85%>%
  group_by(layout_name)%>%
  summarise(numbers = n())

# total number of unique questions per layout
merge_count<-output%>%
  group_by(layout_name)%>%
  summarise(total=n())

# add a column: the proportion of invalid questions per layout 
merge_total_count<-merge(data_85_total, merge_count, by='layout_name')%>%
  mutate(percentage= numbers/total)

# reorder the layout name
merge_total_count<- within(merge_total_count,layout_name <- factor(
  layout_name,levels=c("8_15_days", "1_month", "2_months", "3_months", "4_months", "5_months", "6_months", 
                       "7_months", "8_months", "9_months", "11_months", "12_months", "16_months", "18_months",
                       "21_months", "24_months", "30_months","36_months")))

# final graph  (take correct answers rate is greater than 80 % for example)
percen80<-ggplot(merge_total_count, aes(x=layout_name, y=total, fill=percentage))+
  geom_col()+
  coord_flip()+      
  scale_fill_gradient(low = "lightblue", high = "deepskyblue4")+
  labs(title='Percentage of Questions asked too early or too late', x='Layout Name', y='Nº of Questions')

percen80<-percen80+theme(title=element_text(size=9))  

#-------------------------------------------------------------------------------
# TASK 2 : discover the correlation between two categories of questions 

# STEP 1 : combine id and question_title 
data$id <- transform(str_c("id_",as.numeric(factor(data$id))))       
data$question_title <- transform(str_c("q_", 
                                       data$n_section_id,
                                       "_",
                                       as.numeric(factor(data$question_title))))

# step 2 : filter questions gotten wrong answer and transfer the text into a number
newTable <- date%>%
  filter(satisfied=='FALSE')%>%
  select(id,question_title,satisfied)%>%
  distinct()                          
# transfer "FALSE" into number as 1 
newTable$satisfied <- as.numeric(!newTable$satisfied)
# modify the column name
colnames(newTable)[3] <- "value"

# inverse the dataframe, set NA value as 0 and transfer the dataframe into matrix 
tb.id_q <- dcast(newTable, id~question_title)
tb.id_q[is.na(tb.id_q)] <- 0
tb.id_q2 <- as.matrix(tb.id_q[,-1])

# STEP 4 : model building 
# frequent item generation : two questions once
questionsets1 <- apriori(tb.id_q2,parameter=list(minlen=2,maxlen=2,support=0.001,target="frequent itemsets"))
summary(questionsets1)

# top 10 questions got wrong answer together frequently 
inspect(head(sort(itemsets1, by = "support"), 10))

# create the rules
rules <- apriori(tb.id_q2, parameter = list(supp=0.02, conf=0.8))     

# highlight the top 15 rules of 52 rules
highLiftRules<-head(sort(rules, by="lift"),15) 
inspect(highLiftRules)

# final graph 
plot(highLiftRules, method="graph" , control=list (type="items" )) 
plot(rules, method = NULL, measure = "support")   

#-------------------------------------------------------------------------------
# TASK 3 : identify the questions which attribute to the alerting system of Malo

# STEP 1 : filter the analysis_id and select only answers are wrong
grade <-data%>%
  filter(satisfied==FALSE)%>%            
  group_by(grade, analysis_id)%>%
  summarise(numbers=n())%>%              
  mutate(analysis_grade=case_when(
    analysis_id=='analysis_grade_0'~'grade 0',       
    analysis_id=='analysis_grade_1'~'grade 1',
    analysis_id=='analysis_grade_2'~'grade 2',
    analysis_id=='analysis_grade_3'~'grade 3',
    analysis_id=='analysis_grade_4'~'grade 4'))

# total number of people in each question_grade
grade_1<-data%>%
  filter(satisfied==FALSE)%>%
  group_by(grade)%>%
  summarise(total=n())

# merge two dateframe together 
result<-merge(grade, grade_1, by='grade', all.x=T)%>%
  mutate(percentage=numbers/total)   # add a new columnn to count the percentage 

# final graph 
ggplot(result, aes(x=grade, y=percentage, fill=analysis_grade))+
  geom_col()+
  scale_fill_manual(values=c('#F2C80F', '#FD625E', '#A66999', '#FE9666', '#A3D0D4'))+
  labs(title='', x="Question's Grade", y='Propotion of Unsatisfied Answers per Grade', fill="Analysis Grade")

# STEP 2 : find the questions which have the high correlation to the alert
# take one category for example 

# function to weight attributes (under 6 months)
f_weight<-function(x){
  weight<-data%>%
    filter(layout_name==x&
             attributes.weight==TRUE)       
  weight1<-weight%>%
    filter(satisfied==FALSE&
             (n_analysis_id==0|
                n_analysis_id==1))%>%
    group_by(question_id, grade, n_section_title)%>%
    summarise(numbers=n())
  weight2<-weight%>%
    filter(satisfied==FALSE&
             (n_analysis_id==2|
                n_analysis_id==3))%>%
    group_by(question_id, grade, n_section_title)%>%
    summarise(numbers=n())
  weight3<-merge(weight1, weight2, by='question_id', all=TRUE)%>%
    filter(is.na(numbers.x)&grade.y==3)}

# STEP 3 : apply the function to each layout 
# create a vector
months_att<-c("8_15_days", "1_month", "2_months", "3_months", "4_months", "5_months", "6_months")

#  apply the function to each questionnaire in vector called "months"
weight<-lapply(months_att, f_weight)

# create the first table to merge in order to union the rest of the questionnaires
merge_att<-as.data.table(weight[1])

# create the first table to output in order to union the rest of the questionnaires
output_att<-as.data.table(weight[1])

# write a for loop function to transfer all the list into data table, and union them 
for(i in 1:length(weight)){
  merge_2<-as.data.table(weight[i])             
  if(i<1){                                       
    output<-union(merge_att, merge_2)        
  }else{
    output<-union(output_att, merge_2)}                   
  i<-i+1                            
}  

# STEP 3 : repeat the same process to other categories 

# ------------------------------------------------------------------------------
# TASK 4 : comparison with Malo algorithms and French government criteria 

# STEP 1 : filter the question which are same to government criteria and whose analysis_grade is 2 or 3
# take one layout for example 
month6_gov_1<-data%>%
  filter(layout_name=='6_months'&(
    question_id=='6_months_ms_07'|
      question_id=='6_months_ms_05'|
      question_id=='6_months_ms_14'|
      question_id=='6_months_ms_12'|
      question_id=='6_months_lan_44'|
      question_id=='6_months_lan_43'|
      question_id=='6_months_soc_16'|
      question_id=='6_months_lan_45')&            
      (n_analysis_id==2|n_analysis_id==3)) 


# STEP 2 : filter and count the wrong answers 
month6_gov_2<-month6_gov_1%>%
  filter(satisfied==FALSE)%>%
  group_by(id, n_section_id)%>%                     
  summarise(total=n())

# STEP 3 : inverse the data frame and set missing values as 0
month6_gov_3 <- dcast(month6_gov_2, id~n_section_id)   
month6_gov_3[is.na(month6_gov_3)] <- 0


# STEP 4 : calculate the sum(total) and delete the “total” column
month6_gov_4 <-month6_gov_3%>%
  group_by(id)%>%                                           
  summarise(total=sum(language+motor_skill))%>%             
  filter(total>1)%>%                                   
  mutate(Gov=1)                                            
month6_gov_4<-month6_gov_4%>%
  select(-total)

# STEP 5 :  merge the dataframe into the original one and set NA value in GOV column as 0
month6_gov_5<-merge(month6_gov_1, month6_gov_4, by='id',all=T)
month6_gov_5$Gov[is.na(month6_gov_5$Gov)]<-0  

# STEP 6 : data visualization 
# merge all the results
merge_gov<-rbind(month6_gov_5, month12_gov_5)
merge_gov<-rbind(merge_gov, month18_gov_5)
merge_gov<-rbind(merge_gov, month24_gov_5)
merge_gov<-rbind(merge_gov, month36_gov_5)

# create a new data frame to do data voz
merge_gov_df<-merge_gov%>%
  group_by(analysis_id, Gov)%>%
  summarise(total=n())%>%               
  mutate(gov=case_when(Gov=='1'~'TRUE',    
                       Gov=='0'~'FALSE')) 

# final graph 
ggplot(merge_gov_df, aes(x=analysis_id, y=gov))+
  geom_point(aes(size=total))+        
  theme_minimal()+
  labs(title='Grade Analysis According to Government Critiria', x='Analysis Grade', y='')
