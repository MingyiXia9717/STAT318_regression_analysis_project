##########################################
######Data cleaning and manipulation######
##########################################
library(tidyverse)
##Part 1: Merge data files
##import all 12 data files
attendance_data<-read.csv("/Users/mayxia1020/Desktop/318finalproject/data/CSV/attendance.csv",header=TRUE)
gradcollege_data<-read.csv("/Users/mayxia1020/Desktop/318finalproject/data/CSV/Gradsattendingcollege.csv",header=TRUE)
artcourse_data<-read.csv("/Users/mayxia1020/Desktop/318finalproject/data/CSV/artcourse.csv",header=TRUE)
classsize_data<-read.csv("/Users/mayxia1020/Desktop/318finalproject/data/CSV/ClassSizebyGenPopulation.csv",header=TRUE)
enrollmentrace_data<-read.csv("/Users/mayxia1020/Desktop/318finalproject/data/CSV/enrollmentbyracegender.csv",header=TRUE)
gradeninecourse_data<-read.csv("/Users/mayxia1020/Desktop/318finalproject/data/CSV/gradeninecoursepasss.csv",header=TRUE)
perpupilexpenditure_data<-read.csv("/Users/mayxia1020/Desktop/318finalproject/data/CSV/PerPupilExpenditures.csv",header=TRUE)
selectedpop_data<-read.csv("/Users/mayxia1020/Desktop/318finalproject/data/CSV/selectedpopulations.csv",header=TRUE)
staffretention_data<-read.csv("/Users/mayxia1020/Desktop/318finalproject/data/CSV/staffingretention.csv",header=TRUE)
teacher_data<-read.csv("/Users/mayxia1020/Desktop/318finalproject/data/CSV/teacherdata.csv",header=TRUE)
masscore_data<-read.csv("/Users/mayxia1020/Desktop/318finalproject/data/CSV/masscore.csv",header=TRUE)
sat_data<-read.csv("/Users/mayxia1020/Desktop/318finalproject/data/CSV/sat_performance.csv",header=TRUE)

##make tibbles
attendance<-as_tibble(attendance_data)
gradcollege<-as_tibble(gradcollege_data)
art<-as_tibble(artcourse_data)
class_size<-as_tibble(classsize_data)
race<-as_tibble(enrollmentrace_data)
gradenine<-as_tibble(gradeninecourse_data)
perpupilexp<-as_tibble(perpupilexpenditure_data)
selectedpop<-as_tibble(selectedpop_data)
staffretention<-as_tibble(staffretention_data)
teacher<-as_tibble(teacher_data)
masscore<-as_tibble(masscore_data)
sat<-as_tibble(sat_data)

##data manipulation (merging files)
sub_attend<-attendance %>%
  select(c(District.Name,District.Code,Attendance.Rate))

sub_gradcollege<-gradcollege %>%
  rename(attend_college="Attending.Coll..Univ......1")%>%
  select(c(District.Code,attend_college))
#merge attendance and grads going to college
merge1<-merge(sub_attend,sub_gradcollege,by="District.Code")
#art course
sub_art<-art %>%
  select(District.Code,All.Grades,Total.Students)%>%
  mutate(All.Grades=as.character(All.Grades)) %>%
  mutate(All.Grades=gsub(",","",All.Grades))%>%
  mutate(All.Grades=as.numeric(All.Grades))%>%
  mutate(Total.Students=as.character(Total.Students)) %>%
  mutate(Total.Students=gsub(",","",Total.Students))%>%
  mutate(Total.Students=as.numeric(Total.Students))%>%
  mutate(artcourse=(All.Grades/Total.Students)*100) %>%
  select(District.Code,artcourse)
#merge2
merge2<-merge(merge1,sub_art,by="District.Code")
#class size
sub_classsize<-class_size %>%
  rename(avgclass_size="Average.Class.Size") %>%
  select(District.Code,avgclass_size)
#merge3
merge3<-merge(merge2,sub_classsize,by="District.Code")
#race
sub_race<-race %>%
  select(District.Code,African.American,Asian,Hispanic,White,Native.American,Native.Hawaiian..Pacific.Islander,
         Multi.Race..Non.Hispanic,Males,Females,Non.Binary)
#merge4
merge4<-merge(merge3,sub_race,by="District.Code")
#pass rate of ninth grade courses
sub_gradenine<-gradenine %>%
  rename(grade9_passrate="X..Passing.All.Courses.1") %>%
  select(District.Code,grade9_passrate)
#merge 5
merge5<-merge(merge4,sub_gradenine,by="District.Code")
#district per pupil expenditure
sub_exp<-perpupilexp%>%
  select(District.Code,In.District.Expenditures.per.Pupil)%>%
  mutate(In.District.Expenditures.per.Pupil=as.character(In.District.Expenditures.per.Pupil)) %>%
  mutate(In.District.Expenditures.per.Pupil=gsub(",","",In.District.Expenditures.per.Pupil))%>%
  mutate(In.District.Expenditures.per.Pupil=gsub("\\$","",In.District.Expenditures.per.Pupil))%>%
  mutate(In.District.Expenditures.per.Pupil=as.numeric(In.District.Expenditures.per.Pupil))%>%
  rename(expenditure="In.District.Expenditures.per.Pupil")
#merge6
merge6<-merge(merge5,sub_exp,by="District.Code")
#merge7: selected populations
sub_selectpop<-selectedpop %>%
  rename(notnativespeaker="First.Language.Not.English...1")%>%
  rename(englishlearner="English.Language.Learner...1")%>%
  rename(highneed="High.Needs...1")%>%
  rename(disability="Students.With.Disabilities...1")%>%
  rename(econdisadv="Economically.Disadvantaged...1")%>%
  select(District.Code,notnativespeaker,englishlearner,highneed,disability,econdisadv)
merge7<-merge(merge6,sub_selectpop,by="District.Code")
#staff retention rate
sub_staffretention<-staffretention%>%
  rename(teacher_retained="Teacher...Retained.1")%>%
  select(District.Code,teacher_retained)
#merge8
merge8<-merge(merge7,sub_staffretention,by="District.Code")
#teacher license
sub_teacher<-teacher %>%
  rename(teacher_license="X..of.Teachers.Licensed") %>%
  select(District.Code,teacher_license)
#merge9
merge9<-merge(merge8,sub_teacher,by="District.Code")
#mass core curriculum
sub_masscore<-masscore %>%
  rename(masscore_complete="X..Completed.MassCore.1") %>%
  select(District.Code,masscore_complete)
#merge10
merge10<-merge(merge9,sub_masscore,by="District.Code")
#sat score
sub_sat<-sat %>%
  rename(sat_verbal="Reading...Writing") %>%
  rename(sat_math="Math")%>%
  select(District.Code,sat_verbal,sat_math)
#merge11
merge11<-merge(merge10,sub_sat,by="District.Code")

## Part 2: Clean data (rename, check missing values, etc.)
#rename terms
masspublic_cleanv1<-merge11 %>%
  rename(district_code="District.Code")%>%
  rename(name="District.Name")%>%
  rename(attendance_rate="Attendance.Rate")%>%
  rename(pacific_islander="Native.Hawaiian..Pacific.Islander")%>%
  rename(multi_race="Multi.Race..Non.Hispanic")%>%
  rename(nonbinary="Non.Binary")

#delete first row (state total)
masspublic_cleanv1<-masspublic_cleanv1[-1,]
dim(masspublic_cleanv1)
write.csv(masspublic_cleanv1,"masspublic_cleanv1.csv")

#check missing values in the response variable
which(is.na(masspublic_cleanv1$attend_college))

#remove rows that contain NA in the response variable
masspublic_cleanv2<-masspublic_cleanv1[-c(191,293),]
dim(masspublic_cleanv2)
write.csv(masspublic_cleanv2,"masspublic_cleanv2.csv")
hist(na.omit(masspublic_cleanv2$sat_verbal))
hist(na.omit(masspublic_cleanv2$sat_math))
which(is.na(masspublic_cleanv2$sat_verbal))
which(is.na(masspublic_cleanv2$sat_math))

##remove rows that miss SAT verbal and math scores
masspublic_cleanv3<-masspublic_cleanv2[-c(203,205,206,229,267),]
write.csv(masspublic_cleanv3,"masspublic_cleanv3.csv")
  
## manually code 0 for ordinary public and charter schools
## then code 1 for agricultural/technical vocational school
## manually coding because the schools in the data are not arranged in order


