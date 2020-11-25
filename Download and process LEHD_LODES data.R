### Install LODES package
#install.packages("devtools")
#devtools::install_git("https://github.com/hrbrmstr/lodes.git")

### Install LEHD package
#install.packages("remotes")
#remotes::install_github("jamgreen/lehdR")

setwd("D:/Dropbox (UFL)/Micromobility Project - Louis, Jacob, Xilei, Yiming/GIS Data/Output")

#library(lehdr)
library(lodes)
library(dplyr)
options(scipen=999)
# Lodes type: od, wac(workplace),rac(residence), for od --- specify "main" (workplace/residence both within state) or "aux" (residence out of state)

# Job type --  JT00 (all jobs),JT01 (worker's primary jobs),JT02
# JT00 to JT03 are available to 2017, JT04-

# segment -- "S000" total number of jobs for workers, 
#"SA01" number of jobs forworker aged 29 or younger, 
#"SA02" number of jobs for workers aged 30-54,
#"SA03" number of jobs for workers 55 and older, 
#"SE01" number of jobs with earnings $1,250/month or less, 
#"SE02" number of jobs with earnings $1,251 to $3,333/month, 
#"SE03" number of jobs with earnings greater than $3,333/month, 
#"SI01" number of jobs in Goods Producing industry sectors, 
#"SI02" number of jobs in Trade, Transportation, and Utilities industry sectors, 
#"SI03" number of jobs in All Other Services industry sectors

# for wac/rac: characteristics
# C000=S000,CA01=SA01,CA02=SA02,CA03=SA03,CE01=SE01,CE02=SE02,CE03=SE03
#[SLD Classification: 
  #Retail: CNS07, 
  #Office: CNS09+CNS10+CNS11+CNS13+CNS20
  #Industrial:CNS01+CNS02+CNS03+CNS04+CNS05+CNS06+CNS08
  #Serice:CNS12+CNS14+CNS15+CNS16+CNS19
  #Entertainment:CNS17+CNS18]
#[#CR/CT... Race/Ethnicity
  #CD..Education
  #CS..Gender
#]

# year  -- 2015

##### For OD Matrics, all segments were included
dc.od.main <- grab_lodes(state = "dc", year = 2017, lodes_type = "od", job_type = "JT00", 
                         state_part = "main") 

dc.wac.main <- grab_lodes(state = "dc", year = 2017, lodes_type = "wac", job_type = "JT00", 
                          state_part = "main") 

dc.rac.main <- grab_lodes(state = "dc", year = 2017, lodes_type = "rac", job_type = "JT00", 
                          state_part = "main") 


w_block = dc.od.main$w_geocode
h_block = dc.od.main$h_geocode
od_alljob	= dc.od.main$S000
od_worker29	=dc.od.main$SA01
od_worker3054	=dc.od.main$SA02
od_worker55	=dc.od.main$SA03
od_joblowwage	=dc.od.main$SE01
od_jobmidwage	=dc.od.main$SE02
od_jobhighwage	=dc.od.main$SE03
od_jobgoodprod	=dc.od.main$SI01
od_jobtransprt	=dc.od.main$SI02
od_jobservice	=dc.od.main$SI03
od_pctworker29	=dc.od.main$SA01/dc.od.main$S000
od_pctworker3054	=dc.od.main$SA02/dc.od.main$S000
od_pctworker55	=dc.od.main$SA03/dc.od.main$S000
od_pctjoblowwage	=dc.od.main$SE01/dc.od.main$S000
od_pctjobmidwage	=dc.od.main$SE02/dc.od.main$S000
od_pctjobhighwage	=dc.od.main$SE03/dc.od.main$S000
od_pctjobgoodprod	=dc.od.main$SI01/dc.od.main$S000
od_pctjobtransprt	=dc.od.main$SI02/dc.od.main$S000
od_pctjobservice	=dc.od.main$SI03/dc.od.main$S000

od <- cbind(w_block,h_block,od_alljob,od_worker29,
            od_worker3054,od_worker55,od_joblowwage,od_jobmidwage,od_jobhighwage,od_jobgoodprod,
            od_jobtransprt,od_jobservice,od_pctworker29,od_pctworker3054,od_pctworker55,
            od_pctjoblowwage,od_pctjobmidwage,od_pctjobhighwage,od_pctjobgoodprod,od_pctjobtransprt,
            od_pctjobservice)

write.csv(od, "OD_Data.csv",row.names = FALSE)

# Workplace
w_block = dc.wac.main$w_geocode
wac_alljob	=dc.wac.main$CA00
wac_worker29	=dc.wac.main$CA01
wac_worker3054	=dc.wac.main$CA02
wac_worker55	=dc.wac.main$CA03
wac_joblowwage	=dc.wac.main$CE01
wac_jobmidwage	=dc.wac.main$CE02
wac_jobhighwage	=dc.wac.main$CE03
wac_retail	=dc.wac.main$CNS07
wac_office	=dc.wac.main$CNS09+dc.wac.main$CNS10+dc.wac.main$CNS11+dc.wac.main$CNS13+dc.wac.main$CNS20
wac_indus	=dc.wac.main$CNS01+dc.wac.main$CNS02+dc.wac.main$CNS03+dc.wac.main$CNS04+dc.wac.main$CNS05+dc.wac.main$CNS06+dc.wac.main$CNS08
wac_service	=dc.wac.main$CNS12+dc.wac.main$CNS14+dc.wac.main$CNS15+dc.wac.main$CNS16+dc.wac.main$CNS19
wac_entertain	=dc.wac.main$CNS17+dc.wac.main$CNS18
wac_workerwhite	=dc.wac.main$CR01
wac_workerblack	=dc.wac.main$CR02
wac_workerasian	=dc.wac.main$CR04
wac_workerhisp	=dc.wac.main$CT02
wac_workerhighschool	=dc.wac.main$CD01+dc.wac.main$CD02
wac_workersomecollege	=dc.wac.main$CD03
wac_workerbachelor	=dc.wac.main$CD04
wac_workerfemale	=dc.wac.main$CS02

wac <- cbind(w_block,wac_alljob,wac_worker29,wac_worker3054,wac_worker55,wac_joblowwage,
             wac_jobmidwage,wac_jobhighwage,wac_retail,wac_office,wac_indus,wac_service,wac_entertain,
             wac_workerwhite,wac_workerblack,wac_workerasian,wac_workerhisp,wac_workerhighschool,
             wac_workersomecollege,wac_workerbachelor,wac_workerfemale)

write.csv(wac, "Workplace_Data.csv",row.names = FALSE)

# Residence
h_block = dc.rac.main$h_geocode
rac_alljob	=dc.rac.main$CA00
rac_worker29	=dc.rac.main$CA01
rac_worker3054	=dc.rac.main$CA02
rac_worker55	=dc.rac.main$CA03
rac_joblowwage	=dc.rac.main$CE01
rac_jobmidwage	=dc.rac.main$CE02
rac_jobhighwage	=dc.rac.main$CE03
rac_retail	=dc.rac.main$CNS07
rac_office	=dc.rac.main$CNS09+dc.rac.main$CNS10+dc.rac.main$CNS11+dc.rac.main$CNS13+dc.rac.main$CNS20
rac_indus	=dc.rac.main$CNS01+dc.rac.main$CNS02+dc.rac.main$CNS03+dc.rac.main$CNS04+dc.rac.main$CNS05+dc.rac.main$CNS06+dc.rac.main$CNS08
rac_service	=dc.rac.main$CNS12+dc.rac.main$CNS14+dc.rac.main$CNS15+dc.rac.main$CNS16+dc.rac.main$CNS19
rac_entertain	=dc.rac.main$CNS17+dc.rac.main$CNS18
rac_workerwhite	=dc.rac.main$CR01
rac_workerblack	=dc.rac.main$CR02
rac_workerasian	=dc.rac.main$CR04
rac_workerhisp	=dc.rac.main$CT02
rac_workerhighschool	=dc.rac.main$CD01+dc.rac.main$CD02
rac_workersomecollege	=dc.rac.main$CD03
rac_workerbachelor	=dc.rac.main$CD04
rac_workerfemale	=dc.rac.main$CS02

rac <- cbind(h_block,rac_alljob,rac_worker29,
             rac_worker3054,rac_worker55,rac_joblowwage,rac_jobmidwage,rac_jobhighwage,rac_retail,
             rac_office,rac_indus,rac_service,rac_entertain,rac_workerwhite,rac_workerblack,rac_workerasian,
             rac_workerhisp,rac_workerhighschool,rac_workersomecollege,rac_workerbachelor,rac_workerfemale)

write.csv(rac, "Residence_Data.csv",row.names = FALSE)
 
