# 동별 인구자료 정리
## 인구자료를 chs의 인구구간으로 형태를 변형 (1999-2017)  

fix_age_class_tochs <- function(data_set, class_var='age', value_var=c('pop_all','pop_male','pop_female'), 
                                josa_year, fix_ver = 'death') {
  
  age_range_1999 <- c("0 - 4세", "5 - 9세","10 - 14세", "15 - 19세", "20 - 24세", "25 - 29세", "30 - 34세", "35 - 39세",
                      "40 - 44세", "45 - 49세", "50 - 54세", "55 - 59세", "60 - 64세", "65 - 69세", "70 - 74세",
                      "75 - 79세", "80 - 84세", "85 - 89세", "90 - 94세","95+" )
  
  age_range_2011 <- c(age_range_1999, "95 - 99세", "100+")
  
  chs_class_range <-  c("1.19-29","2.19-29","1.30-39","2.30-39","1.40-49","2.40-49",
                        "1.50-59", "2.50-59", "1.60-69", "2.60-69", "1.70이상", "2.70이상")
  
  if(josa_year %in% 2011:2019) {
    
    pop <- data_set[data_set[[class_var]] %in% age_range_2011,
                    c(class_var,value_var)]
    
    male.20_29 <- pop[pop[[class_var]]=='20 - 24세',value_var[2]] + pop[pop[[class_var]]=='25 - 29세',value_var[2]] 
    female.20_29 <- pop[pop[[class_var]]=='20 - 24세',value_var[3]] + pop[pop[[class_var]]=='25 - 29세',value_var[3]] 
    male.30_39 <- pop[pop[[class_var]]=='30 - 34세',value_var[2]] + pop[pop[[class_var]]=='35 - 39세',value_var[2]] 
    female.30_39 <- pop[pop[[class_var]]=='30 - 34세',value_var[3]] + pop[pop[[class_var]]=='35 - 39세',value_var[3]] 
    male.40_49 <- pop[pop[[class_var]]=='40 - 44세',value_var[2]] + pop[pop[[class_var]]=='45 - 49세',value_var[2]] 
    female.40_49 <- pop[pop[[class_var]]=='40 - 44세',value_var[3]] + pop[pop[[class_var]]=='45 - 49세',value_var[3]] 
    male.50_59 <- pop[pop[[class_var]]=='50 - 54세',value_var[2]] + pop[pop[[class_var]]=='55 - 59세',value_var[2]] 
    female.50_59 <- pop[pop[[class_var]]=='50 - 54세',value_var[3]] + pop[pop[[class_var]]=='55 - 59세',value_var[3]] 
    male.60_69 <- pop[pop[[class_var]]=='60 - 64세',value_var[2]] + pop[pop[[class_var]]=='65 - 69세',value_var[2]] 
    female.60_69 <- pop[pop[[class_var]]=='60 - 64세',value_var[3]] + pop[pop[[class_var]]=='65 - 69세',value_var[3]] 
    
    male.70 <- pop[pop[[class_var]]=='70 - 74세',value_var[2]] + pop[pop[[class_var]]=='75 - 79세',value_var[2]] +
      pop[pop[[class_var]]=='80 - 84세',value_var[2]] + pop[pop[[class_var]]=='85 - 89세',value_var[2]] +
      pop[pop[[class_var]]=='90 - 94세',value_var[2]] + pop[pop[[class_var]]=='95 - 99세',value_var[2]] + 
      pop[pop[[class_var]]=='100+',value_var[2]]
    
    female.70 <- pop[pop[[class_var]]=='70 - 74세',value_var[3]] + pop[pop[[class_var]]=='75 - 79세',value_var[3]] +
      pop[pop[[class_var]]=='80 - 84세',value_var[3]] + pop[pop[[class_var]]=='85 - 89세',value_var[3]] +
      pop[pop[[class_var]]=='90 - 94세',value_var[3]] + pop[pop[[class_var]]=='95 - 99세',value_var[3]] + 
      pop[pop[[class_var]]=='100+',value_var[3]]
    
    result_pop <- c(male.20_29, female.20_29, male.30_39, female.30_39, male.40_49, female.40_49, 
                    male.50_59, female.50_59, male.60_69, female.60_69, male.70, female.70)
    
  } else if (josa_year %in% 1999:2010) {
    pop <- data_set[data_set[[class_var]] %in% age_range_1999,
                    c(class_var,value_var)]
    
    male.20_29 <- pop[pop[[class_var]]=='20 - 24세',value_var[2]] + pop[pop[[class_var]]=='25 - 29세',value_var[2]] 
    female.20_29 <- pop[pop[[class_var]]=='20 - 24세',value_var[3]] + pop[pop[[class_var]]=='25 - 29세',value_var[3]] 
    male.30_39 <- pop[pop[[class_var]]=='30 - 34세',value_var[2]] + pop[pop[[class_var]]=='35 - 39세',value_var[2]] 
    female.30_39 <- pop[pop[[class_var]]=='30 - 34세',value_var[3]] + pop[pop[[class_var]]=='35 - 39세',value_var[3]] 
    male.40_49 <- pop[pop[[class_var]]=='40 - 44세',value_var[2]] + pop[pop[[class_var]]=='45 - 49세',value_var[2]] 
    female.40_49 <- pop[pop[[class_var]]=='40 - 44세',value_var[3]] + pop[pop[[class_var]]=='45 - 49세',value_var[3]] 
    male.50_59 <- pop[pop[[class_var]]=='50 - 54세',value_var[2]] + pop[pop[[class_var]]=='55 - 59세',value_var[2]] 
    female.50_59 <- pop[pop[[class_var]]=='50 - 54세',value_var[3]] + pop[pop[[class_var]]=='55 - 59세',value_var[3]] 
    male.60_69 <- pop[pop[[class_var]]=='60 - 64세',value_var[2]] + pop[pop[[class_var]]=='65 - 69세',value_var[2]] 
    female.60_69 <- pop[pop[[class_var]]=='60 - 64세',value_var[3]] + pop[pop[[class_var]]=='65 - 69세',value_var[3]] 
    
    male.70 <- pop[pop[[class_var]]=='70 - 74세',value_var[2]] + pop[pop[[class_var]]=='75 - 79세',value_var[2]] +
      pop[pop[[class_var]]=='80 - 84세',value_var[2]] + pop[pop[[class_var]]=='85 - 89세',value_var[2]] +
      pop[pop[[class_var]]=='90 - 94세',value_var[2]] + pop[pop[[class_var]]=='95+',3]
    
    female.70 <- pop[pop[[class_var]]=='70 - 74세',value_var[3]] + pop[pop[[class_var]]=='75 - 79세',value_var[3]] +
      pop[pop[[class_var]]=='80 - 84세',value_var[3]] + pop[pop[[class_var]]=='85 - 89세',value_var[3]] +
      pop[pop[[class_var]]=='90 - 94세',value_var[3]] + pop[pop[[class_var]]=='95+',value_var[3]]
    
    result_pop <- c(male.20_29, female.20_29, male.30_39, female.30_39, male.40_49, female.40_49, 
                    male.50_59, female.50_59, male.60_69, female.60_69, male.70, female.70)
    
  }
  
  names(result_pop) <- chs_class_range
  result_pop <- t(as_tibble(result_pop))
  return(result_pop)
  
}


# 연도별 인구자료 만들기(동별)

pop_list <- function(pop_data = pop_data, sigungu_code = sigungu_code, d_dong=d_dong, years=years) {
  
  if (d_dong=='동인동') {
    # 2010년은 동인1.2.4가동, 동인3가동 합산
    year0 <- data.frame(fix_age_class_tochs(pop_data[[1]][substr(pop_data[[1]][['d_num']],1,6)== sigungu_code &
                                                            pop_data[[1]][['d']]== '동인1_2_4가동', ], josa_year = 2010) +
                          fix_age_class_tochs(pop_data[[1]][substr(pop_data[[1]][['d_num']],1,6)== sigungu_code &
                                                              pop_data[[1]][['d']]== '동인3가동', ], josa_year = 2010))
    
  } else if (d_dong == '대현동') {
    year0 <- data.frame(fix_age_class_tochs(pop_data[[1]][substr(pop_data[[1]][['d_num']],1,6)== sigungu_code &
                                                            pop_data[[1]][['d']]== '대현1동', ], josa_year = 2010) +
                          fix_age_class_tochs(pop_data[[1]][substr(pop_data[[1]][['d_num']],1,6)== sigungu_code &
                                                              pop_data[[1]][['d']]== '대현2동', ], josa_year = 2010))
    
    
  } else if (d_dong == "성당동"){
    year0 <-  data.frame(fix_age_class_tochs(pop_data[[1]][substr(pop_data[[1]][['d_num']],1,6)== sigungu_code &
                                                             pop_data[[1]][['d']]== '성당1동', ], josa_year = 2010) +
                           fix_age_class_tochs(pop_data[[1]][substr(pop_data[[1]][['d_num']],1,6)== sigungu_code &
                                                               pop_data[[1]][['d']]== '성당2동', ], josa_year = 2010))
    
    
  } else if (d_dong == "두류1_2동"){
    year0 <-  data.frame(fix_age_class_tochs(pop_data[[1]][substr(pop_data[[1]][['d_num']],1,6)== sigungu_code &
                                                             pop_data[[1]][['d']]== '두류1동', ], josa_year = 2010) +
                           fix_age_class_tochs(pop_data[[1]][substr(pop_data[[1]][['d_num']],1,6)== sigungu_code &
                                                               pop_data[[1]][['d']]== '두류2동', ], josa_year = 2010))
    
    
  } else {
    year0 <- fix_age_class_tochs(pop_data[[1]][substr(pop_data[[1]][['d_num']],1,6)== sigungu_code &
                                                 pop_data[[1]][['d']]== d_dong, ],
                                 josa_year = (years[1]-1))
  }
  
  year1 <- fix_age_class_tochs(pop_data[[2]][substr(pop_data[[2]][['d_num']],1,6)== sigungu_code &
                                               pop_data[[2]][['d']]== d_dong, ],
                               josa_year = years[1])
  
  if (d_dong == '논공읍') {
    
    year2 <-  data.frame(fix_age_class_tochs(pop_data[[3]][substr(pop_data[[3]][['d_num']],1,6)== sigungu_code &
                                                             pop_data[[3]][['d']]== '논공읍', ], josa_year = 2012) +
                           fix_age_class_tochs(pop_data[[3]][substr(pop_data[[3]][['d_num']],1,6)== sigungu_code &
                                                               pop_data[[3]][['d']]== '논공읍공단출장소', ], josa_year = 2012))
    
    year3 = data.frame(
      fix_age_class_tochs(pop_data[[4]][substr(pop_data[[4]][['d_num']],1,6)== sigungu_code &
                                          pop_data[[4]][['d']]== '논공읍', ], josa_year = 2013) +
        fix_age_class_tochs(pop_data[[4]][substr(pop_data[[4]][['d_num']],1,6)== sigungu_code &
                                            pop_data[[4]][['d']]== '논공읍공단출장소', ], josa_year = 2013))
    
    year4 = data.frame(
      fix_age_class_tochs(pop_data[[5]][substr(pop_data[[5]][['d_num']],1,6)== sigungu_code &
                                          pop_data[[5]][['d']]== '논공읍', ], josa_year = 2014) +
        fix_age_class_tochs(pop_data[[5]][substr(pop_data[[5]][['d_num']],1,6)== sigungu_code &
                                            pop_data[[5]][['d']]== '논공읍공단출장소', ], josa_year = 2014))
    
    year5 = data.frame(
      fix_age_class_tochs(pop_data[[6]][substr(pop_data[[6]][['d_num']],1,6)== sigungu_code &
                                          pop_data[[6]][['d']]== '논공읍', ], josa_year = 2015) +
        fix_age_class_tochs(pop_data[[6]][substr(pop_data[[6]][['d_num']],1,6)== sigungu_code &
                                            pop_data[[6]][['d']]== '논공읍공단출장소', ], josa_year = 2015))
    
    year6 = data.frame(
      fix_age_class_tochs(pop_data[[7]][substr(pop_data[[7]][['d_num']],1,6)== sigungu_code &
                                          pop_data[[7]][['d']]== '논공읍', ], josa_year = 2016) +
        fix_age_class_tochs(pop_data[[7]][substr(pop_data[[7]][['d_num']],1,6)== sigungu_code &
                                            pop_data[[7]][['d']]== '논공읍공단출장소', ], josa_year = 2016))
    year7 = data.frame(
      fix_age_class_tochs(pop_data[[8]][substr(pop_data[[8]][['d_num']],1,6)== sigungu_code &
                                          pop_data[[8]][['d']]== '논공읍', ], josa_year = 2017) +
        fix_age_class_tochs(pop_data[[8]][substr(pop_data[[8]][['d_num']],1,6)== sigungu_code &
                                            pop_data[[8]][['d']]== '논공읍공단출장소', ], josa_year = 2017))
    
  } else if (d_dong=='다사읍') {
    year2 <-  data.frame(
      fix_age_class_tochs(pop_data[[3]][substr(pop_data[[3]][['d_num']],1,6)== sigungu_code &
                                          pop_data[[3]][['d']]== '다사읍', ], josa_year = 2012) +
        fix_age_class_tochs(pop_data[[3]][substr(pop_data[[3]][['d_num']],1,6)== sigungu_code &
                                            pop_data[[3]][['d']]== '다사읍서재출장소', ], josa_year = 2012))
    
    year3 = data.frame(
      fix_age_class_tochs(pop_data[[4]][substr(pop_data[[4]][['d_num']],1,6)== sigungu_code &
                                          pop_data[[4]][['d']]== '다사읍', ], josa_year = 2013) +
        fix_age_class_tochs(pop_data[[4]][substr(pop_data[[4]][['d_num']],1,6)== sigungu_code &
                                            pop_data[[4]][['d']]== '다사읍서재출장소', ], josa_year = 2013))
    
    year4 = data.frame(
      fix_age_class_tochs(pop_data[[5]][substr(pop_data[[5]][['d_num']],1,6)== sigungu_code &
                                          pop_data[[5]][['d']]== '다사읍', ], josa_year = 2014) +
        fix_age_class_tochs(pop_data[[5]][substr(pop_data[[5]][['d_num']],1,6)== sigungu_code &
                                            pop_data[[5]][['d']]== '다사읍서재출장소', ], josa_year = 2014))
    
    year5 = data.frame(
      fix_age_class_tochs(pop_data[[6]][substr(pop_data[[6]][['d_num']],1,6)== sigungu_code &
                                          pop_data[[6]][['d']]== '다사읍', ], josa_year = 2015) +
        fix_age_class_tochs(pop_data[[6]][substr(pop_data[[6]][['d_num']],1,6)== sigungu_code &
                                            pop_data[[6]][['d']]== '다사읍서재출장소', ], josa_year = 2015))
    
    year6 = data.frame(
      fix_age_class_tochs(pop_data[[7]][substr(pop_data[[7]][['d_num']],1,6)== sigungu_code &
                                          pop_data[[7]][['d']]== '다사읍', ], josa_year = 2016) +
        fix_age_class_tochs(pop_data[[7]][substr(pop_data[[7]][['d_num']],1,6)== sigungu_code &
                                            pop_data[[7]][['d']]== '다사읍서재출장소', ], josa_year = 2016))
    year7 = data.frame(
      fix_age_class_tochs(pop_data[[8]][substr(pop_data[[8]][['d_num']],1,6)== sigungu_code &
                                          pop_data[[8]][['d']]== '다사읍', ], josa_year = 2017) +
        fix_age_class_tochs(pop_data[[8]][substr(pop_data[[8]][['d_num']],1,6)== sigungu_code &
                                            pop_data[[8]][['d']]== '다사읍서재출장소', ], josa_year = 2017))
    
    
  } else {
    
    
    year2 <- fix_age_class_tochs(pop_data[[3]][substr(pop_data[[3]][['d_num']],1,6)== sigungu_code &
                                                 pop_data[[3]][['d']]== d_dong, ],
                                 josa_year = years[2])
    year3 = fix_age_class_tochs(pop_data[[4]][substr(pop_data[[4]][['d_num']],1,6)== sigungu_code &
                                                pop_data[[4]][['d']]== d_dong, ],
                                josa_year = years[3])
    year4 = fix_age_class_tochs(pop_data[[5]][substr(pop_data[[5]][['d_num']],1,6)== sigungu_code &
                                                pop_data[[5]][['d']]== d_dong, ],
                                josa_year = years[4])
    year5 = fix_age_class_tochs(pop_data[[6]][substr(pop_data[[6]][['d_num']],1,6)== sigungu_code &
                                                pop_data[[6]][['d']]== d_dong, ],
                                josa_year = years[5])
    year6 = fix_age_class_tochs(pop_data[[7]][substr(pop_data[[7]][['d_num']],1,6)== sigungu_code &
                                                pop_data[[7]][['d']]== d_dong, ],
                                josa_year = years[6])
    year7 = fix_age_class_tochs(pop_data[[8]][substr(pop_data[[8]][['d_num']],1,6)== sigungu_code &
                                                pop_data[[8]][['d']]== d_dong, ],
                                josa_year = years[7])
    
  }
  pop_years <- data.frame(year0,year1, year2, year3, year4, year5, year6, year7)
  
  colnames(pop_years) <- c("year0","year1","year2","year3","year4","year5","year6", "year7")
  
  return(pop_years)
}

## 6개년 인구 평균구하기

pop_sum <- function(pop_data = p, sigungu_code ="'27110", d_dong='동인동' , years=2011:2017, target_years = target_years) {
  
  pop_years <- pop_list(pop_data = pop_data, sigungu_code = sigungu_code, d_dong=d_dong, years=years)  
  
  period <- length(target_years)
  start_year_num <- target_years[1] - years[1] + 1
  
  if(period == 3) {
    pop_sum <- data.frame(pop = (pop_years[,start_year_num] +  pop_years[,start_year_num + 1]*2 + pop_years[,start_year_num + 2]*2 + pop_years[, start_year_num + 3])/6, 
                          stringsAsFactors = FALSE)
  } else if (period == 4) {
    pop_sum <- data.frame(pop = (pop_years[,start_year_num] +  pop_years[,start_year_num + 1]*2 + pop_years[,start_year_num + 2]*2 + pop_years[, start_year_num + 3]*2 + 
                                   pop_years[,start_year_num + 4])/8, 
                          stringsAsFactors = FALSE)
  } else if (period == 5) {
    pop_sum <- data.frame(pop = (pop_years[,start_year_num] +  pop_years[,start_year_num + 1]*2 + pop_years[,start_year_num + 2]*2 + pop_years[, start_year_num + 3]*2 + 
                                   pop_years[,start_year_num + 4]*2 + pop_years[,start_year_num + 5])/10, 
                          stringsAsFactors = FALSE)
  } else if (period == 6) {
    pop_sum <- data.frame(pop = (pop_years[,start_year_num] +  pop_years[,start_year_num + 1]*2 + pop_years[,start_year_num + 2]*2 + pop_years[, start_year_num + 3]*2 + 
                                   pop_years[,start_year_num + 4]*2 + pop_years[,start_year_num + 5]*2 + pop_years[,start_year_num + 6])/12, 
                          stringsAsFactors = FALSE)
  } else if (period == 7) {
    pop_sum <- data.frame(pop = (pop_years[,start_year_num] +  pop_years[,start_year_num + 1]*2 + pop_years[,start_year_num + 2]*2 + pop_years[, start_year_num + 3]*2 + 
                                   pop_years[,start_year_num + 4]*2 + pop_years[,start_year_num + 5]*2 + pop_years[,start_year_num + 6]*2 + pop_years[,start_year_num + 7])/14, 
                          stringsAsFactors = FALSE)
  }
  
  return(pop_sum)
  
}



#2 권역별 인구정리 

## daegu district coding
dg_district_code <- function(data, new_var="district", region_code_var="d_num", dong_name_var ="d", county_code="'27") {
  data[['district']] <- ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("동인동", '동인1_2_4가동','동인3가동', "삼덕동","성내1동","성내2동","성내3동","대신동", "남산1동","남산2동", "남산3동", "남산4동", "대봉1동", "대봉2동"), "중구",
                               ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("공산동","불로_봉무동","도평동"),"불로공산권",
                                      ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("안심1동", "안심2동", "안심3_4동"),"안심권",
                                             ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("지저동", "동촌동", "방촌동",  "해안동"), "동촌권",
                                                    ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("신암1동", "신암2동", "신암3동","신암4동", "신암5동", "신천1_2동", "신천3동", "신천4동",  "효목1동", "효목2동"), "동대구권",
                                                           ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("내당1동", "내당2_3동", "내당4동"), "내당권",
                                                                  ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("비산1동","비산2_3동","비산4동","비산5동","비산6동","비산7동", "원대동"), "비산권",
                                                                         ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("평리1동","평리2동",  "평리3동", "평리4동","평리5동", "평리6동", "상중이동"), "평리권",
                                                                                ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("이천동", "봉덕1동", "봉덕2동", "봉덕3동"),"이천봉덕권",
                                                                                       ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("대명1동","대명2동", "대명3동", "대명4동", "대명5동", "대명10동"),"대명권",
                                                                                              ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("대명6동", "대명9동", "대명11동"),"앞산권",
                                                                                                     ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("고성동", "칠성동", "침산1동","침산2동",  "침산3동","산격1동", "산격2동", "산격3동", "산격4동","대현1동","대현2동" ,"대현동", "복현1동", "복현2동", "검단동", "무태조야동", "노원동"),"강남지역",
                                                                                                            ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("관문동", "태전1동", "태전2동", "구암동", "관음동", "읍내동", "동천동", "국우동"), "강북지역",
                                                                                                                   ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("범어1동","범어2동","범어3동","범어4동","만촌1동","만촌2동", "만촌3동"), "범어만촌권",
                                                                                                                          ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("수성1가동", "수성2_3가동", "수성4가동", "황금1동", "황금2동", "중동", "상동", "파동", "두산동"), "중동권",
                                                                                                                                 ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("지산1동","지산2동","범물1동", "범물2동"), "지산범물권",
                                                                                                                                        ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("고산1동","고산2동","고산3동"),"고산권",
                                                                                                                                               ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("성당1동", "성당2동","성당동", "두류1_2동", "두류1동", "두류2동","두류3동", "감삼동", "죽전동", "용산1동", "본리동", "본동"),"두류권",
                                                                                                                                                      ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("용산2동","이곡1동","이곡2동","신당동","장기동"), "성서권",
                                                                                                                                                             ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("월성1동","월성2동","진천동", "상인1동","상인2동", "상인3동", "도원동", "송현1동", "송현2동"), "상인권",
                                                                                                                                                                    ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("다사읍","다사읍서재출장소", "하빈면"), "북부권",
                                                                                                                                                                           ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("화원읍", "옥포면", "옥포읍", "가창면"), "중부권",
                                                                                                                                                                                  ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("논공읍","논공읍공단출장소","현풍면", "현풍읍", "유가면", "구지면"),"남부권", NA)))))))))))))))))))))))
  
  return(data)  
}


# sum_by_district(추가)

sum_distirct_pop <- function(data, josa_year) {
  
  result <- data %>%
    group_by(district, age) %>%
    summarise(pop_all = sum(pop_all), pop_male = sum(pop_male), pop_female=sum(pop_female))
  
  return(result)
}

# 연령대를 지역사회건강조사 형태로_권역별
fix_age_class_tochs_district<- function(data, sido_code="'27", district_name="남부권", josa_year) {
  pop_district <- sum_distirct_pop(data[which(substr(data$d_num,1,3)== sido_code &
                                                data$district==district_name),],josa_year = josa_year)
  result <- fix_age_class_tochs(pop_district, josa_year = josa_year, 
                                class_var = 'age', 
                                value_var = c("pop_all","pop_male","pop_female"))
  return(result)
}


# 연도별 인구자료 구하기_권역별
pop_list_district <- function(pop_data, sido_code="'27",district_name="남부권", years=2010:2017) {
  
  year0 <- fix_age_class_tochs_district(pop_data[[1]], district_name = district_name, josa_year = years[1])
  year1 <- fix_age_class_tochs_district(pop_data[[2]], district_name = district_name, josa_year = years[2])
  year2 <- fix_age_class_tochs_district(pop_data[[3]], district_name = district_name, josa_year = years[3])
  year3 <- fix_age_class_tochs_district(pop_data[[4]], district_name = district_name, josa_year = years[4])
  year4 <- fix_age_class_tochs_district(pop_data[[5]], district_name = district_name, josa_year = years[5])
  year5 <- fix_age_class_tochs_district(pop_data[[6]], district_name = district_name, josa_year = years[6])
  year6 <- fix_age_class_tochs_district(pop_data[[7]], district_name = district_name, josa_year = years[7])
  year7 <- fix_age_class_tochs_district(pop_data[[8]], district_name = district_name, josa_year = years[8])
  
  
  pop_years <- data.frame(year0,year1, year2, year3, year4, year5, year6, year7)
  
  colnames(pop_years) <- c("year0","year1","year2","year3","year4","year5","year6", "year7")
  
  return(pop_years)
  
}

# 6개년의 평균인구수_district


pop_sum_district <- function(pop_data, sido_code ="'27", district_name='남부권' , years=2010:2017, target_years = target_years) {
  
  pop_years <- pop_list_district(pop_data = pop_data, sido_code = sido_code, district_name = district_name, years=years)  
  
  period <- length(target_years)
  start_year_num <- target_years[1] - years[1]
  
  if(period == 3) {
    pop_sum <- data.frame(pop = (pop_years[,start_year_num] +  pop_years[,start_year_num + 1]*2 + pop_years[,start_year_num + 2]*2 + pop_years[, start_year_num + 3])/6, 
                          stringsAsFactors = FALSE)
  } else if (period == 4) {
    pop_sum <- data.frame(pop = (pop_years[,start_year_num] +  pop_years[,start_year_num + 1]*2 + pop_years[,start_year_num + 2]*2 + pop_years[, start_year_num + 3]*2 + 
                                   pop_years[,start_year_num + 4])/8, 
                          stringsAsFactors = FALSE)
  } else if (period == 5) {
    pop_sum <- data.frame(pop = (pop_years[,start_year_num] +  pop_years[,start_year_num + 1]*2 + pop_years[,start_year_num + 2]*2 + pop_years[, start_year_num + 3]*2 + 
                                   pop_years[,start_year_num + 4]*2 + pop_years[,start_year_num + 5])/10, 
                          stringsAsFactors = FALSE)
  } else if (period == 6) {
    pop_sum <- data.frame(pop = (pop_years[,start_year_num] +  pop_years[,start_year_num + 1]*2 + pop_years[,start_year_num + 2]*2 + pop_years[, start_year_num + 3]*2 + 
                                   pop_years[,start_year_num + 4]*2 + pop_years[,start_year_num + 5]*2 + pop_years[,start_year_num + 6])/12, 
                          stringsAsFactors = FALSE)
  } else if (period == 7) {
    pop_sum <- data.frame(pop = (pop_years[,start_year_num] +  pop_years[,start_year_num + 1]*2 + pop_years[,start_year_num + 2]*2 + pop_years[, start_year_num + 3]*2 + 
                                   pop_years[,start_year_num + 4]*2 + pop_years[,start_year_num + 5]*2 + pop_years[,start_year_num + 6]*2 + pop_years[,start_year_num + 7])/14, 
                          stringsAsFactors = FALSE)
  }
  

  return(pop_sum)
}


