
message=FALSE
echo=FALSE
#codeblock to bring in all your packages at the top


list.of.packages <- c("eeptools", "effsize","wooldridge","rstudioapi","haven","dtplyr","plyr","reshape2","ggplot2","tidyverse","tidyquery","queryparser","likert","kableExtra","tinytex","dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
tinytex::install_tinytex()
#load what you want
library(kableExtra)
library(eeptools)
library(effsize)
library(wooldridge)
library(rstudioapi)
library(haven)
library(dtplyr)
library(reshape2)
library(ggplot2)
library(tidyverse)
library(tidyselect)
library(tidyquery)
library(queryparser)
library(likert)
library(tinytex)
library(dplyr)
library(lemon)


get_raw_data_dimension <- function()
{
  anes_timeseries_2020_dta <- file.path(getwd(),"anes_datasets","anes_timeseries_2020_stata_20210719.dta")
  #print(anes_timeseries_2020_dta)
  anes_timeseries <- read_dta(anes_timeseries_2020_dta)
  return (list(nrow(anes_timeseries), ncol(anes_timeseries)))
}  
get_cleaned_up_aenes_file_basic <- function()
{

anes_timeseries_2020_dta <- file.path(getwd(),"anes_datasets","anes_timeseries_2020_stata_20210719.dta")
#print(anes_timeseries_2020_dta)
anes_timeseries <- read_dta(anes_timeseries_2020_dta)
#head(anes_timeseries$V201231x)
#head(anes_timeseries$V201231x)
combined_pre_and_post = data.frame(query(
  "
  select 
  V202119 as post_voting_difficulty,
  V202073 as post_who_vote_for_president,
  V200004 as when_survey_was_taken,
  V201507x as age_of_respondent,
  V201549x as ethnicity,
  V201013a as state_of_registration,
  case when V201231x in (1,2,3)
    then 'Democrats'
    when V201231x in (5,6,7)
    then 'Republicans'
  end as party_text,
  case when V202119 in (1) 
      then 'no difficulty'
    else 'some difficulty'
  end 
  as bin_difficulty,
  case when V202119 in (1) 
      then 0
    else 1
  end 
  as bin_diff  
  from
  anes_timeseries
  where 
    V201231x in (1,2,3,5,6,7) -- valid party leanings
  and 
    V202119 in (1,2,3,4,5) -- valid dificulties.
  and 
    V201549x in (1,2,3,4,5,6) -- specified ethnicities
  and
    V200004 = 3 -- taken before and after 
  "
))


combined_pre_and_post$voting_difficulty <- as.factor(combined_pre_and_post$post_voting_difficulty)

factor_levels <- c("Not difficult at all", 
                   "A little difficult", 
                   "Moderately difficult", 
                   "Very difficult", 
                   "Extremely Difficult")

levels(combined_pre_and_post$voting_difficulty)<-factor_levels


a_combined_pre_and_post <- data.frame(combined_pre_and_post)

return(a_combined_pre_and_post)

}

get_non_white_voters <- function()
{
  cleaned_up_data <- get_cleaned_up_aenes_file_basic()
  non_white_voters <- data.frame(query("select * from cleaned_up_data where ethnicity <>1")) 
  return(non_white_voters)
}

get_young_voters_18_to_32 <- function()
{
  cleaned_up_data <- get_cleaned_up_aenes_file_basic()
  young_voters = data.frame(query("select * from cleaned_up_data where age_of_respondent >= 18 and age_of_respondent <= 32"))
  return(young_voters)
}

get_swing_state_voters <- function()
{
  #Look at swing State voters
  #Michigan, Pennsylvania and Wisconsin.
  #Michigan = 26
  #Pennsylvanian = 42
  #Wisconsin = 55
  #Ohio 39
  #Florida 12
  cleaned_up_data <- get_cleaned_up_aenes_file_basic()
  swing_state_voters = data.frame(query("select * from cleaned_up_data where state_of_registration in (26,42,55,39,12)")) 
  return(swing_state_voters)
  
}


likertize_and_plot <- function(a_combined_pre_and_post,graph_title = "Difficulty in Voting",
                               x_label = "Percentage",
                               y_label = "Level of Difficulty",
                               legend_title = "Party")
{


likert_dem = data.frame(a_combined_pre_and_post[a_combined_pre_and_post$party_text=="Democrats",c("voting_difficulty")])

likert_gop = data.frame(a_combined_pre_and_post[a_combined_pre_and_post$party_text=="Republicans",c("voting_difficulty")])

len_1 = nrow(likert_dem)
len_2 = nrow(likert_gop)
sample_size = 0
if(len_1<len_2)
  sample_size = len_1
if(len_2<len_1)
  sample_size = len_2


likert_dem = data.frame(likert_dem[sample(nrow(likert_dem),sample_size),])
likert_gop = data.frame(likert_gop[sample(nrow(likert_gop),sample_size),])


names(likert_gop)<-c("voting_difficulty")
names(likert_dem)<-c("voting_difficulty")
likert_combined_1 = data.frame(rbind(likert_dem,likert_gop))
names(likert_dem) <-c('Democrats')
likert_dem$Republicans <- likert_gop$voting_difficulty
likert_combined <- data.frame(likert_dem)
dplyr::distinct(likert_combined,Democrats)
dplyr::distinct(likert_combined,Republicans)
library(likert)
p <- likert(likert_combined)
t <- data.frame(t(p$results))[-1,]
final_result <- data.frame(t)
names(final_result)<-c("Democrats","Republicans")
final_result$Democrats = as.numeric(as.character(final_result$Democrats))
final_result$Republicans =  as.numeric(as.character(final_result$Republicans))
final_result <- final_result %>% 
  tibble::rownames_to_column("voting_difficulty")
dat_l <- melt(final_result, id.vars = c("voting_difficulty"))

options(repr.plot.width = 10, repr.plot.height = 4)
p <-ggplot(dat_l, aes(x = value, y = voting_difficulty, fill = variable)) 
p <- p+  geom_col(position = position_dodge()) + coord_flip()
p <- p + theme_bw()
p <- p + theme(axis.text.x = element_text(angle = 20, vjust=.5,hjust=.7))
p <- p + scale_y_discrete(limits = c("Not difficult at all", 
                                     "A little difficult", 
                                     "Moderately difficult", 
                                     "Very difficult", 
                                     "Extremely Difficult"))+
  ggtitle(graph_title)+
  xlab(x_label) +
  ylab(y_label)
p <- p + guides(fill=guide_legend(title=legend_title))
p <- p +  scale_fill_manual(values = c("blue", "red"))
p <- p + theme(legend.position = c(0.8, 0.7))

return(p)

}


do_wilcox <- function(a_combined_pre_and_post)
{
  
  #a_combined_pre_and_post <- get_cleaned_up_aenes_file_basic()
  wilcox_dem = data.frame(a_combined_pre_and_post[a_combined_pre_and_post$party_text=="Democrats",c("post_voting_difficulty","party_text")])
  wilcox_gop = data.frame(a_combined_pre_and_post[a_combined_pre_and_post$party_text=="Republicans",c("post_voting_difficulty","party_text")])
  
  len_1 = nrow(wilcox_dem)
  len_2 = nrow(wilcox_gop)
  
  if(len_1<len_2)
    sample_size = len_1
  if(len_2<len_1)
    sample_size = len_2
  
  wilcox_dem = data.frame(wilcox_dem[sample(nrow(wilcox_dem),sample_size),])
  wilcox_gop = data.frame(wilcox_gop[sample(nrow(wilcox_gop),sample_size),])

  wilcox_combined = data.frame(rbind(wilcox_dem,wilcox_gop))
  return = wilcox.test(post_voting_difficulty ~ party_text, data=wilcox_combined)
  
  Test_Attribute <- c("Method","Alternative","Statistic","P-Value")
  Test_Attribute
  Test_Value <- c(return$method,
                  return$alternative,
                  return$statistic,
                  return$p.value
  )
  rdf <- data.frame(Test_Attribute,Test_Value)
  return(rdf)
  
}

do_two_sample_t_test <- function(a_combined_pre_and_post)
{
  
  #a_combined_pre_and_post <- get_cleaned_up_aenes_file_basic()
  sample_dem = data.frame(a_combined_pre_and_post[a_combined_pre_and_post$party_text=="Democrats",c("bin_diff")])
  sample_gop = data.frame(a_combined_pre_and_post[a_combined_pre_and_post$party_text=="Republicans",c("bin_diff")])
  
  len_1 = nrow(sample_dem)
  len_2 = nrow(sample_gop)
  
  if(len_1<len_2)
    sample_size = len_1
  if(len_2<len_1)
    sample_size = len_2
  
  sample_dem = data.frame(sample_dem[sample(nrow(sample_dem),sample_size),])
  sample_gop = data.frame(sample_gop[sample(nrow(sample_gop),sample_size),])
  
  
  
  names(sample_dem) = c('bin_diff')
  names(sample_gop) = c('bin_diff')
  paired_tcombined = data.frame(rbind(sample_dem,sample_gop))
  return <- t.test(sample_dem$bin_diff,sample_gop$bin_diff, paired=FALSE)
  Test_Attribute <- c("Method","Alternative","Statistic","P-Value","CI From", "CI To","Mean of X","Mean of Y")
  Test_Attribute
  Test_Value <- c(return$method,
                  return$alternative,
                  return$statistic,
                  return$p.value,
                  return$conf.int,
                  return$estimate
                  )
  rdf <- data.frame(Test_Attribute,Test_Value)
  Test_Value
  return(rdf)

  
}

do_two_sample_t_test_non_white_voters <- function(a_combined_pre_and_post)
{
  set.seed(898)
  #a_combined_pre_and_post <- get_cleaned_up_aenes_file_basic()
  
  paired_t_white = data.frame(query("select bin_diff from a_combined_pre_and_post where ethnicity =1 "))
  paired_t_non_white = data.frame(query("select bin_diff from a_combined_pre_and_post where ethnicity <>1 "))
  
  len_1 = nrow(paired_t_white)
  len_2 = nrow(paired_t_non_white)
  
  if(len_1<len_2)
    sample_size = len_1
  if(len_2<len_1)
    sample_size = len_2
  
  paired_t_white = data.frame(paired_t_white[sample(nrow(paired_t_white),sample_size),])
  paired_t_non_white = data.frame(paired_t_non_white[sample(nrow(paired_t_non_white),sample_size),])
  
  
  
  names(paired_t_white) = c('bin_diff')
  names(paired_t_non_white) = c('bin_diff')
  paired_tcombined = data.frame(rbind(paired_t_white,paired_t_non_white))
  return <- t.test(paired_t_white$bin_diff,paired_t_non_white$bin_diff, paired=FALSE)
  Test_Attribute <- c("Method","Alternative","Statistic","P-Value","CI From", "CI To","Mean of X","Mean of Y")
  Test_Attribute
  Test_Value <- c(return$method,
                  return$alternative,
                  return$statistic,
                  return$p.value,
                  return$conf.int,
                  return$estimate
  )
  rdf <- data.frame(Test_Attribute,Test_Value)
  Test_Value
  return(rdf)
  
  
}

do_wilcox_white_non_white <- function(a_combined_pre_and_post)
{
  set.seed(789)
  #a_combined_pre_and_post <- get_cleaned_up_aenes_file_basic()
  wilcox_white = data.frame(query("select 
                                  'white' as ethnicity
                                  ,post_voting_difficulty from a_combined_pre_and_post where ethnicity =1 "))
  wilcox_non_white = data.frame(query("select 
                                  'non-white'
                                  as ethnicity,
                                      post_voting_difficulty from a_combined_pre_and_post where ethnicity <>1 "))
  
  len_1 = nrow(wilcox_white)
  len_2 = nrow(wilcox_non_white)
  
  if(len_1<len_2)
    sample_size = len_1
  if(len_2<len_1)
    sample_size = len_2
  
  wilcox_white = data.frame(wilcox_white[sample(nrow(wilcox_white),sample_size),])
  wilcox_non_white = data.frame(wilcox_non_white[sample(nrow(wilcox_non_white),sample_size),])
  
  wilcox_combined = data.frame(rbind(wilcox_white,wilcox_non_white))
  return = wilcox.test(post_voting_difficulty ~ ethnicity, data=wilcox_combined)
  
  Test_Attribute <- c("Method","Alternative","Statistic","P-Value")
  Test_Attribute
  Test_Value <- c(return$method,
                  return$alternative,
                  return$statistic,
                  return$p.value
  )
  rdf <- data.frame(Test_Attribute,Test_Value)
  return (rdf)
  
}

ethnicity_sum <- function(a_combined_pre_and_post)
{
  set.seed(789)
  #a_combined_pre_and_post <- get_cleaned_up_aenes_file_basic()
  wilcox_white = data.frame(query("select 
                                  'white' as ethnicity
                                  ,post_voting_difficulty from a_combined_pre_and_post where ethnicity =1 "))
  wilcox_non_white = data.frame(query("select 
                                  'non-white'
                                  as ethnicity,
                                      post_voting_difficulty from a_combined_pre_and_post where ethnicity <>1 "))
  
  len_1 = nrow(wilcox_white)
  len_2 = nrow(wilcox_non_white)
return (list(len1, len2))
}


likertize <- function(a_combined_pre_and_post)
{
  
  likert_dem = data.frame(a_combined_pre_and_post[a_combined_pre_and_post$party_text=="Democrats",c("voting_difficulty")])
  likert_gop = data.frame(a_combined_pre_and_post[a_combined_pre_and_post$party_text=="Republicans",c("voting_difficulty")])
  
  len_1 = nrow(likert_dem)
  len_2 = nrow(likert_gop)
  
  if(len_1<len_2)
    sample_size = len_1
  if(len_2<len_1)
    sample_size = len_2
  
  likert_dem = data.frame(likert_dem[sample(nrow(likert_dem),sample_size),])
  likert_gop = data.frame(likert_gop[sample(nrow(likert_gop),sample_size),])
  names(likert_gop)<-c("voting_difficulty")
  names(likert_dem)<-c("voting_difficulty")
  likert_combined_1 = data.frame(rbind(likert_dem,likert_gop))
  names(likert_dem) <-c('Democrats')
  likert_dem$Republicans <- likert_gop$voting_difficulty
  likert_combined <- data.frame(likert_dem)
  dplyr::distinct(likert_combined,Democrats)
  dplyr::distinct(likert_combined,Republicans)
  library(likert)
  p <- likert(likert_combined)
  t <- data.frame(t(p$results))[-1,]
  final_result <- data.frame(t)
  names(final_result)<-c("Democrats","Republicans")
  final_result <- final_result %>% 
    tibble::rownames_to_column("Voting Difficulty")  
  return(final_result)

}

get_summary_data_set <- function()
{
  cleaned_up_data <- get_cleaned_up_aenes_file_basic()
  
  summary_cleaned_up <- likertize(cleaned_up_data)
  summary_cleaned_up_plot_object <- likertize_and_plot(cleaned_up_data,graph_title = "Dem and GOP Difficulty in Voting")
  wilcox_results = do_wilcox(cleaned_up_data)
  two_sample_t_test <- do_two_sample_t_test(cleaned_up_data)
  return_object <- vector()
  return_object$cleaned_up_data <- cleaned_up_data
  return_object$summary <- summary_cleaned_up
  return_object$plot_object <- summary_cleaned_up_plot_object
  return_object$wilcox_results <- wilcox_results
  return_object$two_sample_t_test <- two_sample_t_test
  return_object$gop_voter_count <- as.numeric(data.frame((query("select count(*) from cleaned_up_data where party_text = 'Republicans' ")))[1,])
  return_object$dem_voter_count <- as.numeric(data.frame((query("select count(*) from cleaned_up_data where party_text = 'Democrats' ")))[1,])
  
  
    return(return_object)
}

get_non_white_data_set <- function()
{
  cleaned_up_data <- get_non_white_voters()
  summary_cleaned_up <- likertize(cleaned_up_data)
  summary_cleaned_up_plot_object <- likertize_and_plot(cleaned_up_data,graph_title = "GOP & Dem Non White Voting Difficulty")
  wilcox_results = do_wilcox(cleaned_up_data)
  two_sample_t_test <- do_two_sample_t_test(cleaned_up_data)
  return_object <- vector()
  return_object$cleaned_up_data <- cleaned_up_data
  return_object$summary <- summary_cleaned_up
  return_object$plot_object <- summary_cleaned_up_plot_object
  return_object$wilcox_results <- wilcox_results
  return_object$two_sample_t_test_result <- two_sample_t_test
  return_object$gop_voter_count <- as.numeric(data.frame((query("select count(*) from cleaned_up_data where party_text = 'Republicans' ")))[1,])
  return_object$dem_voter_count <- as.numeric(data.frame((query("select count(*) from cleaned_up_data where party_text = 'Democrats' ")))[1,])
  
  return(return_object)

}

get_young_voters_18_to_32_data_set <- function()
{
  cleaned_up_data <- get_young_voters_18_to_32()
  summary_cleaned_up <- likertize(cleaned_up_data)
  summary_cleaned_up_plot_object <- likertize_and_plot(cleaned_up_data,graph_title = "GOP and Dem Voters 18 to 32")
  wilcox_results = do_wilcox(cleaned_up_data)
  two_sample_t_test <- do_two_sample_t_test(cleaned_up_data)
  return_object <- vector()
  return_object$cleaned_up_data <- cleaned_up_data
  return_object$summary <- summary_cleaned_up
  return_object$plot_object <- summary_cleaned_up_plot_object
  return_object$wilcox_results <- wilcox_results
  return_object$two_sample_t_test_result <- two_sample_t_test
  return_object$gop_voter_count <- as.numeric(data.frame((query("select count(*) from cleaned_up_data where party_text = 'Republicans' ")))[1,])
  return_object$dem_voter_count <- as.numeric(data.frame((query("select count(*) from cleaned_up_data where party_text = 'Democrats' ")))[1,])
  
  return(return_object)
}

get_swing_state_voters_data_set <- function()
{
  cleaned_up_data <- get_swing_state_voters()
  summary_cleaned_up <- likertize(cleaned_up_data)
  summary_cleaned_up_plot_object <- likertize_and_plot(cleaned_up_data,graph_title = "GOP and Dem Voters 18 to 32")
  wilcox_results = do_wilcox(cleaned_up_data)
  two_sample_t_test <- do_two_sample_t_test(cleaned_up_data)
  return_object <- vector()
  return_object$cleaned_up_data <- cleaned_up_data
  return_object$summary <- summary_cleaned_up
  return_object$plot_object <- summary_cleaned_up_plot_object
  return_object$wilcox_results <- wilcox_results
  return_object$two_sample_t_test_result <- two_sample_t_test
  return_object$gop_voter_count <- as.numeric(data.frame((query("select count(*) from cleaned_up_data where party_text = 'Republicans' ")))[1,])
  return_object$dem_voter_count <- as.numeric(data.frame((query("select count(*) from cleaned_up_data where party_text = 'Democrats' ")))[1,])
  
  return(return_object)
}
