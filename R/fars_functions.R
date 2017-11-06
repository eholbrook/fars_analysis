# function to create a 95% confidence interval
perc_cis <- function(x, n) {
  
  prob <- (x / n)
  SE <- sqrt((prob*(1-prob)/n))
  CI_lower <- prob - (1.96 * (SE*prob))
  CI_upper <- prob + (1.96 * (SE*prob))
  
  CI_lower_perc <- round(CI_lower*100, digits = 1)
  CI_upper_perc <- round(CI_upper*100, digits = 1)
  prob_perc <- round(prob*100, digits = 1)
  print_out <- paste0(prob_perc, "%", " (", CI_lower_perc, "%",", ", CI_upper_perc, "%", ")")
  print_out
}


#function for testing for trend using Cochran-Armitage trend test
test_trend_ca <- function(drug, df = clean_fars){
  if(drug == "Nonalcohol") {
    
    nonalcohol <- df %>% 
      mutate(alchyesno = drug_type != "Alcohol") %>% 
      filter(alchyesno == "TRUE") %>%
      group_by(unique_id, year) %>% 
      summarize(positive_for_drug = any(positive_for_drug, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(drug_type = "Nonalcohol") %>%
      group_by(year) %>%
      summarize(positive = sum(positive_for_drug, na.rm = TRUE),
                trials = sum(!is.na(positive_for_drug)))
    ca_alcohol <- prop.trend.test(x = nonalcohol$positive,
                                  n = nonalcohol$trials)
    ca_alcohol <- data.frame(Z = sqrt(ca_alcohol$statistic), p.value = ca_alcohol$p.value)
    row.names(ca_alcohol) <- NULL
    as_tibble(ca_alcohol)  
  }
  else {
    to_test <- df %>%
      filter(drug_type == drug) %>%
      group_by(year) %>%
      summarize(positive = sum(positive_for_drug, na.rm = TRUE),
                trials = sum(!is.na(positive_for_drug)))
    ca_alcohol <- prop.trend.test(x = to_test$positive,
                                  n = to_test$trials)
    
    ca_alcohol <- data.frame(Z = sqrt(ca_alcohol$statistic), p.value = ca_alcohol$p.value)
    row.names(ca_alcohol) <- NULL
    as_tibble(ca_alcohol)
  }
}


#function for testing for trend using logistic regression
test_trend_log_reg <- function(drug, df = clean_fars){
  if(drug == "Nonalcohol") {
    
    nonalcohol <- df %>% 
      mutate(alchyesno = drug_type != "Alcohol") %>% 
      filter(alchyesno == "TRUE") %>%
      group_by(unique_id, year) %>% 
      summarize(positive_for_drug = any(positive_for_drug, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(drug_type = "Nonalcohol")
    log_reg <- glm(positive_for_drug ~ year, data = nonalcohol,
                   family = binomial(link = "logit"))
    x <- summary(log_reg)$coefficients
    x = as.data.frame(x)
    x %>% 
      slice(2) %>% 
      select(3:4)    
    
  }
  else {
    alcohol <- df %>%
      filter(drug_type == drug)
    log_reg <- glm(positive_for_drug ~ year, data = alcohol,
                   family = binomial(link = "logit"))
    x <- summary(log_reg)$coefficients 
    x = as.data.frame(x)
    x %>% 
      slice(2) %>% 
      select(3:4)
  }
}
