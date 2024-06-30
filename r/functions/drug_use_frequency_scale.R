##################################################################################################
# for the drug use frequency for CHAMP
# Gabriel Cook slicesofdata@github
##################################################################################################

# Drug Use Frequency Scale
# Source: Items 17 and 18 developed based on D. Paul Moberg, Center for Health Policy and Program Evaluation,
# University of Wisconsin Medical School.  Adapted with permission from Mayer, J., Filstead, W.J. (1979).
# The Adolescent Alcohol Involvement Scale. An instrument for measuring adolescents' use and misuse of alcohol.
# Journal of Studies on Alcohol, 40(3), 291-300.

# The definition of drinks was provided by Johnston, L.D., O’Malley, P.M., Bachman, J.G., & Schulenberg, J.E. (2014) 2011
# Monitoring the future survey (MTF). Retrieved from http://www.monitoringthefuture.org/. Ann Arbor: Institute for Social
# Research, The University of Michigan.

# Items 18-20 were developed for the first time for this study.

# All other items adapted from Graham, J.W., Flay B.R., Johnson, C.A., Hansen, W.B., Grossman, L., Sobel, J.L. (1984)
# Reliability  of self-report measures of drug use in prevention research: Evaluation of the Project SMART questionnaire
# via the test-retest reliability matrix. Journal of Drug Education, 14, 75–193.

# It should also be noted that this measure can be used to identify polysubstance users.
# Polysubstance use is typically defined as the consumption of multiple drugs in the past 30 days according to Moss,
# H.B., Chen, C.M., Yi, H.Y. (2014). Early adolescent patterns of alcohol, cigarettes, and marijuana polysubstance
# use and young adult substance use outcomes in a nationally representative sample. Drug and Alcohol Dependence, 136(1), 51-62.

# Designed For: Adolescents
# Psychometrics: Coefficient Alphas range from .60 to .86.  Across Time Coefficient Alphas range from .52 to .80

drug_use_freq <- function(data) {
  #drug_names_old    = names(select(data, contains({{var_pattern}})))[1:16]
  drug_names_old    = names(select(data, contains("bmq")))[1:16]
  #drug_names_old    = names(select(matches("^bmq\\d{1,2}$")))[1:16]
  #drug_names_old_30 = names(select(matches("^bnq\\d{1,2}$")))[1:11]
  drug_names_old_30 = names(select(data, contains("bnq")))[1:11]

  #  drug_names_old = drug_names_old[1:16]
  drug_names = c("cigarette", "e_cig", "cigars", "chewing_tobacco",
                 "alcohol", "marijuana", "cocaine", "caffeine",
                 "ecstacy", "hallucinogens", "methamphetamine", "tranquilizers",
                 "opiates", "inhalants", "other", "ritalin")

  drug_names_30 = paste0(
    c("cigarette", "e_cig", "cigars", "chewing_tobacco",
    "alcohol", "marijuana", #"cocaine", "caffeine", "ecstacy", "hallucinogens",
    "methamphetamine", "ritalin",
    "tobacco_alcohol", "tobacco_marijuana", "tobacco_meth"
    #"tranquilizers", "opiates", "inhalants", "other", "ritalin"
    ), "_30")

  data =
    data |>
    # move to end of data frame to be grouped
    relocate(all_of(drug_names_old), .after = last_col()) |>
    relocate(all_of(drug_names_old_30), .after = last_col()) |>

    # rename as drug base
    rename(!!!setNames(drug_names_old, drug_names)) |>
    rename(!!!setNames(drug_names_old_30, drug_names_30)) |>

    # in the past year
    # duplicate as frequency of use
    mutate(across(.cols = all_of(drug_names),
                  .fns = ~.x,
                  .names = "{.col}_freq"
    )
    ) |>
#    # make NA 0?
#    mutate(across(.cols = all_of(drug_names),
#                  .fns = ~ifelse(is.na(.x), 0, .x), # if missing, 0, else value
#                  .names = "{.col}_freq"
#    )
#    ) |>


    # create a binary variable yes/no
    mutate(across(.cols = all_of(drug_names),
                  .fns =  ~ifelse(.x > 0, 1, 0),
                  .names = "{.col}")
    ) |>


    # create a poly use variable across all others
    mutate(poly_drug_use = rowSums(across(all_of(drug_names)))) |>
    mutate(poly_drug_use_freq = rowSums(across(contains("_freq")))) |>

    # 30 day usa
    # duplicate as frequency of use
    mutate(across(.cols = all_of(drug_names_30),
                  .fns = ~.x,
                  .names = "{.col}_freq"
    )
    ) |>
#    # make NA 0?
#    mutate(across(.cols = all_of(drug_names_30),
#                  .fns = ~ifelse(is.na(.x), 0, .x), # if missing, 0, else value
#                  .names = "{.col}"
#    )
#    ) |>
    # create a binary variable yes/no
    mutate(across(.cols = all_of(drug_names_30),
                  .fns =  ~ifelse(.x > 0, 1, 0),
                  .names = "{.col}")
    ) |>
    # create a poly use variable across all questions
    mutate(poly_drug_use_30 = rowSums(across(all_of(drug_names_30)))) |>
    mutate(poly_drug_use_freq_30 = rowSums(across(contains("_30_freq"))))

    # factor poly use factors
#    mutate(poly_drug_use_fact = factor(case_when(
#      poly_drug_use == 0 ~ "None",
#      poly_drug_use == 1 ~ "1",
#      poly_drug_use > 1  ~ ">1",
#      TRUE ~ ""
#      ),
#      labels = c("None", "1", ">1"),
#      ordered = TRUE
#      )) |>
#    mutate(poly_drug_use_30_fact = factor(case_when(
#      poly_drug_use_30 == 0 ~ "None",
#      poly_drug_use_30 == 1 ~ "1",
#      poly_drug_use_30 > 1  ~ ">1",
#      TRUE ~ ""
#    ),
#    labels = c("None", "1", ">1"),
#    ordered = TRUE
#    ))
  return(data)

}
