                        #***************************************#
                        # Project: Fondecyt 11241178            #
                        # Autores: QRamond + RCantillan         #
                        # Year: 2025                            #
                        #***************************************#

# This script links the sample of applicants to the SIMCE data to obtain their SES characteristics

# Clean R
  rm(list = ls())
  
# load packages
  install.packages("DescTools")
  packages <- c("sf", "sp", "ggplot2", "dplyr", "readxl", "DescTools")
  lapply(packages, library, character.only = TRUE)

# Set file directory
  setwd("G:/Mi unidad/projets chili/neighbors school choice")

# Load files of SIMCE
  alumnos_8b_2019 <- read_excel("./data/original datasets/simce-ind/simce8b2019_alu.xlsx")
  padres_8b_2019 <- read_excel("./data/original datasets/simce-ind/simce8b2019_cpad.xlsx")
  sample_2019 <- read.csv("./data/final datasets/sample_2019.csv")
  
# Select relevant variables in both files
  alumnos_8b_2019 <- alumnos_8b_2019 %>% select(idalumno, mrun, rbd, cod_curso)
  padres_8b_2019 <- padres_8b_2019 %>% select(idalumno, cpad_p05, cpad_p06, cpad_p11)
  
# Keep only non-duplicated mrun in the student file
  alumnos_8b_2019 <- alumnos_8b_2019 %>%
                       filter(!duplicated(mrun))

# Merge SIMCE files students and parents and rename variables
  alumnos_8b_2019 <- alumnos_8b_2019 %>%
                     left_join(padres_8b_2019, by = "idalumno") %>%
                     rename(income = cpad_p11) %>%
                     rename(educ_father = cpad_p05) %>%
                     rename(educ_mother = cpad_p06)

# Replace values without meaning by N/A
  alumnos_8b_2019 <- alumnos_8b_2019 %>%
                     mutate(across(c(educ_father, educ_mother, income), ~ replace(., . %in% c(0, 21, 99), NA)))  
      
# Impute missing values using the mode 
  # Create new dataset without missing values
    imputation_income <- alumnos_8b_2019 %>%
                         filter(!is.na(income))
    
    imputation_educ_mother  <- alumnos_8b_2019 %>%
                               filter(!is.na(educ_mother))
  
    imputation_educ_father  <- alumnos_8b_2019 %>%
                               filter(!is.na(educ_father))
    
  # Create a function to get the mode (and multiple mode in case of tie)
    getmode <- function(v) {
      uniqv <- unique(v[!is.na(v)])
      tab <- table(match(v, uniqv))
      modes <- uniqv[tab == max(tab)]  # Return all modes
      return(modes)
    }

  # Create the mode applying the function getmode
    imputation_income <- imputation_income %>%
                         group_by(rbd, cod_curso) %>%
                         reframe(Mode = getmode(income))
    
    imputation_educ_mother <- imputation_educ_mother %>%
                              group_by(rbd, cod_curso) %>%
                              reframe(Mode = getmode(educ_mother))
    
    imputation_educ_father <- imputation_educ_father %>%
                              group_by(rbd, cod_curso) %>%
                              reframe(Mode = getmode(educ_father))
  
  # Keep the highest mode
    imputation_income <- imputation_income %>%
                         group_by(rbd, cod_curso) %>%       
                         slice_max(Mode, n = 1) %>%        
                         ungroup() %>%
                         rename(imputed_income = Mode)
    
    imputation_educ_mother <- imputation_educ_mother %>%
                              group_by(rbd, cod_curso) %>%       
                              slice_max(Mode, n = 1) %>%        
                              ungroup() %>%
                              rename(imputed_educ_mother = Mode)
    
    imputation_educ_father <- imputation_educ_father %>%
                              group_by(rbd, cod_curso) %>%       
                              slice_max(Mode, n = 1) %>%        
                              ungroup() %>%
                              rename(imputed_educ_father = Mode)
    

    alumnos_8b_2019 <- alumnos_8b_2019 %>%
      left_join(imputation_income, by = c("rbd", "cod_curso"))
    
    alumnos_8b_2019 <- alumnos_8b_2019 %>%
      left_join(imputation_educ_mother, by = c("rbd", "cod_curso"))
    
    alumnos_8b_2019 <- alumnos_8b_2019 %>%
      left_join(imputation_educ_father, by = c("rbd", "cod_curso"))
  
  # Replace missing values in origina variable with imputed values
    alumnos_8b_2019 <- alumnos_8b_2019 %>%
      mutate(income = if_else(is.na(income), imputed_income, income))

    alumnos_8b_2019 <- alumnos_8b_2019 %>%
      mutate(educ_mother = if_else(is.na(educ_mother), imputed_educ_mother, educ_mother))
    
    alumnos_8b_2019 <- alumnos_8b_2019 %>%
      mutate(educ_father = if_else(is.na(educ_father), imputed_educ_father, educ_father))

  # Drop useless variable
    alumnos_8b_2019 <- alumnos_8b_2019 %>% select(-imputed_income, -imputed_educ_mother,
                                                  -imputed_educ_father)

  # Merge Simce file with the sample
    sample_2019 <- sample_2019 %>%
      left_join(alumnos_8b_2019, by = "mrun")

    