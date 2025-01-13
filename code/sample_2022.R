                        #***************************************#
                        # Project: Fondecyt 11241178            #
                        # Autores: QRamond + RCantillan         #
                        # Year: 2025                            #
                        #***************************************#

# This script select the sample, applying the following criteria:
  # 1) Drop student who only participate in the complementary round
  # 2) Drop student with imputed home address
  # 3) Drop student who live out of Santiago Metro Area
  # 4) Drop student without enrolled school
  # 5) Keep student who applies to 9th grade ("primero medio")

# This is an initial sample. Further drop may occur if student have missing values.

# Clean R
  rm(list = ls())
  
# load packages
  packages <- c("sf", "sp", "ggplot2", "dplyr")
  lapply(packages, library, character.only = TRUE)

# Set file directory
  setwd("G:/Mi unidad/projets chili/neighbors school choice")

# Load files
  student_main <- read.csv2("./data/original datasets/SAE/SAE_2022/B1_Postulantes_etapa_regular_2022_Admision_2023_PUBL.csv", header = TRUE, sep= ";", encoding="UTF-8")
  student_complementary <- read.csv2("./data/original datasets/SAE/SAE_2022/B2_Postulantes_etapa_complementaria_2022_Admision_2023_PUBL.csv", header = TRUE, sep= ";", encoding="UTF-8")

  application_main <- read.csv2("./data/original datasets/SAE/SAE_2022/C1_Postulaciones_etapa_regular_2022_Admision_2023_PUBL.csv", header = TRUE, sep= ";", encoding="UTF-8")
  application_complementary <- read.csv2("./data/original datasets/SAE/SAE_2022/C2_Postulaciones_etapa_complementaria_2022_Admision_2023_PUBL.csv", header = TRUE, sep= ";", encoding="UTF-8")

  enrolment_2023 <- read.csv("./data/original datasets/matricula-por-estudiante/Matricula-por-estudiante-2023/20230906_Matrícula_unica_2023_20230430_WEB.csv", header = TRUE, sep= ";", encoding="UTF-8")

# Merge files of student with their application using "mrun" (we do not include students who only )
  sample_2022 <- student_main %>% left_join(application_main, by = "mrun") %>%
                 select(-cod_nivel.y) %>%
                 rename(cod_nivel = cod_nivel.x) %>%
                 filter(preferencia_postulante == 1) # Keep only one observation per student, which corresponds to their first preference

# STEP 1: We keep only applicants who live in Santiago Metropolitan Area (34 municipalities)
   cities <- st_read("./data/original datasets/shapefiles/all-cities.shp") # load shapefile
    ggplot() +
    geom_sf(data = cities, lwd = .5, fill = NA) # Just plot the shapefile to be sure
    
  sample_sf_2022 <- sample_2022 %>% 
    select(mrun, lon_con_error, lat_con_error) %>%
    st_as_sf(coords = c('lon_con_error', 'lat_con_error'), crs = 4674) # Create a new sf file from sample
    
  sample_sf_2022 <- st_transform(sample_sf_2022, 3395) # Set SCR with meter metrics
  cities <- st_transform(cities, 3395) # same SCR with meter metrics

  sample_sf_2022 <- st_join(sample_sf_2022, cities, join = st_intersects) # make the intersection. st_within returns the same results, but it is slower.
  
  sample_sf_2022 <- sample_sf_2022 %>% 
                         as.data.frame() %>% # Transform as dataframe
                         filter(COMUNA!="NA") %>% # keep only applicant living in Santiago Metropolitan area
                         select(mrun, COMUNA, NOM_COMUNA, GEOCODIGO, city) %>% # select relevant variable
                         rename_all(tolower) # lower case variable name
  
  sample_2022 <- left_join(sample_sf_2022, sample_2022, by="mrun") # merge with the national sample and keep only applicant in Santiago

# Step 2: Keep only applicant to 9th grade ("primero medio")
  sample_2022 <- sample_2022 %>%
                filter(cod_nivel==9)

# STEP 3: We drop applicant with imputed address
  sample_2022 <- sample_2022 %>% filter(calidad_georef!=4)
  
# STEP 4: we drop students who do not have enrolled school in the next year and those enrolled in specific programs
  enrolment_2023 <- enrolment_2023 %>% 
                    rename_all(tolower) %>% # lower case variable name
                    select(rbd, nom_rbd, mrun, cod_ense) %>%
                    rename(nom_rbd_matriculado = nom_rbd) %>%
                    rename(rbd_matriculado = rbd) %>%
                    rename(cod_ense_matriculado = cod_ense)
  
  sample_2022 <- left_join(sample_2022, enrolment_2023, by = "mrun") %>%
                 filter(!is.na(rbd_matriculado)) %>% # remove applicant without a school in the subsequent year
                 filter(cod_ense_matriculado %in% seq(310, 910, by = 100)) # keep only applicant enrolled in "normal" secondary schools in the subsequent year

# STEP 5: keep only relevant variables and export file
  sample_2022 <- sample_2022 %>%
                 select(mrun, lat_con_error, lon_con_error, comuna, nom_comuna, geocodigo, city, rbd, rbd_matriculado, nom_rbd_matriculado) %>%
                 rename(rbd_postulado = rbd)
                  
  sample_2022$cohort <- 2022 
   
  write.csv(sample_2022, "./data/final datasets/sample_2022.csv")
  
# APPENDIX
# We do not include students who only participated in the complementary round because they do not have information about the quality of their address.
# To find these students, run the following:
#  student_main <- student_main %>% 
#                  mutate(round = "main") 
#  student_complementary <- student_complementary %>% 
#                          mutate(round = "complementary")
#  only_complementary <- left_join(student_complementary, student_main, by="mrun") %>%
#                        filter(is.na(round.y))