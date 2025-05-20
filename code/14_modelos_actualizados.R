
rm(d)
gc()
################################################################################
# post-post
################################################################################

## SES--------------------------------------------------------------------------
m1_post <- glm(mismo_post_post ~ 
                 ses_ego 
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d_post)
summary(m1_post)
m1_post_robust <- get_dyadic_robust_se_post(m1_post)
m1_post_robust


m2_post <- glm(mismo_post_post ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d_post)
summary(m2_post)
m2_post_robust <- get_dyadic_robust_se_post(m2_post)
m2_post_robust


m3_post <- glm(mismo_post_post ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               + distance 
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d_post)
summary(m3_post)
m3_post_robust <- get_dyadic_robust_se_post(m3_post)
m3_post_robust


m4_post <- glm(mismo_post_post ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               + distance 
               #sociodemographic
               + factor(sexo_ego) 
               + factor(sexo_alter) 
               + edad_ego 
               + edad_alter 
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d_post)
summary(m4_post)
m4_post_robust <- get_dyadic_robust_se_post(m4_post)
m4_post_robust


m5_post <- glm(mismo_post_post ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               + distance 
               #sociodemographic
               + factor(sexo_ego) 
               + factor(sexo_alter) 
               + edad_ego 
               + edad_alter 
               #egohood
               + ses_mean 
               + ses_sd 
               + shannon_index 
               + network_size 
               + alter_apply_pct
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d_post)
summary(m5_post)
m5_post_robust <- get_dyadic_robust_se_post(m5_post)
m5_post_robust


m6_post <- glm(mismo_post_post ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               + distance 
               #sociodemographic
               + factor(sexo_ego) 
               + factor(sexo_alter) 
               + edad_ego 
               + edad_alter 
               #egohood
               + ses_mean 
               + ses_sd 
               + shannon_index 
               + network_size 
               + alter_apply_pct
               #school peers
               + factor(same_rbd) 
               + pct_same_as_gdemates    
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d_post)
summary(m6_post)
m6_post_robust <- get_dyadic_robust_se_post(m6_post)
m6_post_robust


m7_post <- glm(mismo_post_post ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               + distance
               #sociodemographic
               + factor(sexo_ego) 
               + factor(sexo_alter) 
               + edad_ego 
               + edad_alter 
               #egohood
               + ses_mean 
               + ses_sd 
               + shannon_index 
               + network_size 
               + alter_apply_pct
               #school peers
               + factor(same_rbd) 
               + pct_same_as_gdemates    
               #school
               + num_school
               + mean_math
               + sd_math
               + mean_reading
               + sd_reading
               + pct_public
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d_post)
summary(m7_post)
m7_post_robust <- get_dyadic_robust_se_post(m7_post)
m7_post_robust


m8_post <- glm(mismo_post_post ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               + distance
               #sociodemographic
               + factor(sexo_ego) 
               + factor(sexo_alter) 
               + edad_ego 
               + edad_alter 
               #egohood
               + ses_mean 
               + ses_sd 
               + shannon_index 
               + network_size 
               + alter_apply_pct
               #school peers
               + factor(same_rbd) 
               + pct_same_as_gdemates    
               #school
               + num_school
               + mean_math
               + sd_math
               + mean_reading
               + sd_reading
               + pct_public
               # school alter
               + factor(dependency_post_alter)
               + math_post_alter
               + read_post_alter
               + growth_math_post_alter
               + growth_read_post_alter
               + priority_student_post_alter
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d_post)
summary(m8_post)
m8_post_robust <- get_dyadic_robust_se_post(m8_post)
m8_post_robust;gc()



##score-------------------------------------------------------------------------

m1_post_score <- glm(mismo_post_post ~ 
                 average_score_z
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d_post)
summary(m1_post_score)
m1_post_score_robust <- get_dyadic_robust_se_post(m1_post_score)
m1_post_score_robust


m2_post_score <- glm(mismo_post_post ~ 
                 average_score_z
               + score_distance_z
               + I(score_distance_z^2)
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d_post)
summary(m2_post_score)
m2_post_score_robust <- get_dyadic_robust_se_post(m2_post_score)
m2_post_score_robust


m3_post_score <- glm(mismo_post_post ~ 
                 average_score_z
               + score_distance_z
               + I(score_distance_z^2)
               + distance 
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d_post)
summary(m3_post_score)
m3_post_score_robust <- get_dyadic_robust_se_post(m3_post_score)
m3_post_score_robust


m4_post_score <- glm(mismo_post_post ~ 
                 average_score_z
               + score_distance_z
               + I(score_distance_z^2)
               + distance 
               #sociodemographic
               + factor(sexo_ego) 
               + factor(sexo_alter) 
               + edad_ego 
               + edad_alter 
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d_post)
summary(m4_post_score)
m4_post_score_robust <- get_dyadic_robust_se_post(m4_post_score)
m4_post_score_robust


m5_post_score <- glm(mismo_post_post ~ 
                 average_score_z
               + score_distance_z
               + I(score_distance_z^2)
               + distance 
               #sociodemographic
               + factor(sexo_ego) 
               + factor(sexo_alter) 
               + edad_ego 
               + edad_alter 
               #egohood
               + ses_mean 
               + ses_sd 
               + shannon_index 
               + network_size 
               + alter_apply_pct
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d_post)
summary(m5_post_score)
m5_post_score_robust <- get_dyadic_robust_se_post(m5_post_score)
m5_post_score_robust


m6_post_score <- glm(mismo_post_post ~ 
                 average_score_z
               + score_distance_z
               + I(score_distance_z^2)
               + distance 
               #sociodemographic
               + factor(sexo_ego) 
               + factor(sexo_alter) 
               + edad_ego 
               + edad_alter 
               #egohood
               + ses_mean 
               + ses_sd 
               + shannon_index 
               + network_size 
               + alter_apply_pct
               #school peers
               + factor(same_rbd) 
               + pct_same_as_gdemates    
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d_post)
summary(m6_post_score)
m6_post_score_robust <- get_dyadic_robust_se_post(m6_post_score)
m6_post_score_robust


m7_post_score <- glm(mismo_post_post ~ 
                 average_score_z
               + score_distance_z
               + I(score_distance_z^2)
               + distance 
               #sociodemographic
               + factor(sexo_ego) 
               + factor(sexo_alter) 
               + edad_ego 
               + edad_alter 
               #egohood
               + score_mean 
               + score_sd 
               + shannon_index 
               + network_size 
               + alter_apply_pct
               #school peers
               + factor(same_rbd) 
               + pct_same_as_gdemates    
               ##school
               + num_school
               + mean_math
               + sd_math
               + mean_reading
               + sd_reading
               + pct_public
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d_post)
summary(m7_post_score)
m7_post_score_robust <- get_dyadic_robust_se_post(m7_post_score)
m7_post_score_robust


m8_post_score <- glm(mismo_post_post ~ 
                       average_score_z
                     + score_distance_z
                     + I(score_distance_z^2)
                     + distance 
                     #sociodemographic
                     + factor(sexo_ego) 
                     + factor(sexo_alter) 
                     + edad_ego 
                     + edad_alter 
                     #egohood
                     + score_mean 
                     + score_sd 
                     + shannon_index 
                     + network_size 
                     + alter_apply_pct
                     #school peers
                     + factor(same_rbd) 
                     + pct_same_as_gdemates    
                     ##school
                     + num_school
                     + mean_math
                     + sd_math
                     + mean_reading
                     + sd_reading
                     + pct_public
                     # school alter
                     + factor(dependency_post_alter)
                     + math_post_alter
                     + read_post_alter
                     + growth_math_post_alter
                     + growth_read_post_alter
                     + priority_student_post_alter
                     # efectos fijos
                     + factor(city)
                     + factor(reference_year), 
                     family = "binomial", data = d_post)
summary(m8_post_score)
m8_post_score_robust <- get_dyadic_robust_se_post(m8_post_score)
m8_post_score_robust;gc()



################################################################################
# post-matric 
################################################################################

m1_matric <- glm(mismo_post_matric ~ 
                 ses_ego 
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d_matric)
summary(m1_matric)
m1_matric_robust <- get_dyadic_robust_se_matric(m1_matric)
m1_matric_robust


m2_matric <- glm(mismo_post_matric ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d_matric)
summary(m2_matric)
m2_matric_robust <- get_dyadic_robust_se_matric(m2_matric)
m2_matric_robust


m3_matric <- glm(mismo_post_matric ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               + distance 
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d_matric)
summary(m3_matric)
m3_matric_robust <- get_dyadic_robust_se_matric(m3_matric)
m3_matric_robust


m4_matric <- glm(mismo_post_matric ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               + distance 
               #sociodemographic
               + factor(sexo_ego) 
               + factor(sexo_alter) 
               + edad_ego 
               + edad_alter 
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d_matric)
summary(m4_matric)
m4_matric_robust <- get_dyadic_robust_se_matric(m4_matric)
m4_matric_robust


m5_matric <- glm(mismo_post_matric ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               + distance 
               #sociodemographic
               + factor(sexo_ego) 
               + factor(sexo_alter) 
               + edad_ego 
               + edad_alter 
               #egohood
               + ses_mean 
               + ses_sd 
               + shannon_index 
               + network_size 
               + alter_apply_pct
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d_matric)
summary(m5_matric)
m5_matric_robust <- get_dyadic_robust_se_matric(m5_matric)
m5_matric_robust


m6_matric <- glm(mismo_post_matric ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               + distance 
               #sociodemographic
               + factor(sexo_ego) 
               + factor(sexo_alter) 
               + edad_ego 
               + edad_alter 
               #egohood
               + ses_mean 
               + ses_sd 
               + shannon_index 
               + network_size 
               + alter_apply_pct
               #school peers
               + factor(same_rbd) 
               + pct_same_as_gdemates    
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d_matric)
summary(m6_matric)
m6_matric_robust <- get_dyadic_robust_se_matric(m6_matric)
m6_matric_robust


m7_matric <- glm(mismo_post_matric ~ 
                 ses_ego 
               + ses_distance 
               + I(ses_distance^2)
               + distance 
               #sociodemographic
               + factor(sexo_ego) 
               + factor(sexo_alter) 
               + edad_ego 
               + edad_alter 
               #egohood
               + ses_mean 
               + ses_sd 
               + shannon_index 
               + network_size 
               + alter_apply_pct
               #school peers
               + factor(same_rbd) 
               + pct_same_as_gdemates    
               #school
               + num_school
               + mean_quality
               + std_quality
               + pct_public
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d_matric)
summary(m7_matric)
m7_matric_robust <- get_dyadic_robust_se_matric(m7_matric)
m7_matric_robust


m8_matric <- glm(mismo_post_matric ~ 
                   ses_ego 
                 + ses_distance 
                 + I(ses_distance^2)
                 + distance 
                 #sociodemographic
                 + factor(sexo_ego) 
                 + factor(sexo_alter) 
                 + edad_ego 
                 + edad_alter 
                 #egohood
                 + ses_mean 
                 + ses_sd 
                 + shannon_index 
                 + network_size 
                 + alter_apply_pct
                 #school peers
                 + factor(same_rbd) 
                 + pct_same_as_gdemates    
                 #school
                 + num_school
                 + mean_quality
                 + std_quality
                 + pct_public
                 # school alter
                 + factor(dependency_matric_alter)
                 + math_matric_alter
                 + read_matric_alter
                 + growth_math_matric_alter
                 + growth_read_matric_alter
                 + priority_student_matric_alter
                 # efectos fijos
                 + factor(city)
                 + factor(reference_year), 
                 family = "binomial", data = d_matric)
summary(m8_matric)
m8_matric_robust <- get_dyadic_robust_se_matric(m8_matric)
m8_matric_robust; gc()


##score-------------------------------------------------------------------------

m1_matric_score <- glm(mismo_post_matric ~ 
                       average_score_z
                     # efectos fijos
                     + factor(city)
                     + factor(reference_year), 
                     family = "binomial", data = d_matric)
summary(m1_matric_score)
m1_matric_score_robust <- get_dyadic_robust_se_matric(m1_matric_score)
m1_matric_score_robust


m2_matric_score <- glm(mismo_post_matric ~ 
                       average_score_z
                     + score_distance_z
                     + I(score_distance_z^2)
                     # efectos fijos
                     + factor(city)
                     + factor(reference_year), 
                     family = "binomial", data = d_matric)
summary(m2_matric_score)
m2_matric_score_robust <- get_dyadic_robust_se_matric(m2_matric_score)
m2_matric_score_robust


m3_matric_score <- glm(mismo_post_matric ~ 
                       average_score_z
                     + score_distance_z
                     + I(score_distance_z^2)
                     + distance 
                     # efectos fijos
                     + factor(city)
                     + factor(reference_year), 
                     family = "binomial", data = d_matric)
summary(m3_matric_score)
m3_matric_score_robust <- get_dyadic_robust_se_matric(m3_matric_score)
m3_matric_score_robust


m4_matric_score <- glm(mismo_post_matric ~ 
                       average_score_z
                     + score_distance_z
                     + I(score_distance_z^2)
                     + distance 
                     #sociodemographic
                     + factor(sexo_ego) 
                     + factor(sexo_alter) 
                     + edad_ego 
                     + edad_alter 
                     # efectos fijos
                     + factor(city)
                     + factor(reference_year), 
                     family = "binomial", data = d_matric)
summary(m4_matric_score)
m4_matric_score_robust <- get_dyadic_robust_se_matric(m4_matric_score)
m4_matric_score_robust


m5_matric_score <- glm(mismo_post_matric ~ 
                       average_score_z
                     + score_distance_z
                     + I(score_distance_z^2)
                     + distance 
                     #sociodemographic
                     + factor(sexo_ego) 
                     + factor(sexo_alter) 
                     + edad_ego 
                     + edad_alter 
                     #egohood
                     + ses_mean 
                     + ses_sd 
                     + shannon_index 
                     + network_size 
                     + alter_apply_pct
                     # efectos fijos
                     + factor(city)
                     + factor(reference_year), 
                     family = "binomial", data = d_matric)
summary(m5_matric_score)
m5_matric_score_robust <- get_dyadic_robust_se_matric(m5_matric_score)
m5_matric_score_robust


m6_matric_score <- glm(mismo_post_matric ~ 
                       average_score_z
                     + score_distance_z
                     + I(score_distance_z^2)
                     + distance 
                     #sociodemographic
                     + factor(sexo_ego) 
                     + factor(sexo_alter) 
                     + edad_ego 
                     + edad_alter 
                     #egohood
                     + ses_mean 
                     + ses_sd 
                     + shannon_index 
                     + network_size 
                     + alter_apply_pct
                     #school peers
                     + factor(same_rbd) 
                     + pct_same_as_gdemates    
                     # efectos fijos
                     + factor(city)
                     + factor(reference_year), 
                     family = "binomial", data = d_matric)
summary(m6_matric_score)
m6_matric_score_robust <- get_dyadic_robust_se_matric(m6_matric_score)
m6_matric_score_robust


m7_matric_score <- glm(mismo_post_matric ~ 
                       average_score_z
                     + score_distance_z
                     + I(score_distance_z^2)
                     + distance 
                     #sociodemographic
                     + factor(sexo_ego) 
                     + factor(sexo_alter) 
                     + edad_ego 
                     + edad_alter 
                     #egohood
                     + ses_mean 
                     + ses_sd 
                     + shannon_index 
                     + network_size 
                     + alter_apply_pct
                     #school peers
                     + factor(same_rbd) 
                     + pct_same_as_gdemates    
                     #school
                     + num_school
                     + mean_quality
                     + std_quality
                     + pct_public
                     # efectos fijos
                     + factor(city)
                     + factor(reference_year), 
                     family = "binomial", data = d_matric)
summary(m7_matric_score)
m7_matric_score_robust <- get_dyadic_robust_se_matric(m7_matric_score)
m7_matric_score_robust


m8_matric_score <- glm(mismo_post_matric ~ 
                         average_score_z
                       + score_distance_z
                       + I(score_distance_z^2)
                       + distance 
                       #sociodemographic
                       + factor(sexo_ego) 
                       + factor(sexo_alter) 
                       + edad_ego 
                       + edad_alter 
                       #egohood
                       + ses_mean 
                       + ses_sd 
                       + shannon_index 
                       + network_size 
                       + alter_apply_pct
                       #school peers
                       + factor(same_rbd) 
                       + pct_same_as_gdemates    
                       #school
                       + num_school
                       + mean_quality
                       + std_quality
                       + pct_public
                       # school alter
                       + factor(dependency_matric_alter)
                       + math_matric_alter
                       + read_matric_alter
                       + growth_math_matric_alter
                       + growth_read_matric_alter
                       + priority_student_matric_alter
                       # efectos fijos
                       + factor(city)
                       + factor(reference_year), 
                       family = "binomial", data = d_matric)
summary(m8_matric_score)
m8_matric_score_robust <- get_dyadic_robust_se_matric(m8_matric_score)
m8_matric_score_robust; gc()


################################################################################
# Modelos extras con interacciones entre quintiles 
################################################################################

m8_post_q <- glm(mismo_post_post ~ 
                 ses_ego 
               + ses_ego_quintil*ses_alter_quintil
               + distance
               #sociodemographic
               + factor(sexo_ego) 
               + factor(sexo_alter) 
               + edad_ego 
               + edad_alter 
               #egohood
               + ses_mean 
               + ses_sd 
               + shannon_index 
               + network_size 
               + alter_apply_pct
               #school peers
               + factor(same_rbd) 
               + pct_same_as_gdemates    
               #school
               + num_school
               + mean_math
               + sd_math
               + mean_reading
               + sd_reading
               + pct_public
               # school alter
               + factor(dependency_post_alter)
               + math_post_alter
               + read_post_alter
               + growth_math_post_alter
               + growth_read_post_alter
               + priority_student_post_alter
               # efectos fijos
               + factor(city)
               + factor(reference_year), 
               family = "binomial", data = d_post)
summary(m8_post_q)
m8_post_robust_q <- get_dyadic_robust_se_post(m8_post_q)
m8_post_robust_q;gc()


m8_post_score_q <- glm(mismo_post_post ~ 
                       average_score_z
                     + score_ego_quintil*score_alter_quintil
                     + distance 
                     #sociodemographic
                     + factor(sexo_ego) 
                     + factor(sexo_alter) 
                     + edad_ego 
                     + edad_alter 
                     #egohood
                     + score_mean 
                     + score_sd 
                     + shannon_index 
                     + network_size 
                     + alter_apply_pct
                     #school peers
                     + factor(same_rbd) 
                     + pct_same_as_gdemates    
                     ##school
                     + num_school
                     + mean_math
                     + sd_math
                     + mean_reading
                     + sd_reading
                     + pct_public
                     # school alter
                     + factor(dependency_post_alter)
                     + math_post_alter
                     + read_post_alter
                     + growth_math_post_alter
                     + growth_read_post_alter
                     + priority_student_post_alter
                     # efectos fijos
                     + factor(city)
                     + factor(reference_year), 
                     family = "binomial", data = d_post)
summary(m8_post_score_q)
m8_post_score_robust_q <- get_dyadic_robust_se_post(m8_post_score_q)
m8_post_score_robust_q;gc()


m8_matric_q <- glm(mismo_post_matric ~ 
                   ses_ego 
                 + ses_ego_quintil*ses_alter_quintil
                 + distance 
                 #sociodemographic
                 + factor(sexo_ego) 
                 + factor(sexo_alter) 
                 + edad_ego 
                 + edad_alter 
                 #egohood
                 + ses_mean 
                 + ses_sd 
                 + shannon_index 
                 + network_size 
                 + alter_apply_pct
                 #school peers
                 + factor(same_rbd) 
                 + pct_same_as_gdemates    
                 #school
                 + num_school
                 + mean_quality
                 + std_quality
                 + pct_public
                 # school alter
                 + factor(dependency_matric_alter)
                 + math_matric_alter
                 + read_matric_alter
                 + growth_math_matric_alter
                 + growth_read_matric_alter
                 + priority_student_matric_alter
                 # efectos fijos
                 + factor(city)
                 + factor(reference_year), 
                 family = "binomial", data = d_matric)
summary(m8_matric_q)
m8_matric_robust_q <- get_dyadic_robust_se_matric(m8_matric_q)
m8_matric_robust_q; gc()


m8_matric_score_q <- glm(mismo_post_matric ~ 
                         average_score_z
                       + score_ego_quintil*score_alter_quintil
                       + distance 
                       #sociodemographic
                       + factor(sexo_ego) 
                       + factor(sexo_alter) 
                       + edad_ego 
                       + edad_alter 
                       #egohood
                       + ses_mean 
                       + ses_sd 
                       + shannon_index 
                       + network_size 
                       + alter_apply_pct
                       #school peers
                       + factor(same_rbd) 
                       + pct_same_as_gdemates    
                       #school
                       + num_school
                       + mean_quality
                       + std_quality
                       + pct_public
                       # school alter
                       + factor(dependency_matric_alter)
                       + math_matric_alter
                       + read_matric_alter
                       + growth_math_matric_alter
                       + growth_read_matric_alter
                       + priority_student_matric_alter
                       # efectos fijos
                       + factor(city)
                       + factor(reference_year), 
                       family = "binomial", data = d_matric)
summary(m8_matric_score_q)
m8_matric_score_robust_q <- get_dyadic_robust_se_matric(m8_matric_score_q)
m8_matric_score_robust_q; gc()

















