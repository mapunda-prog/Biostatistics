import delimited "C:\Users\User\Desktop\survival analysis\cleaned_after_comment.csv"
stset months_to_rebound, failure(rebound_occurred==1)
describe
encode age_cat, generate(num_age_cat)
encode cd4_category, generate(num_cd4_cat)
encode hf_distance_category, generate(num_hf_distance_cat)
encode hf_distance_cat2 , generate(num_hf_distance_cat2)
encode vl_test_freq, generate(num_vl_test_freq)
encode interuption_cat, generate(num_interuption_cat)
encode sex, generate(gender)
encode residence, generate(num_residence)
encode who_clinical_stage, generate(num_who_stage)
tab art_adherence
encode art_adherence, generate(num_art_adherence)
tab art_regimen
encode art_regimen, generate (num_art_regimen)
tab arv_combination
tab type_of_health_facility
tab type_of_health_facility rebound_occurred
encode type_of_health_facility, generate(num_type_HF)
recode facility_patient_volume (1/199=1) (200/399=2) (400/999=3) (1000/max= 4), generate(num_facility_volume)
lab define num_facility_volume 1 "1-199" 2 "200-399" 3 "400-999" 4 "1000 and above"
lab values num_facility_volume num_facility_volume
tab num_facility_volume
recode facility_staffing (1/4=1) (5/9=2) (10/max= 3), generate(num_facility_staff)
lab define num_facility_staff 1 "1-4" 2 "5-9" 3 "10 and above"
lab values num_facility_staff num_facility_staff
stset months_to_rebound, id( patient_id) failure(rebound_occurred==1)
stsum
describe
encode llv_cat_baseline, generate (num_llv_cat_baseline)
stcox i.num_llv_cat_baseline
stcox i.num_llv_cat_baseline i.num_facility_staff i.num_facility_volume i.num_type_HF i.num_art_regimen i.num_art_adherence i.num_who_stage i.num_residence i.gender i.num_interuption_cat i.num_vl_test_freq i.num_hf_distance_cat2 i.num_cd4_cat i.num_age_cat
stcox i.num_llv_cat_baseline i.num_facility_staff i.num_facility_volume i.num_type_HF i.num_art_regimen i.num_art_adherence i.num_who_stage i.gender i.num_interuption_cat i.num_vl_test_freq i.num_hf_distance_cat2 i.num_cd4_cat i.num_age_cat
stcox i.num_llv_cat_baseline i.num_facility_staff i.num_facility_volume i.num_type_HF i.num_art_regimen i.num_art_adherence i.num_who_stage i.gender i.num_interuption_cat i.num_vl_test_freq i.num_hf_distance_cat2 i.num_cd4_cat i.num_age_cat
stset time, failure(rebound_occurred==1) scale(1)
stcox i.num_llv_cat_baseline i.num_facility_staff i.num_facility_volume i.num_type_HF i.num_art_regimen i.num_art_adherence i.num_who_stage i.gender i.num_interuption_cat i.num_vl_test_freq i.num_hf_distance_cat2 i.num_cd4_cat i.num_age_cat
stset months_to_rebound, failure(rebound_occurred==1) scale(1)
stcox num_llv_cat_baseline num_facility_staff num_facility_volume num_type_HF num_art_regimen num_art_adherence num_who_stage gender num_interuption_cat num_vl_test_freq num_hf_distance_cat2 num_cd4_cat num_age_cat
stcox i.num_llv_cat_baseline
stcox i.num_facility_staff
stcox i.num_facility_volume
stcox i.num_type_HF
stcox i.num_art_regimen
stcox i2.num_art_regimen
stcox i1.num_art_regimen
stcox i.num_art_adherence
tab num_art_adherence
tab num_art_adherence rebound_occurred, chi2
stcox i.num_who_stage
tab client_category
encode client_category, generate(num_client_category)
stcox i.num_client_category
stcox i.num_llv_cat_baseline i.num_facility_staff i.num_facility_volume i.num_type_HF i.num_art_regimen i.num_art_adherence i.num_who_stage i.num_residence i.gender i.num_interuption_cat i.num_vl_test_freq i.num_hf_distance_cat2 i.num_cd4_cat i.num_age_cat i.gender i.num_client_category
stcox i.num_llv_cat_baseline i.num_facility_staff i.num_facility_volume i.num_type_HF i.num_art_regimen i.num_art_adherence i.num_who_stage i.gender i.num_interuption_cat i.num_vl_test_freq i.num_hf_distance_cat2 i.num_cd4_cat i.num_age_cat i.gender i.num_client_category
stcox num_llv_cat_baseline i.num_facility_staff i.num_facility_volume i.num_type_HF i.num_art_regimen i.num_art_adherence i.num_who_stage i.gender i.num_interuption_cat i.num_vl_test_freq i.num_hf_distance_cat2 i.num_cd4_cat i.num_age_cat i.gender i.num_client_category
stcox i.num_llv_cat_baseline i.num_facility_staff i.num_facility_volume i.num_type_HF i.num_art_regimen i.num_art_adherence i.num_who_stage i.gender i.num_interuption_cat i.num_vl_test_freq i.num_hf_distance_cat2 i.num_cd4_cat i.num_age_cat i1.gender i.num_client_category
tab residence_lvl
encode residence_lvl, generate(num_residence_lvl)
tab residence_lvl rebound_occurred, chi2
stcox i.num_llv_cat_baseline i.num_facility_staff i.num_facility_volume i.num_type_HF i.num_art_regimen i.num_art_adherence i.num_who_stage i.gender i.num_interuption_cat i.num_vl_test_freq i.num_hf_distance_cat2 i.num_cd4_cat i.num_age_cat i1.gender i.num_residence_lvl i.num_client_category
stcox i.num_llv_cat_baseline i.num_facility_staff i.num_facility_volume i.num_art_adherence i.gender i.num_interuption_cat i.num_vl_test_freq i.num_hf_distance_cat2 i.num_age_cat i.gender i.num_client_category
stcox i.num_llv_cat_baseline i.num_facility_staff i.num_facility_volume i.num_art_adherence i.gender i.num_vl_test_freq i.num_hf_distance_cat2 i.num_age_cat i.gender i.num_client_category
stcox i.num_llv_cat_baseline i.num_facility_staff i.num_facility_volume i.num_art_adherence i.gender i.num_vl_test_freq i.num_hf_distance_cat2 i.num_age_cat i.gender i.num_client_category, hr
stcox i.num_llv_cat_baseline i.num_art_adherence i.num_hf_distance_cat2 i.num_age_cat i.gender i.num_client_category, hr
stcox i.num_llv_cat_baseline i.num_facility_staff i.num_facility_volume i.num_art_adherence i.gender i.num_vl_test_freq i.num_hf_distance_cat2 ib2.num_age_cat i.gender i.num_client_category, hr


**incidence rates**
stptime
stptime, by(marital_status)
stptime, by(age_cat)
stptime, by(gender)
