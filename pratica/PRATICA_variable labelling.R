
# Swap out some numbers for words
fever <- fever %>%
  mutate(seek_tx = case_when(fever_treatment == 1 ~ "Sim",
                             fever_treatment == 0 ~ "Não",
                             fever_treatment == 8 ~ "Não sabe",
                             TRUE ~ as.character(fever_treatment)),
         loc_tx = case_when(treatment_place == 1 ~ "Hospital central",
                            treatment_place == 2 ~ "Hospital provincial",
                            treatment_place == 3 ~ "Hospital distrital/rural",
                            treatment_place == 4 ~ "Centro/posto de saúde",
                            treatment_place == 5 ~ "Brigadas moveis",
                            treatment_place == 6 ~ "Farmácia",
                            treatment_place == 7 ~ "APE",
                            treatment_place == 8 ~ "Hospital/clinica privada",
                            treatment_place == 9 ~ "Farmácia privada",
                            treatment_place == 10 ~ "Médico privado",
                            treatment_place == 11 ~ "Mercado/dumba nengue",
                            treatment_place == 12 ~ "Médico trad",
                            treatment_place == 13 ~ "Amigos/parentes",
                            treatment_place == 96 ~ "Outro",
                            TRUE ~ as.character(treatment_place)),
         loc_tx = fct_expand(loc_tx, "Hospital central", "Hospital provincial", "Hospital distrital/rural", "Centro/posto de saúde", "Brigadas moveis", "Farmácia", "APE", "Hospital/clinica privada", "Farmácia privada", "Médico privado", "Mercado/dumba nengue", "Médico trad", "Amigos/parentes", "Outro"),
         why_notx = case_when(no_treatment_reason == 1 ~ "Não estava disponível",
                              no_treatment_reason == 2 ~ "É caro demais",
                              no_treatment_reason == 3 ~ "É muito distante",
                              no_treatment_reason == 4 ~ "Não havia transporte",
                              no_treatment_reason == 5 ~ "Tinha muito trabalho",
                              no_treatment_reason == 6 ~ "A febre não era grave",
                              no_treatment_reason == 7 ~ "Não tinha permissão",
                              no_treatment_reason == 96 ~ "Outro",
                              TRUE ~ as.character(no_treatment_reason)),
         why_notx = fct_expand(why_notx, "Não estava disponível", "É caro demais", "Não havia transporte", "Tinha muito trabalho", "A febre não era grave", "Não tinha permissão", "Outro"),
         mal_test = case_when(test_malaria == 1 ~ "Sim",
                              test_malaria == 0 ~ "Não",
                              test_malaria == 8 ~ "Não sabe",
                              TRUE ~ as.character(test_malaria)),
         mal_result = case_when(result_malaria == 1 ~ "Positivo",
                                result_malaria == 2 ~ "Negativo",
                                result_malaria == 8 ~ "Não sabe",
                                TRUE ~ as.character(result_malaria)),
         meds_yn = case_when(medication == 1 ~ "Sim",
                             medication == 0 ~ "Não",
                             medication == 8 ~ "Não sabe",
                             TRUE ~ as.character(result_malaria)))







#--- Label variables
# haven::zap_labels() can be used to drop labels
# haven::as_factor() can be used to convert labelled variables to factors


#--- Women's data

# Rename variables
women <- women %>% rename(test_reading = w_intro_g_testreading_test_reading)

# Deal with select multiple
# Who did you see for ANC?
woexp = women %>% mutate(doc_type = strsplit(doc_type, " ")) %>% 
  unnest(doc_type) %>% # Split up using space as seperator 
  mutate(doc_type = paste0("doc_", doc_type),  # Add doc_ prefix so var names won't cause pain
         doc_type = fct_expand(doc_type, "doc_1", "doc_2", "med_3", "med_4", "med_5", "med_6", "doc_NA"), # add all factor levels (even ones that aren't in the data)
         count = 1) %>% # Create a count var that indicates that response was present in the data
  select(id, doc_type, count) %>% # Select only relevant vars
  complete(id, doc_type) %>% # Now add in rows for empty levels
  replace(is.na(.), 0) %>% # Replace NAs with 0
  pivot_wider(names_from = doc_type, values_from = count) %>% # Pivot wider
  select(id, doc_1, doc_2, doc_3, doc_4, doc_5, doc_6) # Can drop the NA here

# 1	= Médico
# 2	=	Enfermeira SMI
# 3	=	Parteira
# 4	=	Parteira tradicional
# 5	=	APE
# 6	=	Outra

# Add back into data  
women <- left_join(women, woexp, by = "id")
# Remove the temp stuff
rm(woexp)

# What are symptoms of malaria?
woexp = women %>% mutate(mal_sympt = strsplit(mal_sympt, " ")) %>% 
  unnest(mal_sympt) %>% # Split up using space as seperator 
  mutate(mal_sympt = paste0("sympt_", mal_sympt),  # Add sympt_ prefix so var names won't cause pain
         mal_sympt = fct_expand(mal_sympt, "sympt_1", "sympt_2", "sympt_3", "sympt_4", "sympt_5", "sympt_6", "sympt_7", "sympt_8", "sympt_9", "sympt_96", "sympt_98", "sympt_NA"), # add all factor levels (even ones that aren't in the data)
         count = 1) %>% # Create a count var that indicates that response was present in the data
  select(id, mal_sympt, count) %>% # Select only relevant vars
  complete(id, mal_sympt) %>% # Now add in rows for empty levels
  replace(is.na(.), 0) %>% # Replace NAs with 0
  pivot_wider(names_from = mal_sympt, values_from = count) %>% # Pivot wider
  select(id, sympt_1, sympt_2, sympt_3, sympt_4, sympt_5, sympt_6, sympt_7, sympt_8, sympt_9, sympt_96, sympt_98) # Can drop the NA here

# 1	= Febre
# 2	=	Calafrios/tremores
# 3	=	Dor de cabeça
# 4	=	Dor nas articulações
# 5	=	Apetite reduzido
# 6	=	Vómitos
# 7 = Convulsões
# 8 = Tosse
# 9 = Congestão nasal
# 96 = Outro
# 98 = Não sabe

# Add back into data  
women <- left_join(women, woexp, by = "id")
# Remove the temp stuff
rm(woexp)

# How can you prevent malaria?
woexp = women %>% mutate(mal_prev = strsplit(mal_prev, " ")) %>% 
  unnest(mal_prev) %>% # Split up using space as seperator 
  mutate(mal_prev = paste0("prev_", mal_prev),  # Add prev_ prefix so var names won't cause pain
         mal_prev = fct_expand(mal_prev, "prev_1", "prev_2", "prev_3", "prev_4", "prev_5", "prev_6", "prev_7", "prev_8", "prev_9", "prev_10", "prev_96", "prev_98", "prev_NA"), # add all factor levels (even ones that aren't in the data)
         count = 1) %>% # Create a count var that indicates that response was present in the data
  select(id, mal_prev, count) %>% # Select only relevant vars
  complete(id, mal_prev) %>% # Now add in rows for empty levels
  replace(is.na(.), 0) %>% # Replace NAs with 0
  pivot_wider(names_from = mal_prev, values_from = count) %>% # Pivot wider
  select(id, prev_1, prev_2, prev_3, prev_4, prev_5, prev_6, prev_7, prev_8, prev_9, prev_10, prev_96, prev_98) # Can drop the NA here

# 1	= Dorme dentro duma rede mosquiteira
# 2	=	Dorme dentro duma RMTI
# 3	=	Pulverização da casa com insecticida
# 4	=	Usar serpentina/baygon
# 5	=	Manter portas e janelas fechadas
# 6	=	Usar repelentes de insectos
# 7 = Cortar o capim
# 8 = Eliminar águas paradas a volta da casa
# 9 = Queimar folhas/eucalipto
# 10 = Condicionadores de ar / ventiladores
# 96 = Outro
# 98 = Não sabe

# Add back into data  
women <- left_join(women, woexp, by = "id")
# Remove the temp stuff
rm(woexp)

# How can you prevent malaria in pregnancy?
woexp = women %>% mutate(mal_preg_prev = strsplit(mal_preg_prev, " ")) %>% 
  unnest(mal_preg_prev) %>% # Split up using space as seperator 
  mutate(mal_preg_prev = paste0("pregv_", mal_preg_prev),  # Add pregv_ prefix so var names won't cause pain
         mal_preg_prev = fct_expand(mal_preg_prev, "pregv_1", "pregv_2", "pregv_3", "pregv_4", "pregv_5", "pregv_6", "pregv_7", "pregv_8", "pregv_9", "pregv_10", "pregv_11", "pregv_96", "pregv_98", "pregv_NA"), # add all factor levels (even ones that aren't in the data)
         count = 1) %>% # Create a count var that indicates that response was present in the data
  select(id, mal_preg_prev, count) %>% # Select only relevant vars
  complete(id, mal_preg_prev) %>% # Now add in rows for empty levels
  replace(is.na(.), 0) %>% # Replace NAs with 0
  pivot_wider(names_from = mal_preg_prev, values_from = count) %>% # Pivot wider
  select(id, pregv_1, pregv_2, pregv_3, pregv_4, pregv_5, pregv_6, pregv_7, pregv_8, pregv_9, pregv_10, pregv_11, pregv_96, pregv_98) # Can drop the NA here

# 1	= Dorme dentro duma rede mosquiteira
# 2	=	Dorme dentro duma RMTI
# 3	=	Pulverização da casa com insecticida
# 4	=	Usar serpentina/baygon
# 5	=	Manter portas e janelas fechadas
# 6	=	Usar repelentes de insectos
# 7 = Cortar o capim
# 8 = Eliminar águas paradas a volta da casa
# 9 = Queimar folhas/eucalipto
# 10 = Tomar medicamentos preventivos durante a gravidez
# 11 = Condicionadores de ar / ventiladores
# 96 = Outro
# 98 = Não sabe

# Add back into data  
women <- left_join(women, woexp, by = "id")
# Remove the temp stuff
rm(woexp)

# What drugs can be used to cure malaria
woexp = women %>% mutate(med_cure_mal = strsplit(med_cure_mal, " ")) %>% 
  unnest(med_cure_mal) %>% # Split up using space as seperator 
  mutate(med_cure_mal = paste0("cure_", med_cure_mal),  # Add med_ prefix so var names won't cause pain
         med_cure_mal = fct_expand(med_cure_mal, "cure_1", "cure_2", "cure_3", "cure_4", "cure_5", "cure_6", "cure_7", "cure_8", "cure_9", "cure_10", "cure_11", "cure_12", "cure_13", "cure_14", "cure_96", "cure_98", "cure_NA"), # add all factor levels (even ones that aren't in the data)
         count = 1) %>% # Create a count var that indicates that response was present in the data
  select(id, med_cure_mal, count) %>% # Select only relevant vars
  complete(id, med_cure_mal) %>% # Now add in rows for empty levels
  replace(is.na(.), 0) %>% # Replace NAs with 0
  pivot_wider(names_from = med_cure_mal, values_from = count) %>% # Pivot wider
  select(id, cure_1, cure_2, cure_3, cure_4, cure_5, cure_6, cure_7, cure_8, cure_9, cure_10, cure_11, cure_12, cure_13, cure_14, cure_96, cure_98) # Can drop the NA here

# 1	= Artemisinin-based combination therapy (tca/coartem)
# 2	=	Sp/fansidar
# 3	=	Chloroquine
# 4	=	Amodiaquine
# 5	=	Quinine met
# 6	=	Quinine injection/iv
# 7	=	Suppository artesunate
# 8	=	Artesunate injection/iv
# 9	=	Another anti-malarial
# 10 =	Antibiotics pill/syrup
# 11	=	Injection/iv antibiotics
# 12	=	Other Aspirin Medications
# 13	=	Other paracetemol medicines
# 14	=	Other ibuprofen medications
# 96	=	Other
# 98	=	Do not know

# Add back into data  
women <- left_join(women, woexp, by = "id")
# Remove the temp stuff
rm(woexp)

# What messages about malaria have you heard or seen?
woexp = women %>% mutate(mal_message = strsplit(mal_message, " ")) %>% 
  unnest(mal_message) %>% # Split up using space as seperator 
  mutate(mal_message = paste0("mssg_", mal_message),  # Add mssg_ prefix so var names won't cause pain
         mal_message = fct_expand(mal_message, "mssg_1", "mssg_2", "mssg_3", "mssg_4", "mssg_5", "mssg_6", "mssg_7", "mssg_8", "mssg_9", "mssg_10", "mssg_11", "mssg_12", "mssg_96", "mssg_98", "cure_NA"), # add all factor levels (even ones that aren't in the data)
         count = 1) %>% # Create a count var that indicates that response was present in the data
  select(id, mal_message, count) %>% # Select only relevant vars
  complete(id, mal_message) %>% # Now add in rows for empty levels
  replace(is.na(.), 0) %>% # Replace NAs with 0
  pivot_wider(names_from = mal_message, values_from = count) %>% # Pivot wider
  select(id, mssg_1, mssg_2, mssg_3, mssg_4, mssg_5, mssg_6, mssg_7, mssg_8, mssg_9, mssg_10, mssg_11, mssg_12, mssg_96, mssg_98) # Can drop the NA here

# 1	= Malaria is dangerous
# 2	=	Malaria can kill
# 3	=	Mosquito transmit malaria
# 4	=	It is important to sleep under a mosquito net
# 5	=	Who should sleep under a mosquito net
# 6	=	Seek fever treatment
# 7	=	Seek fever treatment quickly (in 24 hours)
# 8	=	Importance of home spraying
# 9	=	Do not rub walls after spraying
# 10 =	Environmental sanitization activities
# 11	=	It is important for pregnant women to receive prenatal care
# 12	=	Pregnant women should take sp/fansidar
# 96	=	Other 
# 98	=	DK

# Add back into data  
women <- left_join(women, woexp, by = "id")
# Remove the temp stuff
rm(woexp)

# Where did you see/hear messages about malaria? 
woexp = women %>% mutate(mal_mess_from = strsplit(mal_mess_from, " ")) %>% 
  unnest(mal_mess_from) %>% # Split up using space as seperator 
  mutate(mal_mess_from = paste0("wmssg_", mal_mess_from),  # Add wmssg_ prefix so var names won't cause pain
         mal_mess_from = fct_expand(mal_mess_from, "wmssg_1", "wmssg_2", "wmssg_3", "wmssg_4", "wmssg_5", "wmssg_6", "wmssg_7", "wmssg_8", "wmssg_9", "wmssg_10", "wmssg_11", "wmssg_12", "wmssg_96", "wmssg_98", "cure_NA"), # add all factor levels (even ones that aren't in the data)
         count = 1) %>% # Create a count var that indicates that response was present in the data
  select(id, mal_mess_from, count) %>% # Select only relevant vars
  complete(id, mal_mess_from) %>% # Now add in rows for empty levels
  replace(is.na(.), 0) %>% # Replace NAs with 0
  pivot_wider(names_from = mal_mess_from, values_from = count) %>% # Pivot wider
  select(id, wmssg_1, wmssg_2, wmssg_3, wmssg_4, wmssg_5, wmssg_6, wmssg_7, wmssg_8, wmssg_9, wmssg_10, wmssg_11, wmssg_12, wmssg_96, wmssg_98) # Can drop the NA here

# 1	= Radio
# 2	=	Tv
# 3	=	Elementary multi-valent agent
# 4	=	Activist/volunteer
# 5	=	Phone/sms
# 6	=	Mosque/church
# 7	=	Event in the community
# 8	=	Advertising poster / t-shirt / flyers / brochures
# 9	=	School
# 10 =	Phone/internet/social media (as sms, facebook, whatsapp, twitter)
# 11	=	Prenatal care visit
# 12	=	Health center or hospital
# 96	=	Other 
# 98	=	DK

# Add back into data  
women <- left_join(women, woexp, by = "id")
# Remove the temp stuff
rm(woexp)

# Add labels
women$visit_w <- labelled(women$visit_w,
                          c(`Começa entrevista individual` = 1, `Ausente` = 2, `Adiada` = 3, `Recusa` = 4, `Incompleta ` = 5, `Incapacitada ` = 6, `Outro` = 96),
                          label = "Status of women's visit")

women$school_yn <- labelled(women$school_yn, 
                            c(Sim = 1, Não = 0), 
                            label = "Ever attended school")

women$school_lvl <- labelled(women$school_lvl, 
                             c(`Nenhum` = 0, `Pré-escolar` = 1, `Alfabetização` = 2, `Primário EP1` = 3, `Primário EP2` = 4, `Secundário ESG1` = 5, `Secundário ESG2` = 6, `Técnico elementar` = 7, `Técnico básico` = 8, `Técnico médio` = 9, `For. de Professores Primários` = 10, `Bacharelato` = 11, `Licenciatura` = 12, `Mestrado` = 13, `Doutoramento/PHD` = 14, `Não sabe` = 98), 
                             label = "Highest level of school attended")

women$test_reading <- labelled(women$test_reading, 
                               c(`Não pode ler` = 1, `Pode ler uma parte da frase` = 2, `Pode ler a frase inteira` = 3, `Não há cartão com a lingua da inquirida` = 4, `Cega/deficiência visual` = 5), 
                               label = "Reading test result")

women$kids <- labelled(women$kids,
                      c(`Não` = 0, `Sim` = 1),
                      label = "Ever given birth")

women$kids_hh <- labelled(women$kids_hh,
                          c(`Não` = 0, `Sim` = 1),
                          label = "Children living with you")

women$kids_out_hh <- labelled(women$kids_hh,
                              c(`Não` = 0, `Sim` = 1),
                              label = "Living children not living with you")

women$kids_dead <- labelled(women$kids_dead,
                            c(`Não` = 0, `Sim` = 1),
                            label = "Child born alive but later died")

women$preg <- labelled(women$preg, 
                       c(`Não` = 0, `Sim` = 1),
                       label = "Pregnant now")

women$doc_preg <- labelled(women$doc_preg,
                           c(`Não` = 0, `Sim` = 1),
                           label = "Any ANC")

women$doc_1 <- labelled(women$doc_1, 
                        c(`Não` = 0, `Sim` = 1),
                        label = "Doctor for ANC")
women$doc_2 <- labelled(women$doc_2,
                        c(`Não` = 0, `Sim` = 1),
                        label = "Nurse SMI for ANC")
women$doc_3 <- labelled(women$doc_3, 
                        c(`Não` = 0, `Sim` = 1),
                        label = "Midwife for ANC")
women$doc_4 <- labelled(women$doc_4, 
                        c(`Não` = 0, `Sim` = 1),
                        label = "Traditional midwife for ANC")
women$doc_5 <- labelled(women$doc_5, 
                        c(`Não` = 0, `Sim` = 1),
                        label = "Community health worker for ANC")
women$doc_6 <- labelled(women$doc_6, 
                        c(`Não` = 0, `Sim` = 1),
                        label = "Other for ANC")

women$preg_spf <- labelled(women$preg_spf,
                           c(`Não` = 0, `Sim` = 1, `DK` = 8),
                           label = "Took SP/Fansidar")
women$preg_spf_nb <- labelled(women$preg_spf_nb, 
                              label = "Number of times took SP")

women$spf_source <- labelled(women$spf_source,
                             c(`Cuidados pré-natais` = 1, `Outra consulta` = 2, `Outro local` = 6),
                             label = "Source of SP")

women$doc_net <- labelled(women$doc_net,
                          c(`Não` = 0, `Sim` = 1),
                          label = "Received net during ANC")

women$sympt_1 <- labelled(women$sympt_1, 
                          c(`Não` = 0, `Sim` = 1), 
                          label = "Fever is mal sign")

women$sympt_2 <- labelled(women$sympt_2, 
                          c(`Não` = 0, `Sim` = 1), 
                          label = "Chills is mal sign")

women$sympt_3 <- labelled(women$sympt_3, 
                          c(`Não` = 0, `Sim` = 1), 
                          label = "Headache is mal sign")

women$sympt_4 <- labelled(women$sympt_4, 
                          c(`Não` = 0, `Sim` = 1), 
                          label = "Joint pain is mal sign")

women$sympt_5 <- labelled(women$sympt_5, 
                          c(`Não` = 0, `Sim` = 1), 
                          label = "No appetite is mal sign")

women$sympt_6 <- labelled(women$sympt_6, 
                          c(`Não` = 0, `Sim` = 1), 
                          label = "Vomiting is mal sign")

women$sympt_7 <- labelled(women$sympt_7, 
                          c(`Não` = 0, `Sim` = 1), 
                          label = "Seizures is mal sign")

women$sympt_8 <- labelled(women$sympt_8, 
                          c(`Não` = 0, `Sim` = 1), 
                          label = "Cough is mal sign")

women$sympt_9 <- labelled(women$sympt_9, 
                          c(`Não` = 0, `Sim` = 1), 
                          label = "Congestion is mal sign")

women$sympt_96 <- labelled(women$sympt_96,
                           c(`Não` = 0, `Sim` = 1),
                           label = "Listed other mal sign")

women$sympt_98 <- labelled(women$sympt_98,
                           c(`Não` = 0, `Sim` = 1),
                           label = "Didn't know mal sign")

women$mal_catch <- labelled(women$mal_catch,
                            c(`Picada de mosquito` = 1, `Pulgas/piolhos/percevejos` = 2, `Ingestão de alimentos contaminados` = 3, `Beber água suja` = 4, `Lixo/sujidade nas proximidades da casa` = 5, `Feitiço` = 6, `Higiene pessoal deficiente` = 7, `Outro` = 96, `Não sabe` = 98))


women$prev_1 <- labelled(women$prev_1,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Net to prevent mal")

women$prev_2 <- labelled(women$prev_2,
                         c(`Não` = 0, `Sim` = 1),
                         label = "ITN to prevent mal")

women$prev_3 <- labelled(women$prev_3,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Spray to prevent mal")

women$prev_4 <- labelled(women$prev_4,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Serpentine to prevent mal")

women$prev_5 <- labelled(women$prev_5,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Close doors/windows to prevent mal")

women$prev_6 <- labelled(women$prev_6,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Insect repellent to prevent mal")

women$prev_7 <- labelled(women$prev_7,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Cut the grass to prevent mal")

women$prev_8 <- labelled(women$prev_8,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Eliminate standing water to prevent mal")

women$prev_9 <- labelled(women$prev_9,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Burn leaves to prevent mal")

women$prev_10 <- labelled(women$prev_10,
                          c(`Não` = 0, `Sim` = 1),
                          label = "AC/fans to prevent mal")

women$prev_96 <- labelled(women$prev_96,
                          c(`Não` = 0, `Sim` = 1),
                          label = "Other way to prevent mal")

women$prev_98 <- labelled(women$prev_98,
                          c(`Não` = 0, `Sim` = 1),
                          label = "Don't know how to prevent mal")

women$pregv_1 <- labelled(women$pregv_1,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Net to prevent mal in preg")

women$pregv_2 <- labelled(women$pregv_2,
                         c(`Não` = 0, `Sim` = 1),
                         label = "ITN to prevent mal in preg")

women$pregv_3 <- labelled(women$pregv_3,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Spray to prevent mal in preg")

women$pregv_4 <- labelled(women$pregv_4,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Serpentine to prevent mal in preg")

women$pregv_5 <- labelled(women$pregv_5,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Close doors/windows to prevent mal in preg")

women$pregv_6 <- labelled(women$pregv_6,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Insect repellent to prevent mal in preg")

women$pregv_7 <- labelled(women$pregv_7,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Cut the grass to prevent mal in preg")

women$pregv_8 <- labelled(women$pregv_8,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Eliminate standing water to prevent mal in preg")

women$pregv_9 <- labelled(women$pregv_9,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Burn leaves to prevent mal in preg")

women$pregv_10 <- labelled(women$pregv_10,
                          c(`Não` = 0, `Sim` = 1),
                          label = "Take medicine to prevent mal in preg")

women$pregv_11 <- labelled(women$pregv_11,
                           c(`Não` = 0, `Sim` = 1),
                           label = "AC/fans to prevent mal in preg")

women$pregv_96 <- labelled(women$pregv_96,
                          c(`Não` = 0, `Sim` = 1),
                          label = "Other way to prevent mal in preg")

women$pregv_98 <- labelled(women$pregv_98,
                          c(`Não` = 0, `Sim` = 1),
                          label = "Don't know how to prevent mal in preg")

women$mal_cura_yn <- labelled(women$mal_cura_yn,
                              c(`Não` = 0, `Sim` = 1),
                              label = "Is malaria curable?")

women$cure_1 <- labelled(women$cure_1,
                         c(`Não` = 0, `Sim` = 1),
                         label = "ACT/Coartem cures malaria")

women$cure_2 <- labelled(women$cure_2,
                         c(`Não` = 0, `Sim` = 1),
                         label = "SP/fansidar cures malaria")

women$cure_3 <- labelled(women$cure_3,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Chloroquine cures malaria")

women$cure_4 <- labelled(women$cure_4,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Amodiaquine cures malaria")

women$cure_5 <- labelled(women$cure_5,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Quinine pills cure malaria")

women$cure_6 <- labelled(women$cure_6,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Quinine IV cures malaria")

women$cure_7 <- labelled(women$cure_7,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Artesunate suppository cures malaria")

women$cure_8 <- labelled(women$cure_8,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Artesunate IV cures malaria")

women$cure_9 <- labelled(women$cure_9,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Another anti-malarial cures malaria")

women$cure_10 <- labelled(women$cure_10,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Antibiotic pill/syrup cures malaria")

women$cure_11 <- labelled(women$cure_11,
                          c(`Não` = 0, `Sim` = 1),
                          label = "Injection/iv antibiotics cure malaria")

women$cure_12 <- labelled(women$cure_12,
                          c(`Não` = 0, `Sim` = 1),
                          label = "Other aspirin med cure malaria")

women$cure_13 <- labelled(women$cure_13,
                          c(`Não` = 0, `Sim` = 1),
                          label = "Other paracet med cure malaria")

women$cure_14 <- labelled(women$cure_14,
                          c(`Não` = 0, `Sim` = 1),
                          label = "Other ibupro med cure malaria")

women$cure_96 <- labelled(women$cure_96,
                          c(`Não` = 0, `Sim` = 1),
                          label = "Other med cure malaria")

women$cure_98 <- labelled(women$cure_98,
                          c(`Não` = 0, `Sim` = 1),
                          label = "Don't know what meds cure malaria")

women$mess_mal_yn <- labelled(women$mess_mal_yn,
                              c(`Não` = 0, `Sim` = 1),
                              label = "Seen/heard any mal messgs in past 6mo?")

# Messages heard about malaria
women$mssg_1 <- labelled(women$mssg_1,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Malaria is dangerous")

women$mssg_2 <- labelled(women$mssg_2,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Malaria can kill")

women$mssg_3 <- labelled(women$mssg_3,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Mosquito transmit malaria")

women$mssg_4 <- labelled(women$mssg_4,
                         c(`Não` = 0, `Sim` = 1),
                         label = "It is important to sleep under a mosquito net")

women$mssg_5 <- labelled(women$mssg_5,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Who should sleep under a mosquito net")

women$mssg_6 <- labelled(women$mssg_6,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Seek fever treatment")

women$mssg_7 <- labelled(women$mssg_7,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Seek fever treatment quickly (in 24 hours)")

women$mssg_8 <- labelled(women$mssg_8,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Importance of home spraying")

women$mssg_9 <- labelled(women$mssg_9,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Do not rub walls after spraying")

women$mssg_10 <- labelled(women$mssg_10,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Environmental sanitization activities")

women$mssg_11 <- labelled(women$mssg_11,
                         c(`Não` = 0, `Sim` = 1),
                         label = "It is important for pregnant women to receive prenatal care")

women$mssg_12 <- labelled(women$mssg_12,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Pregnant women should take sp/fansidar")

women$mssg_96 <- labelled(women$mssg_96,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Other")

women$mssg_98 <- labelled(women$mssg_98,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Do not know")

# Source of malaria messages
women$wmssg_1 <- labelled(women$wmssg_1,
                          c(`Não` = 0, `Sim` = 1),
                          label = "Radio")

women$wmssg_2 <- labelled(women$wmssg_2,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Tv")

women$wmssg_3 <- labelled(women$wmssg_3,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Elementary multi-valent agent")

women$wmssg_4 <- labelled(women$wmssg_4,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Activist/volunteer")

women$wmssg_5 <- labelled(women$wmssg_5,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Phone/sms")

women$wmssg_6 <- labelled(women$wmssg_6,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Mosque/church")

women$wmssg_7 <- labelled(women$wmssg_7,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Event in the community")

women$wmssg_8 <- labelled(women$wmssg_8,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Advertising poster / t-shirt / flyers / brochures")

women$wmssg_9 <- labelled(women$wmssg_9,
                         c(`Não` = 0, `Sim` = 1),
                         label = "School")

women$wmssg_10 <- labelled(women$wmssg_10,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Phone/internet/social media (as sms, facebook, whatsapp, twitter)")

women$wmssg_11 <- labelled(women$wmssg_11,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Prenatal care visit")

women$wmssg_12 <- labelled(women$wmssg_12,
                         c(`Não` = 0, `Sim` = 1),
                         label = "Health center or hospital")

women$wmssg_96 <- labelled(women$wmssg_96,
                          c(`Não` = 0, `Sim` = 1),
                          label = "Other")

women$wmssg_98 <- labelled(women$wmssg_98,
                          c(`Não` = 0, `Sim` = 1),
                          label = "Do not know")

# Images
women$mal_images_g_mal_im_out <- labelled(women$mal_images_g_mal_im_out,
                                          c(`Não` = 0, `Sim` = 1),
                                          label = "Seen Malaria Fora logo")

women$mal_images_g_mal_im_pncm <- labelled(women$mal_images_g_mal_im_pncm,
                                          c(`Não` = 0, `Sim` = 1),
                                          label = "Seen PNCM logo")

women$mal_images_g_mal_im_logo <- labelled(women$mal_images_g_mal_im_logo,
                                           c(`Não` = 0, `Sim` = 1),
                                           label = "Seen fake logo")







