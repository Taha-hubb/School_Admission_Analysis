# Install Libraries -------------------------------------------------------
library(readr) 
library(here)  
library(ggplot2) 
library(dplyr) 
library(ggrepel)
install.packages("rmarkdown")
library(rmarkdown)
install.packages('tinytex')
tinytex::install_tinytex()  # install TinyTeX
installr::install.pandoc()

# Importing and cleaning Data ----------------------------------------------------------

# Import the data sent by Ena Rabat !<>
liste_rabat <- read.csv(file = "data/pre_examen/15-07-2023/raw/Rabat.csv", head = TRUE, sep=";")
liste_rabat <- as_tibble(liste_rabat)

# Change the name of liste_rabat columns
liste_rabat <- liste_rabat %>% select(field_cne_value,field_nom_value,field_tel_value,field_gsm_value,field_cin_value) 
colnames(liste_rabat) <- c("cne", "nom", "tel" ,"gsm" ,"cin")

# change the letter case of the data
liste_rabat <- liste_rabat %>% 
  mutate(
    cne = tolower(cne),
    cin = tolower(cin)
    #nom = tolower(nom)
  )

# Import the data recieved from The ministry of education
liste_massar <- read.csv(file = "data/pre_examen/15-07-2023/raw/Massar.csv", head = TRUE, sep=";")
liste_massar <- as_tibble(liste_massar)

# Change the name of liste_massar columns
colnames(liste_massar) <- c("cne", "cin","nom","prenom","date_naissance","cd_serie","filier_bac","annee_bac","moy_gen","moy_nat","moy_reg","moy_calcule","classement")

# Change the data type of the columns
liste_massar_num <- liste_massar %>%
  mutate(
    moy_gen = suppressWarnings(as.numeric(moy_gen)) , #suppressWarnings ignores the warning message “NAs introduced by coercion”
    moy_nat = suppressWarnings(as.numeric(moy_nat)),
    moy_reg = suppressWarnings(as.numeric(moy_reg)) ,
    moy_calcule = suppressWarnings(as.numeric(moy_calcule)) # note that as.numeric has rounded moy_calcule to four decimal point
  )

# change the letter case of the data
liste_massar_case <- liste_massar_num %>% 
  mutate(
    cne = tolower(cne),
    cin = tolower(cin),
    nom = tolower(nom),
    prenom = tolower(prenom),
    filier_bac = tolower(filier_bac)
  )

# Now our data has been cleaned and it's ready to be manipulated



# 1. la liste recu de l'enseignement sup (2037) ------------------------------------------------------


# On a recu combien des étudiants de L'Enseignement Sup ?
liste_massar_case %>% 
  count() # 2037

# How many students does Rabat sent us ?
liste_rabat %>% 
  count() # 1989

# Are all the students in Rabat existed in Massar ?
liste_rabat %>% 
  anti_join(liste_massar_case, by = "cne") # there are 7 students existed in Rabat and Massar didn't give them to us which means they have been "preinscit" in Rabat but there cne doesn't exist in Massar, they aren' a valid condidate

# Are all students in Massar existed in Rabat
reussites %>% 
  anti_join(liste_rabat, by = "cne") 

# Rabat give us 1989 students but massar give us 2037 , where does 2037 - 1989 = 48 students comes from ? puisque there is no student is added in Massar outside from Rabat list, I think the added students may be duplicated Bac
# 
all_doubled_bac <- liste_massar_case %>% 
  group_by(cne) %>% 
  summarize(numb_bac = n()) %>% 
  filter(numb_bac > 1) %>% 
  arrange(-numb_bac) # there is one students who have tripled Bac, and 46 have doubled Bac, which means there are (3-1)* 1 + (2-1) * 46 = 48 bac must be deleted 

all_doubled_bac %>% 
  left_join(liste_massar_case, by = "cne", multiple = "all")

Nodouble <- liste_massar_case %>% 
  group_by(cne) %>% 
  summarize(moy_calcule = max(moy_calcule, na.rm = TRUE)) # 1989
# How many students from Agadir are there?

liste_rabat %>%
  mutate(region_letter = substr(cne,1,1),
         region = case_when(
           region_letter == "d" ~ "Agadir",
           TRUE ~ "Autres Ville")
  ) %>% 
  group_by(region) %>%
  summarize(numb_students = n()) %>%
  arrange(-numb_students) %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(region, numb_students), y = numb_students)) +
  labs(x = "La région", y = "Le nombre des étudiants preinscrit") +
  coord_flip()

# qu'elle est la filier la plus preinscrite
# library(forcats)
# install.packages("forcats")
# liste_rabat %>% 
#   ggplot(mapping = aes(x = fct_rev(fct_infreq(filier_bac)))) +
#   geom_bar() +
#   labs(x = "Filiére Baccalauriat", y = "Le nombre des étudiants preinscrit") +
#   coord_flip()

# Filtration
# 2. Rodoublants

# 2. ROUDOUBLANTS (31) ----------------------------------------------------------

# on a 2037 étudiants

# qu'il est le nombre des étudiants qui ont réussi
reussites <- liste_massar_case %>% 
  filter(is.na(moy_gen) == FALSE) # reussites: 2006

redoublants <- liste_massar_case %>% 
  filter(is.na(moy_gen)) # redoublants: 31

write_csv(redoublants, path = "data/pre_examen/15-07-2023/prepped/2.redoublants.csv")

# il reste 2037 - 31 = 2006

# 3. Bac double (31)   -----------------------------------------------------------

# on a 2006 étudiants

doubled_bac <- reussites %>% 
  group_by(cne) %>% 
  summarize(numb_bac = n()) %>% 
  filter(numb_bac > 1) %>% 
  arrange(-numb_bac) # there are  31 have doubled Bac, which means there are 31 bac must be deleted 


doubled_bac <- doubled_bac %>% 
  left_join(reussites, by = "cne", multiple = "all")

liste_massar_Nodouble <- reussites %>% 
  group_by(cne) %>% 
  summarize(moy_calcule = max(moy_calcule, na.rm = TRUE)) # 1975 "2006 - 31"

# save the 48 rows deleted
duplicated_bac <- reussites %>% 
  anti_join(liste_massar_Nodouble, by = c("cne","moy_calcule")) %>% 
  arrange(nom)  # duplicated_bac 48
write_csv(duplicated_bac, path = here("data/pre_examen/15-07-2023/prepped/3.duplicated_bac.csv"))

# add to liste_massar_Nodouble the other columns
liste_massar_Nodouble <- liste_massar_Nodouble %>% 
  left_join(liste_massar_case, by = c("moy_calcule","cne"))

# Make sure that the duplicated bac "qui contient le moins des moyen calcule" are not in the liste_massar_Nodouble
duplicated_bac %>% 
  anti_join(liste_massar_Nodouble, by = "cne")

# il reste 2006 - 31 = 1975

# 4. Reg inférieure à 12 (73)  ----------------------------------------------------------------

# on a  1975 étudiants 

# Régional > 12 
reg_sup_12 <- liste_massar_Nodouble %>% 
  filter(moy_reg >= 12) # 1902

reg_inf_12 <- liste_massar_Nodouble %>% 
  filter(moy_reg < 12) # 73

write_csv(reg_inf_12, path = here("data/pre_examen/15-07-2023/prepped/4.reg_inf_12.csv"))

# il reste 1975 - 73 = 1902

# 5. Nat < 12 (332)  ----------------------------------------------------------------

# on a 1902 étudiants 

# Moyenne National > 12 "we have 1902 students"
MoyNat_sup_12 <- reg_sup_12 %>% 
  filter(moy_nat >= 12) # 1570

MoyNat_inf_12 <- reg_sup_12 %>% 
  filter(moy_nat < 12) # 332

write_csv(MoyNat_inf_12, path = here("data/pre_examen/15-07-2023/prepped/5.MoyNat_inf_12.csv"))

# il reste 1902 - 332 = 1570

# 6. age < 12 (5)  ----------------------------------------------------------------

# on a 1570 étudiants

age_inf22 <- MoyNat_sup_12 %>% 
  mutate(age = as.numeric(difftime("2023/07/23",date_naissance, units = "weeks"))/52.25) %>% 
  filter(age <= 22) %>% # 1565
  arrange(-age) # to make sure that all students are born after 2001/07/25

age_sup22 <- MoyNat_sup_12 %>% 
  mutate(age = as.numeric(difftime("2023/07/23",date_naissance, units = "weeks"))/52.25) %>% 
  filter(age > 22) # 5
write_csv(age_inf22, path = here("data/pre_examen/15-07-2023/prepped/6.age_inf22.csv"))

# il reste 1570 - 5 = 1565

# 7. filier de baccalaureate (0) -------------------------------------------------

# on a 1565 étudiants

age_inf22 %>% 
  distinct(filier_bac) # to know which filiere we have  "14 filiere" all are valid except one "Baccaluréat Pro -Maintenance de Véhicules Auto(Voitures)"

 bac_approprie <- age_inf22 %>% 
 filter(filier_bac != "baccaluréat pro -maintenance de véhicules auto(voitures)") # 1565
 
 bac_NonApproprie <- age_inf22 %>% 
  filter(filier_bac == "baccaluréat pro -maintenance de véhicules auto(voitures)") # 0

 write_csv(bac_NonApproprie, path = here("data/pre_examen/15-07-2023/prepped/7.bac_NonApproprie.csv"))

# il reste 1565 - 0 = 1565
 
# 8. Candidats sélectionné (1002) ---------------------------------------------------


# on a 1565 étudiants plus une bac étranger --> 1566 
bac_approprie <- bac_approprie %>% add_row(cne = "BW50592", cin = "BW50592", nom = "Douieb", prenom = "Salma", date_naissance = "2005-10-17", filier_bac = "SCIENCES ECO ET SOCIA & MATH", moy_gen = 14.76, moy_nat = 14.76, moy_reg = 14.76, moy_calcule = 14.76)
Condidats_sélectioné <- bac_approprie %>% 
  slice_max(round(moy_calcule, digits = 5) ,n = 1002) # on a trouvé 1001, les 3 deriniers condidats "999:1001" ont la méme note 14.2275

write_csv(Condidats_sélectioné, path = here("data/pre_examen/15-07-2023/prepped/8.Condidats_sélectione.csv"))

# il reste 1566 - 1002 = 564

# 9. Candidats Non selectioné (564) ------------------------------------------------

# on finalise par 564 étudiants plus un bac étranger --> 565

Condidats_Non_sélectioné <- bac_approprie %>% 
  anti_join(Condidats_sélectioné, by = "cne") %>% 
  arrange(-moy_calcule) # 564

# ajouter le candidat bac étranger 
Condidats_Non_sélectioné <- Condidats_Non_sélectioné %>% add_row(cne = "E1", cin = "JA140199", nom = "OUINAFEN", prenom = "ELHOUCINE", date_naissance = "09/07/1989", filier_bac = "BAC", moy_gen = 12, moy_nat = 12, moy_reg = 12, moy_calcule = 12)

write_csv(Condidats_Non_sélectioné, path = here("data/pre_examen/15-07-2023/prepped/9.Condidats_Nonsélectioné.csv")) #565


# 10.Répartition par salle ---------------------------------------------------

# on a 16 salles 15 salle contient 63 candidats et 57 candidats dans la salle 16

#install.packages("stringr")
library(stringr)
Condidats_sélectioné <- Condidats_sélectioné %>%
  mutate(nom = str_trim(nom),nom = tolower(nom))

n <- 63
numbered <- Condidats_sélectioné %>% 
  arrange(nom) %>%
  mutate(order_alpha = 1:n(),# or use order_alpha = row_number()
         salle = case_when(
           order_alpha <= n ~ "Salle 1",
           order_alpha > n & order_alpha <= 2*n ~ "Salle 2",
           order_alpha > 2*n & order_alpha <= 3*n ~ "Salle 3",
           order_alpha > 3*n & order_alpha <= 4*n ~ "Salle 4",
           order_alpha > 4*n & order_alpha <= 5*n ~ "Salle 5",
           order_alpha > 5*n & order_alpha <= 6*n ~ "Salle 6",
           order_alpha > 6*n & order_alpha <= 7*n ~ "Salle 7",
           order_alpha > 7*n & order_alpha <= 8*n ~ "Salle 8",
           order_alpha > 8*n & order_alpha <= 9*n ~ "Salle 9",
           order_alpha > 9*n & order_alpha <= 10*n ~ "Salle 10",
           order_alpha > 10*n & order_alpha <= 11*n ~ "Salle 11",
           order_alpha > 11*n & order_alpha <= 12*n ~ "Salle 12",
           order_alpha > 12*n & order_alpha <= 13*n ~ "Salle 13",
           order_alpha > 13*n & order_alpha <= 14*n ~ "Salle 14",
           order_alpha > 14*n & order_alpha <= 15*n ~ "Salle 15",
           order_alpha > 15*n & order_alpha <= 16*n ~ "Salle 16",
         )) 
write_csv(numbered, path = here("data/pre_examen/15-07-2023/prepped/10.Liste_concours_salle.csv"))


# CIN NULL ----------------------------------------------------------------

# Working with the result data --------------------------------------------

# Notes_QCM <- read.csv(file = "data/post_examen/raw/Notes_QCM.xls", head = TRUE)
# Notes_QCM <- as_tibble(Notes_QCM)

# reading QCM results
library(readxl)   
Notes_QCM_sheets = excel_sheets("data/post_examen/raw/Notes_QCM.xls") # show you the number of sheets you have

Notes_QCM <-  lapply(setNames(Notes_QCM_sheets, Notes_QCM_sheets), 
                     function(x) read_excel("data/post_examen/raw/Notes_QCM.xls", sheet=x)) # I don't really understand this function but it works

Notes_QCM <-  bind_rows(Notes_QCM) # 1002

# Change the name of Notes_QCM columns
colnames(Notes_QCM) <- c("numero", "nom","prenom","point_150","point_20")

# change the letter case of the data
Notes_QCM <- Notes_QCM %>% 
  mutate(
    nom = tolower(nom),
    prenom = tolower(prenom)
  )

# Change the data type of the columns
Notes_QCM_num <- Notes_QCM %>%
  mutate(
    numero = suppressWarnings(as.numeric(numero)) , #suppressWarnings ignores the warning message “NAs introduced by coercion”
    point_150 = suppressWarnings(as.numeric(point_150)) # note that as.numeric has rounded moy_calcule to four decimal point
  )


# How many students have passed the exam 802?
qcm_presents <- Notes_QCM_num %>% 
  filter(point_150 != "Absent") #%>% # delete les absents
  #mutate(point_20_R = point_150 / 150 * 20) # add a column for points over 20

# can you show us the students who do not pass the exam ?
absents <- numbered %>%
  anti_join(qcm_presents,by = c("order_alpha" = "numero"))# 201

absents %>% 
  ggplot(mapping = aes(x = order_alpha , y = moy_calcule, colour = salle)) +
  geom_point()

write_csv(absents, path = here("data/post_examen/prepped/1.absents.csv"))

# reading DEG results
library(readxl)   
Notes_DEG_sheets = excel_sheets("data/post_examen/raw/Notes_DEG.xlsx") # show you the number of sheets you have

# applying sheet names to dataframe names
Notes_DEG <-  lapply(setNames(Notes_DEG_sheets, Notes_DEG_sheets), 
                     function(x) read_excel("data/post_examen/raw/Notes_DEG.xlsx", sheet=x)) # I don't really understand this function but it works

Notes_DEG <-  bind_rows(Notes_DEG) # 1003

# reading Soumia results
library(readxl)   
notes_sheets = excel_sheets("data/post_examen/raw/note.xlsx") # show you the number of sheets you have

# applying sheet names to dataframe names
note <-  lapply(setNames(notes_sheets, notes_sheets), 
                     function(x) read_excel("data/post_examen/raw/note.xlsx", sheet=x)) # I don't really understand this function but it works

note <-  bind_rows(note) # 1003

note <- note %>%
  mutate(
    Numéro = suppressWarnings(as.numeric(Numéro)) #suppressWarnings ignores the warning message “NAs introduced by coercion”
    # note that as.numeric has rounded moy_calcule to four decimal point
  )

note_soumia <- note %>% 
  left_join(Notes_DEG, by = c("Numéro" = "num_examen"))

write_csv(note_soumia, path = here("data/post_examen/prepped/Liste_dattente1_00.csv"))

notes2_sheets = excel_sheets("data/post_examen/raw/note2.xlsx") # show you the number of sheets you have

# applying sheet names to dataframe names
note2 <-  lapply(setNames(notes2_sheets, notes2_sheets), 
                function(x) read_excel("data/post_examen/raw/note2.xlsx", sheet=x)) # I don't really understand this function but it works

note2 <-  bind_rows(note2) # 1003

note2 <- note2 %>%
  mutate(
    Numéro = suppressWarnings(as.numeric(Numéro)) #suppressWarnings ignores the warning message “NAs introduced by coercion”
    # note that as.numeric has rounded moy_calcule to four decimal point
  )

note2_soumia <- note2 %>% 
  left_join(Notes_DEG, by = c("Numéro" = "num_examen"))

write_csv(note_soumia, path = here("data/post_examen/prepped/Liste_dattente2_00.csv"))

# Change the name of Notes_DEG columns
colnames(Notes_DEG) <- c("num_examen", "cne","cin","nom","prenom","note_deg","salle")

# change the letter case of the data
Notes_DEG <- Notes_DEG %>% 
  mutate(
    cne = tolower(cne),
    cin = tolower(cin),
    nom = tolower(nom),
    prenom = tolower(prenom)
  )

# how many students are present in DEG
DEG_present <- Notes_DEG %>% 
  filter(note_deg != "ABSENT") # 802 are present in DEG

DEG_present <- DEG_present %>%
  mutate(
    note_deg = suppressWarnings(as.numeric(note_deg)) # note that as.numeric has rounded moy_calcule to four decimal point
  )

# DEG_present <- mutate(DEG_present,
#                       note_deg = as.numeric(unlist(DEG_present$note_deg))
# )

DEG_present %>% 
  anti_join(qcm_presents, by = c("num_examen" = "numero")) # all students in QCM are also in DEG # 802

full_liste <- DEG_present %>% 
  select(num_examen, note_deg, cne) %>% 
  left_join(qcm_presents, by = c("num_examen" = "numero")) %>% 
  mutate(
    note_qcm_rounded = format(round(point_20, 3), nsmall = 3),
    note_qcm_rounded = as.numeric(note_qcm_rounded),
    QCM_60  = note_qcm_rounded * 0.6,
    note_deg_rounded = format(round(note_deg, 3), nsmall = 3),
    note_deg_rounded = as.numeric(note_deg_rounded),
    DEG_40 = note_deg_rounded * 0.4,
    notes_final =  DEG_40 + QCM_60) %>% 
  arrange(-notes_final) %>% 
  select(num_examen,cne,nom, prenom, note_deg, note_deg_rounded, DEG_40, point_20, note_qcm_rounded, QCM_60, notes_final)
# select(-ends_with(".x"),-ends_with(".y")) # get rid of the duplicated columns
full_liste
write_csv(full_liste, path = here("data/post_examen/prepped/2.full_liste.csv"))

# La liste des admis
Liste_principale <- full_liste %>% 
  slice_max(round(notes_final, digits = 5) ,n = 31)

write_csv(Liste_principale, path = here("data/post_examen/prepped/3.Liste_principale.csv"))

# La liste d'attente
Liste_dattente1 <- full_liste %>%   
  anti_join(Liste_principale, by = "num_examen") %>% 
  slice_max(notes_final,n =55) 
write_csv(Liste_dattente1, path = here("data/post_examen/prepped/4.Liste_dattente1.csv"))

Liste_dattente2 <- full_liste %>% 
  anti_join(Liste_principale, by = "num_examen") %>% 
  anti_join(Liste_dattente1, by = "num_examen") %>% 
  slice_max(notes_final ,n = 150)

write_csv(Liste_dattente2, path = here("data/post_examen/prepped/5.Liste_dattente2.csv"))

# is there a correlation between QCM and DEG
plot <- Liste_principale %>% 
  ggplot(mapping = aes(x = note_deg, y = point_20)) +
  geom_point(colour = "blue",size = 3) 
#geom_text(aes(label = NOM)) +
#geom_smooth(se = FALSE) 

plot +
  geom_label_repel(aes(label = nom),
                   box.padding   = 0.8, 
                   point.padding = 0.7,
                   max.overlaps = 30,
                   segment.color = 'grey50') +
  theme_classic()


# More informations about the 30 selected students
Selected <- Liste_principale %>% 
  select(cne, note_deg, point_20, notes_final) %>% 
  left_join(numbered, by = "cne")
# Selected <- distinct(Selected, cin, .keep_all = TRUE)

# what is the baccalaureate the most succeed 
library(forcats)
Selected %>% 
  ggplot(mapping = aes(x = fct_rev(fct_infreq(filier_bac)))) +
  geom_bar()

# Le percentage de bacallauréat qui on réussi "par exemple, il ya 4 personne ont un baccaluréat arts appliqués ont éte passé l'examen, tous les quatre condidats ont été réussi 100%
full_liste %>%
  select(cne) %>% 
  left_join(numbered, by = "cne") %>% 
  filter(filier_bac == "baccaluréat arts appliqués") %>% 
  count() #4

Selected %>% 
  filter(filier_bac == "baccaluréat arts appliqués") %>% 
  count() #4

# is there a correlation between QCM and DEG
plot_colored <- Selected %>% 
  ggplot(mapping = aes(x = note_deg, y = point_20, color = filier_bac )) +
  geom_point(colour = "blue",size = 3) 
#geom_text(aes(label = NOM)) +
#geom_smooth(se = FALSE) 

plot_colored +
  geom_label_repel(aes(label = nom),
                   box.padding   = 0.8, 
                   point.padding = 0.7,
                   max.overlaps = 30,
                   segment.color = 'grey50') +
  theme_classic()



# Relationship between moy_gen et point_20
Selected %>%
  filter(moy_gen > 14) %>% 
  ggplot(mapping = aes(x = moy_gen, y = point_20)) +
  geom_point() +
  coord_cartesian(xlim = c(14,20)) +
  geom_smooth(se = FALSE)

# Relationship between moy_gen et note_deg
Selected %>% 
  filter(moy_gen > 14) %>%
  ggplot(mapping = aes(x = moy_gen, y = note_deg)) +
  geom_point() +
  coord_cartesian(xlim = c(14,20)) +
  geom_smooth(se = FALSE)

# Relationship between moy_gen et notes_final

Selected %>% 
  filter(moy_gen > 14) %>%
  ggplot(mapping = aes(x = moy_gen, y = notes_final)) +
  geom_point() +
  coord_cartesian(xlim = c(14,20)) +
  geom_smooth(se = FALSE)

# from where those students who succeded come from
Selected %>% 
  select(prenom, cne) %>% 
  mutate(region = substr(cne,1,1)) %>% 
  ggplot(mapping = aes(x = region)) +
  geom_bar()

# from where those students who in the first waiting list come from
Liste_dattente1 %>% 
  select(nom, cne, notes_final) %>% 
  slice_max(notes_final, n = 20) %>% 
  mutate(region = substr(cne,1,1)) %>% 
  ggplot(mapping = aes(x = reorder(nom,-notes_final), y = region)) +
  geom_point()


# finish the questions
# compare with the previous year
# compare their admission test result with the result achieved in the year
# Create timetable
# divide first year students by their level
# I need to make a paper contains the steps of selection that we should follow in the process of selection for example:
# combien des étudiants ont été inscrit?
# .................... (Answer of the professor)




