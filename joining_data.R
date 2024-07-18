# Load packages ----

library(dplyr)
library(tidyr)


# Create data ----

students <- tibble(
  name = c("Tshewang", "Kinley", "Romina", "Laura", "Mary", "Leonard", "Jody", "John"),
  department = c("Marketing", "Physics", "Biology", "Marketing", "Economics", "Mechanical engineering", "Economics", "Visitor"),
  n_awards = c(2, 3, 1, 4, 2, 3, 3, 0)
)

colleges <- tibble(
  college = c("Business", "Science", "Engineering"),
  department = c("Marketing, Management, Economics", "Physics, Biology, Environmental sciences", "Electrical engineering, Mechanical engineering, Civil engineering"),
  year_created = c(1920, 1915, 1898)
) %>%
  separate_rows(department, sep = ", ")

# left_join/ right_join: ------

# these two lines of code will give us the same result in different column order
left_join(students, colleges, by = "department") # students columns first, then colleges column
right_join(colleges, students, by = "department")# colleges column first, then students column

# these two lines of code will give us the same result in different column order
left_join(colleges, students, by = "department")# colleges column first, then students column
right_join(students, colleges, by = "department")# students columns first, then colleges column

# inner_join "discard the values that have no matching data" -----
inner_join(students, colleges, by = "department")

# which college has the most awards
inner_join(students, colleges, by = "department") %>% 
  group_by(college) %>% 
  summarize(tota_award = sum(n_awards))

# semi_join "the same as inner join but it masks the data of the second table"-----
inner_join(colleges, students, by = "department")
semi_join(colleges, students, by = "department") # here you're not interested by the names of students

# full_join "everything will be joined even though they have a corresponding NA's" ---------
full_join(colleges, students, by = "department")
full_join(students, colleges, by = "department") 

# anti_join -----
anti_join(colleges, students, by = "department") # there's no student studies in the department of 
# management, Environmental sciences, Electrical engineering, Civil engineering
anti_join(students, colleges, by = "department") # it removes any data shared between students and colleges

