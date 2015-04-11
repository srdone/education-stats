#!/usr/bin/env Rscript

gpa <- function (rawGrades) {
  acceptableGrades <- c('A', 'A-', 'B+', 'B', 'B-', 'C+', 'C', 'C-', 'D+', 'D', 'D-', 'F')

  convertToValue <- function (grade) {
    switch(grade,
      'A' = 4.00,
      'A-' = 3.66,
      'B+' = 3.33,
      'B' = 3.00,
      'B-' = 2.66,
      'C+' = 2.33,
      'C' = 2.00,
      'C-' = 1.66,
      'D+' = 1.33,
      'D' = 1.00,
      'D-' = 0.66,
      'F' = 0.00)
  }

  grades <- rawGrades[rawGrades %in% acceptableGrades]

  totalGradeValue = sum(sapply(grades, convertToValue))
  gradeCount = length(grades)

  totalGradeValue / gradeCount
}

# the gpa function
# should calculate gpa properly
stopifnot( gpa(c('A')) == 4)
stopifnot( gpa(c('F', 'A')) == 2)
gpaTest <- (4+3.66+3.33+3.00+2.66+2.33+2.00+1.66+1.33+1.00+0.66) / 12
stopifnot( gpa(c('A', 'A-', 'B+', 'B', 'B-', 'C+', 'C', 'C-', 'D+', 'D', 'D-', 'F')) == gpaTest)

# should silently filter out grades that do not have a grade point value
stopifnot( gpa(c('W', 'A')) == 4)

# should return NA if there are no valid grades
stopifnot( is.na(gpa(c('99a')) )
