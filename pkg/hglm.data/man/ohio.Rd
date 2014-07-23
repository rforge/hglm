\name{ohio}
\docType{data}
\alias{ohio}
\alias{ohioSchools}
\alias{ohioGrades}
\alias{ohioZipDistMat}
\alias{ohioDistrictDistMat}
\alias{ohioMedian}
\alias{ohioShape}
\title{Ohio elementary school dataset}
\description{
Data set on 1,965 Ohio elementary school buildings for 2001-2002.
}
\format{ The \sQuote{ohio} dataset contains 6 objects as follows. 
 \describe{
 \item{ohioSchools}{Original data \code{ohioschool.dat} from \url{http://www.spatial-econometrics.com/} (J. LeSage and R. Pace 2009). 
                    The data set contains information on, for instance, school building ID, Zip code of 
                    the location of the school, proportion of passing on five subjects, number of teacher, 
                    number of student, etc. The variables are:\cr
                    col 1: zip code\cr
                    col 2: lattitude (zip centroid)\cr
                    col 3: longitude (zip centroid)\cr
                    col 4: buidling irn\cr
                    col 5: district irn\cr
                    col 6: # of teachers (FTE 2001-02)\cr
                    col 7: teacher attendance rate\cr
                    col 8: avg years of teaching experience\cr
                    col 9: avg teacher salary\cr
                    col 10: Per Pupil Spending on Instruction\cr
                    col 11: Per Pupil Spending on Building Operations\cr
                    col 12: Per Pupil Spending on Administration\cr
                    col 13: Per Pupil Spending on Pupil Support\cr
                    col 14: Per Pupil Spending on Staff Support\cr
                    col 15: Total Expenditures Per Pupil\cr
                    col 16: Per Pupil Spending on Instruction \% of Total Spending Per Pupil\cr
                    col 17: Per Pupil Spending on Building Operations \% of Total Spending Per Pupil\cr
                    col 18: Per Pupil Spending on Administration \% of Total Spending Per Pupil\cr
                    col 19: Per Pupil Spending on Pupil Support \% of Total Spending Per Pupil\cr
                    col 20: Per Pupil Spending on Staff Support \% of Total Spending Per Pupil\cr
                    col 21: irn number\cr
                    col 22: avg of all 4th grade proficiency scores\cr
                    col 23: median of 4th grade prof scores\cr
                    col 24: building enrollment\cr
                    col 25: short-term students < 6 months\cr
                    col 26: 4th Grade (or 9th grade) Citizenship \% Passed 2001-2002\cr
                    col 27: 4th Grade (or 9th grade)  math \% Passed 2001-2002\cr
                    col 28: 4th Grade (or 9th grade)  reading \% Passed 2001-2002\cr
                    col 29: 4th Grade (or 9th grade)  writing \% Passed 2001-2002\cr
                    col 30: 4th Grade (or 9th grade)  science \% Passed 2001-2002\cr
                    col 31: pincome per capita income in the zip code area\cr
                    col 32: nonwhite percent of population that is non-white\cr
                    col 33: poverty percent of population in poverty\cr
                    col 34: samehouse \% percent of population living in same house 5 years ago\cr
                    col 35: public \% of population attending public schools\cr
                    col 36: highschool graduates, educ attainment for 25 years plus\cr
                    col 37: associate degrees, educ attainment for 25 years plus\cr
                    col 38: college, educ attainment for 25 years plus\cr
                    col 39: graduate, educ attainment for 25 years plus\cr
                    col 40: professional, educ attainment for 25 years plus}
 \item{ohioGrades}{The derived dataset for analyzing the percentage passed based on Zip codes. The variables are:\cr
                   y: the percentage passed (4th or 9th grade) in each school\cr
                   TchExp: average Teacher's experience\cr
                   Subjects: for five study subjects of Citizenship, Maths, Reading, Writing and Science\cr
                   Stu.Tch: student by teacher ratio\cr
                   School: school index\cr
                   Zip: Zip code}
 \item{ohioMedian}{The derived dataset for analyzing the median of 4th grade scores based on school districts. The variables are:\cr
                   MedianScore: the median of 4th grade prof scores\cr
                   district: school districts}
 \item{ohioShape}{A \code{SpatialPolygonsDataFrame} object (see package \bold{sp}) containing the map information of ohio school districts.}
 \item{ohioZipDistMat}{The spatial distance matrix based on Zip codes. The codes generated this matrix are:\cr
                    \code{Zsp <- model.matrix(~ factor(Zip) - 1, data = ohioGrades)}\cr
                    \code{uzipC <- matrix(0, nrow = ncol(Zsp), ncol = 2)}\cr
                    \code{Zip <- as.numeric(substr(colnames(Zsp), start = 12, stop = 16))}\cr
                    \code{for (i in 1: ncol(Zsp)) \{}\cr
                    \code{    Cord <- as.matrix(ohioSchools[(ohioSchools$V1 == Zip[i]), 2:3])}\cr
                    \code{    uzipC[i,] <- Cord[1,]}\cr
                    \code{\}}\cr
                    \code{Dst <- as.matrix(dist(uzipC))}\cr
                    \code{for(i in 1:nrow(Dst)) \{}\cr
                    \code{    x <- Dst[i,]}\cr
                    \code{    x <- ifelse(x == 0, 0, 1/x)}\cr
                    \code{    Dst[i,] <- ifelse(x > 4, 4, x)}\cr
                    \code{\}}\cr
                    \code{ohioZipDistMat <- Dst/4}}
 \item{ohioDistrictDistMat}{The spatial distance matrix based on school districts. The codes generated this matrix are:\cr
                    \code{ccNb <- poly2nb(ccShape)}\cr
                    \code{W <- matrix(0, 616, 616)}\cr
                    \code{for (i in 1:nrow(W)) \{}\cr
                    \code{    tmp <- as.numeric(ccNb[[i]])}\cr
                    \code{    for (k in tmp) W[i,k] <- 1}\cr
                    \code{\}}\cr
                    \code{W[353,] <- W[,353] <- 0}\cr
                    \code{districtShape <- as.numeric(substr(as.character(ohioShape@data$UNSDIDFP), 3, 7))}\cr
                    \code{dimnames(W) <- list(districtShape, districtShape)}\cr
                    \code{districtSchool <- floor(ohioSchools[,5]/10)}\cr
                    \code{districtSchool <- factor(districtSchool[districtSchool \%in\% districtShape])}\cr
                    \code{levelsShape <- levels(factor(districtShape))}\cr
                    \code{levelsSchool <- levels(districtSchool)}\cr
                    \code{levels(districtSchool) <- c(levelsSchool, levelsShape[!(levelsShape \%in\% levelsSchool)])}\cr
                    \code{ohioDistrictDistMat <- W[levels(districtSchool),levels(districtSchool)]}}
}
}
\source{J. LeSage and R. Pace (2009). \emph{Introduction to Spatial Econometrics}. Chapman \& Hall/CRC, Boca Raton.}
\references{J. LeSage and R. Pace (2009). \emph{Introduction to Spatial Econometrics}. Chapman \& Hall/CRC, Boca Raton.\cr\cr
M. Alam, L. Ronnegard, X. Shen (2014). \bold{Fitting spatial models in hglm}. \emph{Submitted}.}
\keyword{datasets}