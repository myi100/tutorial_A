# Tutorial_A

Correct sleep/create summer and apocalypse variables

### Installation

*If you already have R, RStudio, and Shiny installed on your local machine, skip to Step 3.*

1. Intall R and RStudio. You can find step-by-step nstructions for both installing R and Rstudio for Mac and Windows [here.](https://courses.edx.org/courses/UTAustinX/UT.7.01x/3T2014/56c5437b88fa43cf828bff5371c6a924/)

*New to R and Rstudio? Check out this pre-refresher [video](https://youtu.be/lVKMsaWju8w) and [cheatsheet.](https://www.rstudio.com/wp-content/uploads/2016/01/rstudio-IDE-cheatsheet.pdf)*

2. Open Rstudio and install a stable version of shiny from CRAN, by executing the following command in the RStudio console:

install.packages("shiny")

3. You must also install the following dependencies:

intall.packages("lubridate")

3. Download and run psychophys, execute the following command in the RStudio console: 

shiny::runGitHub('psychophys', 'myi100')
