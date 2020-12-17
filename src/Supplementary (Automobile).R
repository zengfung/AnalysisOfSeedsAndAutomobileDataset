#####################################################################
# LIBRARIES
#####################################################################
library(ggplot2)
library(gridExtra)
library(dplyr)

#####################################################################
# DATA
#####################################################################
automobile <- read.csv("C:/Users/LZFun/Google Drive (lzfliew@ucdavis.edu)/2019-2020/Spring 2020/STA160/Midterm Project/Dataset/Automobile/imports-85.data", 
                       header=FALSE)
names(automobile) <- c("symboling", "normalized-losses", "make",
                      "fuel-type", "aspiration", "num-of-doors",
                      "body-style", "drive-wheels", "engine-location",
                      "wheel-base", "length", "width", "height",
                      "curb-weight", "engine-type", "num-of-cyl",
                      "engine-size", "fuel-system", "bore",
                      "stroke", "compression-ratio", "horsepower",
                      "peak-rpm", "city-mpg", "highway-mpg",
                      "price")

# change certain attributes
automobile[automobile == '?'] <- NA
automobile[,c(3:9,15:16,18)] <- lapply(automobile[,c(3:9,15:16,18)], factor)
automobile$symboling <- as.factor(automobile$symboling)
automobile$`normalized-losses` <- as.numeric(paste(automobile$`normalized-losses`))
automobile$`bore` <- as.numeric(paste(automobile$`bore`))
automobile$`stroke` <- as.numeric(paste(automobile$`stroke`))
automobile$`horsepower` <- as.numeric(paste(automobile$`horsepower`))
automobile$`peak-rpm` <- as.numeric(paste(automobile$`peak-rpm`))
automobile$`price` <- as.numeric(paste(automobile$`price`))
automobile$`num-of-cyl` <- ordered(automobile$`num-of-cyl`,
                                   levels = c("two","three","four","five",
                                              "six","eight","twelve"))
automobile$`num-of-cyl` <- mapvalues(automobile$`num-of-cyl`,
                                     from = c('two','three','four','five','six','eight','twelve'), 
                                     to = c('2','3','4','5','6','8','12'))

#####################################################################
# IMPORT COUNT
#####################################################################
make_count <- data.frame(automobile %>% count(make))
import_plot <- ggplot(make_count,aes(y = reorder(make,n),x = n, fill = make)) + 
        geom_bar(stat = "identity")+ theme(legend.position = "none") +
        xlab("Number of imports") + ylab("Car make")

#####################################################################
# HISTOGRAM OF PRICE
#####################################################################
## histogram of price by make
price_plot1 <- ggplot(data = automobile, aes(price)) + 
        geom_histogram(aes(fill = make),col = "black") + 
        facet_wrap(vars(make), ncol = 4) + theme(legend.position = "none")

## separate car make into tiers based on each car make's price range
tier <- ifelse(automobile$make %in% c('bmw','jaguar','mercedes-benz','porsche'),
               '1', ifelse(automobile$make %in% c('audi','volvo'),
                           '2','3'))
car_attr <- cbind(tier,automobile[,3:26])

price_plot2 <- ggplot(data = car_attr, aes(price)) + 
        geom_histogram(aes(fill = `body-style`),col = "black") +
        facet_grid(tier~.)+ theme(legend.position = "bottom") +
        guides(fill=guide_legend(nrow=3,byrow=TRUE))

price_hist <- ggplot(car_attr, aes(price))
#####################################################################
# ANALYSIS OF FUEL-TYPE
#####################################################################
## histogram of price
fueltype_plot1 <- price_hist + geom_histogram(aes(fill = `fuel-type`)) + 
        facet_grid(tier~.) + theme(legend.position = "bottom")

## barplot
fueltype_plot2 <- ggplot(car_attr, aes(x = `fuel-type`,fill = `tier`)) +
        geom_bar() + theme(legend.position = "bottom")
grid.arrange(fueltype_plot1,fueltype_plot2, ncol = 2)

#####################################################################
# ANALYSIS OF ASPIRATION
#####################################################################
## histogram of price
aspiration_plot1 <- price_hist + 
        geom_histogram(aes(fill = `aspiration`),col = "black") + 
        facet_grid(tier~.) + ggtitle("Aspiration") +
        theme(legend.position = "bottom",legend.title = element_blank()) +
        guides(fill=guide_legend(nrow=1,byrow=TRUE))
#####################################################################
# ANALYSIS OF NUM-OF-DOORS
#####################################################################
## histogram of price
numofdoors_plot1 <- car_attr %>%
        filter(!is.na(car_attr$`num-of-doors`)) %>%
        ggplot(aes(price)) +
        geom_histogram(aes(fill = `num-of-doors`),col = "black") + 
        facet_grid(tier~.) + ggtitle("Num Of Doors") +
        theme(legend.position = "bottom",legend.title = element_blank()) +
        guides(fill=guide_legend(nrow=1,byrow=TRUE))

#####################################################################
# ANALYSIS OF BODY-STYLE
#####################################################################
## histogram of price
bodystyle_plot1 <- price_hist + 
        geom_histogram(aes(fill = `body-style`),col = "black") + 
        facet_grid(tier~.) + ggtitle("Body Style") +
        theme(legend.position = "bottom",legend.title = element_blank(),
              legend.text = element_text(size = rel(0.7))) +
        guides(fill=guide_legend(nrow=2,byrow=TRUE))

#####################################################################
# ANALYSIS OF DRIVE-WHEELS
#####################################################################
# histogram of price
drivewheels_plot1 <- price_hist + 
        geom_histogram(aes(fill = `drive-wheels`),col = "black") + 
        facet_grid(tier~.) + ggtitle("Drive Wheels") + 
        theme(legend.position = "bottom",legend.title = element_blank()) +
        guides(fill=guide_legend(nrow=1,byrow=TRUE))

# barplot
drivewheels_plot2 <- ggplot(car_attr, aes(x = `drive-wheels`,fill = `tier`)) +
        geom_bar() + theme(legend.position = "bottom")

#####################################################################
# ANALYSIS OF ENGINE LOCATION
#####################################################################
# barplot
engineloc_plot1 <- car_attr %>%
        filter(make == 'porsche') %>%
        ggplot(aes(x = `price`, fill = `engine-location`)) +
        geom_histogram(col = "black", bins = 10) + 
        ggtitle("Engine Loc. of Porsche") +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        guides(fill=guide_legend(nrow=1,byrow=TRUE))

#####################################################################
# ANALYSIS OF WHEEL-BASE
#####################################################################
## histogram of price
wheelbase_plot1 <- ggplot(car_attr, aes(x = `wheel-base`, y = `price`)) + 
        geom_point(aes(col = `aspiration`)) + 
        geom_boxplot(aes(y=40000), alpha = I(1/5), width = 5000, color = 'blue') +
        facet_grid(tier~.)  + theme(legend.position = "none")

#####################################################################
# ANALYSIS OF LENGTH
#####################################################################
length_plot1 <- ggplot(car_attr, aes(x = `length`, y = `price`)) + 
        geom_point(aes(col = `aspiration`)) + 
        geom_boxplot(aes(y=40000), alpha = I(1/5), width = 5000, color = 'blue') +
        facet_grid(tier~.)  + theme(legend.position = "none")
#####################################################################
# ANALYSIS OF WIDTH
#####################################################################
width_plot1 <- ggplot(car_attr, aes(x = `width`, y = `price`)) + 
        geom_point(aes(col = `aspiration`)) + 
        geom_boxplot(aes(y=40000), alpha = I(1/5), width = 5000, color = 'blue') +
        facet_grid(tier~.)  + theme(legend.position = "none")
#####################################################################
# ANALYSIS OF HEIGHT
#####################################################################
height_plot1 <- ggplot(car_attr, aes(x = `height`, y = `price`)) + 
        geom_point(aes(col = `aspiration`)) + 
        geom_boxplot(aes(y=40000), alpha = I(1/5), width = 5000, color = 'blue') +
        facet_grid(tier~.)  + theme(legend.position = "none")
#####################################################################
# ANALYSIS OF CURB-WEIGHT
#####################################################################
curbweight_plot1 <- ggplot(car_attr, aes(x = `curb-weight`, y = `price`)) + 
        geom_point(aes(col = `aspiration`)) + 
        geom_boxplot(aes(y=40000), alpha = I(1/5), width = 5000, color = 'blue') +
        facet_grid(tier~.)  + theme(legend.position = "none")
#####################################################################
# ANALYSIS OF ENGINE-TYPE
#####################################################################
## histogram of price
enginetype_plot1 <- price_hist + 
        geom_histogram(aes(fill = `engine-type`),col = "black") + 
        facet_grid(tier~.) + ggtitle("Engine Type") +
        theme(legend.position = "bottom",legend.title = element_blank()) +
        guides(fill=guide_legend(nrow=2,byrow=TRUE))

#####################################################################
# ANALYSIS OF NUM-OF-CYL
#####################################################################
## histogram of price
numofcyl_plot1 <- price_hist + 
        geom_histogram(aes(fill = `num-of-cyl`),col = "black") + 
        facet_grid(tier~.) + ggtitle("Num Of Cyl") +
        theme(legend.position = "bottom",legend.title = element_blank()) +
        guides(fill=guide_legend(nrow=2,byrow=TRUE))
#####################################################################
# ANALYSIS OF ENGINE-SIZE
#####################################################################
enginesize_plot1 <- ggplot(car_attr, aes(x = `engine-size`, y = `price`)) + 
        geom_point(aes(col = `aspiration`)) + 
        geom_boxplot(aes(y=40000), alpha = I(1/5), width = 5000, color = 'blue') +
        facet_grid(tier~.)  + theme(legend.position = "none")
#####################################################################
# ANALYSIS OF FUEL-SYSTEM
#####################################################################
## histogram of price
fuelsys_plot1 <- price_hist + 
        geom_histogram(aes(fill = `fuel-system`),col = "black") + 
        facet_grid(tier~.) + ggtitle("Fuel Sys") +
        theme(legend.position = "bottom",legend.title = element_blank()) +
        guides(fill=guide_legend(nrow=3,byrow=TRUE))
#####################################################################
# ANALYSIS OF BORE
#####################################################################
bore_plot1 <- ggplot(car_attr, aes(x = `bore`, y = `price`)) + 
        geom_point(aes(col = `aspiration`)) + 
        geom_boxplot(aes(y=40000), alpha = I(1/5), width = 5000, color = 'blue') +
        facet_grid(tier~.)  + theme(legend.position = "none")
#####################################################################
# ANALYSIS OF STROKE
#####################################################################
stroke_plot1 <- ggplot(car_attr, aes(x = `stroke`, y = `price`)) + 
        geom_point(aes(col = `aspiration`)) + 
        geom_boxplot(aes(y=40000), alpha = I(1/5), width = 5000, color = 'blue') +
        facet_grid(tier~.)  + theme(legend.position = "none")
#####################################################################
# ANALYSIS OF COMPRESSION-RATIO
#####################################################################
compratio_plot1 <- ggplot(car_attr, aes(x = `compression-ratio`, y = `price`)) + 
        geom_point(aes(col = `fuel-type`)) + 
        geom_boxplot(aes(y=40000), alpha = I(1/5), width = 5000, color = 'blue') +
        facet_grid(tier~.)  + theme(legend.position = "bottom")
#####################################################################
# ANALYSIS OF HORSEPOWER
#####################################################################
horsepower_plot1 <- ggplot(car_attr, aes(x = `horsepower`, y = `price`)) + 
        geom_point(aes(col = `aspiration`)) + 
        geom_boxplot(aes(y=40000), alpha = I(1/5), width = 5000, color = 'blue') +
        facet_grid(tier~.)  + theme(legend.position = "none")
#####################################################################
# ANALYSIS OF PEAK-RPM
#####################################################################
peakrpm_plot1 <- ggplot(car_attr, aes(x = `peak-rpm`, y = `price`)) + 
        geom_point(aes(col = `aspiration`)) + 
        geom_boxplot(aes(y=40000), alpha = I(1/5), width = 5000, color = 'blue') +
        facet_grid(tier~.)  + theme(legend.position = "none")
#####################################################################
# ANALYSIS OF CITY-MPG
#####################################################################
citympg_plot1 <- ggplot(car_attr, aes(x = `city-mpg`, y = `price`)) + 
        geom_point(aes(col = `aspiration`)) + 
        geom_boxplot(aes(y=40000), alpha = I(1/5), width = 5000, color = 'blue') +
        facet_grid(tier~.)  + theme(legend.position = "none")
#####################################################################
# ANALYSIS OF HIGHWAY-MPG
#####################################################################
highwaympg_plot1 <- ggplot(car_attr, aes(x = `highway-mpg`, y = `price`)) + 
        geom_point(aes(col = `aspiration`)) + 
        geom_boxplot(aes(y=40000), alpha = I(1/5), width = 5000, color = 'blue') +
        facet_grid(tier~.)  + theme(legend.position = "none")

#####################################################################
# PRINT PLOTS
#####################################################################
import_plot

price_plot1
aspiration_plot1
numofdoors_plot1
bodystyle_plot1
engineloc_plot1
drivewheels_plot1
numofcyl_plot1
fuelsys_plot1
enginetype_plot1

wheelbase_plot1
length_plot1
width_plot1
height_plot1
curbweight_plot1
enginesize_plot1
bore_plot1
stroke_plot1
horsepower_plot1
peakrpm_plot1
citympg_plot1
highwaympg_plot1

compratio_plot1


#####################################################################
# LINEAR REGRESSION
#####################################################################
car_attr.naomit <- na.omit(car_attr)
n <- nrow(car_attr.naomit)
model1 <- lm(price ~ `make` + `fuel-type` + `aspiration` +
                     `num-of-doors` + `body-style` + `drive-wheels` +
                     `engine-location` + `wheel-base` + `length` + 
                     `width` + `height` + `curb-weight` + 
                     `engine-type` + `num-of-cyl` + `engine-size` + 
                     `fuel-system` + `bore` + `stroke` +
                     `compression-ratio` + `horsepower` + `peak-rpm` +
                     `city-mpg` + `highway-mpg`, data = car_attr.naomit)
drop1(model1, test = "F") # drop `fuel-type`
model2 <- lm(price ~ `make` + `aspiration` + `num-of-doors` + 
                     `body-style` + `drive-wheels` + `engine-location` +
                     `wheel-base` + `length` + `width` + `height` + 
                     `curb-weight` + `engine-type` + `num-of-cyl` + 
                     `engine-size` + `fuel-system` + `bore` + `stroke` +
                     `compression-ratio` + `horsepower` + `peak-rpm` +
                     `city-mpg` + `highway-mpg`, data = car_attr.naomit)
drop1(model2,test = "F") # drop `engine-location`
add1(model2,scope = ~ `make` + `fuel-type` + `aspiration` +
             `num-of-doors` + `body-style` + `drive-wheels` +
             `engine-location` + `wheel-base` + `length` + 
             `width` + `height` + `curb-weight` + 
             `engine-type` + `num-of-cyl` + `engine-size` + 
             `fuel-system` + `bore` + `stroke` +
             `compression-ratio` + `horsepower` + `peak-rpm` +
             `city-mpg` + `highway-mpg`, test = "F")# no add
model3 <- lm(price ~ `make` + `aspiration` + `num-of-doors` + 
                     `body-style` + `drive-wheels` + `wheel-base` + 
                     `length` + `width` + `height` + `curb-weight` + 
                     `engine-type` + `num-of-cyl` + `engine-size` + 
                     `fuel-system` + `bore` + `stroke` +
                     `compression-ratio` + `horsepower` + `peak-rpm` +
                     `city-mpg` + `highway-mpg`, data = car_attr.naomit)
drop1(model3,test = "F") # drop `city-mpg`
add1(model3,scope = ~ `make` + `fuel-type` + `aspiration` +
             `num-of-doors` + `body-style` + `drive-wheels` +
             `engine-location` + `wheel-base` + `length` + 
             `width` + `height` + `curb-weight` + 
             `engine-type` + `num-of-cyl` + `engine-size` + 
             `fuel-system` + `bore` + `stroke` +
             `compression-ratio` + `horsepower` + `peak-rpm` +
             `city-mpg` + `highway-mpg`, test = "F")# no add
model4 <- lm(price ~ `make` + `aspiration` + `num-of-doors` + 
                     `body-style` + `drive-wheels` + `wheel-base` + 
                     `length` + `width` + `height` + `curb-weight` + 
                     `engine-type` + `num-of-cyl` + `engine-size` + 
                     `fuel-system` + `bore` + `stroke` +
                     `compression-ratio` + `horsepower` + `peak-rpm` +
                     `highway-mpg`, data = car_attr.naomit)
drop1(model4, test = "F") # drop `drive-wheels`
add1(model4,scope = ~ `make` + `fuel-type` + `aspiration` +
             `num-of-doors` + `body-style` + `drive-wheels` +
             `engine-location` + `wheel-base` + `length` + 
             `width` + `height` + `curb-weight` + 
             `engine-type` + `num-of-cyl` + `engine-size` + 
             `fuel-system` + `bore` + `stroke` +
             `compression-ratio` + `horsepower` + `peak-rpm` +
             `city-mpg` + `highway-mpg`, test = "F")# no add
model5 <- lm(price ~ `make` + `aspiration` + `num-of-doors` + 
                     `body-style` + `wheel-base` + `length` + `width` + 
                     `height` + `curb-weight` + `engine-type` + 
                     `num-of-cyl` + `engine-size` + `fuel-system` + 
                     `bore` + `stroke` + `compression-ratio` + 
                     `horsepower` + `peak-rpm` + `highway-mpg`, 
             data = car_attr.naomit)
drop1(model5, test = "F") # drop `horsepower`
add1(model5,scope = ~ `make` + `fuel-type` + `aspiration` +
             `num-of-doors` + `body-style` + `drive-wheels` +
             `engine-location` + `wheel-base` + `length` + 
             `width` + `height` + `curb-weight` + 
             `engine-type` + `num-of-cyl` + `engine-size` + 
             `fuel-system` + `bore` + `stroke` +
             `compression-ratio` + `horsepower` + `peak-rpm` +
             `city-mpg` + `highway-mpg`, test = "F")# no add

model6 <- lm(price ~ `make` + `aspiration` + `num-of-doors` + 
                     `body-style` + `wheel-base` + `length` + `width` + 
                     `height` + `curb-weight` + `engine-type` + 
                     `num-of-cyl` + `engine-size` + `fuel-system` + 
                     `bore` + `stroke` + `compression-ratio` + 
                     `peak-rpm` + `highway-mpg`, data = car_attr.naomit)
drop1(model6,test = "F") # drop `num-of-doors`
add1(model6,scope = ~ `make` + `fuel-type` + `aspiration` +
             `num-of-doors` + `body-style` + `drive-wheels` +
             `engine-location` + `wheel-base` + `length` + 
             `width` + `height` + `curb-weight` + 
             `engine-type` + `num-of-cyl` + `engine-size` + 
             `fuel-system` + `bore` + `stroke` +
             `compression-ratio` + `horsepower` + `peak-rpm` +
             `city-mpg` + `highway-mpg`, test = "F")# no add

model7 <- lm(price ~ `make` + `aspiration` + `body-style` +
                     `wheel-base` + `length` + `width` + `height` + 
                     `curb-weight` + `engine-type` + `num-of-cyl` + 
                     `engine-size` + `fuel-system` + `bore` + `stroke` +
                     `compression-ratio` + `peak-rpm` + `highway-mpg`,
             data = car_attr.naomit)
drop1(model7, test = "F") # drop `stroke`
add1(model7,scope = ~ `make` + `fuel-type` + `aspiration` +
             `num-of-doors` + `body-style` + `drive-wheels` +
             `engine-location` + `wheel-base` + `length` + 
             `width` + `height` + `curb-weight` + 
             `engine-type` + `num-of-cyl` + `engine-size` + 
             `fuel-system` + `bore` + `stroke` +
             `compression-ratio` + `horsepower` + `peak-rpm` +
             `city-mpg` + `highway-mpg`, test = "F")# no add

model8 <- lm(price ~ `make` + `aspiration` + `body-style` +
                     `wheel-base` + `length` + `width` + `height` + 
                     `curb-weight` + `engine-type` + `num-of-cyl` + 
                     `engine-size` + `fuel-system` + `bore` +
                     `compression-ratio` + `peak-rpm` + `highway-mpg`, 
             data = car_attr.naomit)
drop1(model8, test = "F") # drop `highway-mpg`
add1(model8,scope = ~ `make` + `fuel-type` + `aspiration` +
             `num-of-doors` + `body-style` + `drive-wheels` +
             `engine-location` + `wheel-base` + `length` + 
             `width` + `height` + `curb-weight` + 
             `engine-type` + `num-of-cyl` + `engine-size` + 
             `fuel-system` + `bore` + `stroke` +
             `compression-ratio` + `horsepower` + `peak-rpm` +
             `city-mpg` + `highway-mpg`, test = "F")# no add

model9 <- lm(price ~ `make` + `aspiration` + `body-style` +
                     `wheel-base` + `length` + `width` + `height` + 
                     `curb-weight` + `engine-type` + `num-of-cyl` + 
                     `engine-size` + `fuel-system` + `bore` +
                     `compression-ratio` + `peak-rpm`, 
             data = car_attr.naomit)
drop1(model9, test = "F") # drop `fuel-system`
add1(model9,scope = ~ `make` + `fuel-type` + `aspiration` +
             `num-of-doors` + `body-style` + `drive-wheels` +
             `engine-location` + `wheel-base` + `length` + 
             `width` + `height` + `curb-weight` + 
             `engine-type` + `num-of-cyl` + `engine-size` + 
             `fuel-system` + `bore` + `stroke` +
             `compression-ratio` + `horsepower` + `peak-rpm` +
             `city-mpg` + `highway-mpg`, test = "F")# no add

model10 <- lm(price ~ `make` + `aspiration` + `body-style` +
                      `wheel-base` + `length` + `width` + `height` + 
                      `curb-weight` + `engine-type` + `num-of-cyl` + 
                      `engine-size` + `bore` + `compression-ratio` + 
                      `peak-rpm`, data = car_attr.naomit)
drop1(model10, test = "F") # drop `compression-ratio`
add1(model10,scope = ~ `make` + `fuel-type` + `aspiration` +
             `num-of-doors` + `body-style` + `drive-wheels` +
             `engine-location` + `wheel-base` + `length` + 
             `width` + `height` + `curb-weight` + 
             `engine-type` + `num-of-cyl` + `engine-size` + 
             `fuel-system` + `bore` + `stroke` +
             `compression-ratio` + `horsepower` + `peak-rpm` +
             `city-mpg` + `highway-mpg`, test = "F")# no add
# model11 best model for alpha = 0.05
model11 <- lm(price ~ `make` + `aspiration` + `body-style` +
                      `wheel-base` + `length` + `width` + `height` + 
                      `curb-weight` + `engine-type` + `num-of-cyl` + 
                      `engine-size` + `bore` + `peak-rpm`, 
              data = car_attr.naomit)
drop1(model11, test = "F") # drop`height`
add1(model11, scope = ~ `make` + `fuel-type` + `aspiration` +
             `num-of-doors` + `body-style` + `drive-wheels` +
             `engine-location` + `wheel-base` + `length` + 
             `width` + `height` + `curb-weight` + 
             `engine-type` + `num-of-cyl` + `engine-size` + 
             `fuel-system` + `bore` + `stroke` +
             `compression-ratio` + `horsepower` + `peak-rpm` +
             `city-mpg` + `highway-mpg`, test = "F") # no add
model12 <- lm(price ~ `make` + `aspiration` + `body-style` +
                      `wheel-base` + `length` + `width` + `curb-weight` + 
                      `engine-type` + `num-of-cyl` + `engine-size` + 
                      `bore` + `peak-rpm`, data = car_attr.naomit)
drop1(model12, test = "F") # drop `wheel-base`
add1(model12, scope = ~ `make` + `fuel-type` + `aspiration` +
             `num-of-doors` + `body-style` + `drive-wheels` +
             `engine-location` + `wheel-base` + `length` + 
             `width` + `height` + `curb-weight` + 
             `engine-type` + `num-of-cyl` + `engine-size` + 
             `fuel-system` + `bore` + `stroke` +
             `compression-ratio` + `horsepower` + `peak-rpm` +
             `city-mpg` + `highway-mpg`, test = "F") # no add
model13 <- lm(price ~ `make` + `aspiration` + `body-style` +
                      `length` + `width` + `curb-weight` + 
                      `engine-type` + `num-of-cyl` + `engine-size` + 
                      `bore` + `peak-rpm`, data = car_attr.naomit)
drop1(model13, test = "F") # drop `length`
add1(model13, scope = ~ `make` + `fuel-type` + `aspiration` +
             `num-of-doors` + `body-style` + `drive-wheels` +
             `engine-location` + `wheel-base` + `length` + 
             `width` + `height` + `curb-weight` + 
             `engine-type` + `num-of-cyl` + `engine-size` + 
             `fuel-system` + `bore` + `stroke` +
             `compression-ratio` + `horsepower` + `peak-rpm` +
             `city-mpg` + `highway-mpg`, test = "F") # no add
model14 <- lm(price ~ `make` + `aspiration` + `body-style` + `width` + 
                      `curb-weight` + `engine-type` + `num-of-cyl` + 
                      `engine-size` + `bore` + `peak-rpm`, 
              data = car_attr.naomit)
drop1(model14, test = "F") # drop `bore`
add1(model14, scope = ~ `make` + `fuel-type` + `aspiration` +
             `num-of-doors` + `body-style` + `drive-wheels` +
             `engine-location` + `wheel-base` + `length` + 
             `width` + `height` + `curb-weight` + 
             `engine-type` + `num-of-cyl` + `engine-size` + 
             `fuel-system` + `bore` + `stroke` +
             `compression-ratio` + `horsepower` + `peak-rpm` +
             `city-mpg` + `highway-mpg`, test = "F") # no add
model15 <- lm(price ~ `make` + `aspiration` + `body-style` + `width` + 
                      `curb-weight` + `engine-type` + `num-of-cyl` + 
                      `engine-size` + `peak-rpm`, data = car_attr.naomit)
drop1(model15, test = "F") # drop `body-style`
add1(model15, scope = ~ `make` + `fuel-type` + `aspiration` +
             `num-of-doors` + `body-style` + `drive-wheels` +
             `engine-location` + `wheel-base` + `length` + 
             `width` + `height` + `curb-weight` + 
             `engine-type` + `num-of-cyl` + `engine-size` + 
             `fuel-system` + `bore` + `stroke` +
             `compression-ratio` + `horsepower` + `peak-rpm` +
             `city-mpg` + `highway-mpg`, test = "F") # no add
model16 <- lm(price ~ `make` + `aspiration` + `width` + 
                      `curb-weight` + `engine-type` + `num-of-cyl` + 
                      `engine-size` + `peak-rpm`, data = car_attr.naomit)
drop1(model16, test = "F") # no drop
add1(model16, scope = ~ `make` + `fuel-type` + `aspiration` +
             `num-of-doors` + `body-style` + `drive-wheels` +
             `engine-location` + `wheel-base` + `length` + 
             `width` + `height` + `curb-weight` + 
             `engine-type` + `num-of-cyl` + `engine-size` + 
             `fuel-system` + `bore` + `stroke` +
             `compression-ratio` + `horsepower` + `peak-rpm` +
             `city-mpg` + `highway-mpg`, test = "F") # no add
# model16 best model for alpha = 0.01 and 0.001
bestmodel <- model16