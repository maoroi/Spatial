# The code here is based on the "Modeling and mapping species distributions" tutorial by Richard Chandler (2019): 
# https://cran.r-project.org/web/packages/unmarked/vignettes/spp-dist.pdf

library(unmarked)
library(raster)

data(crossbill)

umf <- unmarkedFrameOccu(y=as.matrix(crossbill[,c("det991", "det992", "det993")]), siteCovs=crossbill[,c("ele", "forest")],
                         obsCovs=list(date=crossbill[,c("date991", "date992", "date993")]))
sc <- scale(siteCovs(umf))
siteCovs(umf) <- sc
head(umf)

# fitting the occurence model
(fm.occu <- occu(~date ~ele + I(ele^2) + forest, umf))

data(Switzerland)
print(levelplot(elevation ~ x + y, Switzerland, aspect="iso", xlab="Easting (m)", ylab="Northing (m)", 
                col.regions=terrain.colors(100)))


elevation <- rasterFromXYZ(Switzerland[,c("x","y","elevation")],
                           crs="+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs")
forest <- rasterFromXYZ(Switzerland[,c("x","y","forest")],
                        crs="+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs")
attr(sc, "scaled:center")
# ele forest
# 1189.32584 34.74532
attr(sc, "scaled:scale")
# ele forest
# 640.71471 27.67431
ele.s <- (elevation-1189)/640
forest.s <- (forest-34.7)/27.7
ef <- stack(ele.s, forest.s)
names(ef) <- c("ele", "forest")
plot(ef, col=terrain.colors(100))


(beta <- coef(fm.occu, type="state"))
# psi(Int)   psi(ele)   psi(I(ele^2))  psi(forest)
# 0.4638233  1.2276426  -1.3327186     0.6544976
logit.psi <- beta[1] + beta[2]*ele.s + beta[3]*ele.s^2 + beta[4]*forest.s
psi <- exp(logit.psi) / (1 + exp(logit.psi))

# if measures of uncertainty are not required, the following code can be used to quickly produce a species distribution map:
plot(psi, col=terrain.colors(100))
print(spplot(psi, col.regions=terrain.colors(100)))

E.psi <- predict(fm.occu, type="state", newdata=ef)
plot(E.psi, axes=FALSE, col=terrain.colors(100))


## Mapping Population Density
data(issj)
covs <- scale(issj[,c("elevation", "forest", "chaparral")])
area <- pi*300^2 / 10000
jayumf <- unmarkedFrameDS(y=as.matrix(issj[,1:3]), siteCovs=data.frame(covs, area), dist.breaks=c(0,100,200,300), 
                          unitsIn="m", survey="point")
fm1 <- distsamp(~chaparral ~chaparral + elevation + offset(log(area)), jayumf, keyfun="halfnorm", output="abund",
                starts=c(-2.8,1,0,4.5,0))

# format the raster data
data(cruz)
elev <- rasterFromXYZ(cruz[,c("x","y","elevation")], crs="+proj=utm +zone=11 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
forest <- rasterFromXYZ(cruz[,c("x","y","forest")], crs="+proj=utm +zone=11 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
chap <- rasterFromXYZ(cruz[,c("x","y","chaparral")], crs="+proj=utm +zone=11 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
area.raster <- chap
values(area.raster) <- 300*300/10000 # area of a grid pixel
attr(covs, "scaled:center")
# elevation    forest     chaparral
# 202.0023616  0.0673357  0.2703592
attr(covs, "scaled:scale")
# elevation    forest     chaparral
# 124.8818069  0.1368199  0.2338295
elev.s <- (elev-202)/125
forest.s <- (forest-0.0673)/0.137
chap.s <- (chap-0.270)/0.234
habitat <- stack(elev.s, forest.s, chap.s, area.raster)
names(habitat) <- c("elevation", "forest", "chaparral", "area")

E <- predict(fm1, type="state", newdata=habitat)
# doing row 1000 of 5625
# doing row 2000 of 5625
# doing row 3000 of 5625
# doing row 4000 of 5625
# doing row 5000 of 5625
plot(E, axes=FALSE, col=terrain.colors(100))
