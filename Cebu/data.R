library(readr)
library(dplyr)

dx = read_delim("mbirth2.tab", "\t")
vx = c("basewman", "basebrgy", "WEIGHT1", "momweigt", "armcircu", "heightcm", "SKINFLD1", "sexchild")
dx = select(dx, vx)

dy = read_delim("mbase2.tab", "\t")
vy = c("basewman", "basebrgy", "livebrth", "settlmnt", "delmonth")
dy = select(dy, vy)

dz = inner_join(dx, dy, by=c("basewman", "basebrgy"))
