# See data.py for information about the files and variables.

library(readr)
library(dplyr)

dx = read_delim("mbirth2.tab", "\t")
vx = c("basewman", "basebrgy", "WEIGHT1", "momweigt", "armcircu", "SKINFLD1", "sexchild")
dx = select(dx, vx)

dy = read_delim("mbase2.tab", "\t")
vy = c("basewman", "basebrgy", "livebrth", "cmheight", "settlmnt", "delmonth")
dy = select(dy, vy)

dz = read_delim("person.tab", "\t")
vz = c("basewman", "basebrgy", "agehhmem", "wave", "RELNPRW1")
dz = select(dz, vz)
dz = filter(dz, wave == 0)
dz = filter(dz, RELNPRW1 == 30)
dz = select(dz, -wave, -RELNPRW1)

df = inner_join(dx, dy, dz, by=c("basewman", "basebrgy"))
