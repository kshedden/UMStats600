"""
Cebu mother baseline survey

https://dataverse.unc.edu/dataset.xhtml?persistentId=hdl:1902.29/11680

Download the mbirth2.tab and mbase2.tab files, then prepare using
the script below, or using the equivalent R script.

You may also want to refer to the mbase.txt and mbirth.txt files,
which document the variables.
"""

import pandas as pd

dx = pd.read_csv("mbirth2.tab", sep="\t")
vx = ["basewman", "basebrgy", "WEIGHT1", "momweigt", "armcircu", "heightcm", "SKINFLD1", "sexchild"]
dx = dx.loc[:, vx]

dy = pd.read_csv("mbase2.tab", sep="\t")
vy = ["basewman", "basebrgy", "livebrth"]
dy = dy.loc[:, vy]

dz = pd.merge(dx, dy, left_on=["basewman", "basebrgy"], right_on=["basewman", "basebrgy"])
