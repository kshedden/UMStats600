"""
Construct a dataset from the Cebu longitudinal files

https://dataverse.unc.edu/dataset.xhtml?persistentId=hdl:1902.29/11680

Download the mbirth2.tab, mbase2.tab, and person.tab files, then
prepare them using the script below, or using the equivalent R script.

The person.tab file can be found here:

https://dataverse.unc.edu/dataset.xhtml?persistentId=hdl:1902.29/11679

You may also want to refer to the mbase.txt, mbirth.txt, and person.txt
files, which document the variables.
"""

import pandas as pd

dx = pd.read_csv("mbirth2.tab", sep="\t")
vx = ["basewman", "basebrgy", "WEIGHT1", "momweigt", "armcircu", "SKINFLD1", "sexchild"]
dx = dx.loc[:, vx]

dy = pd.read_csv("mbase2.tab", sep="\t")
vy = ["basewman", "basebrgy", "livebrth", "cmheight", "settlmnt", "delmonth"]
dy = dy.loc[:, vy]

dz = pd.read_csv("person.tab", sep="\t")
vz = ["basewman", "basebrgy", "agehhmem", "wave", "RELNPRW1"]
dz = dz.loc[:, vz]
dz = dz.loc[dz.wave == 0, :]
dz = dz.loc[dz.RELNPRW1 == 30, :]
dz = dz.drop(["wave", "RELNPRW1"], axis=1)

df = pd.merge(dx, dy, left_on=["basewman", "basebrgy"], right_on=["basewman", "basebrgy"])
df = pd.merge(df, dz, left_on=["basewman", "basebrgy"], right_on=["basewman", "basebrgy"])
