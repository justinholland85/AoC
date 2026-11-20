
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# AoC Header
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#

from numpy     import dtype
from numpy     import concatenate
from functools import reduce


exec(open("C:/Users/justi/OneDrive/Documents/GitHub/Library_Py/Start.py").read())

import numpy as np
import time
import string

from pathlib import Path

Root = Path("C:/Users/justi/OneDrive/Documents/GitHub/AoC/")
Year = 2022
Day  = 3

Ref  = f"AoC_{Year}_{Day:02d}"
Dir  = Root / str(Year) / Ref

Name_Data_0  = Ref + "_Input_0.txt"
Name_Data_1  = Ref + "_Input_1.txt"

# Data.0 is not useful here - it is just several messy examples
os.chdir(Dir)

with open(Name_Data_0) as f:
    Data_0 = f.read().splitlines()

with open(Name_Data_1) as f:
    Data_1 = f.read().splitlines()

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# F_Part_1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
# Data = Data_0
# Data = Data_1

def F_Part_1(Data): 

    PrioritySpec = list(string.ascii_lowercase + string.ascii_uppercase)

    Nchar        = [len(d) for d in Data]
    Split        = [np.array(list(d)) for d in Data]


    Comp_1       = [x[0:(n//2)] for x, n in zip(Split, Nchar)]
    Comp_2       = [x[(n//2):]  for x, n in zip(Split, Nchar)]

    Intersect    = [np.intersect1d(x1,x2) for x1, x2 in zip(Comp_1, Comp_2)]

    Priority     = Lib_Match(Intersect, PrioritySpec) 
    Priority_Sum = np.sum(Priority + 1)

    return Priority_Sum

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Execution  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

F_Part_1(Data_1)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# F_Part_2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
# Data = Data_0
# Data = Data_1

def F_Part_2(Data): 

    PrioritySpec = list(string.ascii_lowercase + string.ascii_uppercase)

    Nchar        = [len(d) for d in Data]
    Split        = [np.array(list(d)) for d in Data]

    Group        = np.array(range(len(Data))) // 3

    Sets         = Lib_Tapply(Split, Group)

    Intersect    = [reduce(np.intersect1d, x) for x in Sets.values()]

    Priority     = Lib_Match(Intersect, PrioritySpec) 
    Priority_Sum = np.sum(Priority + 1)

    return Priority_Sum



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Execution  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

F_Part_2(Data_1)