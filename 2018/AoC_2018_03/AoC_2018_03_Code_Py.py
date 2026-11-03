
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# AoC Header
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#

from numpy import dtype
from numpy import concatenate
exec(open("C:/Users/justi/OneDrive/Documents/GitHub/Library_Py/Start.py").read())

import numpy as np
import time
from pathlib import Path

Root = Path("C:/Users/justi/OneDrive/Documents/GitHub/AoC/")
Year = 2018
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

    Split = [x.split() for x in Data]

    ID    = [x[0] for x in Split]
    ID    = np.char.replace(ID, "#", "").astype(int)
    
    Pos   = [x[2].split(",") for x in Split]

    Pos_X = [int(x[0]) for x in Pos]
    Pos_Y = [int(np.char.replace(x[1], ":", "")) for x in Pos]

    Dim   = [x[3].split("x") for x in Split]
    Dim_X = [int(x[0]) for x in Dim]
    Dim_Y = [int(x[1]) for x in Dim]

    Claims = pd.DataFrame({
                        "ID":  ID,
                        "rX": Pos_X, 
                        "rY": Pos_Y, 
                        "dX": Dim_X, 
                        "dY": Dim_Y 
                        })

    Claims['X0'] = Claims.rX
    Claims['Y0'] = Claims.rY    
    Claims['X1'] = Claims.X0 + Claims.dX
    Claims['Y1'] = Claims.Y0 + Claims.dY
    

    Count = np.zeros((1000,1000))      

    for i in range(max(ID)):

        Range_X = range(Claims.X0[i], Claims.X1[i])
        Range_Y = range(Claims.Y0[i], Claims.Y1[i])

        Count[np.ix_(Range_X, Range_Y)] += 1

    Dupes    = np.sum(Count > 1)

    return Dupes

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

    Split  = [x.split() for x in Data]

    ID     = [x[0] for x in Split]
    ID     = np.char.replace(ID, "#", "").astype(int)
    
    Pos    = [x[2].split(",") for x in Split]

    Pos_X  = [int(x[0]) for x in Pos]
    Pos_Y  = [int(np.char.replace(x[1], ":", "")) for x in Pos]

    Dim    = [x[3].split("x") for x in Split]
    Dim_X  = [int(x[0]) for x in Dim]
    Dim_Y  = [int(x[1]) for x in Dim]

    Claims = pd.DataFrame({
                        "ID":  ID,
                        "rX": Pos_X, 
                        "rY": Pos_Y, 
                        "dX": Dim_X, 
                        "dY": Dim_Y 
                        })

    Claims['X0'] = Claims.rX
    Claims['Y0'] = Claims.rY    
    Claims['X1'] = Claims.X0 + Claims.dX
    Claims['Y1'] = Claims.Y0 + Claims.dY
    

    Count = np.zeros((1000,1000))      

    for i in range(max(ID)):

        Range_X = range(Claims.X0[i], Claims.X1[i])
        Range_Y = range(Claims.Y0[i], Claims.Y1[i])

        Count[np.ix_(Range_X, Range_Y)] += 1

    Claims['Overlap'] = np.inf

    for i in range(max(ID)):

        Range_X = range(Claims.X0[i], Claims.X1[i])
        Range_Y = range(Claims.Y0[i], Claims.Y1[i])

        Claims.loc[i, "Overlap"]  = np.sum(Count[np.ix_(Range_X, Range_Y)] > 1)

    Independent_ID = Claims.ID[np.where(Claims["Overlap"] == 0)[0]].iloc[0]

    return Independent_ID

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Execution  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

F_Part_2(Data_1)
