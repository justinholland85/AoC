
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# AoC Header
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#

from numpy import append
from numpy     import dtype
from numpy     import concatenate
from functools import reduce


exec(open("C:/Users/justi/OneDrive/Documents/GitHub/Library_Py/Start.py").read())

import numpy as np
import time
import string

from pathlib import Path

Root = Path("C:/Users/justi/OneDrive/Documents/GitHub/AoC/")
Year = 2023
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

    Split   = np.array([list(x) for x in Data])
    # Note not acutally sure which dimension is which because its square, assuming same as R    
    Nx      = Split.shape[1]
    Ny      = Split.shape[0]
 

    Numbers = np.isin(Split, list("0123456789")).astype(int)
    Symbols = np.isin(Split, list("-#$%&*/@+=")).astype(int)

    Adjacent = 0 * Symbols

    for i in range(Ny):
        for j in range(Nx):

            Y0 = max(i - 1, 0)
            Y1 = min(i + 1, Ny) + 1
            X0 = max(j - 1, 0)
            X1 = min(j + 1, Nx) + 1

            Adjacent[i, j] = np.sum(Symbols[Y0:Y1,X0:X1]) > 0

    #----------------------------------------------------------------------------------------------------#
    # F.RowHandle
    #----------------------------------------------------------------------------------------------------#

    def F_RowHandle(i):

        X     = Numbers[i,:] 
        Start = 1 * ((X == 1) & (np.append(0, X[:(Nx-1)]) == 0))
        Seq   = np.cumsum(Start) * X

        N     = np.unique(Seq[X == 1]) 
        # Working here
        Value = np.array([int("".join(Split[i,Seq==n]))  for n in N])
        Keep  = np.array([Adjacent[i,Seq==n].sum() for n in N])

        return Value[Keep >= 1]

    #----------------------------------------------------------------------------------------------------#
    # Loop
    #----------------------------------------------------------------------------------------------------#

    Values    = [F_RowHandle(i) for i in range(Ny)] 
    Sum       = sum(np.concatenate(Values))

    return Sum

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

    Split     =  np.array([list(x) for x in Data])
    # Note not acutally sure which dimension is which because its square, assuming same as R    
    Nx        =  Split.shape[1]
    Ny        =  Split.shape[0]
 

    Numbers   =  np.isin(Split, list("0123456789")).astype(int)
    Symbols   =  np.isin(Split, list("-#$%&*/@+=")).astype(int)
    Stars     =  1 * (Split == "*")
    Stars_ID  =  Lib_Flatwise(Stars, np.cumsum) * Stars


  
    #----------------------------------------------------------------------------------------------------#
    # F.RowHandle
    #----------------------------------------------------------------------------------------------------#

    def F_RowHandle(i):

        X      = Numbers[i,:] 
        Start  = 1 * ((X == 1) & (np.append(0, X[:(Nx-1)]) == 0))
        Seq    = (np.cumsum(Start) + i * 1000) * X

        N      = np.unique(Seq[X == 1]) 
        # Working here
        Value  = np.array([int("".join(Split[i,Seq==n]))  for n in N])
        ID     = Seq[Start == 1]
        ID_Map = pd.DataFrame({"ID": ID, "Value": Value})

        Output = {
            "ID_Map": ID_Map,
            "ID":     Seq
        }

        return Output

    #----------------------------------------------------------------------------------------------------#
    # Loop
    #----------------------------------------------------------------------------------------------------#

    PartNumbers =  [F_RowHandle(i) for i in range(Ny)]
    Part_ID     =  [x["ID"] for x in PartNumbers]
    Part_ID     =  np.array(Part_ID)
    ID_Map      =  [x["ID_Map"] for x in PartNumbers]
    ID_Map      =  pd.concat(ID_Map, ignore_index=True)

    Adjacent = []

    for i in range(Ny):
        for j in range(Nx):

            if Stars[i,j] == 1:

                Y0 = max(i - 1, 0)
                Y1 = min(i + 1, Ny) + 1
                X0 = max(j - 1, 0)
                X1 = min(j + 1, Nx) + 1

                Star_ID = Stars_ID[i,j]

                IDs = pd.unique(
                    Part_ID[Y0:Y1, X0:X1].ravel(order="F")
                    )
                IDs = IDs[IDs != 0]

                Adjacent.append(IDs)

    Adjacent_Count  = [len(x) for x in Adjacent]

    Gears           = [A for A, Count in zip(Adjacent, Adjacent_Count) if Count == 2]
    Gears           = pd.DataFrame(Gears, columns=["ID_A", "ID_B"]).astype(int)

    Gears["Val_A"]  = np.array(ID_Map.Value[Lib_Match(Gears.ID_A, ID_Map.ID)])
    Gears["Val_B"]  = np.array(ID_Map.Value[Lib_Match(Gears.ID_B, ID_Map.ID)])

    Gears["Prod"]   = Gears.Val_A * Gears.Val_B

    Sum             = np.sum(Gears.Prod) 

    return Sum

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Execution  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

F_Part_2(Data_1)

