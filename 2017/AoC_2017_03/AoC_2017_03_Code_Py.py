
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
Year = 2017
Day  = 3

Ref  = f"AoC_{Year}_{Day:02d}"
Dir  = Root / str(Year) / Ref

Name_Data_0  = Ref + "_Input_0.txt"
Name_Data_1  = Ref + "_Input_1.txt"

# Data.0 is not useful here - it is just several messy examples
os.chdir(Dir)

#with open(Name_Data_0) as f:
#    Data_0 = f.read().splitlines()

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

    Target      = int(Data[0])

    OddRoot = np.floor(Target ** .5)
    if OddRoot % 2 == 0:
        OddRoot = OddRoot - 1

    Remainder   =  Target - OddRoot ** 2
    
    BasePos     =  ((OddRoot - 1) / 2) * np.array([1, -1])

    SideLen     = OddRoot + 1

    if Remainder == 0:
        Offset =  np.array([0, 0])
    if (0 * SideLen < Remainder) & (Remainder <= 1 * SideLen):
        UpShift  = Remainder - 1
        Offset   =  np.array([1, UpShift])
    if (1 * SideLen < Remainder) & (Remainder <= 2 * SideLen):
        LeftShift = Remainder - (OddRoot + 1)
        Offset    =  np.array([1 - LeftShift,  OddRoot])
    if (2 * SideLen < Remainder) & (Remainder <= 3 * SideLen):
        DownShift = Remainder - (2 * SideLen)
        Offset    =  np.array([-OddRoot,  OddRoot - DownShift])
    if (3 * SideLen < Remainder) & (Remainder <= 4 * SideLen):
        RightShift = Remainder - (3 * SideLen)
        Offset     =  np.array([RightShift - OddRoot,  -1])

    Pos       = BasePos + Offset

    Manhatten = sum(np.abs(Pos))

    return Manhatten

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

    Target      = int(Data[0])

    #-------------------------------------------------------------------------------------------------#
    # F_FindPos
    #-------------------------------------------------------------------------------------------------#

    def F_FindPos(Target):

        OddRoot = np.floor(Target ** .5)
        if OddRoot % 2 == 0:
            OddRoot = OddRoot - 1

        Remainder   =  Target - OddRoot ** 2
        
        BasePos     =  ((OddRoot - 1) / 2) * np.array([1, -1])

        SideLen     = OddRoot + 1

        if Remainder == 0:
            Offset =  np.array([0, 0])
        if (0 * SideLen < Remainder) & (Remainder <= 1 * SideLen):
            UpShift  = Remainder - 1
            Offset   =  np.array([1, UpShift])
        if (1 * SideLen < Remainder) & (Remainder <= 2 * SideLen):
            LeftShift = Remainder - (OddRoot + 1)
            Offset    =  np.array([1 - LeftShift,  OddRoot])
        if (2 * SideLen < Remainder) & (Remainder <= 3 * SideLen):
            DownShift = Remainder - (2 * SideLen)
            Offset    =  np.array([-OddRoot,  OddRoot - DownShift])
        if (3 * SideLen < Remainder) & (Remainder <= 4 * SideLen):
            RightShift = Remainder - (3 * SideLen)
            Offset     =  np.array([RightShift - OddRoot,  -1])

        Pos       = BasePos + Offset

        return Pos

    #-------------------------------------------------------------------------------------------------#
    # Main loop
    #-------------------------------------------------------------------------------------------------#

    a_ID    = np.array(1)
    a_Value = np.array(1)
    a_X     = np.array(0)
    a_Y     = np.array(0)

    i       = 1

    while True:

        i       = i + 1
        Pos     = F_FindPos(i)

        Which   = (np.abs(a_X - Pos[0]) <= 1) & (np.abs(a_Y - Pos[1]) <= 1)

        Value   = sum(a_Value[Which])

        a_ID    = np.append(a_ID, i)
        a_Value = np.append(a_Value, Value)
        a_X     = np.append(a_X, Pos[0])
        a_Y     = np.append(a_Y, Pos[1])

        if Value > Target:
            break

    return Value

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Execution  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

F_Part_2(Data_1)




