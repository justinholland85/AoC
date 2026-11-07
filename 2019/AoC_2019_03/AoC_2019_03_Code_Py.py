
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
Year = 2019
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

    VectorMap = np.array([
                        [ 0,  1],
                        [ 1,  0],
                        [ 0, -1],
                        [-1,  0]
                        ])

    DirMap = {"U": 0, "R": 1, "D": 2, "L": 3}

    X = Data[0]

    def F_Path(X):

        Dir             = [DirMap[x[0]] for x in X.split(",")]
        Len             = [int(x[1:]) for x in X.split(",")]

        Vectors         = VectorMap[Dir]
        RepeatedVectors = np.repeat(Vectors, Len, axis=0)
        Move            = np.vstack(RepeatedVectors)

        Position        = np.cumsum(Move, axis = 0)

        return Position


    Positions  = [F_Path(x) for x in Data]

    Match      = Lib_TupleMatch(Positions[0], Positions[1])

    Intersects = Positions[0][Match >=0]

    Dist       = np.sum(np.abs(Intersects), axis = 1)

    MinDist    = min(Dist)
    
    return MinDist

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

    VectorMap = np.array([
                        [ 0,  1],
                        [ 1,  0],
                        [ 0, -1],
                        [-1,  0]
                        ])

    DirMap = {"U": 0, "R": 1, "D": 2, "L": 3}

    X = Data[0]

    def F_Path(X):

        Dir             = [DirMap[x[0]] for x in X.split(",")]
        Len             = [int(x[1:]) for x in X.split(",")]

        Vectors         = VectorMap[Dir]
        RepeatedVectors = np.repeat(Vectors, Len, axis=0)
        Move            = np.vstack(RepeatedVectors)

        Position        = np.cumsum(Move, axis = 0)
        Position        = pd.DataFrame(Position, columns=["X", "Y"])

        return Position


    Positions           = [F_Path(x) for x in Data]

    Match               = Lib_TupleMatch(Positions[0], Positions[1])

    Intersects          = Positions[0]
    Intersects["ID_A"]  = np.array(range(len(Intersects))) + 1
    Intersects["ID_B"]  = Match +  1
    Intersects          = Intersects[Intersects.ID_B > 0 ]

    MinDist             = min(Intersects.ID_A + Intersects.ID_B)
    
    return MinDist

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Execution  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

F_Part_2(Data_1)



                        

