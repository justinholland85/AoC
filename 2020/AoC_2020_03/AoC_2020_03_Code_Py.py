
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
Year = 2020
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

    Data_Subs   =  np.char.replace(Data,      ".", 0)
    Data_Subs   =  np.char.replace(Data_Subs, "#", 1)

    Trees       =  np.array([list(x) for x in Data_Subs], dtype = int)
    
    N_Pattern   =  len(Trees[0])

    ID          =  np.array(range(len(Trees))) + 1
   
    dX          =  3

    Ref_X       =  (1 + (ID - 1) * dX) % N_Pattern
    Ref_X[Ref_X == 0] = N_Pattern
    Ref_X       = Ref_X - 1
 
    HitTree     = [T[x] for T, x in zip(Trees, Ref_X)]

    N_Trees_Hit =  sum(HitTree[1:])

    return N_Trees_Hit

    
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

    Data_Subs   =  np.char.replace(Data,      ".", 0)
    Data_Subs   =  np.char.replace(Data_Subs, "#", 1)

    Trees       =  np.array([list(x) for x in Data_Subs], dtype = int)
    
    N_Pattern   =  len(Trees[0])
    Height      =  len(Trees)

    Slopes      = np.array([[1,1],
                            [3,1],
                            [5,1],
                            [7,1],
                            [1,2]])

    # Vec         = np.array([3,1])

    #----------------------------------------------------------------------------------------------------#
    # F.PathWay
    #----------------------------------------------------------------------------------------------------#
  
    def F_Pathway(Vec):

        Pos          = np.array([1,1])

        Pathway     = []

        while True:
            Pos = Pos + Vec
            if Pos[1] > Height:
                break
            else:
                Pathway.append(Pos)

        Path_X      = np.array([x[0] for x in Pathway]) % N_Pattern
        Path_X[Path_X == 0] = N_Pattern

        Path_Y      = np.array([x[1] for x in Pathway])

        HitTree     = [Trees[y-1][x-1] for x,y in zip(Path_X, Path_Y)]

        N_Trees_Hit = sum(HitTree)

        return N_Trees_Hit

    #----------------------------------------------------------------------------------------------------#
    # F.PathWay
    #----------------------------------------------------------------------------------------------------#
    
    N_Trees_Hit = [F_Pathway(v) for v in Slopes]

    TreeProduct = np.prod(N_Trees_Hit)

    return TreeProduct        
       
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Execution  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

F_Part_2(Data_1)
