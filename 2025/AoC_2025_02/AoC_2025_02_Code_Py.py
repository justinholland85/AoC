
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# AoC Header
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
from numpy import concatenate
exec(open("C:/Users/justi/OneDrive/Documents/GitHub/Library_Py/Start.py").read())

import numpy as np
import time
from pathlib import Path

Root = Path("C:/Users/justi/OneDrive/Documents/GitHub/AoC/")
Year = 2025
Day  = 2

Ref  = f"AoC_{Year}_{Day:02d}"
Dir  = Root / str(Year) / Ref

Name_Data_0  = Ref + "_Input_0.txt"
Name_Data_1  = Ref + "_Input_1.txt"

# Data.0 is not useful here - it is just several messy examples
os.chdir(Dir)

with open(Name_Data_0) as f:
    Data_0 = f.read().strip()


with open(Name_Data_1) as f:
    Data_1 = f.read().strip()


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# F_Part_1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
# Data = Data_0
# Data = Data_1


def F_Part_1(Data): 

    #------------------------------------------------------------------------------------------------#
    # Data setup
    #------------------------------------------------------------------------------------------------#
  
    Split = [x.split("-") for x in Data.split(",")]
    Pairs = np.array(Split, dtype=int)
    Pairs = pd.DataFrame(Pairs, columns=["From", "To"])

    N_Pairs = Pairs.shape[0]

    #------------------------------------------------------------------------------------------------#
    # Obtain list of all possible invalids
    #------------------------------------------------------------------------------------------------#

    MaxDigs = len(str(np.max(Pairs)))

    Digits   = range(0, 10)
    Invalids = []

    for Digs in range(2, MaxDigs + 1):
        for Factor in range(1, Digs // 2 + 1):

            if Digs / Factor == 2:

                Complement = Digs // Factor

                Grid_Inputs = {}

                for i in range(Factor):
                    Grid_Inputs[f"Var{i+1}"] = Digits


                Grid   = Lib_ExpandGrid(**Grid_Inputs)
                ConCat = Grid.astype(str).agg("".join, axis=1)
                Repeat = ConCat.str.repeat(Complement)
                Numbers = np.sort(pd.to_numeric(Repeat).unique())
            
                Invalids.append(Numbers[np.char.str_len(Numbers.astype(str)) == Digs])

    Invalid_Values = np.unique(np.concatenate(Invalids))            
            
    #------------------------------------------------------------------------------------------------#
    # Test Invalids against Ranges
    #------------------------------------------------------------------------------------------------#
    
    RangeMarker = np.zeros(len(Invalid_Values), dtype=bool)

    for i in range(N_Pairs):
        RangeMarker |= (
            (Pairs["From"].iloc[i] <= Invalid_Values) &
            (Invalid_Values <= Pairs["To"].iloc[i])
        )

    Sum_RangeInvalids = np.sum(Invalid_Values[RangeMarker])


    return Sum_RangeInvalids


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

    #------------------------------------------------------------------------------------------------#
    # Data setup
    #------------------------------------------------------------------------------------------------#
  
    Split = [x.split("-") for x in Data.split(",")]
    Pairs = np.array(Split, dtype=int)
    Pairs = pd.DataFrame(Pairs, columns=["From", "To"])

    N_Pairs = Pairs.shape[0]

    #------------------------------------------------------------------------------------------------#
    # Obtain list of all possible invalids
    #------------------------------------------------------------------------------------------------#

    MaxDigs = len(str(np.max(Pairs)))

    Digits   = range(0, 10)
    Invalids = []

    for Digs in range(2, MaxDigs + 1):
        for Factor in range(1, Digs // 2 + 1):

            if Digs % Factor == 0:

                Complement = Digs // Factor

                Grid_Inputs = {}

                for i in range(Factor):
                    Grid_Inputs[f"Var{i+1}"] = Digits


                Grid   = Lib_ExpandGrid(**Grid_Inputs)
                ConCat = Grid.astype(str).agg("".join, axis=1)
                Repeat = ConCat.str.repeat(Complement)
                Numbers = np.sort(pd.to_numeric(Repeat).unique())
            
                Invalids.append(Numbers[np.char.str_len(Numbers.astype(str)) == Digs])

    Invalid_Values = np.unique(np.concatenate(Invalids))            
            
    #------------------------------------------------------------------------------------------------#
    # Test Invalids against Ranges
    #------------------------------------------------------------------------------------------------#
    
    RangeMarker = np.zeros(len(Invalid_Values), dtype=bool)

    for i in range(N_Pairs):
        RangeMarker |= (
            (Pairs["From"].iloc[i] <= Invalid_Values) &
            (Invalid_Values <= Pairs["To"].iloc[i])
        )

    Sum_RangeInvalids = np.sum(Invalid_Values[RangeMarker])


    return Sum_RangeInvalids


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Execution  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

F_Part_2(Data_1)
