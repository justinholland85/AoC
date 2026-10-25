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
Year = 2023
Day  = 2

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

    Games = [x.split(":")[1] for x in Data]

    Max   = pd.Series({"blue": 14, "green": 13, "red": 12})

    #------------------------------------------------------------------------------------------------#
    # F.Game.Valid
    #------------------------------------------------------------------------------------------------#
    
    def F_Game_Valid(Game):

        Pull          = [x.strip().split(",") for x in  Game.strip().split(";")]

        Groups        = [x.strip() for Sublist in Pull for x in Sublist]

        Groups_Split  = [x.split(" ") for x in Groups]

        Colour        = [x[1] for x in Groups_Split]
        N             = [x[0] for x in Groups_Split]

        Counts        = pd.DataFrame({"Colour": Colour,  "N": N})
        Counts["N"]   = pd.to_numeric(Counts["N"])

        Largest       = Counts.groupby("Colour")["N"].max().reindex(Max.index).fillna(0)

        Valid         = np.prod(Largest <= Max)

        return Valid

    #------------------------------------------------------------------------------------------------#
    # Calcs
    #------------------------------------------------------------------------------------------------#
    
    Games_Valid      = [F_Game_Valid(Game) for Game in Games]
    Games_ID         = np.array(range(len(Games_Valid))) + 1

    ID_Sum           = sum(Games_ID * Games_Valid)

    return ID_Sum

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Execution  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

F_Part_1(Data_1)
    
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# F_Part_1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
# Data = Data_0
# Data = Data_1

def F_Part_2(Data): 

    Games = [x.split(":")[1] for x in Data]

    Max   = pd.Series({"blue": 14, "green": 13, "red": 12})

    #------------------------------------------------------------------------------------------------#
    # F_Game_Power
    #------------------------------------------------------------------------------------------------#
    
    def F_Game_Power(Game):

        Pull          = [x.strip().split(",") for x in  Game.strip().split(";")]

        Groups        = [x.strip() for Sublist in Pull for x in Sublist]

        Groups_Split  = [x.split(" ") for x in Groups]

        Colour        = [x[1] for x in Groups_Split]
        N             = [x[0] for x in Groups_Split]

        Counts        = pd.DataFrame({"Colour": Colour,  "N": N})
        Counts["N"]   = pd.to_numeric(Counts["N"])

        Largest       = Counts.groupby("Colour")["N"].max().reindex(Max.index).fillna(0)

        Power         = np.prod(Largest)

        return Power

    #------------------------------------------------------------------------------------------------#
    # Calcs
    #------------------------------------------------------------------------------------------------#
    
    Games_Power       = [F_Game_Power(Game) for Game in Games]

    Power_Tot         = sum(Games_Power)

    return Power_Tot

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Execution  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

F_Part_2(Data_1)
    





  