
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# AoC Header
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#

from numpy     import append
from numpy     import dtype
from numpy     import concatenate
from functools import reduce


exec(open("C:/Users/justi/OneDrive/Documents/GitHub/Library_Py/Start.py").read())

import numpy as np
import time
import string
import re

from pathlib import Path

Root = Path("C:/Users/justi/OneDrive/Documents/GitHub/AoC/")
Year = 2024
Day  = 3

Ref  = f"AoC_{Year}_{Day:02d}"
Dir  = Root / str(Year) / Ref

Name_Data_0a  = Ref + "_Input_0a.txt"
Name_Data_0b  = Ref + "_Input_0b.txt"
Name_Data_1   = Ref + "_Input_1.txt"

# Data.0 is not useful here - it is just several messy examples
os.chdir(Dir)

with open(Name_Data_0a) as f:
    Data_0a = f.read().splitlines()

with open(Name_Data_0b) as f:
    Data_0b = f.read().splitlines()

with open(Name_Data_1) as f:
    Data_1  = f.read().splitlines()

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# F_Part_1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
# Data = Data_0a
# Data = Data_1

def F_Part_1(Data): 

    X       = "".join(Data)

    Pattern = r"mul\(([0-9]{1,3}),([0-9]{1,3})\)"

    Matches = list(re.finditer(Pattern, X))

    Numbers = pd.DataFrame([ [int(m.group(1)), int(m.group(2))] for m in Matches], columns=["X", "Y"])

    SumProd = sum(Numbers.X * Numbers.Y)

    return  SumProd

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Execution  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

F_Part_1(Data_1)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# F_Part_2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
# Data = Data_0b
# Data = Data_1

def F_Part_2(Data): 

    X       = "".join(Data)

    #------------------------------------------------------------------------------------------------#
    # Base Info
    #------------------------------------------------------------------------------------------------#
  
    Pattern = r"mul\(([0-9]{1,3}),([0-9]{1,3})\)"

    Matches = list(re.finditer(Pattern, X))

    Numbers = pd.DataFrame([ [int(m.group(1)), int(m.group(2))] for m in Matches], columns=["X", "Y"])
    Numbers['Start'] = [m.start() for m in Matches]


    #------------------------------------------------------------------------------------------------#
    # do() and don't() handles
    #------------------------------------------------------------------------------------------------#

    Matches_Do      = list(re.finditer(r"do\(\)", X))
    Matches_Dont    = list(re.finditer(r"don't\(\)", X))

    Pos_Do          = [m.start() for m in Matches_Do]
    Pos_Dont        = [m.start() for m in Matches_Dont]
 
    OnOff           = pd.DataFrame({"Pos": np.unique([0, *Pos_Do, *Pos_Dont])})

    OnOff["Switch"] = np.where(OnOff["Pos"].isin(Pos_Dont), -1, 1)

    OnOff["Change"] = (OnOff["Switch"] != OnOff["Switch"].shift(1, fill_value=0)).astype(int)

      
    OnOff           = OnOff[OnOff.Change == 1]
    OnOff["Seq"]    = np.array(range(len(OnOff))) + 1

    On              = OnOff.Pos[OnOff.Seq % 2 == 1]
    Off             = np.append(OnOff.Pos[OnOff.Seq % 2 == 0], np.inf)
  
    #------------------------------------------------------------------------------------------------#
    # Range Check
    #------------------------------------------------------------------------------------------------#

    Numbers['On']   = 0

    for i in range(len(On)):
         Numbers["On"] += Numbers["Start"].between(On.iloc[i], Off[i], inclusive="neither").astype(int)


    Numbers['On']   = 0
    SumProd = sum(Numbers.X * Numbers.Y * Numbers.On)

    return  SumProd

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Execution  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

F_Part_2(Data_1)