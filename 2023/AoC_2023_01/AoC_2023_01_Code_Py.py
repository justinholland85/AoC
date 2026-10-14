
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
Year = 2023
Day  = 1

Ref  = f"AoC_{Year}_{Day:02d}"
Dir  = Root / str(Year) / Ref

Name_Data_0a = Ref + "_Input_0_p1.txt"
Name_Data_0b = Ref + "_Input_0_p2.txt"
Name_Data_1  = Ref + "_Input_1.txt"

# Data.0 is not useful here - it is just several messy examples
os.chdir(Dir)

Data_0a = pd.read_csv(Name_Data_0a, header=None, skip_blank_lines=False)[0].to_numpy()
Data_0b = pd.read_csv(Name_Data_0b, header=None, skip_blank_lines=False)[0].to_numpy()
Data_1  = pd.read_csv(Name_Data_1, header=None, skip_blank_lines=False)[0].to_numpy()

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# F_Part_1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
# Data = Data_0a
# Data = Data_1

def F_Part_1(Data): 

    Numbers = pd.Series(Data).str.replace(r"[a-z]", "", regex=True)
    Numbers = Numbers.apply(list)

    First   = [x[0]  for x in Numbers]
    Last    = [x[-1] for x in Numbers]


    # zip() pairs corresponding elements from First and Last.
    # f + l joins the two digit strings into a single string.
    # int() converts the combined string to a number.
    # np.array() converts the results into a NumPy array.

    CalibVal = np.array([int(f + l) for f, l in zip(First, Last)])

    CalibVal_Sum =  sum(CalibVal)

    return CalibVal_Sum


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Execution  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

F_Part_1(Data_1)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# F_Part_1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
# Data = Data_0a
# Data = Data_1

def F_Part_2(Data): 

    Numbers       =  ["one",
                      "two",
                      "three",
                      "four",
                      "five",
                      "six",
                      "seven",
                      "eight",
                      "nine"]
    
    First_Letter   = [x[0]  for x in Numbers]
    Last_Letter    = [x[-1] for x in Numbers]

    # zip() pairs corresponding elements from First_Letter, range(1, 10), and Last_Letter.
    # The f-string f"{First}{N}{Last}" combines the three values into a single string,
        # inserting each value where its {} placeholder appears.
    # The f prefix tells Python to evaluate the expressions inside the {}.
    Sub = np.array([
    f"{First}{N}{Last}"
    for First, N, Last in zip(First_Letter, range(1, 10), Last_Letter)])

    for Number, Replacement in zip(range(1, 10), Sub):
        X = np.char.replace(X, Number, Replacement)

# Pandas version
#        X = X.str.replace(Number, Replacement, regex=False)





    # zip() pairs corresponding elements from First and Last.
    # f + l joins the two digit strings into a single string.
    # int() converts the combined string to a number.
    # np.array() converts the results into a NumPy array.

    CalibVal = np.array([int(f + l) for f, l in zip(First, Last)])

    CalibVal_Sum =  sum(CalibVal)


    return CalibVal_Sum


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Execution  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

F_Part_1(Data_1)