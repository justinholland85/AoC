import numpy as np

with open("AoC_2025_01_Input_0.txt") as File:
    Data_0 = File.read().splitlines()

with open("AoC_2025_01_Input_1.txt") as File:
    Data_1 = File.read().splitlines()


Data = np.array(Data_1)

######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# F.Part.1
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

X0        = 50
N         = 100

Direction = Data.astype("U1")
Move      = np.array([int(x[1:]) for x in Data])
dX        = Move * np.where(Direction == "L", -1, 1)
X         = (X0 + np.cumsum(dX)) % N
Password  = np.sum(X == 0)
print(Password)

######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# F.Part.2
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################


X0        = 50
N         = 100


Lambda    = len(Data)

Direction = Data.astype("U1")
Move      = np.array([int(x[1:]) for x in Data])
dX        = Move * np.where(Direction == "L", -1, 1)
X         = np.insert((X0 + np.cumsum(dX)) % N, 0, X0)

FullRots  = np.floor(Move / N)
Remainder = Move % N 

Pass      = ((Direction == "L") * (Remainder > X[:-1]) + 
             (Direction == "R") * (Remainder > ((N - X[:-1]) % 100)))


Password  = np.sum(Pass) + np.sum(FullRots) + (X[-1] == 0)
print(Password)
