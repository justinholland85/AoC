with open("AoC_Input_2025_01_0.txt") as File:
    Data_0 = File.read().splitlines()

with open("AoC_Input_2025_01_1.txt") as File:
    Data_1 = File.read().splitlines()

######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# F.Part.1
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

Data = Data_1
X0   = 50
N    = 100

Direction = [x[0] for x in Data]
Move      = [int(x[1:]) for x in Data]

dX        = [-Move[i] if Direction[i] == "L" else Move[i]
              for i in range(len(Move))]

X = []
Total = X0

for Change in dX:
    Total += Change
    X.append(Total % N)

Count = sum(x == 0 for x in X)