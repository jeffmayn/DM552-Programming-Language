kage = [6,6,6,6,6,6,12,6,6,6,6,6,6,11]
split = splitAt 7 kage
first = fst split
second = snd split
vT = last first
vF = last second
winner = vT-vF
