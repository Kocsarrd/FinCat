module Examples where

import FinCat

-- Discrete category
discrete :: [ob] -> Category ob ob 
discrete obs = FinCat objects arrows dom cod id comp where
    objects = obs
    arrows = obs
    dom a = a
    cod a = a
    id a = a
    comp a b = a 

-- The walking arrow
interval :: Category Int String
interval = FinCat objects arrows dom cod id comp where
    objects = [0,1]
    arrows = ["id0","id1","f"]
    dom "id0" = 0
    dom "id1" = 1
    dom "f" = 0

    cod "id0" = 0
    cod "id1" = 1
    cod "f" = 1

    id 0 = "id0"
    id 1 = "id1"

    comp "id0" a = a
    comp "id1" a = a
    comp "f" "id0" = "f"

-- The walking isomorphism
iso :: Category String String
iso = FinCat objects arrows dom cod id comp
  where
    objects = ["A","B"]
    arrows = ["f","g","idA","idB"]

    dom "f" = "A"
    dom "g" = "B"
    dom "idA" = "A"
    dom "idB" = "B"
    
    cod "f" = "B"
    cod "g" = "A"
    cod "idA" = "A"
    cod "idB" = "B"

    id "A" = "idA"
    id "B" = "idB"

    comp "idA" a = a
    comp "idB" a = a
    
    comp "f" "g" = "idB"
    comp "f" "idA" = "f"

    comp "g" "f" = "idA"
    comp "g" "idB" = "g"

-- The walking reflexive coequalizer
refcoeq :: Category String String
refcoeq = FinCat objects arrows dom cod id comp
  where
    objects = ["A","B"]
    arrows = ["idA","idB","s","t","r","rs","rt"]

    dom "idA" = "A"
    dom "idB" = "B"
    dom "s" = "A"
    dom "t" = "A"
    dom "r" = "B"
    dom "rs" = "A"
    dom "rt" = "A"

    cod "idA" = "A"
    cod "idB" = "B"
    cod "s" = "B"
    cod "t" = "B"
    cod "r" = "A"
    cod "rs" = "A"
    cod "rt" = "A"

    id "A" = "idA"
    id "B" = "idB"

    comp "idA" a = a
    comp "idB" a = a

    comp "s" "r" = "idB"
    comp "s" "idA" = "s"
    comp "s" "rs" = "s"
    comp "s" "rt" = "t"

    comp "t" "idA" = "t"
    comp "t" "r" = "idB"
    comp "t" "rs" = "s"
    comp "t" "rt" = "t"

    comp "rs" "r" = "r"
    comp "rs" "rs" = "rs"
    comp "rs" "rt" = "rt"
    comp "rs" "idA" = "rs"

    comp "rt" "r" = "r"
    comp "rt" "rs" = "rs"
    comp "rt" "rt" = "rt"
    comp "rt" "idA" = "rt"

    comp "r" "idB" = "r"
    comp "r" "s" = "rs"
    comp "r" "t" = "rt"

-- The walking pushout coequalizer
pushoutcoeq :: Category String String
pushoutcoeq = FinCat objects arrows dom cod id comp
  where
    objects = ["A","B","C"]
    arrows = ["idA","idB","idC","s","t","r","rs","rt","f","fs","ft"]

    dom "idA" = "A"
    dom "idB" = "B"
    dom "idC" = "C"
    dom "s" = "A"
    dom "t" = "A"
    dom "r" = "B"
    dom "rs" = "A"
    dom "rt" = "A"
    dom "f" = "B"
    dom "fs" = "A"
    dom "ft" = "A"

    cod "idA" = "A"
    cod "idB" = "B"
    cod "idC" = "C"
    cod "s" = "B"
    cod "t" = "B"
    cod "r" = "A"
    cod "rs" = "A"
    cod "rt" = "A"
    cod "f" = "C"
    cod "fs" = "C"
    cod "ft" = "C"

    id "A" = "idA"
    id "B" = "idB"
    id "C" = "idC"

    comp "idA" a = a
    comp "idB" a = a
    comp "idC" a = a

    comp "s" "r" = "idB"
    comp "s" "idA" = "s"
    comp "s" "rs" = "s"
    comp "s" "rt" = "t"

    comp "t" "idA" = "t"
    comp "t" "r" = "idB"
    comp "t" "rs" = "s"
    comp "t" "rt" = "t"

    comp "rs" "r" = "r"
    comp "rs" "rs" = "rs"
    comp "rs" "rt" = "rt"
    comp "rs" "idA" = "rs"

    comp "rt" "r" = "r"
    comp "rt" "rs" = "rs"
    comp "rt" "rt" = "rt"
    comp "rt" "idA" = "rt"

    comp "fs" "r" = "f"
    comp "fs" "rs" = "fs"
    comp "fs" "rt" = "ft"
    comp "fs" "idA" = "fs"

    comp "ft" "r" = "f"
    comp "ft" "rs" = "fs"
    comp "ft" "rt" = "ft"
    comp "ft" "idA" = "ft" --

    comp "r" "idB" = "r"
    comp "r" "s" = "rs"
    comp "r" "t" = "rt"

    comp "f" "idB" = "f"
    comp "f" "s" = "fs"
    comp "f" "t" = "ft"