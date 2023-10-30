# FinCat

A small library for brute-force computations with finite categories. Particular application: Find out if a category is [sifted](https://ncatlab.org/nlab/show/sifted+category).

```haskell
-- the data of a finite category
data Category ob arr = FinCat {
  objects :: [ob],
  arrows :: [arr],
  dom :: arr -> ob,
  cod :: arr -> ob,
  id :: ob -> arr,
  comp :: arr -> arr -> arr
}

-- check that reflexive coequalizers are sifted
refcoeq :: Category String String

sifted refcoeq -- True 
```