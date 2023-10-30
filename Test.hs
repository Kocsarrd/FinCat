import FinCat
import Examples

cC = pushoutcoeq
tt = [ (a,b) | a <- objects cC, b <- objects cC, not $ connected (cospans cC a b)]

-- damnit, this is not sifted. The cospans (fs,idC) and (ft,idC) are not connected!