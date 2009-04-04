module Resolver.PrettyPrint where

import Data.ByteString.Char8(unpack)
import Data.List
import Resolver.Types

class PP a where
    ppS :: a -> ShowS

instance PP Version where
    ppS (Version pkg verName) = ppS pkg . (' ':) . (unpack verName++)

instance PP Package where
    ppS (Package pkgName) = (unpack pkgName++)

instance PP Dep where
    ppS (Dep src solvers isSoft) = let arrow = if isSoft
                                               then " -S> {"
                                               else " -> {" in
                                   ppS src . (arrow++) . (\x -> foldr (++) x $ intersperse ", " $ map pp solvers) . ('}':)

pp x = ppS x ""
