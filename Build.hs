module Build where

import Language
import Linearize

addField :: Type -> Name -> Type -> Type
addField t n (Class cn ps fs ms) = (Class cn (Parameter t n : ps) (Field Protected t n : fs) ms)

addGetter :: Type -> Name -> Type -> Type
addGetter t fn (Class cn ps fs ms) =
  let b = "return" +++ (this fn) ++ ";\n"
      m = (Method Public t [] ("get" ++ cap fn) b)
  in (Class cn ps fs (m:ms))

addSetter :: Type -> Name -> Type -> Type
addSetter t fn (Class cn ps fs ms) =
  let b = this fn +++ "=" +++ dol fn ++ ";\n"
      m = (Method Public Void [Parameter t fn] ("set" ++ cap fn) b)
  in (Class cn ps fs (m:ms))

addImmutableField :: Type -> Name -> Type -> Type
addImmutableField t n = addGetter t n . addField t n

addMutableField :: Type -> Name -> Type -> Type
addMutableField t n = addSetter t n . addImmutableField t n
