module Language where

data Type
  = Class {className :: Name, classParams :: [Parameter], classFields :: [Field], classMethods :: [Method]}
  | Integer | Boolean | String | Void

data Field
  = Field Visibility Type Name

data Method
  = Method Visibility Type [Parameter] Name Body

data Parameter
  = Parameter Type Name

type Name = String

type Body = String

data Visibility
  = Private
  | Protected
  | Public
