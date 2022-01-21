module NameSupply
  ( NameSupply
  , getName
  , getNames
  , initialNameSupply
  , makeName
  ) where

type NameSupply = Int

getName :: NameSupply -> String -> (NameSupply, String)
getName name_supply prefix = (name_supply + 1, makeName prefix name_supply)

getNames :: NameSupply -> [String] -> (NameSupply, [String])
getNames name_supply prefixes =
  (name_supply + length prefixes, zipWith makeName prefixes [name_supply ..])

initialNameSupply :: NameSupply
initialNameSupply = 0

makeName :: String -> NameSupply -> String
makeName prefix ns = prefix ++ "_" ++ show ns
