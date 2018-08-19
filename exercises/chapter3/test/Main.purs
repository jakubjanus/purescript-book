module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Data.AddressBook (AddressBook, Entry, emptyBook, insertEntry, findEntry, showEntry)
import Data.Maybe (Maybe)

example :: Entry
example =
  { firstName: "John"
  , lastName: "Smith"
  , address: { street: "123 Fake St."
             , city: "Faketown"
             , state: "CA"
             }
  }

book0 :: AddressBook
book0 = emptyBook

printEntry :: String -> String -> AddressBook -> Maybe String
printEntry firstName lastName book = showEntry <$> findEntry firstName lastName book

iden :: String -> String
iden s = s

main :: Effect Unit
main = do
  let book1 = insertEntry example emptyBook

  log $ show $ printEntry "John" "Smith" book0
  log $ show $ printEntry "John" "Smith" book1
