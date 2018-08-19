module Test.Main where

import Prelude

import Data.AddressBook (AddressBook, Entry, emptyBook, findEntry, findEntryByStret, insertEntry, showEntry, testLastName)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Console (log)

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

printEntry :: Maybe Entry -> Maybe String
printEntry entry = showEntry <$> entry


main :: Effect Unit
main = do
  let book1 = insertEntry example emptyBook

  log $ show $ printEntry $ findEntry "John" "Smith" book0
  log $ show $ printEntry $ findEntry "John" "Smith" book1

  let entry = findEntryByStret "123 Fake St." book1

  log $ show $ printEntry entry

  log $ show $ testLastName "Smith" book1
  log $ show $ testLastName "James" book1
