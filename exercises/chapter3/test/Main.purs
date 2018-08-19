module Test.Main where

import Prelude

import Data.AddressBook (AddressBook, Entry, emptyBook, findEntry, findEntryByStret, insertEntry, removeDup, showEntry, testLastName)
import Data.List (length)
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

example2 :: Entry
example2 =
  { firstName: "John"
  , lastName: "Smith"
  , address: { street: "999 Fake St."
             , city: "Newe York"
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

  let book2 = insertEntry example2 book1
  let withoutDups = removeDup book2

  log "Duplicates:"

  log $ show $ length book2
  log $ show $ length withoutDups
