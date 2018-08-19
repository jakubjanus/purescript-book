module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, null, nubBy)
import Data.Maybe (Maybe)

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <> addr.city <> ", " <> addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <> entry.firstName <> ": " <> showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

findEntryByStret :: String -> AddressBook -> Maybe Entry
findEntryByStret street = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.address.street == street

testLastName :: String -> AddressBook -> Boolean
testLastName lastName = not <<< null <<< filter filterBook
  where
  filterBook :: Entry -> Boolean
  filterBook entry = entry.lastName == lastName

removeDup :: AddressBook -> AddressBook
removeDup = nubBy check
  where
  check :: Entry -> Entry -> Boolean
  check e1 e2 = e1.firstName == e2.firstName && e1.lastName == e2.lastName
