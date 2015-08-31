---
layout: post
title: Handling JSON Data in Haskell
excerpt: JSON is a common interchange format and Haskell can grok it.
categories: articles
tags: [haskell,json,aeson,data,parsing,functional,programming,fp]
comments: true
share: true
redirect_from: /post/110309915014/json-parse-haskell
---

<p>Haskell is a great language and there are many resources already online to learning the language. That said, I remember that I found it difficult to really see any &ldquo;real-world&rdquo; examples of certain libraries when I started learning. Given the current state of the internet, it seems that JSON is king when it comes to the many data transfer formats in the age of the web. As a result, I am going to describe a short few use-cases and examples on how to parse JSON data in Haskell.</p><p><b>Caution:</b> Before you proceed, I assume you understand at least the <i>basics</i> of Haskell. There are many great resources online and, most notably, <a href="http://learnyouahaskell.com/" target="_blank">this online book</a> is a fantastic starter resource (yes, you really should read it cover to cover from beginning to end).</p><p>Alright, so the problem we&rsquo;re looking to solve is how to translate some text code (i.e. JSON) to an internal Haskell representation. As in any programming language, we prefer to use popular libraries whenever possible (especially for error-prone tasks such as parsing). As luck would have it, Haskell does have a library known as <a href="https://hackage.haskell.org/package/aeson-0.8.0.2/docs/Data-Aeson.html" target="_blank">Aeson</a> which can help us here. The library is pretty well designed and reasonably easy to use. In any case, this post is designed to help all of you trying to get a grasp of using Haskell in real-world applications, so we&rsquo;re going to go through an example. Consequently, we can model an address book using a JSON file to store the data. An example file would look as follows:</p>

```json
[
  {
    "name": "dennis",
    "phone": 1234567890,
    "email": "me@some-address.com"
  },
  {
    "name": "walter white",
    "phone": 5551230123,
    "email": "cant@catchme.com"
  },
  {
    "name": "dexter morgan",
    "phone": 7778238293,
    "email": "justalittbit@crazy.com"
  }
]
```

<p>Basically, we store our data as an array of Person objects. Now, we&rsquo;ll show how we are going to represent this data in Haskell and how to parse it.</p>

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Main
Description : Short Json example to show quasi-complex loading
Author      : Dennis J. McWherter, Jr.
Stability   : experimental
Portability : POSIX
-}
module Main where
import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.HashMap.Strict as HM
import Data.Text
import qualified Data.Vector as V
import System.Environment
import System.IO

-- Type declarations
type Name = Text  -- ^ Type for persons name
type Phone = Int  -- ^ Type for phone number
type Email = Text -- ^ Type for email

-- Data types
data Person = Person { name :: Name   -- ^ Name corresponding to the represented person
                     , phone :: Phone -- ^ Phone number for person
                     , email :: Email -- ^ Email for person
                     } deriving (Show)

type PersonMap = HM.HashMap Name Person -- ^ Type corresponding to a hashmap of people
newtype AddressBook = People PersonMap  -- ^ New type for address book (requird to instance FromJSON)
                      deriving Show

-- JSON decoding
instance FromJSON Person where
  parseJSON (Object v) = Person <$> v .: "name" <*> v .: "phone" <*> v .: "email"
  parseJSON _ = mzero

instance FromJSON AddressBook where
  parseJSON (Array v) = do
    map <- V.foldl insertPeople (return HM.empty) v
    return $ People map
    where
      insertPeople :: Parser PersonMap -> Value -> Parser PersonMap
      insertPeople m pObj = do
        person <- (return pObj >>= parseJSON) :: Parser (Maybe Person)
        hmap <- m
        case person of
         Just p -> return $ HM.insert (name p) p hmap
         Nothing -> m
  parseJSON _ = mzero

-- Program execution
main :: IO ()
main = do
  (fileName:_) <- getArgs
  json <- readFile fileName
  putStrLn $ "Read json from file: " ++ fileName
  putStrLn $ json
  book <- return (decode $ C.pack json :: Maybe AddressBook)
  case book of
   Just (People abook) -> putStrLn "Please type a name to search for. Using 'exit' will quit the program." >> queryLoop abook ""
   _ -> putStrLn "Could not parse address book json correctly."
  return ()

queryLoop :: PersonMap -- ^ The address book to perform queries on
          -> Name      -- ^ Name of person to lookup (NOTE: Exit ends loop)
          -> IO ()
queryLoop book name
  | name == "exit" = return ()
  | name == "" = getQuery >>= queryLoop book
  | otherwise = do
      person <- return $ HM.lookup name book
      case person of
       Just p -> do
                 putStrLn $ (show name) ++ " found:"
                 putStrLn $ "  Name: " ++ (show name)
                 putStrLn $ "  Phone: " ++ (show $ phone p)
                 putStrLn $"  Email: " ++ (show $ email p)
       Nothing -> putStrLn "Person not found."
      queryLoop book ""
  where getQuery :: IO Text
        getQuery = putStr "query> " >> hFlush stdout >> getLine >>= return . pack
```

<p>Aside from the fact that this isn&rsquo;t quite the cleanest or most terse Haskell code ever written (hey, give me a break&ndash; it&rsquo;s example code for teaching), the first thing you should notice is the OverloadedStrings extensions. In this post we won&rsquo;t be explaining this in detail, however, you can read more about it <a href="https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/basic-syntax-extensions#overloadedstrings" target="_blank">here</a>. The next thing you will notice is that we implemented both <b>Person</b> and <b>AddressBook</b> data types as instances of the <b>FromJSON</b> class. This makes it easier for us to decode the objects in isolation. Since we define that an AddressBook is composed of Person objects, we can simply decode all of the Person objects within the list to construct our map. Similarly, you&rsquo;ll notice that the type for <b>Array</b> is actually just a <b>Vector</b>. As a result, we can use our Vector methods to map over the collection.</p><p>This post is aimed at providing a slightly more complicated and complete example of JSON parsing using Aeson in Haskell. There are many other tutorials and examples out there, but all of them seem to be far less complete. If you have any questions, please feel free to submit them in the comments below!</p>
