module Css.Internal.Browser 
  ( BrowserPrefix, toPrefix, stringPrefix
  ) where
  
type BrowserPrefix = BrowserPrefix String

toPrefix : String -> BrowserPrefix
toPrefix str = BrowserPrefix str

stringPrefix : BrowserPrefix -> String
stringPrefix (BrowserPrefix str) = str
