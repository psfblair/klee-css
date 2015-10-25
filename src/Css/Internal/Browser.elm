module Css.Internal.Browser 
  ( BrowserPrefix, toBrowserPrefix, stringPrefix
  ) where
  
type BrowserPrefix = BrowserPrefix String

toBrowserPrefix : String -> BrowserPrefix
toBrowserPrefix str = BrowserPrefix str

stringPrefix : BrowserPrefix -> String
stringPrefix (BrowserPrefix str) = str
