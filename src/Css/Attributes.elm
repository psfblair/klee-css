module Css.Attributes where

import Css.Selector exposing (filterFromString)

-- From: http://www.whatwg.org/specs/web-apps/current-work/multipage/section-index.html#index

{-| Special cases. These items occur both as an HTML tag and an HTML
attribute. These will need qualified imports.
-}
abbr            = "abbr"              |> filterFromString
cite            = "cite"              |> filterFromString
command         = "command"           |> filterFromString
data_           = "data"              |> filterFromString
form            = "form"              |> filterFromString
label           = "label"             |> filterFromString
span            = "span"              |> filterFromString
style           = "style"             |> filterFromString
title           = "title"             |> filterFromString

{-| HTML Attributes.
-}
accept          = "accept"            |> filterFromString
acceptCharset   = "accept-charset"    |> filterFromString
accesskey       = "accesskey"         |> filterFromString
action          = "action"            |> filterFromString
alt             = "alt"               |> filterFromString
async           = "async"             |> filterFromString
autocomplete    = "autocomplete"      |> filterFromString
autofocus       = "autofocus"         |> filterFromString
autoplay        = "autoplay"          |> filterFromString
challenge       = "challenge"         |> filterFromString
charset         = "charset"           |> filterFromString
checked         = "checked"           |> filterFromString
class_          = "class"             |> filterFromString
cols            = "cols"              |> filterFromString
colspan         = "colspan"           |> filterFromString
content         = "content"           |> filterFromString
contenteditable = "contenteditable"   |> filterFromString
contextmenu     = "contextmenu"       |> filterFromString
controls        = "controls"          |> filterFromString
coords          = "coords"            |> filterFromString
crossorigin     = "crossorigin"       |> filterFromString
datetime        = "datetime"          |> filterFromString
default_        = "default"           |> filterFromString
defer           = "defer"             |> filterFromString
dir             = "dir"               |> filterFromString
dirname         = "dirname"           |> filterFromString
disabled        = "disabled"          |> filterFromString
download        = "download"          |> filterFromString
draggable       = "draggable"         |> filterFromString
dropzone        = "dropzone"          |> filterFromString
enctype         = "enctype"           |> filterFromString
for             = "for"               |> filterFromString
formaction      = "formaction"        |> filterFromString
formenctype     = "formenctype"       |> filterFromString
formmethod      = "formmethod"        |> filterFromString
formnovalidate  = "formnovalidate"    |> filterFromString
formtarget      = "formtarget"        |> filterFromString
headers         = "headers"           |> filterFromString
height          = "height"            |> filterFromString
hidden          = "hidden"            |> filterFromString
high            = "high"              |> filterFromString
href            = "href"              |> filterFromString
hreflang        = "hreflang"          |> filterFromString
httpEquiv       = "http-equiv"        |> filterFromString
icon            = "icon"              |> filterFromString
id              = "id"                |> filterFromString
inert           = "inert"             |> filterFromString
inputmode       = "inputmode"         |> filterFromString
ismap           = "ismap"             |> filterFromString
itemid          = "itemid"            |> filterFromString
itemprop        = "itemprop"          |> filterFromString
itemref         = "itemref"           |> filterFromString
itemscope       = "itemscope"         |> filterFromString
itemtype        = "itemtype"          |> filterFromString
keytype         = "keytype"           |> filterFromString
kind            = "kind"              |> filterFromString
lang            = "lang"              |> filterFromString
list            = "list"              |> filterFromString
loop            = "loop"              |> filterFromString
low             = "low"               |> filterFromString
manifest        = "manifest"          |> filterFromString
max             = "max"               |> filterFromString
maxlength       = "maxlength"         |> filterFromString
media           = "media"             |> filterFromString
mediagroup      = "mediagroup"        |> filterFromString
method          = "method"            |> filterFromString
min             = "min"               |> filterFromString
multiple        = "multiple"          |> filterFromString
muted           = "muted"             |> filterFromString
name            = "name"              |> filterFromString
novalidate      = "novalidate"        |> filterFromString
open            = "open"              |> filterFromString
optimum         = "optimum"           |> filterFromString
pattern         = "pattern"           |> filterFromString
ping            = "ping"              |> filterFromString
placeholder     = "placeholder"       |> filterFromString
poster          = "poster"            |> filterFromString
preload         = "preload"           |> filterFromString
radiogroup      = "radiogroup"        |> filterFromString
readonly        = "readonly"          |> filterFromString
rel             = "rel"               |> filterFromString
required        = "required"          |> filterFromString
reversed        = "reversed"          |> filterFromString
rows            = "rows"              |> filterFromString
rowspan         = "rowspan"           |> filterFromString
sandbox         = "sandbox"           |> filterFromString
scope           = "scope"             |> filterFromString
scoped          = "scoped"            |> filterFromString
seamless        = "seamless"          |> filterFromString
selected        = "selected"          |> filterFromString
shape           = "shape"             |> filterFromString
size            = "size"              |> filterFromString
sizes           = "sizes"             |> filterFromString
spellcheck      = "spellcheck"        |> filterFromString
src             = "src"               |> filterFromString
srcdoc          = "srcdoc"            |> filterFromString
srclang         = "srclang"           |> filterFromString
srcset          = "srcset"            |> filterFromString
start           = "start"             |> filterFromString
step            = "step"              |> filterFromString
tabindex        = "tabindex"          |> filterFromString
target          = "target"            |> filterFromString
translate       = "translate"         |> filterFromString
type_           = "type"              |> filterFromString
typemustmatch   = "typemustmatch"     |> filterFromString
usemap          = "usemap"            |> filterFromString
value           = "value"             |> filterFromString
width           = "width"             |> filterFromString
wrap            = "wrap"              |> filterFromString