module Css.Attributes where

import Css.Internal.Selector as Selector

-------------------------------------------------------------------------------

-- From: http://www.whatwg.org/specs/web-apps/current-work/multipage/section-index.html#index

{-| Special cases. These items occur both as an HTML tag and an HTML
attribute. These will need qualified imports.
-}
abbr            = "abbr"              |> Selector.filterFromString
cite            = "cite"              |> Selector.filterFromString
command         = "command"           |> Selector.filterFromString
data_           = "data"              |> Selector.filterFromString
form            = "form"              |> Selector.filterFromString
label           = "label"             |> Selector.filterFromString
span            = "span"              |> Selector.filterFromString
style           = "style"             |> Selector.filterFromString
title           = "title"             |> Selector.filterFromString

{-| HTML Attributes.
-}
accept          = "accept"            |> Selector.filterFromString
acceptCharset   = "accept-charset"    |> Selector.filterFromString
accesskey       = "accesskey"         |> Selector.filterFromString
action          = "action"            |> Selector.filterFromString
alt             = "alt"               |> Selector.filterFromString
async           = "async"             |> Selector.filterFromString
autocomplete    = "autocomplete"      |> Selector.filterFromString
autofocus       = "autofocus"         |> Selector.filterFromString
autoplay        = "autoplay"          |> Selector.filterFromString
challenge       = "challenge"         |> Selector.filterFromString
charset         = "charset"           |> Selector.filterFromString
checked         = "checked"           |> Selector.filterFromString
class_          = "class"             |> Selector.filterFromString
cols            = "cols"              |> Selector.filterFromString
colspan         = "colspan"           |> Selector.filterFromString
content         = "content"           |> Selector.filterFromString
contenteditable = "contenteditable"   |> Selector.filterFromString
contextmenu     = "contextmenu"       |> Selector.filterFromString
controls        = "controls"          |> Selector.filterFromString
coords          = "coords"            |> Selector.filterFromString
crossorigin     = "crossorigin"       |> Selector.filterFromString
datetime        = "datetime"          |> Selector.filterFromString
default_        = "default"           |> Selector.filterFromString
defer           = "defer"             |> Selector.filterFromString
dir             = "dir"               |> Selector.filterFromString
dirname         = "dirname"           |> Selector.filterFromString
disabled        = "disabled"          |> Selector.filterFromString
download        = "download"          |> Selector.filterFromString
draggable       = "draggable"         |> Selector.filterFromString
dropzone        = "dropzone"          |> Selector.filterFromString
enctype         = "enctype"           |> Selector.filterFromString
for             = "for"               |> Selector.filterFromString
formaction      = "formaction"        |> Selector.filterFromString
formenctype     = "formenctype"       |> Selector.filterFromString
formmethod      = "formmethod"        |> Selector.filterFromString
formnovalidate  = "formnovalidate"    |> Selector.filterFromString
formtarget      = "formtarget"        |> Selector.filterFromString
headers         = "headers"           |> Selector.filterFromString
height          = "height"            |> Selector.filterFromString
hidden          = "hidden"            |> Selector.filterFromString
high            = "high"              |> Selector.filterFromString
href            = "href"              |> Selector.filterFromString
hreflang        = "hreflang"          |> Selector.filterFromString
httpEquiv       = "http-equiv"        |> Selector.filterFromString
icon            = "icon"              |> Selector.filterFromString
id              = "id"                |> Selector.filterFromString
inert           = "inert"             |> Selector.filterFromString
inputmode       = "inputmode"         |> Selector.filterFromString
ismap           = "ismap"             |> Selector.filterFromString
itemid          = "itemid"            |> Selector.filterFromString
itemprop        = "itemprop"          |> Selector.filterFromString
itemref         = "itemref"           |> Selector.filterFromString
itemscope       = "itemscope"         |> Selector.filterFromString
itemtype        = "itemtype"          |> Selector.filterFromString
keytype         = "keytype"           |> Selector.filterFromString
kind            = "kind"              |> Selector.filterFromString
lang            = "lang"              |> Selector.filterFromString
list            = "list"              |> Selector.filterFromString
loop            = "loop"              |> Selector.filterFromString
low             = "low"               |> Selector.filterFromString
manifest        = "manifest"          |> Selector.filterFromString
max             = "max"               |> Selector.filterFromString
maxlength       = "maxlength"         |> Selector.filterFromString
media           = "media"             |> Selector.filterFromString
mediagroup      = "mediagroup"        |> Selector.filterFromString
method          = "method"            |> Selector.filterFromString
min             = "min"               |> Selector.filterFromString
multiple        = "multiple"          |> Selector.filterFromString
muted           = "muted"             |> Selector.filterFromString
name            = "name"              |> Selector.filterFromString
novalidate      = "novalidate"        |> Selector.filterFromString
open            = "open"              |> Selector.filterFromString
optimum         = "optimum"           |> Selector.filterFromString
pattern         = "pattern"           |> Selector.filterFromString
ping            = "ping"              |> Selector.filterFromString
placeholder     = "placeholder"       |> Selector.filterFromString
poster          = "poster"            |> Selector.filterFromString
preload         = "preload"           |> Selector.filterFromString
radiogroup      = "radiogroup"        |> Selector.filterFromString
readonly        = "readonly"          |> Selector.filterFromString
rel             = "rel"               |> Selector.filterFromString
required        = "required"          |> Selector.filterFromString
reversed        = "reversed"          |> Selector.filterFromString
rows            = "rows"              |> Selector.filterFromString
rowspan         = "rowspan"           |> Selector.filterFromString
sandbox         = "sandbox"           |> Selector.filterFromString
scope           = "scope"             |> Selector.filterFromString
scoped          = "scoped"            |> Selector.filterFromString
seamless        = "seamless"          |> Selector.filterFromString
selected        = "selected"          |> Selector.filterFromString
shape           = "shape"             |> Selector.filterFromString
size            = "size"              |> Selector.filterFromString
sizes           = "sizes"             |> Selector.filterFromString
spellcheck      = "spellcheck"        |> Selector.filterFromString
src             = "src"               |> Selector.filterFromString
srcdoc          = "srcdoc"            |> Selector.filterFromString
srclang         = "srclang"           |> Selector.filterFromString
srcset          = "srcset"            |> Selector.filterFromString
start           = "start"             |> Selector.filterFromString
step            = "step"              |> Selector.filterFromString
tabindex        = "tabindex"          |> Selector.filterFromString
target          = "target"            |> Selector.filterFromString
translate       = "translate"         |> Selector.filterFromString
type_           = "type"              |> Selector.filterFromString
typemustmatch   = "typemustmatch"     |> Selector.filterFromString
usemap          = "usemap"            |> Selector.filterFromString
value           = "value"             |> Selector.filterFromString
width           = "width"             |> Selector.filterFromString
wrap            = "wrap"              |> Selector.filterFromString
