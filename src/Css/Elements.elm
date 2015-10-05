module Css.Elements where

import Css.Selector exposing (text)

-- From: http://www.whatwg.org/specs/web-apps/current-work/multipage/section-index.html#index

{-| Special cases, these items occur both as an HTML tag and an HTML
attribute. These will need qualified imports. -}

abbr          = "abbr"        |> text
cite          = "cite"        |> text
command       = "command"     |> text
data_         = "data"        |> text
form          = "form"        |> text
label         = "label"       |> text
span          = "span"        |> text
style         = "style"       |> text
title         = "title"       |> text

a             = "a"           |> text
address       = "address"     |> text
area          = "area"        |> text
article       = "article"     |> text
aside         = "aside"       |> text
audio         = "audio"       |> text
b             = "b"           |> text
base          = "base"        |> text
bdi           = "bdi"         |> text
bdo           = "bdo"         |> text
blockquote    = "blockquote"  |> text
body          = "body"        |> text
br            = "br"          |> text
button        = "button"      |> text
canvas        = "canvas"      |> text
caption       = "caption"     |> text
code          = "code"        |> text
col           = "col"         |> text
colgroup      = "colgroup"    |> text
datalist      = "datalist"    |> text
dd            = "dd"          |> text
del           = "del"         |> text
details       = "details"     |> text
dfn           = "dfn"         |> text
dialog        = "dialog"      |> text
div           = "div"         |> text
dl            = "dl"          |> text
dt            = "dt"          |> text
em            = "em"          |> text
embed         = "embed"       |> text
fieldset      = "fieldset"    |> text
figcaption    = "figcaption"  |> text
figure        = "figure"      |> text
footer        = "footer"      |> text
h1            = "h1"          |> text
h2            = "h2"          |> text
h3            = "h3"          |> text
h4            = "h4"          |> text
h5            = "h5"          |> text
h6            = "h6"          |> text
head          = "head"        |> text
header        = "header"      |> text
hgroup        = "hgroup"      |> text
hr            = "hr"          |> text
html          = "html"        |> text
i             = "i"           |> text
iframe        = "iframe"      |> text
img           = "img"         |> text
input         = "input"       |> text
ins           = "ins"         |> text
kbd           = "kbd"         |> text
keygen        = "keygen"      |> text
legend        = "legend"      |> text
li            = "li"          |> text
link          = "link"        |> text
main_         = "main"        |> text
map           = "map"         |> text
mark          = "mark"        |> text
menu          = "menu"        |> text
meta          = "meta"        |> text
meter         = "meter"       |> text
math          = "math"        |> text
nav           = "nav"         |> text
noscript      = "noscript"    |> text
object        = "object"      |> text
ol            = "ol"          |> text
optgroup      = "optgroup"    |> text
option        = "option"      |> text
output        = "output"      |> text
p             = "p"           |> text
param         = "param"       |> text
pre           = "pre"         |> text
progress      = "progress"    |> text
q             = "q"           |> text
rp            = "rp"          |> text
rt            = "rt"          |> text
ruby          = "ruby"        |> text
s             = "s"           |> text
samp          = "samp"        |> text
script        = "script"      |> text
section       = "section"     |> text
select        = "select"      |> text
small         = "small"       |> text
source        = "source"      |> text
strong        = "strong"      |> text
sub           = "sub"         |> text
summary       = "summary"     |> text
sup           = "sup"         |> text
svg           = "svg"         |> text
table         = "table"       |> text
tbody         = "tbody"       |> text
td            = "td"          |> text
template      = "template"    |> text
textarea      = "textarea"    |> text
tfoot         = "tfoot"       |> text
th            = "th"          |> text
thead         = "thead"       |> text
time          = "time"        |> text
tr            = "tr"          |> text
track         = "track"       |> text
u             = "u"           |> text
ul            = "ul"          |> text
var           = "var"         |> text
video         = "video"       |> text
wbr           = "wbr"         |> text
