module Css.Elements where

import Css.Internal.Selector exposing (SelectorData, star, selectorDataFromString)
import Css.Internal.SelectorCombinators exposing (Selector, createSelector)

-------------------------------------------------------------------------------

-- From: http://www.whatwg.org/specs/web-apps/current-work/multipage/section-index.html#index

{-| The star selector applies to all elements. Maps to @*@ in CSS.
-}
star : Selector
star = Css.Internal.Selector.star |> createSelector

{-| Special cases, these items occur both as an HTML tag and an HTML
attribute. These will need qualified imports. -}
abbr : Selector
abbr = "abbr" |> selectorDataFromString |> createSelector

cite : Selector
cite = "cite" |> selectorDataFromString |> createSelector

command : Selector
command = "command" |> selectorDataFromString |> createSelector

data_ : Selector
data_ = "data" |> selectorDataFromString |> createSelector

form : Selector
form = "form" |> selectorDataFromString |> createSelector

label : Selector
label = "label" |> selectorDataFromString |> createSelector

span : Selector
span = "span" |> selectorDataFromString |> createSelector

style : Selector
style = "style" |> selectorDataFromString |> createSelector

title : Selector
title = "title" |> selectorDataFromString |> createSelector


a : Selector
a = "a" |> selectorDataFromString |> createSelector

address : Selector
address = "address" |> selectorDataFromString |> createSelector

area : Selector
area = "area" |> selectorDataFromString |> createSelector

article : Selector
article = "article" |> selectorDataFromString |> createSelector

aside : Selector
aside = "aside" |> selectorDataFromString |> createSelector

audio : Selector
audio = "audio" |> selectorDataFromString |> createSelector

b : Selector
b = "b" |> selectorDataFromString |> createSelector

base : Selector
base = "base" |> selectorDataFromString |> createSelector

bdi : Selector
bdi = "bdi" |> selectorDataFromString |> createSelector

bdo : Selector
bdo = "bdo" |> selectorDataFromString |> createSelector

blockquote : Selector
blockquote = "blockquote" |> selectorDataFromString |> createSelector

body : Selector
body = "body" |> selectorDataFromString |> createSelector

br : Selector
br = "br" |> selectorDataFromString |> createSelector

button : Selector
button = "button" |> selectorDataFromString |> createSelector

canvas : Selector
canvas = "canvas" |> selectorDataFromString |> createSelector

caption : Selector
caption = "caption" |> selectorDataFromString |> createSelector

code : Selector
code = "code" |> selectorDataFromString |> createSelector

col : Selector
col = "col" |> selectorDataFromString |> createSelector

colgroup : Selector
colgroup = "colgroup" |> selectorDataFromString |> createSelector

datalist : Selector
datalist = "datalist" |> selectorDataFromString |> createSelector

dd : Selector
dd = "dd" |> selectorDataFromString |> createSelector

del : Selector
del = "del" |> selectorDataFromString |> createSelector

details : Selector
details = "details" |> selectorDataFromString |> createSelector

dfn : Selector
dfn = "dfn" |> selectorDataFromString |> createSelector

dialog : Selector
dialog = "dialog" |> selectorDataFromString |> createSelector

div : Selector
div = "div" |> selectorDataFromString |> createSelector

dl : Selector
dl = "dl" |> selectorDataFromString |> createSelector

dt : Selector
dt = "dt" |> selectorDataFromString |> createSelector

em : Selector
em = "em" |> selectorDataFromString |> createSelector

embed : Selector
embed = "embed" |> selectorDataFromString |> createSelector

fieldset : Selector
fieldset = "fieldset" |> selectorDataFromString |> createSelector

figcaption : Selector
figcaption = "figcaption" |> selectorDataFromString |> createSelector

figure : Selector
figure = "figure" |> selectorDataFromString |> createSelector

footer : Selector
footer = "footer" |> selectorDataFromString |> createSelector

h1 : Selector
h1 = "h1" |> selectorDataFromString |> createSelector

h2 : Selector
h2 = "h2" |> selectorDataFromString |> createSelector

h3 : Selector
h3 = "h3" |> selectorDataFromString |> createSelector

h4 : Selector
h4 = "h4" |> selectorDataFromString |> createSelector

h5 : Selector
h5 = "h5" |> selectorDataFromString |> createSelector

h6 : Selector
h6 = "h6" |> selectorDataFromString |> createSelector

head : Selector
head = "head" |> selectorDataFromString |> createSelector

header : Selector
header = "header" |> selectorDataFromString |> createSelector

hgroup : Selector
hgroup = "hgroup" |> selectorDataFromString |> createSelector

hr : Selector
hr = "hr" |> selectorDataFromString |> createSelector

html : Selector
html = "html" |> selectorDataFromString |> createSelector

i : Selector
i = "i" |> selectorDataFromString |> createSelector

iframe : Selector
iframe = "iframe" |> selectorDataFromString |> createSelector

img : Selector
img = "img" |> selectorDataFromString |> createSelector

input : Selector
input = "input" |> selectorDataFromString |> createSelector

ins : Selector
ins = "ins" |> selectorDataFromString |> createSelector

kbd : Selector
kbd = "kbd" |> selectorDataFromString |> createSelector

keygen : Selector
keygen = "keygen" |> selectorDataFromString |> createSelector

legend : Selector
legend = "legend" |> selectorDataFromString |> createSelector

li : Selector
li = "li" |> selectorDataFromString |> createSelector

link : Selector
link = "link" |> selectorDataFromString |> createSelector

main_ : Selector
main_ = "main" |> selectorDataFromString |> createSelector

map : Selector
map = "map" |> selectorDataFromString |> createSelector

mark : Selector
mark = "mark" |> selectorDataFromString |> createSelector

menu : Selector
menu = "menu" |> selectorDataFromString |> createSelector

meta : Selector
meta = "meta" |> selectorDataFromString |> createSelector

meter : Selector
meter = "meter" |> selectorDataFromString |> createSelector

math : Selector
math = "math" |> selectorDataFromString |> createSelector

nav : Selector
nav = "nav" |> selectorDataFromString |> createSelector

noscript : Selector
noscript = "noscript" |> selectorDataFromString |> createSelector

object : Selector
object = "object" |> selectorDataFromString |> createSelector

ol : Selector
ol = "ol" |> selectorDataFromString |> createSelector

optgroup : Selector
optgroup = "optgroup" |> selectorDataFromString |> createSelector

option : Selector
option = "option" |> selectorDataFromString |> createSelector

output : Selector
output = "output" |> selectorDataFromString |> createSelector

p : Selector
p = "p" |> selectorDataFromString |> createSelector

param : Selector
param = "param" |> selectorDataFromString |> createSelector

pre : Selector
pre = "pre" |> selectorDataFromString |> createSelector

progress : Selector
progress = "progress" |> selectorDataFromString |> createSelector

q : Selector
q = "q" |> selectorDataFromString |> createSelector

rp : Selector
rp = "rp" |> selectorDataFromString |> createSelector

rt : Selector
rt = "rt" |> selectorDataFromString |> createSelector

ruby : Selector
ruby = "ruby" |> selectorDataFromString |> createSelector

s : Selector
s = "s" |> selectorDataFromString |> createSelector

samp : Selector
samp = "samp" |> selectorDataFromString |> createSelector

script : Selector
script = "script" |> selectorDataFromString |> createSelector

section : Selector
section = "section" |> selectorDataFromString |> createSelector

select : Selector
select = "select" |> selectorDataFromString |> createSelector

small : Selector
small = "small" |> selectorDataFromString |> createSelector

source : Selector
source = "source" |> selectorDataFromString |> createSelector

strong : Selector
strong = "strong" |> selectorDataFromString |> createSelector

sub : Selector
sub = "sub" |> selectorDataFromString |> createSelector

summary : Selector
summary = "summary" |> selectorDataFromString |> createSelector

sup : Selector
sup = "sup" |> selectorDataFromString |> createSelector

svg : Selector
svg = "svg" |> selectorDataFromString |> createSelector

table : Selector
table = "table" |> selectorDataFromString |> createSelector

tbody : Selector
tbody = "tbody" |> selectorDataFromString |> createSelector

td : Selector
td = "td" |> selectorDataFromString |> createSelector

template : Selector
template = "template" |> selectorDataFromString |> createSelector

textarea : Selector
textarea = "textarea" |> selectorDataFromString |> createSelector

tfoot : Selector
tfoot = "tfoot" |> selectorDataFromString |> createSelector

th : Selector
th = "th" |> selectorDataFromString |> createSelector

thead : Selector
thead = "thead" |> selectorDataFromString |> createSelector

time : Selector
time = "time" |> selectorDataFromString |> createSelector

tr : Selector
tr = "tr" |> selectorDataFromString |> createSelector

track : Selector
track = "track" |> selectorDataFromString |> createSelector

u : Selector
u = "u" |> selectorDataFromString |> createSelector

ul : Selector
ul = "ul" |> selectorDataFromString |> createSelector

var : Selector
var = "var" |> selectorDataFromString |> createSelector

video : Selector
video = "video" |> selectorDataFromString |> createSelector

wbr : Selector
wbr = "wbr" |> selectorDataFromString |> createSelector
