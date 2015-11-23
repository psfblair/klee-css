module Css.Elements where

import Css.Internal.Selector as Selector
import Css.Internal.SelectorCombinators as Combinators

-------------------------------------------------------------------------------

-- From: http://www.whatwg.org/specs/web-apps/current-work/multipage/section-index.html#index

{-| The star selector applies to all elements. Maps to @*@ in CSS.
-}
star : Combinators.Selector
star = Selector.star |> Combinators.createSelector

{-| Special cases, these items occur both as an HTML tag and an HTML
attribute. These will need qualified imports. -}
abbr : Combinators.Selector
abbr = "abbr" |> Selector.selectorDataFromString |> Combinators.createSelector

cite : Combinators.Selector
cite = "cite" |> Selector.selectorDataFromString |> Combinators.createSelector

command : Combinators.Selector
command = "command" |> Selector.selectorDataFromString |> Combinators.createSelector

data_ : Combinators.Selector
data_ = "data" |> Selector.selectorDataFromString |> Combinators.createSelector

form : Combinators.Selector
form = "form" |> Selector.selectorDataFromString |> Combinators.createSelector

label : Combinators.Selector
label = "label" |> Selector.selectorDataFromString |> Combinators.createSelector

span : Combinators.Selector
span = "span" |> Selector.selectorDataFromString |> Combinators.createSelector

style : Combinators.Selector
style = "style" |> Selector.selectorDataFromString |> Combinators.createSelector

title : Combinators.Selector
title = "title" |> Selector.selectorDataFromString |> Combinators.createSelector


a : Combinators.Selector
a = "a" |> Selector.selectorDataFromString |> Combinators.createSelector

address : Combinators.Selector
address = "address" |> Selector.selectorDataFromString |> Combinators.createSelector

area : Combinators.Selector
area = "area" |> Selector.selectorDataFromString |> Combinators.createSelector

article : Combinators.Selector
article = "article" |> Selector.selectorDataFromString |> Combinators.createSelector

aside : Combinators.Selector
aside = "aside" |> Selector.selectorDataFromString |> Combinators.createSelector

audio : Combinators.Selector
audio = "audio" |> Selector.selectorDataFromString |> Combinators.createSelector

b : Combinators.Selector
b = "b" |> Selector.selectorDataFromString |> Combinators.createSelector

base : Combinators.Selector
base = "base" |> Selector.selectorDataFromString |> Combinators.createSelector

bdi : Combinators.Selector
bdi = "bdi" |> Selector.selectorDataFromString |> Combinators.createSelector

bdo : Combinators.Selector
bdo = "bdo" |> Selector.selectorDataFromString |> Combinators.createSelector

blockquote : Combinators.Selector
blockquote = "blockquote" |> Selector.selectorDataFromString |> Combinators.createSelector

body : Combinators.Selector
body = "body" |> Selector.selectorDataFromString |> Combinators.createSelector

br : Combinators.Selector
br = "br" |> Selector.selectorDataFromString |> Combinators.createSelector

button : Combinators.Selector
button = "button" |> Selector.selectorDataFromString |> Combinators.createSelector

canvas : Combinators.Selector
canvas = "canvas" |> Selector.selectorDataFromString |> Combinators.createSelector

caption : Combinators.Selector
caption = "caption" |> Selector.selectorDataFromString |> Combinators.createSelector

code : Combinators.Selector
code = "code" |> Selector.selectorDataFromString |> Combinators.createSelector

col : Combinators.Selector
col = "col" |> Selector.selectorDataFromString |> Combinators.createSelector

colgroup : Combinators.Selector
colgroup = "colgroup" |> Selector.selectorDataFromString |> Combinators.createSelector

datalist : Combinators.Selector
datalist = "datalist" |> Selector.selectorDataFromString |> Combinators.createSelector

dd : Combinators.Selector
dd = "dd" |> Selector.selectorDataFromString |> Combinators.createSelector

del : Combinators.Selector
del = "del" |> Selector.selectorDataFromString |> Combinators.createSelector

details : Combinators.Selector
details = "details" |> Selector.selectorDataFromString |> Combinators.createSelector

dfn : Combinators.Selector
dfn = "dfn" |> Selector.selectorDataFromString |> Combinators.createSelector

dialog : Combinators.Selector
dialog = "dialog" |> Selector.selectorDataFromString |> Combinators.createSelector

div : Combinators.Selector
div = "div" |> Selector.selectorDataFromString |> Combinators.createSelector

dl : Combinators.Selector
dl = "dl" |> Selector.selectorDataFromString |> Combinators.createSelector

dt : Combinators.Selector
dt = "dt" |> Selector.selectorDataFromString |> Combinators.createSelector

em : Combinators.Selector
em = "em" |> Selector.selectorDataFromString |> Combinators.createSelector

embed : Combinators.Selector
embed = "embed" |> Selector.selectorDataFromString |> Combinators.createSelector

fieldset : Combinators.Selector
fieldset = "fieldset" |> Selector.selectorDataFromString |> Combinators.createSelector

figcaption : Combinators.Selector
figcaption = "figcaption" |> Selector.selectorDataFromString |> Combinators.createSelector

figure : Combinators.Selector
figure = "figure" |> Selector.selectorDataFromString |> Combinators.createSelector

footer : Combinators.Selector
footer = "footer" |> Selector.selectorDataFromString |> Combinators.createSelector

h1 : Combinators.Selector
h1 = "h1" |> Selector.selectorDataFromString |> Combinators.createSelector

h2 : Combinators.Selector
h2 = "h2" |> Selector.selectorDataFromString |> Combinators.createSelector

h3 : Combinators.Selector
h3 = "h3" |> Selector.selectorDataFromString |> Combinators.createSelector

h4 : Combinators.Selector
h4 = "h4" |> Selector.selectorDataFromString |> Combinators.createSelector

h5 : Combinators.Selector
h5 = "h5" |> Selector.selectorDataFromString |> Combinators.createSelector

h6 : Combinators.Selector
h6 = "h6" |> Selector.selectorDataFromString |> Combinators.createSelector

head : Combinators.Selector
head = "head" |> Selector.selectorDataFromString |> Combinators.createSelector

header : Combinators.Selector
header = "header" |> Selector.selectorDataFromString |> Combinators.createSelector

hgroup : Combinators.Selector
hgroup = "hgroup" |> Selector.selectorDataFromString |> Combinators.createSelector

hr : Combinators.Selector
hr = "hr" |> Selector.selectorDataFromString |> Combinators.createSelector

html : Combinators.Selector
html = "html" |> Selector.selectorDataFromString |> Combinators.createSelector

i : Combinators.Selector
i = "i" |> Selector.selectorDataFromString |> Combinators.createSelector

iframe : Combinators.Selector
iframe = "iframe" |> Selector.selectorDataFromString |> Combinators.createSelector

img : Combinators.Selector
img = "img" |> Selector.selectorDataFromString |> Combinators.createSelector

input : Combinators.Selector
input = "input" |> Selector.selectorDataFromString |> Combinators.createSelector

ins : Combinators.Selector
ins = "ins" |> Selector.selectorDataFromString |> Combinators.createSelector

kbd : Combinators.Selector
kbd = "kbd" |> Selector.selectorDataFromString |> Combinators.createSelector

keygen : Combinators.Selector
keygen = "keygen" |> Selector.selectorDataFromString |> Combinators.createSelector

legend : Combinators.Selector
legend = "legend" |> Selector.selectorDataFromString |> Combinators.createSelector

li : Combinators.Selector
li = "li" |> Selector.selectorDataFromString |> Combinators.createSelector

link : Combinators.Selector
link = "link" |> Selector.selectorDataFromString |> Combinators.createSelector

main_ : Combinators.Selector
main_ = "main" |> Selector.selectorDataFromString |> Combinators.createSelector

map : Combinators.Selector
map = "map" |> Selector.selectorDataFromString |> Combinators.createSelector

mark : Combinators.Selector
mark = "mark" |> Selector.selectorDataFromString |> Combinators.createSelector

menu : Combinators.Selector
menu = "menu" |> Selector.selectorDataFromString |> Combinators.createSelector

meta : Combinators.Selector
meta = "meta" |> Selector.selectorDataFromString |> Combinators.createSelector

meter : Combinators.Selector
meter = "meter" |> Selector.selectorDataFromString |> Combinators.createSelector

math : Combinators.Selector
math = "math" |> Selector.selectorDataFromString |> Combinators.createSelector

nav : Combinators.Selector
nav = "nav" |> Selector.selectorDataFromString |> Combinators.createSelector

noscript : Combinators.Selector
noscript = "noscript" |> Selector.selectorDataFromString |> Combinators.createSelector

object : Combinators.Selector
object = "object" |> Selector.selectorDataFromString |> Combinators.createSelector

ol : Combinators.Selector
ol = "ol" |> Selector.selectorDataFromString |> Combinators.createSelector

optgroup : Combinators.Selector
optgroup = "optgroup" |> Selector.selectorDataFromString |> Combinators.createSelector

option : Combinators.Selector
option = "option" |> Selector.selectorDataFromString |> Combinators.createSelector

output : Combinators.Selector
output = "output" |> Selector.selectorDataFromString |> Combinators.createSelector

p : Combinators.Selector
p = "p" |> Selector.selectorDataFromString |> Combinators.createSelector

param : Combinators.Selector
param = "param" |> Selector.selectorDataFromString |> Combinators.createSelector

pre : Combinators.Selector
pre = "pre" |> Selector.selectorDataFromString |> Combinators.createSelector

progress : Combinators.Selector
progress = "progress" |> Selector.selectorDataFromString |> Combinators.createSelector

q : Combinators.Selector
q = "q" |> Selector.selectorDataFromString |> Combinators.createSelector

rp : Combinators.Selector
rp = "rp" |> Selector.selectorDataFromString |> Combinators.createSelector

rt : Combinators.Selector
rt = "rt" |> Selector.selectorDataFromString |> Combinators.createSelector

ruby : Combinators.Selector
ruby = "ruby" |> Selector.selectorDataFromString |> Combinators.createSelector

s : Combinators.Selector
s = "s" |> Selector.selectorDataFromString |> Combinators.createSelector

samp : Combinators.Selector
samp = "samp" |> Selector.selectorDataFromString |> Combinators.createSelector

script : Combinators.Selector
script = "script" |> Selector.selectorDataFromString |> Combinators.createSelector

section : Combinators.Selector
section = "section" |> Selector.selectorDataFromString |> Combinators.createSelector

select : Combinators.Selector
select = "select" |> Selector.selectorDataFromString |> Combinators.createSelector

small : Combinators.Selector
small = "small" |> Selector.selectorDataFromString |> Combinators.createSelector

source : Combinators.Selector
source = "source" |> Selector.selectorDataFromString |> Combinators.createSelector

strong : Combinators.Selector
strong = "strong" |> Selector.selectorDataFromString |> Combinators.createSelector

sub : Combinators.Selector
sub = "sub" |> Selector.selectorDataFromString |> Combinators.createSelector

summary : Combinators.Selector
summary = "summary" |> Selector.selectorDataFromString |> Combinators.createSelector

sup : Combinators.Selector
sup = "sup" |> Selector.selectorDataFromString |> Combinators.createSelector

svg : Combinators.Selector
svg = "svg" |> Selector.selectorDataFromString |> Combinators.createSelector

table : Combinators.Selector
table = "table" |> Selector.selectorDataFromString |> Combinators.createSelector

tbody : Combinators.Selector
tbody = "tbody" |> Selector.selectorDataFromString |> Combinators.createSelector

td : Combinators.Selector
td = "td" |> Selector.selectorDataFromString |> Combinators.createSelector

template : Combinators.Selector
template = "template" |> Selector.selectorDataFromString |> Combinators.createSelector

textarea : Combinators.Selector
textarea = "textarea" |> Selector.selectorDataFromString |> Combinators.createSelector

tfoot : Combinators.Selector
tfoot = "tfoot" |> Selector.selectorDataFromString |> Combinators.createSelector

th : Combinators.Selector
th = "th" |> Selector.selectorDataFromString |> Combinators.createSelector

thead : Combinators.Selector
thead = "thead" |> Selector.selectorDataFromString |> Combinators.createSelector

time : Combinators.Selector
time = "time" |> Selector.selectorDataFromString |> Combinators.createSelector

tr : Combinators.Selector
tr = "tr" |> Selector.selectorDataFromString |> Combinators.createSelector

track : Combinators.Selector
track = "track" |> Selector.selectorDataFromString |> Combinators.createSelector

u : Combinators.Selector
u = "u" |> Selector.selectorDataFromString |> Combinators.createSelector

ul : Combinators.Selector
ul = "ul" |> Selector.selectorDataFromString |> Combinators.createSelector

var : Combinators.Selector
var = "var" |> Selector.selectorDataFromString |> Combinators.createSelector

video : Combinators.Selector
video = "video" |> Selector.selectorDataFromString |> Combinators.createSelector

wbr : Combinators.Selector
wbr = "wbr" |> Selector.selectorDataFromString |> Combinators.createSelector
