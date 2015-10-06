module Css.Elements where

import Css.Selector exposing (Selector, star, text)

import Css.Stylesheet exposing (CssGenerator, assignSelector)
-- From: http://www.whatwg.org/specs/web-apps/current-work/multipage/section-index.html#index

{-| The star selector applies to all elements. Maps to @*@ in CSS.
-}
star : List (CssGenerator a) -> CssGenerator Selector
star = Css.Selector.star |> assignSelector

{-| Select elements by name. The preferred syntax is to just use one of
the predefined elements from "Css.Elements".
-}
element : String -> List (CssGenerator a) -> CssGenerator Selector
element name = text name |> assignSelector

{-| Special cases, these items occur both as an HTML tag and an HTML
attribute. These will need qualified imports. -}
abbr : List (CssGenerator a) -> CssGenerator Selector
abbr = "abbr" |> text |> assignSelector

cite : List (CssGenerator a) -> CssGenerator Selector
cite = "cite" |> text |> assignSelector

command : List (CssGenerator a) -> CssGenerator Selector
command = "command" |> text |> assignSelector

data_ : List (CssGenerator a) -> CssGenerator Selector
data_ = "data" |> text |> assignSelector

form : List (CssGenerator a) -> CssGenerator Selector
form = "form" |> text |> assignSelector

label : List (CssGenerator a) -> CssGenerator Selector
label = "label" |> text |> assignSelector

span : List (CssGenerator a) -> CssGenerator Selector
span = "span" |> text |> assignSelector

style : List (CssGenerator a) -> CssGenerator Selector
style = "style" |> text |> assignSelector

title : List (CssGenerator a) -> CssGenerator Selector
title = "title" |> text |> assignSelector


a : List (CssGenerator a) -> CssGenerator Selector
a = "a" |> text |> assignSelector

address : List (CssGenerator a) -> CssGenerator Selector
address = "address" |> text |> assignSelector

area : List (CssGenerator a) -> CssGenerator Selector
area = "area" |> text |> assignSelector

article : List (CssGenerator a) -> CssGenerator Selector
article = "article" |> text |> assignSelector

aside : List (CssGenerator a) -> CssGenerator Selector
aside = "aside" |> text |> assignSelector

audio : List (CssGenerator a) -> CssGenerator Selector
audio = "audio" |> text |> assignSelector

b : List (CssGenerator a) -> CssGenerator Selector
b = "b" |> text |> assignSelector

base : List (CssGenerator a) -> CssGenerator Selector
base = "base" |> text |> assignSelector

bdi : List (CssGenerator a) -> CssGenerator Selector
bdi = "bdi" |> text |> assignSelector

bdo : List (CssGenerator a) -> CssGenerator Selector
bdo = "bdo" |> text |> assignSelector

blockquote : List (CssGenerator a) -> CssGenerator Selector
blockquote = "blockquote" |> text |> assignSelector

body : List (CssGenerator a) -> CssGenerator Selector
body = "body" |> text |> assignSelector

br : List (CssGenerator a) -> CssGenerator Selector
br = "br" |> text |> assignSelector

button : List (CssGenerator a) -> CssGenerator Selector
button = "button" |> text |> assignSelector

canvas : List (CssGenerator a) -> CssGenerator Selector
canvas = "canvas" |> text |> assignSelector

caption : List (CssGenerator a) -> CssGenerator Selector
caption = "caption" |> text |> assignSelector

code : List (CssGenerator a) -> CssGenerator Selector
code = "code" |> text |> assignSelector

col : List (CssGenerator a) -> CssGenerator Selector
col = "col" |> text |> assignSelector

colgroup : List (CssGenerator a) -> CssGenerator Selector
colgroup = "colgroup" |> text |> assignSelector

datalist : List (CssGenerator a) -> CssGenerator Selector
datalist = "datalist" |> text |> assignSelector

dd : List (CssGenerator a) -> CssGenerator Selector
dd = "dd" |> text |> assignSelector

del : List (CssGenerator a) -> CssGenerator Selector
del = "del" |> text |> assignSelector

details : List (CssGenerator a) -> CssGenerator Selector
details = "details" |> text |> assignSelector

dfn : List (CssGenerator a) -> CssGenerator Selector
dfn = "dfn" |> text |> assignSelector

dialog : List (CssGenerator a) -> CssGenerator Selector
dialog = "dialog" |> text |> assignSelector

div : List (CssGenerator a) -> CssGenerator Selector
div = "div" |> text |> assignSelector

dl : List (CssGenerator a) -> CssGenerator Selector
dl = "dl" |> text |> assignSelector

dt : List (CssGenerator a) -> CssGenerator Selector
dt = "dt" |> text |> assignSelector

em : List (CssGenerator a) -> CssGenerator Selector
em = "em" |> text |> assignSelector

embed : List (CssGenerator a) -> CssGenerator Selector
embed = "embed" |> text |> assignSelector

fieldset : List (CssGenerator a) -> CssGenerator Selector
fieldset = "fieldset" |> text |> assignSelector

figcaption : List (CssGenerator a) -> CssGenerator Selector
figcaption = "figcaption" |> text |> assignSelector

figure : List (CssGenerator a) -> CssGenerator Selector
figure = "figure" |> text |> assignSelector

footer : List (CssGenerator a) -> CssGenerator Selector
footer = "footer" |> text |> assignSelector

h1 : List (CssGenerator a) -> CssGenerator Selector
h1 = "h1" |> text |> assignSelector

h2 : List (CssGenerator a) -> CssGenerator Selector
h2 = "h2" |> text |> assignSelector

h3 : List (CssGenerator a) -> CssGenerator Selector
h3 = "h3" |> text |> assignSelector

h4 : List (CssGenerator a) -> CssGenerator Selector
h4 = "h4" |> text |> assignSelector

h5 : List (CssGenerator a) -> CssGenerator Selector
h5 = "h5" |> text |> assignSelector

h6 : List (CssGenerator a) -> CssGenerator Selector
h6 = "h6" |> text |> assignSelector

head : List (CssGenerator a) -> CssGenerator Selector
head = "head" |> text |> assignSelector

header : List (CssGenerator a) -> CssGenerator Selector
header = "header" |> text |> assignSelector

hgroup : List (CssGenerator a) -> CssGenerator Selector
hgroup = "hgroup" |> text |> assignSelector

hr : List (CssGenerator a) -> CssGenerator Selector
hr = "hr" |> text |> assignSelector

html : List (CssGenerator a) -> CssGenerator Selector
html = "html" |> text |> assignSelector

i : List (CssGenerator a) -> CssGenerator Selector
i = "i" |> text |> assignSelector

iframe : List (CssGenerator a) -> CssGenerator Selector
iframe = "iframe" |> text |> assignSelector

img : List (CssGenerator a) -> CssGenerator Selector
img = "img" |> text |> assignSelector

input : List (CssGenerator a) -> CssGenerator Selector
input = "input" |> text |> assignSelector

ins : List (CssGenerator a) -> CssGenerator Selector
ins = "ins" |> text |> assignSelector

kbd : List (CssGenerator a) -> CssGenerator Selector
kbd = "kbd" |> text |> assignSelector

keygen : List (CssGenerator a) -> CssGenerator Selector
keygen = "keygen" |> text |> assignSelector

legend : List (CssGenerator a) -> CssGenerator Selector
legend = "legend" |> text |> assignSelector

li : List (CssGenerator a) -> CssGenerator Selector
li = "li" |> text |> assignSelector

link : List (CssGenerator a) -> CssGenerator Selector
link = "link" |> text |> assignSelector

main_ : List (CssGenerator a) -> CssGenerator Selector
main_ = "main" |> text |> assignSelector

map : List (CssGenerator a) -> CssGenerator Selector
map = "map" |> text |> assignSelector

mark : List (CssGenerator a) -> CssGenerator Selector
mark = "mark" |> text |> assignSelector

menu : List (CssGenerator a) -> CssGenerator Selector
menu = "menu" |> text |> assignSelector

meta : List (CssGenerator a) -> CssGenerator Selector
meta = "meta" |> text |> assignSelector

meter : List (CssGenerator a) -> CssGenerator Selector
meter = "meter" |> text |> assignSelector

math : List (CssGenerator a) -> CssGenerator Selector
math = "math" |> text |> assignSelector

nav : List (CssGenerator a) -> CssGenerator Selector
nav = "nav" |> text |> assignSelector

noscript : List (CssGenerator a) -> CssGenerator Selector
noscript = "noscript" |> text |> assignSelector

object : List (CssGenerator a) -> CssGenerator Selector
object = "object" |> text |> assignSelector

ol : List (CssGenerator a) -> CssGenerator Selector
ol = "ol" |> text |> assignSelector

optgroup : List (CssGenerator a) -> CssGenerator Selector
optgroup = "optgroup" |> text |> assignSelector

option : List (CssGenerator a) -> CssGenerator Selector
option = "option" |> text |> assignSelector

output : List (CssGenerator a) -> CssGenerator Selector
output = "output" |> text |> assignSelector

p : List (CssGenerator a) -> CssGenerator Selector
p = "p" |> text |> assignSelector

param : List (CssGenerator a) -> CssGenerator Selector
param = "param" |> text |> assignSelector

pre : List (CssGenerator a) -> CssGenerator Selector
pre = "pre" |> text |> assignSelector

progress : List (CssGenerator a) -> CssGenerator Selector
progress = "progress" |> text |> assignSelector

q : List (CssGenerator a) -> CssGenerator Selector
q = "q" |> text |> assignSelector

rp : List (CssGenerator a) -> CssGenerator Selector
rp = "rp" |> text |> assignSelector

rt : List (CssGenerator a) -> CssGenerator Selector
rt = "rt" |> text |> assignSelector

ruby : List (CssGenerator a) -> CssGenerator Selector
ruby = "ruby" |> text |> assignSelector

s : List (CssGenerator a) -> CssGenerator Selector
s = "s" |> text |> assignSelector

samp : List (CssGenerator a) -> CssGenerator Selector
samp = "samp" |> text |> assignSelector

script : List (CssGenerator a) -> CssGenerator Selector
script = "script" |> text |> assignSelector

section : List (CssGenerator a) -> CssGenerator Selector
section = "section" |> text |> assignSelector

select : List (CssGenerator a) -> CssGenerator Selector
select = "select" |> text |> assignSelector

small : List (CssGenerator a) -> CssGenerator Selector
small = "small" |> text |> assignSelector

source : List (CssGenerator a) -> CssGenerator Selector
source = "source" |> text |> assignSelector

strong : List (CssGenerator a) -> CssGenerator Selector
strong = "strong" |> text |> assignSelector

sub : List (CssGenerator a) -> CssGenerator Selector
sub = "sub" |> text |> assignSelector

summary : List (CssGenerator a) -> CssGenerator Selector
summary = "summary" |> text |> assignSelector

sup : List (CssGenerator a) -> CssGenerator Selector
sup = "sup" |> text |> assignSelector

svg : List (CssGenerator a) -> CssGenerator Selector
svg = "svg" |> text |> assignSelector

table : List (CssGenerator a) -> CssGenerator Selector
table = "table" |> text |> assignSelector

tbody : List (CssGenerator a) -> CssGenerator Selector
tbody = "tbody" |> text |> assignSelector

td : List (CssGenerator a) -> CssGenerator Selector
td = "td" |> text |> assignSelector

template : List (CssGenerator a) -> CssGenerator Selector
template = "template" |> text |> assignSelector

textarea : List (CssGenerator a) -> CssGenerator Selector
textarea = "textarea" |> text |> assignSelector

tfoot : List (CssGenerator a) -> CssGenerator Selector
tfoot = "tfoot" |> text |> assignSelector

th : List (CssGenerator a) -> CssGenerator Selector
th = "th" |> text |> assignSelector

thead : List (CssGenerator a) -> CssGenerator Selector
thead = "thead" |> text |> assignSelector

time : List (CssGenerator a) -> CssGenerator Selector
time = "time" |> text |> assignSelector

tr : List (CssGenerator a) -> CssGenerator Selector
tr = "tr" |> text |> assignSelector

track : List (CssGenerator a) -> CssGenerator Selector
track = "track" |> text |> assignSelector

u : List (CssGenerator a) -> CssGenerator Selector
u = "u" |> text |> assignSelector

ul : List (CssGenerator a) -> CssGenerator Selector
ul = "ul" |> text |> assignSelector

var : List (CssGenerator a) -> CssGenerator Selector
var = "var" |> text |> assignSelector

video : List (CssGenerator a) -> CssGenerator Selector
video = "video" |> text |> assignSelector

wbr : List (CssGenerator a) -> CssGenerator Selector
wbr = "wbr" |> text |> assignSelector
