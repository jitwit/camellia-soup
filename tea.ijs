NB. because obviously J is more fun to work in that racket... outside
NB. of scraping etc
load '../jexp/jexp.ijs'

'D T' =: sexp dat =: 1!:1 < 'data/teas.txt'
price =: ,. 50 * ". > T #~ _1 |. T =/ <'price'
tea =: > T #~ _1 |. T =/ <'tea'
type =: > T #~ _1 |. T =/ <'type'
fwheel =: ;: 'floweriness fruitiness woodiness earthiness spiciness vegetativeness'
flavor =: ". > _6 ]\ T #~ _1 |. T e. fwheel
caffeine =: ,.  __ ". > T #~ _1 |. T =/ < 'caffeine'
hd =: ;:'tea type price flavor caffeine'
db =: tea ; type ; price ; flavor ; caffeine

gogo =: 3 : 0
 db =. (+./"1 y E."1 type)&# &.> db
 ord =. \: 2 {:: db
 db =. ord&{ &.> db
 ord =. \: 1 ({"1) 3 {:: db
 db =. -.&' '"1 &.> hd ,: ord&{ &.> db
)

fwheel
gogo 'wulong'
