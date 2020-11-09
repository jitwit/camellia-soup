NB. because obviously J is more fun to work in that racket... outside
NB. of scraping etc
load '../jexp/jexp.ijs'

'D T' =: sexp dat =: 1!:1 < 'data/teas.txt'
price =: ,. 50 * ". > T #~ _1 |. T =/ <'price'
tea =: }. &> T #~ _1 |. T =/ <'tea'
type =: }. &> T #~ _1 |. T =/ <'type'
fwheel =: ;: 'floweriness fruitiness woodiness earthiness spiciness vegetativeness'
flavor =: ". > _6 ]\ T #~ _1 |. T e. fwheel
caffeine =: ,.  __ ". > T #~ _1 |. T =/ < 'caffeine'
db =: tea ; type ; price ; flavor ; caffeine
]db =: ((/:price)&{) &.> db
