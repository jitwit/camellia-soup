NB. because obviously J is more fun to work in that racket... outside
NB. of scraping etc
load '../jexp/jexp.ijs'

'D T' =: parse dat =: 1!:1 < 'data/teas.txt'

fwheel =: ;: 'floweriness fruitiness woodiness earthiness spiciness vegetativeness'
prices =: ,. 50 * ". > T #~ _1 |. T =/ <'price'
teas =: }. &> T #~ _1 |. T =/ <'tea'
types =: }. &> T #~ _1 |. T =/ <'type'
flavors =: ". > _6 ]\ T #~ _1 |. T e. fwheel

db =: teas ; prices ; flavors ; types
