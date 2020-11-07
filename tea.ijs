NB. because obviously J is more fun to work in that racket... outside
NB. of scraping etc

NB.  (parse-tea "jingning-yin-zhen")                      9
NB.  (parse-tea "anxi-tie-guan-yin")                      7
NB.  (parse-tea "nan-mei-bourgeons-de-theiers-sauvage")   5(9)
NB.  (parse-tea "bai-hao-jingmai-biologique")             7
NB.  (parse-tea "dong-ding-m-chang")                      6
NB.  (parse-tea "rou-gui-mituoyan-de-m-wu")               8

load '../jexp/jexp.ijs'
'D T' =. parse 1!:1 < 'data/teas.txt'

fwheel =. ;: 'floweriness fruitiness woodiness earthiness spiciness vegetativeness'

prices =. ,. 50 * ". > T #~ _1 |. T =/ <'price'
teas =. }. &> T #~ _1 |. T =/ <'tea'
types =. }. &> T #~ _1 |. T =/ <'type'
flavors =. ". > _6 ]\ T #~ _1 |. T e. fwheel

db =. teas ; prices ; flavors ; types

((/: 1{"1 flavors)&{) &.> db
