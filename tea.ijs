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

teas =. }. &.> t #~ _1 |. (<'tea') =/ T
prices =. __ ". > t #~ _1 |. (<'price') =/ T

((/:prices) { teas) ,. (<@":"0 ] 50 * /:~ prices)
