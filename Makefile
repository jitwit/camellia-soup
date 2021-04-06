.PHONY : tea-time clean

tea-time : tea.ijs data/teas.txt
	jconsole -js "load '$<'" \
		"echo gogo 'wulong'" \
		"echo gogo 'blanc'" \
		"exit 0"

data/teas.txt : scrape.rkt
	rm -rf data/
	racket $<

clean :
	rm -rf data
