.PHONY: watch all

all: 
	stack test 

watch:
	stack test --file-watch

clean:
	$(RM) -r .stack-work

