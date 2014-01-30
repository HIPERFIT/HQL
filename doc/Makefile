filename=hql

NO_COLOR=\x1b[0m
OK_COLOR=\x1b[32;01m
ERROR_COLOR=\x1b[31;01m
WARN_COLOR=\x1b[33;01m

OK_STRING=$(OK_COLOR)[OK]$(NO_COLOR)
ERROR_STRING=$(ERROR_COLOR)[ERRORS]$(NO_COLOR)
WARN_STRING=$(WARN_COLOR)[WARNINGS]$(NO_COLOR)

define colorecho
      @tput setaf 6
      @echo "-->" $1
      @tput sgr0
endef

define pdfdone
      @tput bold
	  @tput setaf 2
      @echo $1
      @tput sgr0
endef

define psdone
      @tput bold
	  @tput setaf 3
      @echo $1
      @tput sgr0
endef

define starting
      @tput bold
	  @tput setaf 3
      @echo $1
      @tput sgr0
endef

.PHONY : pdf


#all: ps
#	$(call colorecho,"Preparing document" $(filename))
#	@pdflatex ${filename}.tex > ex2_output.txt 2>&1
#	$(call doneecho,"Success! Wrote "$(filename)".pdf")

pdf: ps
	@ps2pdf ${filename}.ps > ex2_output.txt 2>&1
	$(call pdfdone,"Success! Wrote "$(filename)".pdf")

ps: dvi
	@dvips ${filename}.dvi > ex2_output.txt 2>&1
	$(call psdone,"Success! Wrote "$(filename)".ps")
dvi:
	@latex -shell-escape ${filename}.tex
	$(call colorecho,"latex ")
	

	$(call colorecho,"bibtex ")
	@bibtex ${filename}

	$(call colorecho,"latex")
	@latex -shell-escape ${filename}	

	$(call colorecho,"makeglossaries")
	@makeglossaries ${filename}

	$(call colorecho,"makeindex")
	@makeindex ${filename}

	$(call colorecho,"latex")
	@latex -shell-escape ${filename}

medit:
	@$(ECHO) -n compiling debug foo.cpp...
	mate *.tex

ledit:
	@tput setaf 6
	@echo "apa"
	@tput sgr0
	@echo "hej"
	$(call colorecho,"Linking with")

readpdf:
	open ${filename}.pdf &

clean:
	rm -f *.ps *.pdf *.log *.aux *.out *.dvi *.bbl *.blg *.aux *.lot *.lof *.toc *.xdy *.gls *.glo *.glg *.acn *.idx *.ist
bib:
	bibtex ${filename} > ex2_output.txt 2>&1

glossaries:
	makeglossaries ${filename} > ex2_output.txt 2>&1
