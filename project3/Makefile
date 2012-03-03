Src=http://unbox.org/stuff/var/timm/12/310/src/

help :
	@echo "  "
	@echo "Initial install:"
	@echo "  1) mkdir newDirectory"
	@echo "  2) cd newDirectory"
	@echo "  3) wget $(Src)/Makefile --user guest --password guest"
	@echo "  4) make update"
	@echo ""
	@echo "Subsequent refresh:"
	@echo "  5) make update"

update:
	@echo " "
	@echo "Are you sure?  This may OVERWRITE updated files."
	@echo "Enter to proceed, control-c to abort"
	@read x
	@echo "Updating from $(Src) ..."
	@wget --quiet --no-parent --cut-dirs=6 --reject index.html \
          -nH -S -N -r --user guest --password guest \
          $(Src) 2> /dev/null
	@rm -f robots.txt 
	@chmod +x lithp submit

commit :
	@echo ""
	@echo "WARNING: this fails unless your name isn't timm or paulmcb!."	
	@echo " "
	@svn commit -m stuff

ed3101 :; 	cd 310.1 && emacs * &
ed3102 :; 	cd 310.2 && emacs * &
ed3103 :; 	cd 310.3 && emacs * &
