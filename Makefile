DIRS = guess phone

BUILDDIRS = $(DIRS:%=build-%)
CLEANDIRS = $(DIRS:%=clean-%)

all: $(BUILDDIRS)

guess: build-guess
phone: build-phone

$(BUILDDIRS):
	$(MAKE) -C $(@:build-%=%)

clean: $(CLEANDIRS)
$(CLEANDIRS): 
	$(MAKE) -C $(@:clean-%=%) clean

.PHONY: subdirs $(DIRS)
.PHONY: subdirs $(BUILDDIRS)
.PHONY: subdirs $(CLEANDIRS)
.PHONY: all clean
