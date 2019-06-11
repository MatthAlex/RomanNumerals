ford_verbose_ = $(ford_verbose_@AM_DEFAULT_V@)
ford_verbose_0 = @echo "  FORD     $<";
ford_verbose = $(ford_verbose_@AM_V@)

ford_quiet_ = $(ford_quiet_@AM_DEFAULT_V@)
ford_quiet_0 = -q
ford_quiet = $(ford_quiet_@AM_V@)

.NOTPARALLEL:
VPATH = ${srcdir}/../src

doccssdir=${htmldir}/css
docmoddir=${htmldir}/module
docsrcdir=${htmldir}/src
docsrcfdir=${htmldir}/sourcefile
doclstdir=${htmldir}/lists
docprcdir=${htmldir}/proc
docprgdir=${htmldir}/program
docfntdir=${htmldir}/fonts
doctipdir=${htmldir}/tipuesearch
doctimgdir=${htmldir}/tipuesearch/img
docjsdir=${htmldir}/js

docmod_DATA=\
   ford/module/parameters.html\
   ford/module/routines.html\
   ford/module/RomanNum.html

docsrc_DATA=\
   ford/src/parameters.f90\
   ford/src/routines.f90\
   ford/src/RomanNum.f90

docsrcf_DATA=\
   ford/sourcefile/parameters.f90.html\
   ford/sourcefile/routines.f90.html\
   ford/sourcefile/RomanNum.f90.html

doclst_DATA=\
   ford/lists/files.html\
   ford/lists/modules.html\
   ford/lists/procedures.html

doc_DATA=\
   ford/favicon.png\
   ford/index.html\
   ford/search.html

docprc_DATA=\
   ford/proc/check_string_valid.html\
   ford/proc/verify_string.html\
   ford/proc/buffer_to_string.html\
   ford/proc/number_to_numeral.html\
   ford/proc/numeral_to_number.html\
   ford/proc/to_upper.html

docprg_DATA=
doccss_DATA=\
   ford/css/bootstrap.css\
   ford/css/bootstrap.min.css\
   ford/css/font-awesome.css\
   ford/css/font-awesome.min.css\
   ford/css/local.css\
   ford/css/pygments.css

doctip_DATA=\
   ford/tipuesearch/tipuesearch_content.js\
   ford/tipuesearch/tipuesearch.css\
   ford/tipuesearch/tipuesearch.js\
   ford/tipuesearch/tipuesearch.min.js\
   ford/tipuesearch/tipuesearch_set.js

doctimg_DATA=\
   ford/tipuesearch/img/loader.gif\
   ford/tipuesearch/img/search.png

docjs_DATA=\
   ford/js/bootstrap.js\
   ford/js/bootstrap.min.js\
   ford/js/ie10-viewport-bug-workaround.js\
   ford/js/jquery-2.1.3.min.js\
   ford/js/svg-pan-zoom.min.js

docfnt_DATA=\
   ford/fonts/fontawesome-webfont.eot\
   ford/fonts/fontawesome-webfont.svg\
   ford/fonts/fontawesome-webfont.ttf\
   ford/fonts/fontawesome-webfont.woff\
   ford/fonts/glyphicons-halflings-regular.eot\
   ford/fonts/glyphicons-halflings-regular.svg\
   ford/fonts/glyphicons-halflings-regular.ttf\
   ford/fonts/glyphicons-halflings-regular.woff

ALLDOC=\
   ${doc_DATA} ${docmod_DATA} ${doccss_DATA} ${docsrc_DATA} ${docsrcf_DATA}\
   ${doclst_DATA} ${docprc_DATA} ${docprg_DATA} ${docfnt_DATA} ${doctip_DATA}\
   ${doctimg_DATA} ${docjs_DATA}

$(ALLDOC): % : ../taskrev-ford.md
	${ford_verbose}$(FORD) ${ford_quiet} $<

$(docsrc_DATA): ford/src/%: %

CLEANFILES=$(ALLDOC)
