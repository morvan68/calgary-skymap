# Subversion $Id$

SVN_REPO=https://calgary-skymap.googlecode.com/svn/

svn-update:
	svn update

svn-commit:
	svn commit

svn-checkout:
	svn checkout ${SVN_REPO}trunk . --username brian.jackel@gmail.com

svn-checkout-all:
	svn checkout ${SVN_REPO} . --username brian.jackel@gmail.com

# Subversion $Id: datalogy.tex 195 2011-01-11 23:21:44Z bjackel $
svn-propset:
	svn propset svn:keywords "Id" $@
#svn-propset:
#	svn propset svn:keywords "Id URL Revision Date Author" $@

#svn propedit svn:ignore fig
#svn propset svn:ignore "*.aux *.bak *.bbl *.blg *.idx *.ilg *.ind *.lof *.log *.lot *.out *.pdf *.sav *.synctex *.toc" .
