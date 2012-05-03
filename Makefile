# Subversion $Id$

SVN_REPO=https://calgary-skymap.googlecode.com/svn/

svn-update:
	svn update

svn-commit:
	svn commit

svn-checkout-skymap:
	svn checkout ${SVN_REPO}trunk/skymap . --username brian.jackel@gmail.com

svn-checkout-all:
	svn checkout ${SVN_REPO} . --username brian.jackel@gmail.com

# Subversion $Id: datalogy.tex 195 2011-01-11 23:21:44Z bjackel $
svn-propset:
	svn propset svn:keywords "Id" $@
#svn-propset:
#	svn propset svn:keywords "Id URL Revision Date Author" $@

IGNORE.LATEX="*.aux *.bak *.bbl *.blg *.dvi *.idx *.ilg *.ind *.lof *.log *.lot *.out *.sav *.synctex *.toc"
svn-ignore-latex: 
	cd skymap/old/csa_report.2002 && svn propset svn:ignore ${IGNORE.LATEX} .

wget http://www.gnu.org/licenses/gpl-3.0.txt >> COPYING

# try for good ASCII under linux and Windows
svn propset svn:eol-style native
#svn propedit svn:ignore fig
#svn propset svn:ignore "*.aux *.bak *.bbl *.blg *.idx *.ilg *.ind *.lof *.log *.lot *.out *.pdf *.sav *.synctex *.toc" .
