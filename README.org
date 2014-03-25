=settings= has Emacs setting files(.el) for developers.
Furthermore, there are useful settings(Xcode, vim, XChat, cygwin, fonts on Linux).
In fact, =settings= is most personal.
If you need them, use them freely.
Give me feedback if you have any problems or issues.

* Quick setup
  Download =settings= in your home directory.
  : ~$ git clone git://github.com/jglee1027/settings.git

  run =install.sh=
  : ~$ cd ~/settings
  : ~/settings$ ./install.sh

* Features
** settings/emacs/jglee-lisp
*** j-dev-assit.el
**** Easily build in Emacs
	 If there is Makefile in your project, you can build a project
     easily by pressing =C-c c= in any subdirectory of your project.

**** Rapidly open a counterpart file
	 You can quickly open the counterpart of the current buffer by
     pressing =C-c j p=. In other words, if the current buffer is c or
     cpp source file, you can easily visit the header file related it
     by pressing =C-c j p=.
	 
**** Briefly find out lines containing a symbol
	 You can simply search positions which a symbol at point is used in
     your project by pressing =C-c j s=. It uses (grep-find).
	 
**** Quickly find a file
	 You can rapidly find a file in your project by pressing =C-c j f=.

**** Easily replace a string in several files
	 If you want to replace a string in several files,
	 press =C-c j 5= or =C-c j %=. You can easily replace a string in
     specified files in your project.
	 
**** Quickly visit a file in your project
	 You can easily open the file you want to visit in your project by
     pressing =C-c j i=. It supports incremental search like TextMate
     or Visual Assist. As you type text, one or more possible matches
     for the text are found and immediately displayed.
	 
**** Rapidly go to a function in the current buffer
	 Press =C-c j m= if you want to go to a function. You can see all
     functions defined in the current buffer. It supports incremental
     search like TextMate or Visual Assist.

**** Easily create a TAG in your project
	 Press =C-c j t= and you can easily create TAG file in your
     project using 'find' and 'etags'.

*** j-highlight.el
**** Highlight symbol
	 If you want to see highlighted symbol at point, press =C-c j h=.
	 After specified idle time, the current symbol at point is highlighted.
	 It only works in file buffer.
	 
* License
  It is distributed under the GNU General Public License.
  See the accompanying =GPL-3.0.txt= file for more details.

* Bug report
  Please use the issues tab to report any issues.
