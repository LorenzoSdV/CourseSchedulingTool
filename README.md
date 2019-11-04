# FinalProject_3110

In degree_reqs/Eng_CS.json is what I am thinking for our JSON files specifying
degree requirements. It's not complete but hopefully you get the gist.



**Note from Chris:** I fucked around with Makefile and should now run "make build"
as long as everything is working. I moved all the files that are causing problems
into the "Xtra Stuff" folder. These either don't do anything useful for us or 
need to be seriously re-written. Probably not worth doing.

Also don't move .gitignore out of main directory...

Also going to need "ocurl" package.
Can install with "opam install ocurl" (I've set 'make check' to fail
if this package isn't detected!)