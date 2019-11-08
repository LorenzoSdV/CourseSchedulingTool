# CS 3110 Final Project 

**Members:** Chris O'Brian (co253), Lorenzo Scotti di Vettimo (ls769), Radha Patel (rdp89)


## Installation Instructions
---
Our system requires the "Ocurl" package to function, to install, please run:

```
opam install ocurl
```

After which, you should run "make check" to ensure that your environment is what it should be. We've edited the checkenv.sh file to reflect our system.

Then, you can start our program by running "make run". Press Enter/return at the main prompt for help on learning which commands can be executed. 

## Some example commands:

To create a new semester(s):
```
add sem FA19
add sem SP19
```

Add a new course:
```
add CS3110 4 A- CScore FA19 
```

Add a new course (and have Class Roster get credits info):
```
add CS3410 B CScore FA19 
```

View Current Schedule:
```
print
```

Edit course attribute (like credits):
_Notice how GPA changes with this (by running print again)_
```
edit CS3110 credits 3
```

Remove a course:
```
remove CS3110
```

Exit:
```
quit
```