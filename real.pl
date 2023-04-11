% importing libraries
:- use_module(library(random)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(random)).
:- use_module(library(apply)).

% member(A, L); check if A is a member of list L
member(H, [H|_]).
member(X, [_|T]) :- member(X,T).

% check if Char is an element of String
contains_char(Char, String) :-
    atom_chars(String, Chars),
    member(Char, Chars).

% Prints letters in a list one-by-one with a space delimeter.
printLetters([]).
printLetters([H|T]) :- write(H), write(" "), printLetters(T).

% Draw hangman ASCII art at state 6
draw_hangman(6) :-
    writeln("  .---."),
    writeln("  |   |"),
    writeln("      |"),
    writeln("      |"),
    writeln("      |"),
    writeln("      |"),
    writeln("=========").

% Draw hangman ASCII art at state 5
draw_hangman(5) :-
    writeln("  .---."),
    writeln("  |   |"),
    writeln("  o   |"),
    writeln("      |"),
    writeln("      |"),
    writeln("      |"),
    writeln("=========").

% Draw hangman ASCII art at state 4
draw_hangman(4) :-
    writeln("  .---."),
    writeln("  |   |"),
    writeln("  o   |"),
    writeln("  |   |"),
    writeln("      |"),
    writeln("      |"),
    writeln("=========").

% Draw hangman ASCII art at state 3
draw_hangman(3) :-
    writeln("  .---."),
    writeln("  |   |"),
    writeln("  o   |"),
    writeln(" /|   |"),
    writeln("      |"),
    writeln("      |"),
    writeln("=========").

% Draw hangman ASCII art at state 2
draw_hangman(2) :-
    writeln("  .---."),
    writeln("  |   |"),
    writeln("  o   |"),
    writeln(" /|\\  |"),
    writeln("      |"),
    writeln("      |"),
    writeln("=========").

% Draw hangman ASCII art at state 1
draw_hangman(1) :-
    writeln("  .---."),
    writeln("  |   |"),
    writeln("  o   |"),
    writeln(" /|\\  |"),
    writeln(" /    |"),
    writeln("      |"),
    writeln("=========").

% Draw hangman ASCII art at state 0 (losing state)
draw_hangman(0) :-
    writeln("  .---."),
    writeln("  |   |"),
    writeln("  o   |   < bruh."),
    writeln(" /|\\  |"),
    writeln(" / \\  |"),
    writeln("      |"),
    writeln("=========").

% Draw hangman ASCII art for wining emote
draw_winning_emote :-
    writeln("  \\O_/"),
    writeln("  ( - )  < Thanks for saving me!!!!!"),
    writeln("  _| |_"),
    writeln(""),
    draw_hangman(6),
    writeln("").

% Clean screen by spamming `\n` fifty times
clean_screen :-
    forall(between(1, 50, _), writeln("")).

% Update the display with the guessed letter
% update_display(RecentlyGuessedLetter, SolutionAsChars, CurrentDisplayAsChars, NewDisplay)
% Recursively compares the recently guessed character as being equal to a letter in the
% SolutionAsChars. If it is, then it will replace that position of the CurrentDisplayAsChars
% and bind that as the new display. 
update_display(_, [], [], []).
update_display(Letter, [Letter|SolutionTail], [_|DisplayTail], [NewDisplayChar|NewDisplayTail]) :-
    NewDisplayChar = Letter,
    update_display(Letter, SolutionTail, DisplayTail, NewDisplayTail).
update_display(Letter, [SolutionChar|SolutionTail], [DisplayChar|DisplayTail], [DisplayChar|NewDisplayTail]) :-
    SolutionChar \== Letter,
    update_display(Letter, SolutionTail, DisplayTail, NewDisplayTail).

% Checks if game has been won by ensuring no `_` variables have been set
game_won(Display) :-
    not(contains_char('_', Display)).

% Recursive hangman loop that runs the main game
% Solution is the game solution as a string
% Display is the display of the game as a string
% Guessed is a list of all guessed characters
% IncorrectGuessed is a subsset of `Guessed` that only include wrong guesses
% Attempts represents number of attempts left
hangman(Solution, Display, Guessed, IncorrectGuessed, Attempts) :-
    clean_screen,
    (Attempts > 0 -> % if you are still alive,
        writeln('------------'),
        write('Attempts left:\n'), draw_hangman(Attempts),% OR writeln(Attempts) [if support for lives\==6]
        write('\nIncorrect guesses: '), printLetters(IncorrectGuessed), write('\n'),
        write('Progress: '), writeln(Display),

        (game_won(Display) -> % if you win the game...
            clean_screen,
            draw_winning_emote,
            writeln('\nCongrats! You won!'),
            write('The word was: '), writeln(Solution)
	
        ; % if you haven't won the game yet...
 
            writeln('Enter next letter: '), read(Letter),
	
            (member(Letter, Guessed) -> % if already guessed... (do not reduce attempt count)
                writeln('Already guessed this letter, try again...\n\n'),
                hangman(Solution, Display, Guessed, IncorrectGuessed, Attempts) %%CALLBACK
            ;
	    
              (contains_char(Letter, Solution) -> % if correct guess...
                  atom_chars(Solution, SolutionChars),
                  atom_chars(Display, DisplayChars),
                  update_display(Letter, SolutionChars, DisplayChars, NewDisplay),
                  atom_chars(NewDisplayString, NewDisplay),
                  hangman(Solution, NewDisplayString, [Letter|Guessed], IncorrectGuessed, Attempts) %%CALLBACK
                   ; % if incorrect guess...
              writeln('Incorrect guess!'), 
              AttemptsNew is Attempts - 1,
              hangman(Solution, Display, [Letter|Guessed], [Letter|IncorrectGuessed], AttemptsNew) %%CALLBACK
              )
       )
 ; % below if no lives left (lose)
 clean_screen,
 draw_hangman(0),
 writeln('No attempts left.'),
 write('The word was: '), writeln(Solution)
 )
).

% Generates a list of underscores that correspond to the length of a word
make_underscores(Word, Underscores) :-
    atom_chars(Word, Chars),
    length(Chars, Length),
    length(UnderscoreList, Length),
    maplist(=('_'), UnderscoreList),
    atomic_list_concat(UnderscoreList, Underscores).

% Starts a game with 6 attempts and a KNOWN solution
start_rigged_game(Solution) :-
    make_underscores(Solution, DisplayInit),
    hangman(Solution, DisplayInit, [], [], 6). % default to six, but could be any (but art breaks)

% Loads JSON file from Path into a list of words
import_json_words(Path, WordsList) :-
    setup_call_cleanup(
        open(Path, read, InputStream),
        json_read_dict(InputStream, JsonExport),
        close(InputStream)
    ),
    maplist(atom_string, WordsList, JsonExport).

% Picks a random word from a list of words
pick_random_word_from_list(WordsList, RandomWord) :-
    length(WordsList, Length),
    random(0, Length, RandomIndex),
    nth0(RandomIndex, WordsList, RandomWord).

% Starts a game with 6 attempts using a random word found in 'words.json'
start_random_game :-
    import_json_words("words.json", WordsList),
    pick_random_word_from_list(WordsList, RandomWord),
    start_rigged_game(RandomWord).
    

