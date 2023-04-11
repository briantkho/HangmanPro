:- use_module(library(pce)).

% check if character is member of list of guesses
member(H, [H|_]).
member(X, [_|T]) :- member(X,T).

% check if character is in a string
contains_char(Char, String) :-
    atom_chars(String, Chars),
    member(Char, Chars).

% atomic_list_concat(['h', 'e', 'l', 'l', 'o'], ',', String).

update_display(_, [], [], []).
update_display(Letter, [Letter|SolutionTail], [_|DisplayTail], [NewDisplayChar|NewDisplayTail]) :-
    NewDisplayChar = Letter,
    update_display(Letter, SolutionTail, DisplayTail, NewDisplayTail).
update_display(Letter, [SolutionChar|SolutionTail], [DisplayChar|DisplayTail], [DisplayChar|NewDisplayTail]) :-
    SolutionChar \== Letter,
    update_display(Letter, SolutionTail, DisplayTail, NewDisplayTail).

game_won(Display) :-
    not(contains_char('_', Display)).

format_attempts(Attempts, Result) :- atom_concat("Attempts left: ", Attempts, Result).
format_incorrect_guessed(_, "Stub for incorrect guessed").
%format_incorrect_guessed([], "Incorrect guesses: ").
%format_incorrect_guessed(IncorrectGuessed, Result) :-
%    atomic_list_concat(IncorrectGuessed, " ", String),
%    atom_concat("Incorrect guesses: ", String, Result).
format_display(Display, Result) :-
    atom_chars(Display, CharList),
    atomic_list_concat(CharList, " ", Result).




:- pce_begin_class(hangman_window, dialog, "Hangman Pro").

% Handle Guess Button
on_click_guess(Window, Solution, Display, Guessed, IncorrectGuessed, Attempts) :->
(Attempts > 0 ->
     (member(Letter, Guessed) ->
	  get(Window, member, loggerLabel, LoggerLabel),
          send(LoggerLabel, selection, 'You already guessed this letter!').
          ;
              (contains_char(Letter,Solution) ->
		   atom_chars(Solution, SolutionChars),
		   atom_chars(Display, DisplayChars),
		   update_display(Letter, SolutionChars, DisplayChars, NewDisplay),
		   atom_chars(NewDisplayString, NewDisplay),   
              ;
	           writeln("Unguessed!").
              )

     )
          ;
writeln("poo").
     ).


    %get(Window, member, progressLabel, ProgressLabel),
    %send(ProgressLabel, selection, 'Text changed after button click!').

% Init window
initialise(Window, Solution, Display, Guessed, IncorrectGuessed, Attempts) :->
    send(Window, send_super, initialise, "Hangman Pro"),
    format_attempts(Attempts, AttemptsLabelText),
    format_incorrect_guessed(IncorrectGuessed, IncorrectGuessedText),
    send(Window, append, new(LoggerLabel, label(loggerLabel, ''))),
    send(Window, append, new(ProgressLabel, label(progressLabel, Display))),
    send(Window, append, new(AttemptsLabel, label(attemptsLabel, AttemptsLabelText))),
    send(Window, append, new(IncorrectGuessedLabel, label(incorrectGuessedLabel, IncorrectGuessedText))),
    send(Window, append, new(InputField, text_item(inputField, ''))),
    send(Window, append, new(Button, button('Guess!'))),
    send(Button, message, message(Window, on_click_guess)),
    send(Window, open).


:- pce_end_class.



% Start the window
start :-
    new(Window, hangman_window("hello", "_____", [], [], 4)),
    send(Window, open).

